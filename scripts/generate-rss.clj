#!/usr/bin/env bb

(ns generate-rss
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant ZoneOffset ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.util Locale)))

(def API_URL "https://gateway.36kr.com/api/mis/nav/home/nav/rank/hot")
(def HOME_URL "https://36kr.com/")

(defn env-number [name fallback]
  (let [raw (System/getenv name)]
    (if (or (nil? raw) (str/blank? raw))
      fallback
      (let [n (try (Long/parseLong raw) (catch Exception _ ::bad))]
        (if (= n ::bad) fallback n)))))

(def FETCH_RETRY_MAX (env-number "FETCH_RETRY_MAX" 3))
(def FETCH_RETRY_BASE_DELAY_MS (env-number "FETCH_RETRY_BASE_DELAY_MS" 500))
(def FETCH_RETRY_MAX_DELAY_MS (env-number "FETCH_RETRY_MAX_DELAY_MS" 8000))

(defn sleep-ms [ms]
  (Thread/sleep (long ms)))

(defn clamp [n lo hi]
  (max lo (min hi n)))

(defn backoff-delay-ms [attempt]
  (let [exp (* FETCH_RETRY_BASE_DELAY_MS (long (Math/pow 2 (double (dec attempt)))))
        jitter (* exp (+ 0.5 (rand)))]
    (clamp (long (Math/floor jitter)) 0 FETCH_RETRY_MAX_DELAY_MS)))

(defn with-retry [f {:keys [max-attempts label]}]
  (let [attempts (max 1 (long (or max-attempts 1)))
        op-label (or label "operation")]
    (loop [attempt 1
           last-err nil]
      (let [result (try
                     {:ok (f)}
                     (catch Exception err
                       {:err err}))]
        (if-let [v (:ok result)]
          v
          (let [err (:err result)]
            (if (>= attempt attempts)
              (throw (or err last-err))
              (let [delay (backoff-delay-ms attempt)
                    msg (or (.getMessage err) (str err))]
                (binding [*out* *err*]
                  (println (format "%s failed (attempt %d/%d), retry in %dms: %s"
                                   op-label attempt attempts delay msg)))
                (sleep-ms delay)
                (recur (inc attempt) err)))))))))

(defn escape-xml [s]
  (-> (str (or s ""))
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&apos;")))

(defn cdata-safe [s]
  (-> (str (or s ""))
      (str/replace "]]>" "]]&gt;")))

(def ^DateTimeFormatter rfc1123
  (-> (DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss 'GMT'" Locale/ENGLISH)))

(defn ->pubdate [epoch-ms]
  (let [inst (if (number? epoch-ms) (Instant/ofEpochMilli (long epoch-ms)) (Instant/now))
        zdt  (ZonedDateTime/ofInstant inst ZoneOffset/UTC)]
    (.format rfc1123 zdt)))

(defn image-mime [url]
  (let [path (-> (str url) (str/split #"\?") first str/lower-case)]
    (cond
      (str/ends-with? path ".png") "image/png"
      (or (str/ends-with? path ".jpg") (str/ends-with? path ".jpeg")) "image/jpeg"
      (str/ends-with? path ".webp") "image/webp"
      :else "image/jpeg")))

(defn fetch-hot-rank []
  (with-retry
    (fn []
      (let [payload {:partner_id "wap"
                     :timestamp (long (quot (System/currentTimeMillis) 1000))
                     :param {:siteId 1 :platformId 2}}
            res (http/post API_URL
                           {:headers {"Content-Type" "application/json"
                                      "User-Agent" "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Mobile Safari/537.36"}
                            :body (json/generate-string payload)
                            :timeout 15000
                            :as :string})
            status (:status res)
            body (:body res)]
        (when-not (<= 200 status 299)
          (throw (ex-info (format "拉取失败: %s" status) {:status status :body body})))
        (let [parsed (json/parse-string body true)]
          (when-not (= 0 (:code parsed))
            (throw (ex-info (format "接口返回 code=%s" (:code parsed)) {:response parsed})))
          (let [hot (get-in parsed [:data :hotRankList])]
            (when-not (sequential? hot)
              (throw (ex-info "未找到 data.hotRankList，接口结构可能变更" {:response parsed})))
            hot))))
    {:max-attempts FETCH_RETRY_MAX
     :label (str "fetch " API_URL)}))

(defn build-rss [hot-list]
  (let [now (->pubdate (System/currentTimeMillis))
        items (->> hot-list
                   (take 8)
                   (map (fn [it]
                          (let [item-id (:itemId it)
                                tm (:templateMaterial it)
                                title (:widgetTitle tm)
                                link (str "https://36kr.com/p/" item-id)
                                guid link
                                author (:authorName tm)
                                pub-ms (or (:publishTime tm) (:publishTime it))
                                img (:widgetImage tm)
                                stat-read (:statRead tm)
                                stat-praise (:statPraise tm)
                                stat-comment (:statComment tm)
                                stat-collect (:statCollect tm)
                                desc-parts (cond-> []
                                             (seq author) (conj (str "作者：" author))
                                             (number? stat-read) (conj (str "阅读：" stat-read))
                                             (number? stat-praise) (conj (str "点赞：" stat-praise))
                                             (number? stat-comment) (conj (str "评论：" stat-comment))
                                             (number? stat-collect) (conj (str "收藏：" stat-collect)))
                                desc (str/join " | " desc-parts)
                                enclosure (when (and (string? img) (not (str/blank? img)))
                                            (format "\n      <enclosure url=\"%s\" type=\"%s\" />"
                                                    (escape-xml img) (image-mime img)))]
                            (str
                              "    <item>\n"
                              "      <title>" (escape-xml title) "</title>\n"
                              "      <link>" (escape-xml link) "</link>\n"
                              "      <guid isPermaLink=\"true\">" (escape-xml guid) "</guid>\n"
                              "      <pubDate>" (escape-xml (->pubdate pub-ms)) "</pubDate>\n"
                              "      <author>" (escape-xml (or author "")) "</author>\n"
                              "      <description><![CDATA[" (cdata-safe desc) "]]></description>"
                              (or enclosure "")
                              "\n"
                              "    </item>"))))
                   (str/join "\n"))]
    (str
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rss version=\"2.0\">\n"
      "  <channel>\n"
      "    <title>" (escape-xml "36氪 - 热榜前8") "</title>\n"
      "    <link>" (escape-xml HOME_URL) "</link>\n"
      "    <description>" (escape-xml "来自 gateway.36kr.com 的 hotRankList（前8）") "</description>\n"
      "    <lastBuildDate>" (escape-xml now) "</lastBuildDate>\n"
      items "\n"
      "  </channel>\n"
      "</rss>\n")))

(defn repo-root []
  (let [script-path (or (System/getProperty "babashka.file") *file*)
        f (io/file script-path)
        scripts-dir (.getParentFile f)]
    (.getParentFile scripts-dir)))

(defn arg->out [args]
  (some (fn [a]
          (when (str/starts-with? a "--out=")
            (subs a (count "--out="))))
        args))

(defn -main [& args]
  (let [out (or (arg->out args) "rss.xml")
        out-file (io/file (repo-root) out)
        hot (fetch-hot-rank)
        rss (build-rss hot)]
    (.mkdirs (.getParentFile out-file))
    (spit out-file rss)
    (println (format "Wrote %s (%d bytes)" out (count (.getBytes rss "UTF-8"))))))

(try
  (apply -main *command-line-args*)
  (catch Exception err
    (binding [*out* *err*]
      (println (or (.getMessage err) (str err)))
      (when-let [d (ex-data err)]
        (println (pr-str d))))
    (System/exit 1)))
