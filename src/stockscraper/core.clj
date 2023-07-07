"
stockscraper is used to identify vegan stocks on various exchanges.
"

(ns stockscraper.core
  "namespace for stockscraper with required libraries"
  (:require
   [clojure.string :as s]
   [hiccup.core :as hc]
   [net.cgrand.enlive-html :as html]
   [tech.ml.dataset :as ds]
   [tech.v2.datatype :as dtype]
   [tech.v2.datatype.functional :as dfn]))


;; useful functions

(defn kwd-read
  "reads a keword file given filename using techascent"
  [filepath]
  (:list (ds/rename-columns (ds/->dataset filepath {:header-row? false})
                            {"column-0" :list})))

(defn join-regex
  "joins regex strings: patterns -> str -> re-pattern"
  [& patterns]
  (re-pattern (apply str
                     (map #(str %) patterns))))


;; definitions of keywords and exchanges

(def mays
  (kwd-read "resources/may.txt"))
(def nays
  (kwd-read "resources/nay.txt"))
(def yays
  (kwd-read "resources/yay.txt"))
(def exchs
  ["AMEX" "ASX" "HKEX" "LSE" "NASDAQ" "NYSE" "SGX" "TSX" "TSXV"])


;; get the descriptions from the yahoo finance profiles of a stock

(defn mkurl
  "creates a url on finance.yahoo using stock symbol"
  [sym]
  (str "https://finance.yahoo.com/quote/" sym "/profile"))

(defn slurper
  "keeps on slurping bypassing 503 error"
  [url]
  (try
    (slurp url)
    (catch Exception e (prn (str "ERROR: " (.toString e))))))

(defn fetch-url
  "parses to tagsoup using the html txt from url"
  [url]
  (loop [txt (slurper url)]
    (if (not-empty txt)
      (html/html-snippet txt)
      (do
        (prn "sleeping 6 seconds")
        (Thread/sleep 6000)
        (recur (slurper url))))))

(defn pull-desc
  "pulls the description of the stock from the parsed text"
  [parsedtxt]
  (html/text
   (first (html/select parsedtxt
                       [(html/attr-contains :class "Mt(15px) Lh(1.6)")]))))

(defn pull-comp
  "pulls the company name of the stock from the parsed text"
  [parsedtxt]
  (let [txt (first (html/texts (html/select parsedtxt [:h1])))
        res (cond
              (nil? txt) "NIL"
              (= txt "") "THIS - IS UNKNOWN"
              :else txt)]
      (s/replace res #".*- " "")))

(defn stock-desc
  "returns stock description when possible or blank string"
  [symb comp]
  (let [txt (fetch-url (mkurl symb))
        title (first (map html/text (html/select txt [:title])))
        lookup? (re-find #"^Symbol Lookup" title)]
    (pr symb)
    (if-not lookup?
      (pull-desc txt)
      "")))


;; dataset processing

(defn countall
  "given (empty) desc and keywords finds count of all word occurences of latter"
  [desc ays]
  (loop [tot 0
         ays ays]
    (if (empty? ays)
      tot
      (recur
       (+ tot (count
               (re-seq (re-pattern
                        (join-regex "\\s" (first ays) "[\\s,.?!]")) desc))) ;confirm it is a word
       (rest ays)))))

(defn mk-kwd-v
  "given desc and kwds produces kwd vector of all matches"
  [desc ays]
  (loop [kv []
         ays ays]
    (if (empty? ays)
      (flatten (remove nil? kv))
      (recur
       (conj kv
             (re-seq (re-pattern
                      (join-regex "\\b" "(?i)" (first ays) "\\b"))
                     desc))
       (rest ays)))))

#_(defn ds-filtered
  "create the ds filtering out blank desc"
  [ds]
  (let [ds0 (ds/assoc ds :desc
                   (map stock-desc (ds :symbol) (ds :company)))
        ds1 (ds/filter-column #(not= "" %) :desc ds0)
        ds2 (ds/assoc ds1 :yay
                   (map #(mk-kwd-v % yays) (ds1 :desc)))
        ds3 (ds/assoc ds2 :nay
                   (map #(mk-kwd-v % nays) (ds2 :desc)))
        ds4 (ds/assoc ds3 :yayk
                   (map count (ds3 :yay)))
        ds5 (ds/assoc ds4 :nayk
                   (map count (ds4 :nay)))
        ds6 (ds/assoc ds5 :may
                   (map #(mk-kwd-v % mays) (ds5 :desc)))
        ds7 (ds/assoc ds6 :mayk
                      (map count (ds6 :may)))
        ds8 (ds/assoc ds7 :wmay
                      (map set (ds7 :may)))
        ds9 (ds/assoc ds8 :wnay
                      (map set (ds8 :nay)))
        dsA (ds/assoc ds9 :wyay
                      (map set (ds8 :yay)))]
    (->> dsA
         (ds/sort-by-column :mayk >)
         (ds/sort-by-column :yayk >)
         (ds/sort-by-column :nayk <))))

(defn ds-filtered
  "create the ds filtering out blank desc"
  [ds]
  (-> ds
      (as-> d (ds/assoc d :desc (map stock-desc (d :symbol) (d :company))))
      (as-> d (ds/filter-column #(not= "" %) :desc d))
      (as-> d (ds/assoc d :yay (map #(mk-kwd-v % yays) (d :desc))))
      (as-> d (ds/assoc d :nay (map #(mk-kwd-v % nays) (d :desc))))
      (as-> d (ds/assoc d :may (map #(mk-kwd-v % mays) (d :desc))))
      (as-> d (ds/assoc d :yayk (map count (d :yay))))
      (as-> d (ds/assoc d :nayk (map count (d :nay))))
      (as-> d (ds/assoc d :mayk (map count (d :may))))
      (as-> d (ds/assoc d :wyay (map set (d :yay))))
      (as-> d (ds/assoc d :wnay (map set (d :nay))))
      (as-> d (ds/assoc d :wmay (map set (d :may))))
      (as-> d (->> d
                  (ds/sort-by-column :mayk >)
                  (ds/sort-by-column :yayk >)
                  (ds/sort-by-column :nayk <)))))

(defn is-a-stock
  "identifies if a stock from company description"
  [comp]
  (when-not (re-find #"ETF|ETN|Ind|Bond|Proshares" comp)
    true))

#_(defn ds-tsv
  "prints tsv file of dataset"
  [ds filname]
  (ds/write-csv!
   (ds/select-columns ds [:symbol :company :yayk :nayk :mayk :yay :nay :may :desc])
   filname))

(defn ds-tsv
  "prints tsv file of dataset"
  [ds filname]
  (ds/write-csv!
   (ds/select-columns ds [:symbol :company :yayk :nayk :mayk :wyay :wnay :wmay :desc])
   filname))


(defn clean-up
  "removes missing rows from dataset"
  [ds]
  (ds/remove-rows
   ds
   (ds/missing ds)))

(defn ds-final
  "creates dataset for an exchange"
  [exch]
  (let [exch-fil (str "resources/" exch ".txt")
        exch-tsv (str "resources/" exch ".tsv")
        full-list (-> (ds/->dataset exch-fil)
                      (ds/rename-columns {"Symbol" :symbol
                                          "Description" :company})
                      (clean-up))
        stocks-list (ds/filter-column is-a-stock :company full-list)
        filtered-stocks (ds-filtered stocks-list)]
    (ds-tsv filtered-stocks exch-tsv)
    filtered-stocks
    ))


;; html page generation

(defn color-me
  "colors the ays in the description"
  [txt ays color]
  (loop [txt txt
         yn (set ays)]
    (if (empty? yn)
      txt
      (recur
       (s/replace txt
                  (join-regex "\\b(" (first yn) ")\\b")
                  (hc/html [:span {:style (str "background-color:" color)} "$1"]))
       (rest yn)))))

(defn exch-html
  "creates the html stock list for an exchange"
  [rows]
  (hc/html [:table {:border 1 :style "text-align:center"}
            (for [row rows]
              (let [sym (:symbol row)
                    com (:company row)
                    yak (:yayk row)
                    nak (:nayk row)
                    mak (:mayk row)
                    des (color-me
                         (color-me
                          (color-me (:desc row)
                                    (:yay row) "lightgreen")
                          (:nay row) "lightpink")
                         (:may row) "lightgrey")
                    url (str "https://finance.yahoo.com/quote/" sym)]
                (conj [:tr
                       [:td [:a {:href url} sym]]
                       [:td com]
                       [:td {:style "background-color:lightgreen"} yak]
                       [:td {:style "background-color:lightpink"} nak]
                       [:td {:style "background-color:lightgrey"} mak]
                       [:td des]]
                      ;; [:tr [:td {:colspan 5 :style "text-align:justify"} des]]
                      )))]))

(defn main
  "given an exchange does all processing and outputs htmlpage"
  [exch]
  (let [exchange-ds (ds-final exch)
        rows-from-ds (ds/mapseq-reader exchange-ds)
        htmlpage (exch-html rows-from-ds)]
    (spit (str "resources/" exch ".html") htmlpage)))


(def pfm (comp pull-comp fetch-url mkurl))
(defn otc2exch
  "writes otc file in exch format"
  [otc]
  (let [otcfile (str "resources/" otc ".txt")
        otcds (ds/->dataset otcfile)
        altered (ds/assoc otcds "Description"
                          (map pfm (otcds "Symbol")))]
    (ds/write-csv! altered otcfile {:separator \tab})
    ))

(defn tsvein
  "given an exchange does processing from the tsv files"
  [exch]
  (let [ds1 (-> (ds/->dataset (str "resources/" exch ".tsv"))
                (ds/rename-columns {"symbol" :symbol
                                    "company" :company
                                    "yayk" :yayk
                                    "nayk" :nayk
                                    "mayk" :mayk
                                    "yay" :yay
                                    "nay" :nay
                                    "may" :may
                                    "desc" :desc}))
        ds2 (assoc ds1 :yay
                   (map #(mk-kwd-v % yays) (ds1 :desc)))
        ds3 (assoc ds2 :nay
                   (map #(mk-kwd-v % nays) (ds2 :desc)))
        ds4 (assoc ds3 :yayk
                   (map count (ds3 :yay)))
        ds5 (assoc ds4 :nayk
                   (map count (ds4 :nay)))
        ds6 (assoc ds5 :may
                   (map #(mk-kwd-v % mays) (ds5 :desc)))
        ds7 (assoc ds6 :mayk
                   (map count (ds6 :may)))
        rows-from-ds (ds/mapseq-reader (->> ds7
                                            (ds/sort-by-column :mayk >)
                                            (ds/sort-by-column :yayk >)
                                            (ds/sort-by-column :nayk <)))
        htmlpage (exch-html rows-from-ds)]
   (spit (str "resources/" exch "-onerow.html") htmlpage)))


