(ns stockscraper.core-test
  (:require [clojure.test :refer :all]
            [stockscraper.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(def s0 "this is a string")

(defn add-to-beg-end1
  "adds strings via successive variables"
  [s0]
  (let [s1 (str "A" s0 "B")
        s2 (str "C" s1 "D")
        s3 (str "E" s2 "F")]
    s3))

(add-to-beg-end1 "ZZZ")
;; => "ECAZZZBDF"


(defn add-to-beg-end2
  "adds strings via nesting"
  [s0]
  (str "E" (str "C" (str "A" s0 "B") "D") "F"))

(add-to-beg-end2 "ZZZ")
;; => "ECAZZZBDF"


(defn add-to-beg-end3
  "adds strings via pipe"
  [s0]
  (-> s0
      (as-> s (str "A" s "B"))
      (as-> s (str "C" s "D"))
      (as-> s (str "E" s "F"))))

(add-to-beg-end3 "ZZZ")
;; => "ECAZZZBDF"
