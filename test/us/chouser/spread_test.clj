(ns us.chouser.spread-test
  (:require [us.chouser.spread :refer [k. keys. strs. syms.] :as s]
            [clojure.test :refer [test-ns deftest is]]))

(def a "alpha")
(def b "beta")
(def mcd {:c "charlie" :d "delta"})
(def mef {:e 3 :f 4})
(def m1234 [[1 2] [3 4]])

(deftest basic-keywords
  (is (= {:a a} (k. a)))
  (is (= {:a a :b b} (k. a b)))
  (is (= (merge {:a a :b b} mef) (k. a ~@mef b)))
  (is (= (merge mcd {:a a} mef) (k. ~@ mcd a ~@mef)))
  (is (= (merge {:h "hotel"} mef) (k. :h "hotel" ~@ mef)))
  (is (= {:h "hotel" :e 3} (k. :h "hotel" ~@(dissoc mef :f))))
  (is (= mef (k. :e 9 ~@mef)))
  (is (= {:e 9 :f 4} (k. ~@mef :e 9)))
  (is (= {"alpha" "beta"} (k. ~a b)))
  (is (= {1 2 3 4} (k. ~@m1234)))
  (is (= {1 2 3 4} (k. ~@[[1 2] [3 4]])))
  (is (= {:s 0 :p 1 :q 2 :r 3} (k. ~@(map vector [:s :p :q :r] (range)))))
  (is (= {:us.chouser.spread-test/a "alpha"} (k. us.chouser.spread-test/a))))

(deftest all-macros
  (is (= (merge {:a a :b b} mef) (k.    a ~@mef b)))
  (is (= (merge {:a a :b b} mef) (keys. a ~@mef b)))
  (is (= (merge {"a" a "b" b} mef) (strs. a ~@mef b)))
  (is (= (merge {'a a 'b b} mef) (syms. a ~@mef b))))

(deftest simplified
  (is (= '{:a a :b b} (macroexpand '(k. a b))))
  (is (= '{:a a :h "hotel"} (macroexpand '(k. :h "hotel", a))))
  (is (= `(into {} ~'m1234) (macroexpand '(k. ~@m1234))))
  (is (= `(into {} ~'mcd) (macroexpand '(k. ~@ mcd))))
  (is (= `(into {} ~'mcd) (macroexpand '(k. ~@mcd)))))

(deftest combinations
  (dotimes [_ 100]
    (let [words (->> '[[a] [b] [~@mcd] [~@mef] [:g "golf"] [:h "hotel"]]
                     (sort-by (fn [_] (rand)))
                     (apply concat))]
      (is (= (merge {:a a :b b :g "golf" :h "hotel"} mcd mef)
             (eval (list* 'k. words)))))))

(deftest errors
  (is (thrown-with-msg? Exception #"No value supplied"
                        (#'s/build-map '(k. a :x) keyword))))

(deftest corner-cases
  (is (= {} (k.))
      "Empty constructor should return an empty map")
  (is (= {} (k. ~@nil))
      "An expression that returns nil should result in an empty map")
  (is (= {} (k. ~@()))
      "An expression that returns empty seq should result in an empty map")
  (is (= {} (k. ~@nil))
      "An expression that returns nil should result in an empty map")
  (is (= {} (k. ~@[]))))
