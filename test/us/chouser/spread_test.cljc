(ns us.chouser.spread-test
  (:require [us.chouser.spread :refer [k. keys. strs. syms.] :as s]
            [clojure.test :refer [deftest is use-fixtures]]))

(def a "alpha")
(def b "beta")
(def mcd {:c "charlie" :d "delta"})
(def mef {:e 3 :f 4})
(def m1234 [[1 2] [3 4]])

;; this ridiculous fixture is required to run the tests that use macroexpand in a test runner
;; https://github.com/technomancy/leiningen/issues/912
(let [ns *ns*]
  (use-fixtures
    :once
    (fn [test-fn]
      (binding [*ns* ns]
        (test-fn)))))

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
  (is (= {:a a :k 1} (k. a ~@{:k 1})))
  (is (= {:us.chouser.spread-test/a "alpha"} (k. us.chouser.spread-test/a)))
  (is (= {[1 2] "beta" :a "alpha"} (k. [1 2] b a)))
  (is (= {'{a b} "beta" :a "alpha"} (k. '{a b} b a)))
  (is (= {0 1, 2 3} (k. ~@(apply array-map (range 4)))))
  (is (= {:a "alpha", :b "beta", :c 3, "alpha" 5, :e 3, :f 4,
          10 11, 12 13,
          "a" "alpha", "b" "beta"}
         (k. a b :c 3 ~a 5 ~@mef
             ~@(apply array-map (range 10 14))
             ~@(strs. a b)))))

(deftest all-macros
  (is (= {"a" "alpha"} (strs. a)))
  (is (= {'a "alpha"} (syms. a)))
  (is (= (merge {:a a :b b} mef) (k.    a ~@mef b)))
  (is (= (merge {:a a :b b} mef) (keys. a ~@mef b)))
  (is (= (merge {"a" a "b" b} mef) (strs. a ~@mef b)))
  (is (= (merge {'a a 'b b} mef) (syms. a ~@mef b)))
  (is (= {:a "alpha", 'b "beta", "a" "alpha"}
         (keys. a ~@(syms. b) ~@(strs. a)))
      "should be composable"))

(deftest simplified
  (is (= '{:a a :b b} (macroexpand '(k. a b))))
  (is (= '{:a a :h "hotel"} (macroexpand '(k. :h "hotel", a))))
  (is (= `(into {} ~'m1234) (macroexpand '(k. ~@m1234))))
  (is (= `(into {} ~'mcd) (macroexpand '(k. ~@ mcd))))
  (is (= '{:a 1} (macroexpand '(k. ~@{:a 1}))))
  (is (= `(reduce into {} [{:a ~'a} {:b 2}]) (macroexpand '(k. a ~@{:b 2}))))
  (is (= `(reduce into {} [{:a ~'a} ~'mef]) (macroexpand '(k. a ~@mef)))))

(deftest combinations
  (is (= {'a "alpha", 'b "beta", 'c "beta", :s :t,
          :c "charlie", :d "delta", :e 3, :f 4, :more "stuff", :k 1, :p 0, :q 1, :r 2}
         (syms. a b 'c b ~@{:s :t} ~@mcd ~@mef :more "stuff" ~@{:k 1} ~@(map vector [:p :q :r] (range))))
      "should support weird combinations")
  (dotimes [_ 100]
    (let [words (->> '[[a] [b] [~@mcd] [~@mef] [:g "golf"] [:h "hotel"]]
                     (sort-by (fn [_] (rand)))
                     (apply concat))]
      (is (= (merge {:a a :b b :g "golf" :h "hotel"} mcd mef)
             (eval (list* 'k. words)))))))

(deftest errors
  (is (thrown-with-msg? Exception #"No value supplied"
                        (#'s/build-map '(k. a :x) keyword)))
  (is (thrown-with-msg? Exception #"ISeq from:.*Long"
                        (k. ~@(range 4)))))

(deftest corner-cases
  (is (= {} (k.))
      "Empty constructor should return an empty map")
  (is (= {} (k. ~@nil))
      "An expression that returns nil should result in an empty map")
  (is (= {} (k. ~@()))
      "An expression that returns empty seq should result in an empty map")
  (is (= {} (k. ~@nil))
      "An expression that returns nil should result in an empty map")
  (is (= {} (k. ~@[])))
  (is (= {:a "alpha"} (k. :a 1 a)))
  (is (= {:a 1} (k. a :a 1)))
  (is (= {:a 1} (k. ~:a b :a 1))))
