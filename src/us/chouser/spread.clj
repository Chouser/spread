(ns us.chouser.spread
  (:require [clojure.core.reducers :as r]))

(defn ^:private collect-expr
  "Conj expr onto the vector exprs, unless expr is a literal map that can be
  merged into a existing map at the end of the vector."
  [exprs expr]
  (if (and (map? (peek exprs)) (map? expr))
    (conj (pop exprs) (merge (peek exprs) expr))
    (conj exprs expr)))

(defn ^:private first= [x expr]
  (and (seq? expr) (= x (first expr))))

(defn ^:private map-exprs
  "Convert the given macro invocation form into a vector of expressions that
  evaluate to maps. Bare symbols are converted to maps with one entry with a val
  of the symbol and a key converted by key-fn."
  [form key-fn]
  (loop [[k maybe-val :as args] (rest form),
         maps []]
    (cond
      (empty? args) maps
      (symbol? k) (recur (next args) (collect-expr maps {(key-fn k) k}))
      (first= `unquote-splicing k) (recur (next args)
                                          (collect-expr maps (second k)))
      (first= `unquote k) (recur (nnext args)
                                 (collect-expr maps {(second k) maybe-val}))
      (next args) (recur (nnext args) (collect-expr maps {k maybe-val}))
      :else (throw (ex-info (format "No value supplied for key %s" (pr-str k))
                            {:id ::no-value :key k :form form})))))

(defn ^:private build-map
  [form key-fn]
  (let [maps (map-exprs form key-fn)
        m (first maps)]
    (cond
      (next maps) `(into {} (r/mapcat seq [~@maps]))
      (map? m) m
      (seq maps) `(into {} ~m)
      :else {})))

(defmacro k.    [& args] (build-map &form keyword))
(defmacro keys. [& args] (build-map &form keyword))
(defmacro strs. [& args] (build-map &form str))
(defmacro syms. [& args] (build-map &form (partial list 'quote)))
