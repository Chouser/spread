(ns us.chouser.spread)

(defn ^:private collect-expr [exprs expr]
  (if (and (map? (peek exprs)) (map? expr))
    (conj (pop exprs) (merge (peek exprs) expr))
    (conj exprs expr)))

(defn ^:private build-map [form key-fn]
  (loop [[k & [maybe-val :as more] :as args] (rest form),
         maps []]
    (cond
      (empty? args) (cond
                      (next maps) `(reduce into {} [~@maps])
                      (seq maps) (let [m (first maps)]
                                   (if (map? m)
                                     m
                                     `(into {} ~m)))
                      :else {})
      (symbol? k) (recur more (collect-expr maps {(key-fn k) k}))
      (and (seq? k)
           (= `unquote-splicing (first k))) (recur more (collect-expr maps (second k)))
      (and (seq? k)
           (= `unquote (first k))) (recur (rest more)
                                          (collect-expr maps {(second k) maybe-val}))
      (seq more) (recur (rest more) (collect-expr maps {k maybe-val}))
      :else (throw (ex-info (format "No value supplied for key %s" (pr-str k))
                            {:id ::no-value :key k :form form})))))

(defmacro k.    [& args] (build-map &form keyword))
(defmacro keys. [& args] (build-map &form keyword))
(defmacro strs. [& args] (build-map &form str))
(defmacro syms. [& args] (build-map &form (partial list 'quote)))
