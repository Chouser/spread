(ns io.chouser.spread)

(def mapcat-identity (mapcat identity))

(defn ^:private add-to-maps [maps k v]
  (if (map? (peek maps))
    (conj (pop maps) (assoc (peek maps) k v))
    (conj maps {k v})))

(defn ^:private build-map [form key-fn]
  (loop [[k & [maybe-val :as more] :as args] (rest form), maps []]
    (cond
      (empty? args) (if (next maps)
                      `(into ~(first maps) mapcat-identity [~@(rest maps)])
                      (first maps))
      (symbol? k) (if (namespace k)
                    (throw (ex-info
                            (format "Namespaced symbol keys not yet supported: %s" k)
                            {:key k :form form}))
                    (recur more (add-to-maps maps (key-fn k) k)))
      (and (seq? k)
           (= `unquote-splicing (first k))) (recur more (conj maps (second k)))
      (seq more) (recur (rest more) (add-to-maps maps k maybe-val))
      :else (throw (ex-info (format "No value supplied for key %s" (pr-str k))
                            {:key k :form form})))))

(defmacro k.    [& args] (build-map &form keyword))
(defmacro keys. [& args] (build-map &form keyword))
(defmacro strs. [& args] (build-map &form str))
(defmacro syms. [& args] (build-map &form (partial list 'quote)))
