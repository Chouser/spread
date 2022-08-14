(ns io.chouser.spread)

(def mapcat-identity (mapcat identity))

(defn add-to-maps [maps k v]
  (if (map? (peek maps))
    (conj (pop maps) (assoc (peek maps) k v))
    (conj maps {k v})))

(defn ^:private build-map [form key-fn]
  (loop [[k & [maybe-val :as more]] (rest form), maps []]
    (if k
      (if (symbol? k)
        (if-let [[_ suffix] (re-matches #"\.\.\.(.*)" (str k))]
          (if (empty? suffix)
            (if (seq more)
              (recur (rest more) (conj maps maybe-val))
              (throw (ex-info "No expression supplied after ellipsis"
                              {:form form})))
            (recur more (conj maps (symbol suffix))))
          (if (namespace k)
            (throw (ex-info
                    (format "Namespaced symbol keys not yet supported: %s" k)
                    {:key k :form form}))
            (recur more (add-to-maps maps (key-fn k) k))))
        (if (seq more)
          (recur (rest more) (add-to-maps maps k maybe-val))
          (throw (ex-info (format "No value supplied for key %s" (pr-str k))
                          {:key k :form form}))))
      (if (next maps)
        `(into ~(first maps) mapcat-identity [~@(rest maps)])
        (first maps)))))

(defmacro k.    [& args] (build-map &form keyword))
(defmacro keys. [& args] (build-map &form keyword))
(defmacro strs. [& args] (build-map &form str))
(defmacro syms. [& args] (build-map &form (partial list 'quote)))
