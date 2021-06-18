(ns sme-clj.typedef
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them.")

(defmulti attribute? (comp type first vector))

(defmulti entity? (comp type first vector))

(defmulti expression? (comp type first vector))

(defmulti expression-args (comp type first vector))

(defmulti expression-functor (comp type first vector))

(defmulti type-function? (comp type first vector))

(defmulti ordered? (comp type first vector))

(defn ancestor?
  "Returns true if a given expression is an ancestor of one of the expressions
  in the base set."
  [kg base-set expr]
  (and (expression? kg expr)
       (or (contains? base-set expr)
           (some #(ancestor? kg base-set %)
                 (map second (expression-args kg expr))))))

(defn get-descendants
  "Returns the seq of descendants of the given expression."
  [kg expr]
  (tree-seq (partial expression? kg)
            (comp (partial map second) (partial expression-args kg))
            expr))

(defn- pretty-demunge
  [fn-object]
  (if (fn? fn-object)
    (let [dem-fn (#?(:clj clojure.main/demunge
                     :cljs demunge)
                  (str fn-object))
          pretty (last (re-find #?(:clj #"(.*?\/(.*?))[\-\-|@].*"
                                   :cljs #"(.*?/(.*?))[--|@].*")
                                dem-fn))]
      (if pretty pretty dem-fn))
    fn-object))

(defmacro cond-pred->
  "Like cond-> but also threads initial expr through tests."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g     (gensym)
        steps (map (fn [[test step]]
                     `(if (-> ~g
                              ~test)
                        (-> ~g
                            ~step)
                        ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defn combine-ids
  [ids]
  (->> ids
       (map (fn [id]
              (cond-pred-> id
                           coll?
                           (-> (->> (mapv (comp keyword pretty-demunge)))
                               combine-ids
                               str
                               (#(str "[" % "]"))
                               (subs 1))
                           ((complement keyword?))
                           (-> keyword
                               str))))
       (map str)
       (map #(subs % 1))
       (interpose "-")
       (apply str)
       keyword))

(defn args->slots
  [args]
  (->> args
       (map-indexed
        (comp (juxt (comp keyword (partial str "e") inc first) second) vector))
       (into {})))

(defmulti add-entity (comp type first vector))

(defmulti add-predicate (comp type first vector))

(defmulti add-concept-graph (comp type first vector))

(defmulti initialize-kg (comp type first vector))

(defmulti extract-common-role-fillers (comp type first vector))

(defn ordered-functor?
  [kg k]
  (when-let [functor (expression-functor kg k)] (ordered? kg functor)))

(defn same-functor?
  [kg & ks]
  (->> ks
       (map (partial expression-functor kg))
       ((every-pred (partial every? some?) (partial apply =)))))

(defn strict-entity?
  [kg]
  (every-pred (partial entity? kg) (complement #{::Entity})))

(defn functor-function?
  [kg]
  (comp (partial type-function? kg) (partial expression-functor kg)))

(defn attribute-functor?
  [kg]
  (comp (partial attribute? kg) (partial expression-functor kg)))
