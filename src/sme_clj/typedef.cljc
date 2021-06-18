(ns sme-clj.typedef
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require [clojure.walk :as walk]
            #?(:clj [mops.records]
               :cljs [mops.records :refer [MopMap]])
            [mops.core :as mops])
  (:import #?(:clj [mops.records MopMap])))



;;; EXPRESSION

;; Originally expressions were simply seqs, with as first element the
;; functor. However, this meant two relations with the same predicate and same
;; entities were equal as per value equality. Though this problem is not
;; detrimental in every situation, they are sufficiently common to change the
;; representation to a uniquely identifiable one including a unique id.
;;
;; An Expression's id is not guaranteed to be related to any implicit ordering
;; of the expressions within a graph, but in practice often is.


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

;;; CONCEPT GRAPH

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

(defmethod add-entity :default
  [m id & slots]
  (let [{:keys [id]
         :as   e}
        {:id    id
         :type  ::Entity
         :slots slots}]
    (assoc m id e)))

(defmulti add-predicate (comp type first vector))

(defmulti add-concept-graph (comp type first vector))

(defmulti initialize-kg (comp type first vector))
