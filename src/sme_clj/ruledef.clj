(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require [clojure.set :as set]
            [sme-clj.typedef :as types]
            [sme-clj.util :refer [vals-as-keys]])
  (:import mop_records.MopMap))

;;; Rule definition helpers
(defn apply-rule
  [kg {:keys [body]} mh]
  (body kg mh))

(def rule-types #{:intern :filter})

(defn make-rule [id type body]
  {:id id
   :type type
   :body body})

(defmulti extract-common-role-fillers (comp type first vector))

(defmethod extract-common-role-fillers :default
  [_ & ms]
  (->> ms
    (map (partial map second))
    (apply map vector)))

(defmethod extract-common-role-fillers MopMap
  [_ & ms]
  (let [ms           (map (partial into {}) ms)
        common-roles (->> ms
                       (map keys)
                       (map set)
                       (apply set/intersection))]
    (map (apply juxt ms) common-roles)))

(defn ordered-functor?
  [kg k]
  (when-let [functor (types/expression-functor kg k)]
    (types/ordered? kg functor)))

(defn same-functor?
  [kg & ks]
  (->> ks
    (map (partial types/expression-functor kg))
    ((every-pred
       (partial every? some?)
       (partial apply =)))))

(defn strict-entity?
  [kg]
  (every-pred (partial types/entity? kg) (complement #{::types/Entity})))

(defn functor-function?
  [kg]
  (comp (partial types/type-function? kg) (partial types/expression-functor kg)))

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity
  (vals-as-keys :id
    [(make-rule :same-functor :filter
       (fn [kg mh]
         [(when (apply same-functor? kg mh)
            mh)]))

     (make-rule :compatible-args :intern
       (fn [kg mh]
         (when (every? (partial ordered-functor? kg) mh)
           (->> mh
             (map (partial types/expression-args kg))
             (apply extract-common-role-fillers kg)
             (filter (some-fn
                       (partial every? (strict-entity? kg))
                       (partial every? (functor-function? kg))))))))

     ;; this rule not tested much yet
     (make-rule :commutative-args :intern
       (fn [kg [base target :as mh]]
         (when (not-any? (partial ordered-functor? kg) mh)
           (for [base   (types/expression-args kg base)
                 target (types/expression-args kg target)
                 :let   [mh [base target]]
                 :when  ((some-fn
                           (partial not-any? (partial types/expression? kg))
                           (partial every? (functor-function? kg)))
                         mh)]
             mh))))]))


(defn attribute-function?
  [kg]
  (comp (types/attribute? kg) (types/expression-functor kg)))

(def analogy
  (vals-as-keys :id
    [(make-rule :same-functor :filter
       (fn [kg [base _ :as mh]]
         [(when (and (apply same-functor? kg mh)
                  ((complement (attribute-function? kg)) base))
            mh)]))

     (make-rule :compatible-args :intern
       (fn [kg mh]
         (when (every? (partial ordered-functor? kg) mh)
           (->> mh
             (map (partial types/expression-args kg))
             (apply extract-common-role-fillers kg)
             (filter (some-fn
                       (partial every? (strict-entity? kg))
                       (partial every? (functor-function? kg))
                       (fn [[base _ :as mh]]
                         (and ((attribute-function? kg) base)
                           (apply same-functor? kg mh)))))))))

     ;; this rule not tested much yet
     (make-rule :commutative-args :intern
       (fn [kg [base target :as mh]]
         (when (not-any? (partial ordered-functor? kg) mh)
           (for [base   (types/expression-args kg base)
                 target (types/expression-args kg target)
                 :let   [mh [base target]]
                 :when  (or ((some-fn
                               (partial every? (strict-entity? kg))
                               (partial every? (functor-function? kg)))
                             mh)
                          (and ((attribute-function? kg) base)
                            (apply same-functor? kg mh)))]
             mh))))]))

                                        ; LocalWords:  mh
