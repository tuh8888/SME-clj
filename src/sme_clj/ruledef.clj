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

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity
  (vals-as-keys :id
    [(make-rule :same-functor :filter
       (fn [kg mh]
         [(when (let [functors (map (partial types/expression-functor kg) mh)]
                  (and (every? identity functors)
                    (apply = functors)))
            mh)]))

     (make-rule :compatible-args :intern
       (fn [kg mh]
         (when (every? #(when-let [functor (types/expression-functor kg %)]
                          (types/ordered? kg functor))
                 mh)
           (->> mh
             (map (partial types/expression-args kg))
             (apply extract-common-role-fillers kg)
             (filter (fn [mh]
                       ((some-fn
                          (partial every? (every-pred (complement (partial types/expression? kg))
                                      (complement #{::types/Entity})))
                          (partial every? (comp (partial types/type-function? kg) (partial types/expression-functor kg))))
                        mh)))))))

     ;; this rule not tested much yet
     (make-rule :commutative-args :intern
       (fn [kg [base target]]
         (when (and
                 (types/expression? kg base)
                 (types/expression? kg target)
                 (not (types/lookup kg base :functor :ordered?))
                 (not (types/lookup kg target :functor :ordered?)))
           (for [bchild (types/lookup kg base :args)
                 tchild (types/lookup kg target :args)]
             (when (or
                     (not (or
                            (types/expression? kg bchild)
                            (types/expression? kg tchild)))
                     (and
                       (types/expression? kg bchild)
                       (types/expression? kg tchild)
                       (= ::types/Function (types/lookup kg bchild :functor :type))
                       (= ::types/Function (types/lookup kg tchild :functor :type))))
               (types/make-match-hypothesis bchild tchild))))))]))

                                        ; LocalWords:  mh
