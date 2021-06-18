(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require [sme-clj.typedef :as types]
            [sme-clj.util :refer [vals-as-keys]]))

;;; Rule definition helpers
(defn apply-rule [kg {:keys [body]} mh] (body kg mh))

(def rule-types #{:intern :filter})

(defn make-rule
  [id type body]
  {:id   id
   :type type
   :body (case type
           :filter (fn [kg mh] (when (body kg mh) [mh]))
           body)})

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity
  (vals-as-keys
   :id
   [(make-rule :same-functor :filter types/same-functor?)
    (make-rule
     :compatible-args
     :intern
     (fn [kg mh]
       (when (every? (partial types/ordered-functor? kg) mh)
         (->> mh
              (map (partial types/expression-args kg))
              (apply types/extract-common-role-fillers kg)
              (filter
               (some-fn (partial every? (partial types/strict-entity? kg))
                        (partial every?
                                 (partial types/functor-function? kg))))))))
    ;; this rule not tested much yet
    (make-rule
     :commutative-args
     :intern
     (fn [kg [base target :as mh]]
       (when (not-any? (partial types/ordered-functor? kg) mh)
         (for [base   (types/expression-args kg base)
               target (types/expression-args kg target)
               :let   [mh [base target]]
               :when  ((some-fn (partial every?
                                         (partial types/strict-entity? kg))
                                (partial every?
                                         (partial types/functor-function? kg)))
                       mh)]
           mh))))]))

(def analogy
  (vals-as-keys
   :id
   [(make-rule
     :same-functor
     :filter
     (fn [kg mh]
       ((every-pred
         (partial types/same-functor? kg)
         (partial every? (partial (complement types/attribute-functor?) kg)))
        mh)))
    (make-rule
     :compatible-args
     :intern
     (fn [kg mh]
       (when (every? (partial types/ordered-functor? kg) mh)
         (->> mh
              (map (partial types/expression-args kg))
              (apply types/extract-common-role-fillers kg)
              (filter
               (some-fn (partial every? (partial types/strict-entity? kg))
                        (partial every? (partial types/functor-function? kg))
                        (every-pred
                         (partial every? (partial types/attribute-functor? kg))
                         (partial types/same-functor? kg))))))))
    ;; this rule not tested much yet
    (make-rule
     :commutative-args
     :intern
     (fn [kg [base target :as mh]]
       (when (not-any? (partial types/ordered-functor? kg) mh)
         (for [base   (types/expression-args kg base)
               target (types/expression-args kg target)
               :let   [mh [base target]]
               :when  ((some-fn
                        (partial every? (partial types/strict-entity? kg))
                        (partial every? (partial types/functor-function? kg))
                        (every-pred (partial every?
                                             (partial types/attribute-functor?
                                                      kg))
                                    (partial types/same-functor? kg)))
                       mh)]
           mh))))]))

; LocalWords:  mh
