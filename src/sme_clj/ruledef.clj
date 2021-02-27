(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require [clojure.set :as set]
            [mops :as mops]
            [sme-clj.typedef :as types]
            [sme-clj.util :refer [vals-as-keys]])
  (:import mop_records.MopMap))

;;; Rule definition helpers
(defn apply-rule
  #_([kg rule base target parent]
     ((:body rule) kg base target parent))
  #_([kg rule base target]
     (apply-rule kg rule base target nil))
  ([kg rule mh]
   ((:body rule) kg mh)))

(def rule-types #{:intern :filter})

(defn make-rule [name type body]
  {:name name
   :type type
   :body body})

(defmulti expression?
  (fn [kg _] (type kg)))

(defmethod expression? :default
  [kg k]
  ((comp (partial = ::types/Expression) #(types/lookup kg % :type)) k))

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity (vals-as-keys :name
                          [(make-rule :same-functor :filter
                             (fn [kg [base target :as mh]]
                               [(when ((every-pred
                                         (partial every? (partial expression? kg)) ; Both expressions
                                         (comp (partial apply =) (partial map #(types/lookup kg % :functor)))) ; Same functors
                                       [base target])
                                  mh)]))

                           (make-rule :compatible-args :intern
                             (fn [kg [base target]]
                               (when (and
                                       (types/lookup kg base :functor :ordered?)
                                       (types/lookup kg target :functor :ordered?))
                                 (map (fn [bchild tchild]
                                        (when (or
                                                (not (or
                                                       (expression? kg bchild)
                                                       (expression? kg tchild)))
                                                (and
                                                  (= ::types/Function (types/lookup kg bchild :functor :type))
                                                  (= ::types/Function (types/lookup kg tchild :functor :type))))
                                          (types/make-match-hypothesis bchild tchild)))
                                   (types/lookup kg base :args)
                                   (types/lookup kg target :args)))))

                           ;; this rule not tested much yet
                           (make-rule :commutative-args :intern
                             (fn [kg [base target]]
                               (when (and
                                       (expression? kg base)
                                       (expression? kg target)
                                       (not (types/lookup kg base :functor :ordered?))
                                       (not (types/lookup kg target :functor :ordered?)))
                                 (for [bchild (types/lookup kg base :args)
                                       tchild (types/lookup kg target :args)]
                                   (when (or
                                           (not (or
                                                  (expression? kg bchild)
                                                  (expression? kg tchild)))
                                           (and
                                             (expression? kg bchild)
                                             (expression? kg tchild)
                                             (= ::types/Function (types/lookup kg bchild :functor :type))
                                             (= ::types/Function (types/lookup kg tchild :functor :type))))
                                     (types/make-match-hypothesis bchild tchild))))))]))

(defmethod expression? MopMap
  [kg k]
  (mops/abstr? kg k ::types/Expression))

(defn expression-args
  [kg k]
  (when (expression? kg k)
    (let [mop (mops/get-mop kg k)]
      (->> mop
        mops/roles
        (remove (conj mops/reserved-roles :concept-graph))
        (map (partial mops/slot mop))))))

(def mops-literal-similarity (vals-as-keys :name
                               [(make-rule :same-functor :filter
                                  (fn [kg [base target :as mh]]
                                    [(when ((every-pred
                                              (partial every? (partial expression? kg)) ; Both expressions
                                              (comp (partial apply =)
                                                (partial map first)
                                                (partial map #(mops/strict-abstrs kg %)))) ; Same functors
                                            [base target])
                                       mh)]))
                                (make-rule :compatible-args :intern
                                  (fn [kg mh]
                                    (->> mh
                                      (map (partial expression-args kg))
                                      (apply map vector)
                                      (filter #(apply = (map first %)))
                                      (map (partial map second))
                                      (map (partial map first))
                                      (map (fn [[bchild tchild]]
                                             (when (or
                                                     (not (or
                                                            (expression? kg bchild)
                                                            (expression? kg tchild)))
                                                     (and
                                                       (mops/abstr? kg bchild ::types/Function)
                                                       (mops/abstr? kg tchild ::types/Function)))
                                               (types/make-match-hypothesis bchild tchild)))))))

                                ;; this rule not tested much yet
                                (make-rule :commutative-args :intern
                                  (fn [kg [base target]]
                                    (when (and
                                            (expression? kg base)
                                            (expression? kg target)
                                            (not (types/lookup kg base :functor :ordered?))
                                            (not (types/lookup kg target :functor :ordered?)))
                                      (for [bchild (types/lookup kg base :args)
                                            tchild (types/lookup kg target :args)]
                                        (when (or
                                                (not (or
                                                       (expression? kg bchild)
                                                       (expression? kg tchild)))
                                                (and
                                                  (expression? kg bchild)
                                                  (expression? kg tchild)
                                                  (= ::types/Function (types/lookup kg bchild :functor :type))
                                                  (= ::types/Function (types/lookup kg tchild :functor :type))))
                                          (types/make-match-hypothesis bchild tchild))))))]))
