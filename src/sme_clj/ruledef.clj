(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require [clojure.set :as set]
            [mops :as mops]
            [sme-clj.typedef :as types]
            [sme-clj.util :refer [vals-as-keys]]))

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

(defn expression? [kg k]
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
                                                  (= :function (types/lookup kg bchild :functor :type))
                                                  (= :function (types/lookup kg tchild :functor :type))))
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
                                             (= :function (types/lookup kg bchild :functor :type))
                                             (= :function (types/lookup kg tchild :functor :type))))
                                     (types/make-match-hypothesis bchild tchild))))))]))

(defn mop-expression? [kg k]
  (mops/abstr? kg k ::types/Expression))

(defn interesting-roles [mop]
  (->> mop
    mops/roles
    (remove #{:id :parents :names :inst? :concept-graph})))

(def mops-literal-similarity (vals-as-keys :name
                               [(make-rule :same-functor :filter
                                  (fn [kg [base target :as mh]]
                                    [(when ((every-pred
                                              (partial every? (partial mop-expression? kg)) ; Both expressions
                                              (comp (partial apply =)
                                                (partial map first)
                                                (partial map #(mops/strict-abstrs kg %)))) ; Same functors
                                            [base target])
                                       mh)]))
                                (make-rule :compatible-args :intern
                                  (fn [kg mh]
                                    (let [[base target] (->> mh
                                                          (map (partial mops/get-mop kg)))]
                                      (->>
                                        (set/intersection
                                          (set (interesting-roles base))
                                          (set (interesting-roles target)))
                                        (map (fn [role]
                                               (let [[bchild tchild] (->> [base target]
                                                                       (map #(mops/filler % role))
                                                                       (map first))]
                                                 (when (or
                                                         (not (or
                                                                (mop-expression? kg bchild)
                                                                (mop-expression? kg tchild)))
                                                         (and
                                                           (mops/abstr? kg bchild :Function)
                                                           (mops/abstr? kg tchild :Function)))
                                                   (types/make-match-hypothesis bchild tchild)))))))))

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
                                                  (= :function (types/lookup kg bchild :functor :type))
                                                  (= :function (types/lookup kg tchild :functor :type))))
                                          (types/make-match-hypothesis bchild tchild))))))]))
