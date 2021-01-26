(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require [sme-clj.typedef :refer :all]
            [sme-clj.util :refer [vals-as-keys]]))

;;; Rule definition helpers
(defn apply-rule
  ([kg rule base target parent]
   ((:body rule) kg base target parent))
  ([kg rule base target]
   (apply-rule kg rule base target nil))
  ([kg rule {:keys [base target]}]
   (apply-rule kg rule base target)))

(def rule-types #{:intern :filter})

(defn make-rule [name type body]
  {:name name
   :type type
   :body body})

(defn expression? [kg k]
  ((comp (partial = :expression) #(lookup kg % :type)) k))

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity (vals-as-keys :name
                          [(make-rule :same-functor :filter
                             (fn [kg base target _]
                               [(when (= (lookup kg base :functor) (lookup kg target :functor))
                                  (->MatchHypothesis base target))]))

                           (make-rule :compatible-args :intern
                             (fn [kg base target _]
                               (when (and
                                       (lookup kg base :functor :ordered?)
                                       (lookup kg target :functor :ordered?))
                                 (map (fn [bchild tchild]
                                        (when (or
                                                (not (or
                                                       (expression? kg bchild)
                                                       (expression? kg tchild)))
                                                (and
                                                  (= :function (lookup kg bchild :functor :type))
                                                  (= :function (lookup kg tchild :functor :type))))
                                          (->MatchHypothesis bchild tchild)))
                                   (lookup kg base :args)
                                   (lookup kg target :args)))))

                           ;; this rule not tested much yet
                           (make-rule :commutative-args :intern
                             (fn [kg base target _]
                               (when (and
                                       (expression? kg base)
                                       (expression? kg target)
                                       (not (lookup kg base :functor :ordered?))
                                       (not (lookup kg target :functor :ordered?)))
                                 (for [bchild (lookup kg base :args)
                                       tchild (lookup kg target :args)]
                                   (when (or
                                           (not (or
                                                  (expression? kg bchild)
                                                  (expression? kg tchild)))
                                           (and
                                             (expression? kg bchild)
                                             (expression? kg tchild)
                                             (= :function (lookup kg bchild :functor :type))
                                             (= :function (lookup kg tchild :functor :type))))
                                     (->MatchHypothesis bchild tchild))))))]))

#_(defrules literal-similarity
    (rule same-functor :filter
      (when (= (expression-functor base) (expression-functor target))
        (make-MH base target)))

    (rule compatible-args :intern
      (when (and (expression? base) (expression? target)
              (:ordered? (expression-functor base))
              (:ordered? (expression-functor target)))
        (map (fn [bchild tchild]
               (when (or
                       (not (or (expression? bchild) (expression? tchild)))
                       (and (expression? bchild) (expression? tchild)
                         (function? (expression-functor bchild))
                         (function? (expression-functor tchild))))
                 (make-MH bchild tchild)))
          (expression-args base)
          (expression-args target))))

    ;; this rule not tested much yet
    (rule commutative-args :intern
      (when (and (expression? base) (expression? target)
              (not (:ordered? (expression-functor base)))
              (not (:ordered? (expression-functor target))))
        (for [bchild (expression-args base)
              tchild (expression-args target)]
          (when (or
                  (not (or (expression? bchild) (expression? tchild)))
                  (and (expression? bchild) (expression? tchild)
                    (function? (expression-functor bchild))
                    (function? (expression-functor tchild))))
            (make-MH bchild tchild))))))
