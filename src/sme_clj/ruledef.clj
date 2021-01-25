(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require [sme-clj.typedef :refer :all]))

;;; Rule definition helpers
(defn apply-rule
  ([kg rule base target parent]
   (println rule)
   ((:body rule) kg base target parent))
  ([kg rule base target]
   (apply-rule kg rule base target nil))
  ([kg rule {:keys [base target]}]
   (apply-rule kg rule base target)))

(def rule-types #{:intern :filter})

(defn add-rule [rules name type body]
  (assoc-in rules [name] {:type type
                          :body body}))


;; As in SME, basic analogical matching rules, direct port
(def literal-similarity (-> {}
                          (add-rule :same-functor :filter
                            (fn [kg base target _]
                              (when (= (lookup kg base :functor) (lookup kg target :functor))
                                (make-MH base target))))

                          (add-rule :compatible-args :intern
                            (fn [kg base target _]
                              (when (and
                                      (= :expression (lookup kg base :type))
                                      (= :expression (lookup kg target :type))
                                      (lookup kg base :functor :ordered?)
                                      (lookup kg target :functor :ordered?))
                                (map (fn [bchild tchild]
                                       (when (or
                                               (not (or
                                                      (= :expression (lookup kg bchild :type))
                                                      (= :expression (lookup kg tchild :type))))
                                               (and
                                                 (= :expression (lookup kg bchild :type))
                                                 (= :expression (lookup kg tchild :type))
                                                 (= :function (lookup kg bchild :functor :type))
                                                 (= :function (lookup kg tchild :functor :type))))
                                         (make-MH bchild tchild)))
                                  (lookup kg base :args)
                                  (lookup kg target :args)))))

                          ;; this rule not tested much yet
                          (add-rule :commutative-args :intern
                            (fn [kg base target _]
                              (when (and
                                      (= :expression (lookup kg base :type))
                                      (= :expression (lookup kg target :type))
                                      (not (lookup kg base :functor :ordered?))
                                      (not (lookup kg target :functor :ordered?)))
                                (for [bchild (lookup kg base :args)
                                      tchild (lookup kg target :args)]
                                  (when (or
                                          (not (or
                                                 (= :expression (lookup kg bchild :type))
                                                 (= :expression (lookup kg tchild :type))))
                                          (and
                                            (= :expression (lookup kg bchild :type))
                                            (= :expression (lookup kg tchild :type))
                                            (= :function (lookup kg bchild :functor :type))
                                            (= :function (lookup kg tchild :functor :type))))
                                    (make-MH bchild tchild))))))))

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
