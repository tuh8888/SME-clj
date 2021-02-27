(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require [mops :as mops]
            [sme-clj.typedef :as types]
            [sme-clj.util :refer [vals-as-keys]]))

;;; Rule definition helpers
(defn apply-rule
  [kg {:keys [body]} mh]
  (body kg mh))

(def rule-types #{:intern :filter})

(defn make-rule [name type body]
  {:name name
   :type type
   :body body})

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity (vals-as-keys :name
                          [(make-rule :same-functor :filter
                             (fn [kg mh]
                               [(when (let [functors (map (partial types/expression-functor kg) mh)]
                                        (and (every? identity functors)
                                          (apply = functors)))
                                  mh)]))

                           (make-rule :compatible-args :intern
                             (fn [kg [base target]]
                               (when (and
                                       (types/lookup kg base :functor :ordered?)
                                       (types/lookup kg target :functor :ordered?))
                                 (map (fn [bchild tchild]
                                        (when (or
                                                (not (or
                                                       (types/expression? kg bchild)
                                                       (types/expression? kg tchild)))
                                                (and
                                                  (= ::types/Function (types/lookup kg bchild :functor :type))
                                                  (= ::types/Function (types/lookup kg tchild :functor :type))))
                                          (types/make-match-hypothesis bchild tchild)))
                                   (map second (types/expression-args kg base))
                                   (map second (types/expression-args kg target))))))

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

(def mops-literal-similarity (vals-as-keys :name
                               [(make-rule :compatible-args :intern
                                  (fn [kg mh]
                                    (->> mh
                                      (map (partial types/expression-args kg))
                                      (apply map vector)
                                      (filter #(apply = (map first %)))
                                      (map (partial map second))
                                      (map (fn [[bchild tchild]]
                                             (when (or
                                                     (not (or
                                                            (types/expression? kg bchild)
                                                            (types/expression? kg tchild)))
                                                     (and
                                                       (mops/abstr? kg bchild ::types/Function)
                                                       (mops/abstr? kg tchild ::types/Function)))
                                               (types/make-match-hypothesis bchild tchild)))))))

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
