(ns sme-clj.ruledef-test
  (:require [clojure.test :as t]
            [mop-records :as mr]
            [mops :as mops]
            [sme-clj.mop-helpers :as moppers]
            [sme-clj.ruledef :as sut]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util]))

(def predicates [(types/make-predicate :flow :type :relation :arity 4)
                 (types/make-predicate :greater :type :relation :arity 2)
                 (types/make-predicate :cause :type :relation :arity 2)
                 (types/make-predicate :temperature :type :function)
                 (types/make-predicate :flat-top :type :function)
                 (types/make-predicate :pressure :type :function)
                 (types/make-predicate :diameter :type :function)
                 (types/make-predicate :liquid :type :attribute)
                 (types/make-predicate :clear :type :attribute)])

(def entities [(types/make-entity :Coffee)
               (types/make-entity :Icecube)
               (types/make-entity :Bar)
               (types/make-entity :Heat)

               (types/make-entity :Water)
               (types/make-entity :Beaker)
               (types/make-entity :Vial)
               (types/make-entity :Pipe)])

;; Concept graph definitions
(def simple-water-flow (types/make-concept-graph :simple-water-flow
                         [:cause
                          [:greater [:pressure :Beaker] [:pressure :Vial]]
                          [:flow :Beaker :Vial :Water :Pipe]]
                         [:greater [:diameter :Beaker] [:diameter :Vial]]
                         [:clear :Beaker]
                         [:flat-top :Water]
                         [:liquid :Water]))

(def simple-heat-flow (types/make-concept-graph :simple-heat-flow
                        [:flow :Coffee :Icecube :Heat :Bar]
                        [:greater [:temperature :Coffee] [:temperature :Icecube]]
                        [:flat-top :Coffee]
                        [:liquid :Coffee]))




(def kg (merge-with (fn [v1 v2]
                      {:help (vector v1 v2)})
          (util/vals-as-keys :name entities)
          (util/vals-as-keys :name predicates)
          (:graph simple-heat-flow)
          (:graph simple-water-flow)))

(def expected-same-functor-matches
  #{[:diameter-Vial :diameter-Vial]
    [:diameter-Vial :diameter-Beaker]
    [:flow-Beaker-Vial-Water-Pipe :flow-Beaker-Vial-Water-Pipe]
    [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
    [:diameter-Beaker :diameter-Vial]
    [:diameter-Beaker :diameter-Beaker]
    [:clear-Beaker :clear-Beaker]
    [:liquid-Coffee :liquid-Coffee]
    [:liquid-Coffee :liquid-Water]
    [:pressure-Vial :pressure-Vial]
    [:pressure-Vial :pressure-Beaker]
    [:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
     :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe]
    [:greater-temperature-Coffee-temperature-Icecube
     :greater-temperature-Coffee-temperature-Icecube]
    [:greater-temperature-Coffee-temperature-Icecube
     :greater-pressure-Beaker-pressure-Vial]
    [:greater-temperature-Coffee-temperature-Icecube
     :greater-diameter-Beaker-diameter-Vial]
    [:flat-top-Coffee :flat-top-Coffee]
    [:flat-top-Coffee :flat-top-Water]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-pressure-Beaker-pressure-Vial]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-diameter-Beaker-diameter-Vial]
    [:temperature-Coffee :temperature-Coffee]
    [:temperature-Coffee :temperature-Icecube]
    [:liquid-Water :liquid-Coffee]
    [:liquid-Water :liquid-Water]
    [:temperature-Icecube :temperature-Coffee]
    [:temperature-Icecube :temperature-Icecube]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-pressure-Beaker-pressure-Vial]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-diameter-Beaker-diameter-Vial]
    [:pressure-Beaker :pressure-Vial]
    [:pressure-Beaker :pressure-Beaker]
    [:flat-top-Water :flat-top-Coffee]
    [:flat-top-Water :flat-top-Water]
    [:flow-Coffee-Icecube-Heat-Bar :flow-Beaker-Vial-Water-Pipe]
    [:flow-Coffee-Icecube-Heat-Bar :flow-Coffee-Icecube-Heat-Bar]})

(def expected-compatible-args-matches #{[:temperature-Icecube :temperature-Icecube]
            [:diameter-Beaker :diameter-Beaker]
            [:pressure-Beaker :diameter-Beaker]
            [:Beaker :Beaker]
            [:Pipe :Pipe]
            [:temperature-Coffee :temperature-Coffee]
            [:Water :Coffee]
            [:Coffee :Water]
            [:Icecube :Vial]
            [:Icecube :Water]
            [:temperature-Coffee :diameter-Beaker]
            [:Icecube :Beaker]
            [:Coffee :Vial]
            [:Beaker :Icecube]
            [:Beaker :Coffee]
            [:Water :Icecube]
            [:temperature-Coffee :pressure-Beaker]
            [:Icecube :Icecube]
            [:Vial :Beaker]
            [:diameter-Vial :temperature-Icecube]
            [:Beaker :Water]
            [:Vial :Coffee]
            [:temperature-Icecube :diameter-Vial]
            [:Vial :Water]
            [:Water :Vial]
            [:diameter-Vial :diameter-Vial]
            [:Coffee :Coffee]
            [:Water :Heat]
            [:Vial :Vial]
            [:pressure-Beaker :temperature-Coffee]
            [:diameter-Beaker :pressure-Beaker]
            [:Water :Beaker]
            [:Pipe :Bar]
            [:diameter-Vial :pressure-Vial]
            [:diameter-Beaker :temperature-Coffee]
            [:temperature-Icecube :pressure-Vial]
            [:Heat :Heat]
            [:Beaker :Vial]
            [:Coffee :Beaker]
            [:Water :Water]
            [:pressure-Beaker :pressure-Beaker]
            [:Vial :Icecube]
            [:pressure-Vial :diameter-Vial]
            [:Heat :Water]
            [:Bar :Pipe]
            [:pressure-Vial :temperature-Icecube]
            [:Icecube :Coffee]
            [:Coffee :Icecube]
            [:pressure-Vial :pressure-Vial]
            [:Bar :Bar]})

(def expected-commutative-args #{})

(t/deftest literal-similarity-test
  (t/testing "Same functor"
    (t/is (= expected-same-functor-matches
            (->> (for [b (keys kg)
                       t (keys kg)]
                   [b t])
              (mapcat (partial sut/apply-rule kg (:same-functor sut/literal-similarity 0)))
              (remove nil?)
              set))))
  (t/testing "Compatible args"
    (t/is (= expected-compatible-args-matches
            (->> (for [b (keys kg)
                       t (keys kg)]
                   [b t])
              (mapcat (partial sut/apply-rule kg (:compatible-args sut/literal-similarity 1)))
              (remove nil?)
              set))))
  (t/testing "Commutative args"
    (t/is (= expected-commutative-args
            (->> (for [b (keys kg)
                       t (keys kg)]
                   [b t])
              (mapcat (partial sut/apply-rule kg (:commutative-args sut/literal-similarity 1)))
              (remove nil?)
              set)))))

(defn make-mop
  ([m id [parent & slots]]
   (mops/add-mop m  (mops/->mop id (into {:parents #{parent}} slots))))
  ([m [parent & slots :as all-slots]]
   (make-mop m (types/combine-ids (-> slots
                                    (->> (map second))
                                    (conj parent)))
     all-slots)))

(-> (mr/make-mop-map)
  (make-mop :greater [:Expression
                      [:entity2    :Expression]
                      [:more    :Expression]])
  mops/infer-hierarchy)


(t/deftest mop-representation
  (let [full-kg (-> (mr/make-mop-map)
                  (moppers/make-mop :cause :Expression {:e1 :Expression
                                                        :e2 :Expression})
                  (moppers/make-mop :greater :Expression {:e1 :Expression
                                                          :e2 :Expression})
                  (moppers/make-mop :flow :Expression {:e1 :Entity
                                                       :e2 :Entity
                                                       :e3 :Entity
                                                       :e4 :Entity})
                  (moppers/make-mop :Function :Expression)
                  (moppers/make-mop :pressure :Function {:e1 :Entity})
                  (moppers/make-mop :diameter :Function {:e1 :Entity})
                  (moppers/make-mop :clear :Expression {:e1 :Entity})
                  (moppers/make-mop :temperature :Function {:e1 :Entity})
                  (moppers/make-mop :flat-top :Function {:e1 :Entity})
                  (moppers/make-mop :liquid :Expression {:e1 :Entity})
                  (moppers/make-mop :Coffee :Entity)
                  (moppers/make-mop :Water :Entity)
                  (moppers/make-mop :Heat :Entity)
                  (moppers/make-mop :Pipe :Entity)
                  (moppers/make-mop :Vial :Entity)
                  (moppers/make-mop :Icecube :Entity)
                  (moppers/make-mop :Bar :Entity)
                  (moppers/make-mop :Beaker :Entity)

                  (moppers/mops-add-concept-graph :simple-water-flow
                    [:flat-top [:e1 :Water]]
                    [:liquid [:e1 :Water]]
                    [:cause
                     [:e1 :greater-pressure-Beaker-pressure-Vial]
                     [:e2 :flow-Beaker-Vial-Water-Pipe]]
                    [:greater
                     [:e1 :pressure-Beaker]
                     [:e2 :pressure-Vial]]
                    [:greater
                     [:e1 :diameter-Beaker]
                     [:e2 :diameter-Vial]]
                    [:clear [:e1 :Beaker]]
                    [:diameter [:e1 :Beaker]]
                    [:diameter [:e1 :Vial]]
                    [:pressure [:e1 :Beaker]]
                    [:pressure [:e1 :Vial]]
                    [:flow
                     [:e1 :Beaker]
                     [:e2 :Vial]
                     [:e3 :Water]
                     [:e4 :Pipe]])

                  (moppers/mops-add-concept-graph :simple-heat-flow
                    [:flow
                     [:e1 :Coffee]
                     [:e2 :Icecube]
                     [:e3 :Heat]
                     [:e4 :Bar]]
                    [:greater
                     [:e1 :temperature-Coffee]
                     [:e2 :temperature-Icecube]]
                    [:temperature [:e1 :Coffee]]
                    [:temperature [:e1 :Icecube]]
                    [:flat-top [:e1 :Coffee]]
                    [:liquid [:e1  :Coffee]])
                  mops/infer-hierarchy)
        expressions (->> full-kg
                      :mops
                      vals
                      (filter :concept-graph)
                      (map :id))
        mhs         (for [b expressions
                          t expressions]
                      [b t])]

    (t/testing "Same functor"
      (t/is (= expected-same-functor-matches
              (->> mhs
                (mapcat (partial sut/apply-rule full-kg (:same-functor sut/mops-literal-similarity)))
                (remove nil?)
                set))))

    (t/testing "Compatible args"
      (t/is (= expected-compatible-args-matches
              (->> mhs
                (mapcat (partial sut/apply-rule full-kg (:compatible-args sut/mops-literal-similarity)))
                (remove nil?)
                set))))))
