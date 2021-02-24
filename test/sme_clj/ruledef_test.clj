(ns sme-clj.ruledef-test
  (:require [clojure.test :as t]
            [mop-records :as mr]
            [mops :as mops]
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

(t/deftest literal-similarity-test
  (t/is (= expected-same-functor-matches
          (->> (for [b (keys kg)
                     t (keys kg)]
                 [b t])
            (mapcat (partial sut/apply-rule kg (first (vals sut/literal-similarity))))
            (remove nil?)
            set))))

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
                      [:less    :Expression]
                      [:more    :Expression]])
  mops/infer-hierarchy)
(t/deftest mop-representation
  (let [kg                (-> (mr/make-mop-map)
                            (make-mop :cause [:Expression
                                              [:upstream   :Expression]
                                              [:downstream :Expression]])
                            (make-mop :greater [:Expression
                                                [:less    :Expression]
                                                [:more    :Expression]])
                            (make-mop :flow [:Expression
                                             [:from    :Entity]
                                             [:to      :Entity]
                                             [:flow-er :Entity]
                                             [:via     :Entity]])
                            (make-mop :pressure    [:Expression [:container :Entity]])
                            (make-mop :diameter [:Expression [:container :Entity]])
                            (make-mop :clear [:Expression [:container :Entity]])
                            (make-mop :temperature [:Expression
                                                    [:thermal-entity :Entity]])
                            (make-mop :flat-top [:Expression
                                                 [:surface-of :Entity]])
                            (make-mop :liquid [:Expression
                                               [:entity  :Entity]])
                            (make-mop :Coffee [:Entity])
                            (make-mop :Water [:Entity])
                            (make-mop :Heat [:Entity])
                            (make-mop :Pipe [:Entity])
                            (make-mop :Vial [:Entity])
                            (make-mop :Icecube [:Entity])
                            (make-mop :Bar [:Entity])
                            (make-mop :Beaker [:Entity]))
        simple-water-flow (-> (mr/make-mop-map)
                            (make-mop [:flat-top [:surface-of :Water]])
                            (make-mop [:liquid [:entity  :Water]])
                            (make-mop [:cause
                                       [:upstream :greater-pressure-Beaker-pressure-Vial]
                                       [:downstream :flow-Beaker-Vial-Water-Pipe]])
                            (make-mop [:greater
                                       [:more :pressure-Beaker]
                                       [:less :pressure-Vial]])
                            (make-mop [:greater
                                       [:more :diameter-Beaker]
                                       [:less :diameter-Vial]])
                            (make-mop [:clear [:container :Beaker]])
                            (make-mop [:diameter [:container :Beaker]])
                            (make-mop [:diameter [:container :Vial]])
                            (make-mop [:pressure [:container :Beaker]])
                            (make-mop [:pressure [:container :Vial]])
                            (make-mop [:flow
                                       [:from    :Beaker]
                                       [:to      :Vial]
                                       [:flow-er :Water]
                                       [:via     :Pipe]]))
        simple-heat-flow  (-> (mr/make-mop-map)
                            (make-mop [:flow
                                       [:from    :Coffee]
                                       [:to      :Icecube]
                                       [:flow-er :Heat]
                                       [:via     :Bar]])
                            (make-mop [:greater
                                       [:more    :temperature-Coffee]
                                       [:less    :temperature-Icecube]])
                            (make-mop [:temperature [:thermal-entity :Coffee]])
                            (make-mop [:temperature [:thermal-entity :Icecube]])
                            (make-mop [:flat-top [:surface-of :Coffee]])
                            (make-mop [:liquid [:entity  :Coffee]]))
        full-kg           (-> kg
                            (update :mops (partial merge-with mr/merge-mop) (:mops simple-heat-flow) (:mops simple-water-flow))
                            mops/infer-hierarchy)
        expressions       (->> [simple-water-flow simple-heat-flow]
                            (map (comp keys :mops))
                            (apply concat))]

    (t/testing "Creating match hypotheses"
      (t/is (= expected-same-functor-matches
              (->> (for [b expressions
                         t expressions]
                     [b t])
                (mapcat (partial sut/apply-rule full-kg (first (vals sut/mops-literal-similarity))))
                (remove nil?)
                set))))))
