(ns sme-clj.simple-water-heat
  (:require [mop-records :as mr]
            [sme-clj.typedef :as types]
            [sme-clj.util :as util]
            [mops :as mops]))

(defn map-vals
  [f m]
  (->> m
    (map (juxt key (comp f val)))
    (into {})))

(defn add-concept-graph
  [kg k & expressions]
  (let [concept-graph (apply types/make-concept-graph k expressions)]
    (-> (merge-with (fn [v1 v2]
                      (throw (ex-info "Value already in kg"
                               {:v1 v1 :v2 v2})))
          kg
          (->> (:graph concept-graph)
            (map-vals #(assoc % :concept-graph k))))
      (assoc k {:name k
                :type :ConceptGraph
                :spec (:spec concept-graph)}))))
(def kg (-> {}
          (merge (->> [[:Coffee]
                       [:Icecube]
                       [:Bar]
                       [:Heat]

                       [:Water]
                       [:Beaker]
                       [:Vial]
                       [:Pipe]]
                   (map (partial apply types/make-entity))
                   (util/vals-as-keys :name)))
          (merge (->> [[:flow :type :relation :arity 4]
                       [:greater :type :relation :arity 2]
                       [:cause :type :relation :arity 2]
                       [:temperature :type :function]
                       [:flat-top :type :function]
                       [:pressure :type :function]
                       [:diameter :type :function]
                       [:liquid :type :attribute]
                       [:clear :type :attribute]]
                   (map (partial apply types/make-predicate))
                   (util/vals-as-keys :name)))
          (add-concept-graph :simple-water-flow
            [:cause
             [:greater [:pressure :Beaker] [:pressure :Vial]]
             [:flow :Beaker :Vial :Water :Pipe]]
            [:greater [:diameter :Beaker] [:diameter :Vial]]
            [:clear :Beaker]
            [:flat-top :Water]
            [:liquid :Water])
          (add-concept-graph :simple-heat-flow
            [:flow :Coffee :Icecube :Heat :Bar]
            [:greater [:temperature :Coffee] [:temperature :Icecube]]
            [:flat-top :Coffee]
            [:liquid :Coffee])))


(defn make-mop
  [m id parent & [slots]]
  (let [mop (mops/->mop id slots)]
    (-> m
      (mops/add-mop mop)
      (mr/-add-slot-to-mop id :parents #{parent}))))

(defn mops-add-concept-graph
  [m k & expressions]
  (reduce (fn [m [parent & slots]]
            (let [id  (types/combine-ids (-> slots
                                           (->> (map second))
                                           (conj parent)))
                  mop (mops/->mop id (into {} slots))]
              (-> m
                (mops/add-mop mop)
                (mops/add-slot-to-mop id :parents parent)
                (mops/add-slot-to-mop id :concept-graph k))))
    m expressions))

(def mops-kg (-> (reduce (partial apply make-mop)
                   (mr/make-mop-map)
                   [[:cause ::types/Expression {:e1 ::types/Expression
                                                :e2 ::types/Expression}]
                    [:greater ::types/Expression {:e1 ::types/Expression
                                                  :e2 ::types/Expression}]
                    [:flow ::types/Expression {:e1 :Entity
                                               :e2 :Entity
                                               :e3 :Entity
                                               :e4 :Entity}]
                    [:Function ::types/Expression]
                    [:pressure :Function {:e1 :Entity}]
                    [:diameter :Function {:e1 :Entity}]
                    [:clear ::types/Expression {:e1 :Entity}]
                    [:temperature :Function {:e1 :Entity}]
                    [:flat-top :Function {:e1 :Entity}]
                    [:liquid ::types/Expression {:e1 :Entity}]
                    [:Coffee :Entity]
                    [:Water :Entity]
                    [:Heat :Entity]
                    [:Pipe :Entity]
                    [:Vial :Entity]
                    [:Icecube :Entity]
                    [:Bar :Entity]
                    [:Beaker :Entity]])

               (mops-add-concept-graph :simple-water-flow
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

               (mops-add-concept-graph :simple-heat-flow
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
               mops/infer-hierarchy))
