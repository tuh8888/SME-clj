(ns calcium-neuron
  (:require [mops.records :as mr]
            [mops.core :as mops]
            [sme-clj.core :as sme]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [ubergraph.core :as uber]))

(defn map-vals
  [f m]
  (->> m
       (map (juxt key (comp f val)))
       (into {})))

(defn as-graph
  [m & {:keys [include-abstrs?]}]
  (reduce
   (fn [g
        [source
         {:keys [concept-graph]
          :as   mop}]]
     (reduce
      (fn [g [role targets]]
        (reduce (fn [g target]
                  (uber/add-directed-edges g [source target {:label role}]))
                g
                targets))
      (uber/add-nodes-with-attrs
       g
       [source
        {:color (cond (#{#{:neuron-signaling}} concept-graph)  :blue
                      (#{#{:calcium-signaling}} concept-graph) :red
                      (nil? concept-graph)                     :black
                      :else                                    :purple)}])
      (dissoc mop
              :id
              :inst?         :names
              :concept-graph (when-not include-abstrs? :parents))))
   (uber/multidigraph)
   (:mops m)))

(defn visualize-analogy
  [kg]
  (let [functors       (->> kg
                            mops/mop-ids
                            (filter #(mops/abstr? kg % ::types/Functor)))
        concept-graphs (->> kg
                            mops/mop-ids
                            (filter #(mops/abstr? kg % ::types/ConceptGraph)))
        boring-mops    (-> (mr/make-mop-map)
                           types/initialize-kg
                           :mops
                           keys
                           set
                           (into functors))]
    (-> kg
        (update
         :mops
         (fn [mops]
           (map-vals (fn [mop]
                       (->> (dissoc mop :id :inst? :names :functor)
                            (remove (fn [[_ filler]]
                                      (some boring-mops
                                            (cond-> filler
                                              (not (coll? filler)) vector))))
                            (into {})))
                     (apply dissoc mops (into boring-mops concept-graphs)))))
        as-graph
        uber/viz-graph)))

#_(def kg
    (as-> (mr/make-mop-map) m
      (types/initialize-kg m)
      (reduce
       (partial apply types/add-entity)
       m
       [[:activates ::types/Function nil ::types/Entity ::types/Entity]
        [:flows ::types/Function nil ::types/Entity ::types/Entity]
        [:waves ::types/Relation nil ::types/Expression ::types/Expression]
        [:sparks ::types/Relation nil ::types/Expression ::types/Expression]
        [:cause ::types/Relation nil ::types/Expression ::types/Expression]
        [:deactivates ::types/Function nil ::types/Entity ::types/Entity]
        [:and
         ::types/Relation
         {:ordered? false}
         ::types/Expression
         ::types/Expression]
        [:convert ::types/Function nil ::types/Entity ::types/Entity]])
      (reduce (partial apply types/add-entity)
              m
              [[:Calcium ::types/Entity nil]
               [:Ryanodine-Receptor ::types/Entity nil]
               [:Voltage-Gated-Channel ::types/Entity nil]
               [:Acid ::types/Entity nil]
               [:High-Membrane-Potential ::types/Entity nil]
               [:Low-Membrane-Potential ::types/Entity nil]
               [:Sodium ::types/Entity nil]])
      (apply
       types/add-concept-graph
       m
       :calcium-signaling
       (let [acr [:activates :Calcium :Ryanodine-Receptor]
             fcr [:flows :Calcium :Ryanodine-Receptor]]
         [[:waves acr fcr]
          [:sparks [:and [:deactivates :Acid :Ryanodine-Receptor] acr] fcr]]))
      (apply
       types/add-concept-graph
       m
       :neuron-signaling
       (let [aai [:activates :Low-Membrane-Potential :Voltage-Gated-Channel]
             fsv [:flows :Sodium :Voltage-Gated-Channel]]
         [[:sparks
           aai
           [:and
            [:deactivates :High-Membrane-Potential :Voltage-Gated-Channel]
            aai]
           fsv]
          [:cause
           fsv
           [:convert :Low-Membrane-Potential :High-Membrane-Potential]]]))
      (mops/infer-hierarchy m)))

(def kg
  (as-> (mr/make-mop-map) m
    (types/initialize-kg m)
    (reduce
     (partial apply types/add-entity)
     m
     [[:flow ::types/Relation nil ::types/Entity ::types/Entity]
      [:waves ::types/Function nil ::types/Expression ::types/Expression]
      [:sparks ::types/Function nil ::types/Expression ::types/Expression]
      [:cause ::types/Relation nil ::types/Expression ::types/Expression]
      [:low ::types/Function nil ::types/Entity]
      [:high ::types/Function nil ::types/Entity]
      [:active ::types/Function nil ::types/Entity]
      [:inactive ::types/Function nil ::types/Entity]
      [:signaling ::types/Function nil ::types/Entity]
      [:and
       ::types/Relation
       {:ordered? false}
       ::types/Expression
       ::types/Expression]
      [:convert ::types/Relation nil ::types/Expression ::types/Expression]
      [:implies ::types/Function nil ::types/Expression ::types/Expression]])
    (reduce (partial apply types/add-entity)
            m
            [[:Calcium ::types/Entity nil]
             [:Calcium-Channel ::types/Entity nil]
             [:Sodium-Channel ::types/Entity nil]
             [:Potassium-Channel ::types/Entity nil]
             [:Acid ::types/Entity nil]
             [:Membrane-Potential ::types/Entity nil]
             [:Sodium ::types/Entity nil]
             [:Potassium ::types/Entity nil]
             [:Signal ::types/Entity nil]])
    (apply types/add-concept-graph
           m
           :calcium-signaling
           (let [low-ca    [:low :Calcium]
                 high-ca   [:high :Calcium]
                 act-cac   [:active :Calcium-Channel]
                 inact-cac [:inactive :Calcium-Channel]
                 flow-cac  [:flow :Calcium :Calcium-Channel]
                 l2h       [:convert low-ca high-ca]
                 h2l       [:convert high-ca low-ca]
                 ca-sparks [:sparks l2h h2l]
                 ca-waves  [:waves h2l h2l]]
             [[:cause high-ca [:convert inact-cac act-cac]]
              [:cause act-cac flow-cac]
              [:cause flow-cac h2l]
              [:cause :Acid [:convert ca-waves ca-sparks]]]))
    (apply types/add-concept-graph
           m
           :neuron-signaling
           (let [low-mp        [:low :Membrane-Potential]
                 high-mp       [:high :Membrane-Potential]
                 mid-mp        [:mid :Membrane-Potential]
                 act-sc        [:active :Sodium-Channel]
                 inact-sc      [:inactive :Sodium-Channel]
                 act-kc        [:active :Potassium-Channel]
                 inact-kc      [:inactive :Potassium-Channel]
                 flow-sc       [:flow :Sodium :Sodium-Channel]
                 flow-kc       [:flow :Potassium :Potassium-Channel]
                 sig           [:signaling :Signal]
                 l2h           [:convert low-mp high-mp]
                 h2l           [:convert high-mp low-mp]
                 neuron-sparks [:sparks l2h h2l]
                 neuron-waves  [:waves h2l h2l]]
             [[:cause sig [:convert low-mp mid-mp]]
              [:cause mid-mp [:convert inact-sc act-sc]]
              [:cause act-sc flow-sc]
              [:cause flow-sc [:convert mid-mp high-mp]]
              [:cause high-mp [:convert act-sc inact-sc]]
              [:cause high-mp [:convert inact-kc act-kc]]
              [:cause act-kc flow-kc]
              [:cause flow-kc h2l]
              [:cause sig [:convert neuron-waves neuron-sparks]]
              [:implies
               [:and [:convert low-mp mid-mp] [:convert mid-mp high-mp]]
               l2h]]))
    (mops/infer-hierarchy m)))


(->> gmaps
     first
     second
     #_((some-fn (partial every? (rules/strict-entity? kg))
                 (partial every? (rules/functor-function? kg))
                 (every-pred (partial every? (rules/attribute-functor? kg))
                             (partial apply rules/same-functor? kg))))
     ((get-in rules/analogy [:same-functor :body]) kg))

;; Visualize the kg
#_(visualize-analogy kg)

[:Sodium-Channel :convert-low-Calcium-high-Calcium]
(def gmaps
  (->> rules/analogy
       (sme/match kg :neuron-signaling :calcium-signaling)))

(def inferences
  (->> gmaps
       (sme/perform-inference kg)
       (sort-by :score)
       (reverse)
       (map :transferred)))

(def new-kg
  (-> (apply types/add-concept-graph kg :new-inferences (first inferences))
      mops/infer-hierarchy))

(visualize-analogy new-kg)

#_(rules/apply-rule
   kg
   (rules/analogy :compatible-args)
   [:cause-gravity-mass-Sun-mass-Planet-attracts-Sun-Planet
    :cause-opposite-sign-charge-Nucleus-charge-Electron-attracts-Nucleus-Electron])
