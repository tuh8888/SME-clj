(ns solar-atom
  (:require [mop-records :as mr]
            [mops :as mops]
            [sme-clj.core :as sme]
            [sme-clj.ruledef :as rules]
            [sme-clj.typedef :as types]
            [ubergraph.core :as uber]))

(def kg
  (as-> (mr/make-mop-map) m
    (types/initialize-kg m)

    (reduce (partial apply types/add-mop-entity)
      m [[:mass ::types/Function nil ::types/Entity]
         [:charge ::types/Function nil ::types/Entity]
         [:greater ::types/Relation nil ::types/Entity ::types/Entity]
         [:attracts ::types/Relation nil ::types/Entity ::types/Entity]
         [:revolve-around ::types/Relation nil ::types/Entity ::types/Entity]
         [:temperature ::types/Function nil ::types/Entity]
         [:cause ::types/Relation nil ::types/Expression ::types/Expression]
         [:and ::types/Relation nil ::types/Expression ::types/Expression]
         [:gravity ::types/Relation nil ::types/Expression ::types/Expression]
         [:opposite-sign ::types/Relation nil ::types/Expression ::types/Expression]

         [:Sun ::types/Entity nil]
         [:Planet ::types/Entity nil]
         [:Nucleus ::types/Entity nil]
         [:Electron ::types/Entity nil]])
    (apply types/add-concept-graph m :solar-system
      (let [attracts    [:attracts :Sun :Planet]
            mass-sun    [:mass :Sun]
            mass-planet [:mass :Planet]]
        [[:cause
          [:and
           [:greater mass-sun mass-planet]
           attracts]
          [:revolve-around :Planet :Sun]]
         [:greater
          [:temperature :Sun]
          [:temperature :Planet]]
         [:cause
          [:gravity mass-sun mass-planet]
          attracts]]))
    (apply types/add-concept-graph m :rutherford-atom
      (let [mass-nucleus  [:mass :Nucleus]
            mass-electron [:mass :Electron]]
        [[:greater mass-nucleus mass-electron]
         [:revolve-around :Electron :Nucleus]
         [:cause
          [:opposite-sign
           [:charge :Nucleus]
           [:charge :Electron]]
          [:attracts :Nucleus :Electron]]]))
    (mops/infer-hierarchy m)))

(defn as-graph
  [m & {:keys [include-abstrs?]}]
  (reduce
    (fn [g [source {:keys [concept-graph] :as mop}]]
      (reduce
        (fn [g [role targets]]
          (reduce
            (fn [g target]
              (uber/add-directed-edges g [source target {:label role}]))
            g targets))
        (uber/add-nodes-with-attrs g [source {:color (if (#{#{:solar-system}} concept-graph)
                                                       :blue
                                                       :red)}])
        (dissoc mop :id :inst? :names :concept-graph (when-not include-abstrs? :parents))))
    (uber/multidigraph)
    (:mops m)))

(defn map-vals
  [f m]
  (->> m
    (map (juxt key (comp f val)))
    (into {})))


(let [boring-mops (-> (mr/make-mop-map)
                    types/initialize-kg
                    :mops
                    keys
                    set)]
  (-> kg
    (update :mops (fn [mops]
                    (map-vals
                      (fn [mop]
                        (->> (dissoc mop :id :inst? :names)
                          (remove (fn [[_ filler]]
                                    (some boring-mops (cond-> filler (complement coll?) vector))))
                          (into {})))
                      (apply dissoc mops boring-mops))))
    as-graph
    uber/viz-graph))

(->> kg
  :mops
  vals
  (map :concept-graph))

(->> (sme/match kg rules/analogy :solar-system :rutherford-atom)
  (sme/perform-inference kg :solar-system)
  (map :transferred))
