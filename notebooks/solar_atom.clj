(ns solar-atom
  (:require [mop-records :as mr]
            [mops :as mops]
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
    (fn [g [source {:keys [concept-graph] :as mop}]]
      (reduce
        (fn [g [role targets]]
          (reduce
            (fn [g target]
              (uber/add-directed-edges g [source target {:label role}]))
            g targets))
        (uber/add-nodes-with-attrs g [source {:color (cond
                                                       (#{#{:solar-system}} concept-graph)    :blue
                                                       (#{#{:rutherford-atom}} concept-graph) :red
                                                       :else                                  :green)}])
        (dissoc mop :id :inst? :names :concept-graph (when-not include-abstrs? :parents))))
    (uber/multidigraph)
    (:mops m)))

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
         [:and ::types/Relation {:ordered? false} ::types/Expression ::types/Expression]
         [:gravity ::types/Function nil ::types/Expression ::types/Expression]
         [:opposite-sign ::types/Function nil ::types/Expression ::types/Expression]

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

;; Visualize the kg
#_(let [boring-mops (-> (mr/make-mop-map)
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
                                      (some boring-mops (cond-> filler (not (coll? filler)) vector))))
                            (into {})))
                        (apply dissoc mops (into boring-mops [:solar-system :rutherford-atom])))))
      as-graph
      uber/viz-graph))

(->> rules/analogy
  (sme/match kg :solar-system :rutherford-atom)
  (sme/perform-inference kg :solar-system)
  (sort-by :score)
  (reverse)
  (map :transferred))

#_(rules/apply-rule kg (rules/analogy :compatible-args) [:cause-gravity-mass-Sun-mass-Planet-attracts-Sun-Planet
                                                         :cause-opposite-sign-charge-Nucleus-charge-Electron-attracts-Nucleus-Electron])
