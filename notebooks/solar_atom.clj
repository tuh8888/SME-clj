(ns solar-atom
  (:require [mop-records :as mr]
            [mops :as mops]
            [sme-clj.core :as sme]
            [sme-clj.typedef :as types]
            [sme-clj.ruledef :as rules]))
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

(->> (sme/match kg rules/literal-similarity :solar-system :rutherford-atom)
  (sme/perform-inference kg :solar-system)
  (map :transferred))
