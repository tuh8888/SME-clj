(ns sme-clj.util
  "Utility/helper functions")

(defn vals-as-keys [k m]
  (zipmap (map k m)
    m))
