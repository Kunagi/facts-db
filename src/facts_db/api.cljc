(ns facts-db.api
  (:require
   [bindscript.api :refer [def-bindscript]]

   [facts-db.validating :as validating]
   [facts-db.reading :as reading]
   [facts-db.updating :as updating]
   [facts-db.assisted-updating :as assisted-updating]))

;;;


(def new-db updating/new-db)

(def ++ updating/update-facts)
(def +++ assisted-updating/add-children)
(def ++- assisted-updating/remove-children)

(def merge-db updating/merge-db)


(defn tree
  [db id refs]
  (reading/tree db id refs))


(def-bindscript ::full-stack
  db (new-db)
  db (validating/validate-db db))
