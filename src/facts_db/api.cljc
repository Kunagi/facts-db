(ns facts-db.api
  (:require
   [bindscript.api :refer [def-bindscript]]

   [facts-db.validating :as validating]
   [facts-db.reading :as reading]
   [facts-db.updating :as updating]))




;;;


(def new-db updating/new-db)

(def update-entity updating/update-entity)

(def update-entities updating/update-entities)

(def merge-db updating/merge-db)



(defn pull
  [db pattern])


(def-bindscript ::full-stack
  db (new-db)
  db (validating/validate-db db))
