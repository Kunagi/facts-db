(ns facts-db.api
  (:require
   [bindscript.api :refer [def-bindscript]]

   [facts-db.validating :as validating]
   [facts-db.reading :as reading]
   [facts-db.updating :as updating]
   [facts-db.assisted-updating :as assisted-updating]))

;;;

(defn new-uuid
  []
  (str #?(:cljs (random-uuid)
          :clj  (java.util.UUID/randomUUID))))


(def new-db updating/new-db)

(defn ++
  ([db entities]
   (updating/update-facts db entities))
  ([db paths childs]
   (assisted-updating/add-children-multiple db paths childs))
  ([db parent-entity-id parent-entity-reference-fact childs]
   (assisted-updating/add-children db parent-entity-id parent-entity-reference-fact childs)))


(def ++- assisted-updating/remove-children)

(def merge-db updating/merge-db)


(defn tree
  [db id refs]
  (reading/tree db id refs))


(def-bindscript ::full-stack
  db (new-db)
  db (validating/validate-db db))
