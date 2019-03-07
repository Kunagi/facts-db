(ns facts-db.api
  (:require
   [bindscript.api :refer [def-bindscript]]

   [facts-db.validating :as validating]
   [facts-db.reading :as reading]
   [facts-db.updating :as updating]
   [facts-db.updating2 :as updating2]
   [facts-db.assisted-updating :as assisted-updating]))

;;;

(defn new-uuid
  []
  (str #?(:cljs (random-uuid)
          :clj  (java.util.UUID/randomUUID))))


(def new-db updating/new-db)

(def update-facts updating2/update-facts)


;; TODO deprecated
(defn ++
  ([db entities]
   (updating/update-facts db entities))
  ([db paths childs]
   (assisted-updating/add-children-multiple db paths childs))
  ([db parent-entity-id parent-entity-reference-fact childs]
   (assisted-updating/add-children db parent-entity-id parent-entity-reference-fact childs)))


;; TODO deprecated
(def ++- assisted-updating/remove-children)

(def merge-db updating/merge-db)


(defn contains-entity?
  [db id]
  (reading/contains-entity? db id))


(defn fact
  [db entity-id fact-name]
  (reading/fact db entity-id fact-name))


(defn tree
  [db id refs]
  (reading/tree db id refs))


(defn tree-or-nil
  [db id refs]
  (if (reading/contains-entity? db id)
    (tree db id refs)))


(defn trees
  [db id refs]
  (reading/trees db id refs))


(def-bindscript ::full-stack
  db (new-db)

  db (++ db {:db/id "w"
             :name "Witek"
             :colors #{:red}})

  db (++ db {:db/id "w"
             :colors+1 :green})

  _  (fact db "w" :colors))


  ;; db (++ db {:db/id "k"
  ;;            :name "Kacper"}))
