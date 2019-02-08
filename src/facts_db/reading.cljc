(ns facts-db.reading
  (:require
   [bindscript.api :refer [def-bindscript]]
   [facts-db.validating :as validating]))


(defn entity
  "Return a single entity by `id`."
  [db id]
  (if-let [entity (get db id)]
    entity
    (throw (ex-info (str "Entity does not exist: " id)
                    {:id id
                     :existing (keys db)}))))


(defn entities
  "Return a collection of entities by `id`."
  [db ids]
  (map (partial entity db) ids))


(defn- resolve-references
  [e refs db]
  (reduce
   (fn [e [ref-key sub-refs]]
     (if-let [value (get e ref-key)]
       (if (coll? value)
         (assoc e
                ref-key
                (map #(resolve-references (entity db %) sub-refs db)
                     value))
         (assoc e
                ref-key
                (resolve-references (entity db value) sub-refs db)))
       e))
   e
   refs))


(defn tree
  "Return a single entity by `id`, while resolving references `refs`."
  [db id refs]
  (validating/validate-db db)
  (-> db
      (entity id)
      (resolve-references refs db)))


(defn trees
  "Return a collection of entities by `ids`, while resolving references `refs`."
  [db ids refs]
  (map #(tree db % refs) ids))


(def-bindscript ::full-stack
  db          {:db/config {:db/id :db/config}
               1 {:db/id 1
                  :name "Homer"
                  :partner 2
                  :children #{3 4}}
               2 {:db/id 2
                  :name "Marge"
                  :partner 1
                  :children #{3 4}}
               3 {:db/id 3
                  :name "Bart"}
               4 {:db/id 4
                  :name "Lisa"}}

  homer        (entity db 1)
  bart+homer   (entities db [3 1])

  homer        (tree db 1 {})
  homer+family (tree db 1 {:partner {} :children {}})
  deep         (tree db 1 {:partner {:partner {}}})

  all          (trees db [1 2 3 4] {:partner {} :children {}}))
