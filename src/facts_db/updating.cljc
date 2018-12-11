(ns facts-db.updating
  (:require
   [bindscript.api :refer [def-bindscript]]))


(defn new-db
  []
  {})


(defn update-entity
  "Update a single entity.
  Only provided facts are updated. Existing facts stay unchanged."
  [db new-facts]
  (let [id (:db/id new-facts)
        entity (get db id)
        entity (if entity entity {:db/id id})
        entity (merge entity new-facts)]
    (assoc db id entity)))


(defn update-entities
  "Update multiple entities.
  Only provided facts are updated. Existing facts stay unchanged."
  [db entities]
  (reduce update-entity db entities))


(def-bindscript ::update-entities
  db {}
  db    {1 {:db/id 1
            :name "Witek"}
         2 {:db/id 2
            :name "Hogi"}}
  :spec :db/db

  db    (update-entities db [{:db/id 1 :name "Koczewski"}
                             {:db/id 2 :name "Hager"}])
  :spec :db/db)


(defn merge-db
  "Merge facts from `db2` into `db`."
  [db db2]
  (update-entities db (vals db2)))
