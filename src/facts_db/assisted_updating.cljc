(ns facts-db.assisted-updating
  (:require
   [bindscript.api :refer [def-bindscript]]
   [facts-db.updating :as updating]
   [facts-db.validating :as validating]))


(defn add-children
  [db parent-entity-id parent-entity-reference-fact childs]
  (validating/validate-db db)

  ;; parent-entity-id is optional
  (if-not parent-entity-id
    (updating/update-facts db childs)
    (let [parent-entity (get db parent-entity-id)
          _ (if-not parent-entity
              (throw (ex-info (str "Parent entity does not exist: " parent-entity-id)
                              {:parent-entity-id parent-entity-id
                               :parent-entity-reference-fact parent-entity-reference-fact
                               :childs childs})))

          single-child? (not (updating/collection? childs))
          childs (updating/conform-entities db childs)
          childs-ids (map :db/id childs)

          reference-value (get parent-entity parent-entity-reference-fact)
          reference-value (if (set? reference-value)
                            (into reference-value childs-ids)
                            (if single-child?
                              (first childs-ids)
                              (into #{} childs-ids)))
          new-parent {:db/id parent-entity-id
                      parent-entity-reference-fact reference-value}]
      (updating/update-facts
       db (conj childs new-parent)))))


(def-bindscript ::add-children
  db (updating/new-db)
  db (updating/update-facts db {:db/id 1
                                :name "Witek"
                                :friends #{}})
  db (add-children db 1 :friends [{:name "Hogi"} {:name "Kace"}])
  witek (get db 1))


(defn add-children-multiple
  [db paths childs]
  (reduce
   (fn [db [parent-entity-id parent-entity-reference-fact]]
     (add-children db parent-entity-id parent-entity-reference-fact childs))
   db
   paths))


(defn remove-children
  [db parent-entity-id parent-entity-reference-fact childs-ids]
  (validating/validate-db db)
  (let [parent-entity (get db parent-entity-id)
        _ (if-not parent-entity
            (throw (ex-info (str "Parent entity does not exist: " parent-entity-id)
                            {:parent-entity-id parent-entity-id
                             :parent-entity-reference-fact parent-entity-reference-fact
                             :childs-ids childs-ids})))

        single-child? (not (updating/collection? childs-ids))
        childs-ids (if single-child? #{childs-ids} childs-ids)

        reference-value (get parent-entity parent-entity-reference-fact)
        reference-value (if (set? reference-value)
                          (apply disj (into [reference-value] childs-ids))
                          nil)
        new-parent {:db/id parent-entity-id
                    parent-entity-reference-fact reference-value}]
    (updating/update-facts
     db [new-parent])))


(def-bindscript ::remove-children
  db (updating/new-db)
  db (updating/update-facts db {:db/id 1
                                :name "Witek"
                                :friends #{2 3}})
  db (remove-children db 1 :friends [2])
  witek (get db 1))
