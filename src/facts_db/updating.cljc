(ns facts-db.updating
  (:require
   [bindscript.api :refer [def-bindscript]]
   [facts-db.validating :as validating]
   [clojure.set :as set]))


(defn new-db
  []
  {:db/config {:db/id :db/config}})


(defn new-uuid
  []
  (str #?(:cljs (random-uuid)
          :clj  (java.util.UUID/randomUUID))))


(defn- update-entity-fact
  [entity [k v]]
  (let [k-name (name k)]
    (cond

      (.endsWith k-name "+1")
      (let [k (keyword (namespace k) (.substring k-name 0 (.indexOf k-name "+1")))]
        (update entity k conj v))

      (.endsWith k-name "+n")
      (let [k (keyword (namespace k) (.substring k-name 0 (.indexOf k-name "+n")))]
        (update entity k into v))

      (.endsWith k-name "-1")
      (let [k (keyword (namespace k) (.substring k-name 0 (.indexOf k-name "-1")))]
        (update entity k disj v))

      (.endsWith k-name "-n")
      (let [k (keyword (namespace k) (.substring k-name 0 (.indexOf k-name "-n")))]
        (update entity k set/difference v))

      :else
      (assoc entity k v))))


(def-bindscript ::update-entity-fact
  e {:db/id 1
     :colors #{:red}}
  e (update-entity-fact e [:colors+1 :green])
  e (update-entity-fact e [:colors+n [:blue :yellow]])
  e (update-entity-fact e [:colors-1 :green])
  e (update-entity-fact e [:colors-n [:blue :yellow]]))


(defn- update-entity-
  [db new-facts]
  (let [id (:db/id new-facts)
        entity (or (get db id) {:db/id id})
        entity (reduce update-entity-fact entity new-facts)]
    (assoc db id entity)))


(defn- update-entities
  [db entities]
  (reduce update-entity- db entities))


(defn assoc-id-if-missing
  [entity]
  (if (:db/id entity)
    entity
    (assoc entity :db/id (new-uuid))))


(defn- conform-cr-entity [db entity]
  (if-not (map? entity)
    (throw (ex-info "Entity map expected."
                    {:entity entity})))
  (-> entity
      (assoc-id-if-missing)))


(def-bindscript ::conform-cr-entity
  e (conform-cr-entity nil {:name "witek"}))


(defn collection? [value]
  (or (vector? value)
      (set? value)
      (list? value)))


(defn conform-cr-to-collection [entities db]
  (if-not (collection? entities)
    (conform-cr-to-collection db (list entities))
    (map (partial conform-cr-entity db) entities)))


;; (defn extract-foreign-facts-from-entity [entity]
;;   (reduce
;;    (fn [])))

(defn find-foreign-facts-in-entity [entity]
  (filter
   #(-> % name (.endsWith "->"))
   (keys entity)))


(defn conform-cr-foreign-refs [entities db]
  (reduce
   (fn [entities entity]
     (let [foreign-facts (find-foreign-facts-in-entity entity)]
       (reduce
        (fn [entities foreign-fact]
          (let [foreign-id (get entity foreign-fact)
                foreign-fact-name (name foreign-fact)
                foreign-fact (keyword (namespace foreign-fact)
                                      (.substring foreign-fact-name
                                                  0
                                                  (- (count foreign-fact-name) 2)))]
            (-> entities
                (conj {:db/id foreign-id foreign-fact (:db/id entity)}))))
        entities
        foreign-facts)))
   entities
   entities))


(def-bindscript ::conform-cr-foreign-refs
  db (new-db)
  db (assoc db 1 {:db/id 1
                  :friends #{}})
  cr (conform-cr-foreign-refs [{:db/id 2
                                :friends+1-> 1}]
                              db))



(defn conform-cr [cr db]
  (-> cr
      (conform-cr-to-collection db)
      (conform-cr-foreign-refs db)))


(def-bindscript ::conform-cr
  cr (conform-cr {:name "Witek"} nil)
  cr (conform-cr [{:name "Witek"} {:name "Hogi"}] nil))




(defn update-facts
  "Update one or multiple entities.
  Only provided facts are updated. Existing facts stay unchanged."
  [db change-request]
  (validating/validate-db db)
  (let [change-entities (conform-cr change-request db)]
    (update-entities db change-entities)))



(def-bindscript ::update-facts
  db (new-db)
  db (update-facts db {:db/id 1 :name "Witek"})
  :spec :db/db
  witek (get db 1)
  :spec #(= {:db/id 1 :name "Witek"} %)

  ;; db    {1 {:db/id 1
  ;;           :name "Witek"}
  ;;        2 {:db/id 2
  ;;           :name "Hogi"}}

  db    (update-facts db [{:db/id 1 :name "Koczewski"}
                          {:db/id 2 :name "Hager"}])
  :spec :db/db)


(defn merge-db
  "Merge facts from `db2` into `db`."
  [db db2]
  (update-entities db (vals db2)))
