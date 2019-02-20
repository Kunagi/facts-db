(ns facts-db.ddapi
  (:require
   [clojure.spec.alpha :as s]

   [conform.api :refer [validate]]
   [facts-db.api :as db]))


;;; spec

(s/def ::db (s/and map?
                   #(= ::identifier (get-in % [:db/config ::identifier]))))


(s/def ::api-id simple-keyword?)

(s/def ::event-id qualified-keyword?)
(s/def ::event-args map?)
(s/def ::event (s/cat :event-id ::event-id :event-args ::event-args))
(s/def ::events (s/coll-of ::event))

(s/def ::query-id qualified-keyword?)
(s/def ::query-args map?)

(s/def ::api-definition (s/keys))

(s/def ::event-definition (s/keys))

(s/def ::query-definition (s/keys))


;;;


(defmulti apply-event (fn [db event args] event))

(defmulti run-query (fn [db query args] query))

(defmulti create-db (fn [api args] api))


;;; client api

(defn events>
  [db events]
  (validate ::events>
            [:map db ::db]
            [:val events ::events])
  (reduce
   (fn [db [event-id args]]
     (apply-event db event-id args))
   db
   events))


(defn <query
  [db query-id args]
  (validate ::<query
            [:map  db       ::db
             :val  query-id ::query-id
             :args args     ::query-args])
  (run-query db query-id args))


(defn new-db
  [api args]
  (create-db api args))


;;; implementor api


(def !apis (atom {}))


(defn def-api
  [api-id & {:as api :keys [db-constructor
                            db-instance-identifier-args-key]}]
  (validate ::def-api
            [:val api-id ::api-id]
            [:val api ::api-definition])
  (let [api (assoc api :id api-id)]
    (defmethod create-db api-id [api args]
      (cond-> (db/new-db)

        true
        (assoc-in [:db/config ::identifier] ::identifier)

        db-constructor
        (db-constructor args)

        true
        (assoc-in [:db/config :events>] events>)

        true
        (assoc-in [:db/config :<query] <query)))
    (swap! !apis assoc api-id api)))


(defn def-event
  [event-id & {:as event :keys [handler]}]
  (validate ::def-event
            [:val event-id ::event-id]
            [:val event ::event-definition])
  (let [event (assoc event :id event-id)
        api-id (keyword (namespace event-id))]
    (defmethod apply-event event-id [db event-id args]
      (handler db args))
    (swap! !apis assoc-in [api-id :events event-id] event)))


(defn def-query
  [query-id & {:as query :keys [handler]}]
  (validate ::def-query
            [:val query-id ::query-id]
            [:val query ::query-definition])
  (let [query (assoc query :id query-id)
        api-id (keyword (namespace query-id))]
    (defmethod run-query query-id [db query-id args]
      (handler db args))
    (swap! !apis assoc-in [api-id :queries query-id] query)))



;;; integrator api


(defn defined-apis
  []
  (-> @!apis vals))
