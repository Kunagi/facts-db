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
(s/def ::event      (s/cat :id    ::event-id
                           :args  (s/? ::event-args)))
(s/def ::events (s/coll-of ::event))

(s/def ::query-id   qualified-keyword?)
(s/def ::query-args map?)
(s/def ::query      (s/cat :id    ::query-id
                           :args  (s/? ::query-args)))

(s/def ::api-definition (s/keys))

(s/def ::event-handler fn?)

(s/def ::query-handler fn?)


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
  [db query]
  (validate ::<query
            [:map  db    ::db
             :val  query ::query])
  (let [[query-id query-args] query
        query-args (or query-args {})]
    (run-query db query-id query-args)))


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
  [event-id event-handler]
  (validate ::def-event
            [:val event-id ::event-id]
            [:val event-handler ::event-handler])
  (let [event {:id event-id
               :handler event-handler}
        api-id (keyword (namespace event-id))]
    (defmethod apply-event event-id [db event-id args]
      (event-handler db args))
    (swap! !apis assoc-in [api-id :events event-id] event)))


(defn def-query
  [query-id query-handler]
  (validate ::def-query
            [:val query-id ::query-id]
            [:val query-handler ::query-handler])
  (let [query {:id query-id
               :handler query-handler}
        api-id (keyword (namespace query-id))]
    (defmethod run-query query-id [db query-id args]
      (query-handler db args))
    (swap! !apis assoc-in [api-id :queries query-id] query)))



;;; integrator api


(defn defined-apis
  []
  (-> @!apis vals))
