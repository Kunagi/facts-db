(ns facts-db.ddapi
  (:require
   [facts-db.api :as db]))



(defmulti apply-event (fn [db event args] event))

(defmulti run-query (fn [db query args] query))

(defmulti create-db (fn [api args] api))



;;; client api

(defn events>
  [model events]
  (reduce
   (fn [model [event-name args]]
     (apply-event model event-name args))
   model
   events))


(defn <query
  [model query args]
  (run-query model query args))


(defn new-db
  [api args]
  (create-db api args))


;;; implementor api


(def !apis (atom {}))


(defn def-api
  [api-id & {:as api :keys [db-constructor
                            db-instance-identifier-args-key]}]
  (let [api (assoc api :id api-id)]
    (defmethod create-db api-id [api args]
      (cond-> (db/new-db)

        db-constructor
        (db-constructor args)

        true
        (assoc-in [:db/config :events>] events>)

        true
        (assoc-in [:db/config :<query] <query)))
    (swap! !apis assoc api-id api)))


(defn def-event
  [event-id & {:as event :keys [handler]}]
  (let [event (assoc event :id event-id)
        api-id (keyword (namespace event-id))]
    (defmethod apply-event event-id [db event-id args]
      (handler db args))
    (swap! !apis assoc-in [api-id :events event-id] event)))


(defn def-query
  [query-id & {:as query :keys [handler]}]
  (let [query (assoc query :id query-id)
        api-id (keyword (namespace query-id))]
    (defmethod run-query query-id [db query-id args]
      (handler db args))
    (swap! !apis assoc-in [api-id :queries query-id] query)))



;;; integrator api


(defn defined-apis
  []
  (-> @!apis vals))
