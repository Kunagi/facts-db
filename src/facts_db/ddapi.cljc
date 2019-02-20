(ns facts-db.ddapi
  (:require
   [facts-db.api :as db]))


;; (defonce !callbacks (atom {:on-def-event nil
;;                            :on-def-query nil}))


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


(defn def-api
  [api-id & {:keys [db-constructor]}]
  (defmethod create-db api-id [api args]
    (cond-> (db/new-db)

      db-constructor
      (db-constructor args)

      true
      (assoc-in [:db/config :events>] events>)

      true
      (assoc-in [:db/config :<query] <query))))



(defn def-event
  [event-id & {:keys [handler]}]
  (defmethod apply-event event-id [db event-id args]
    (handler db args)))
;; (if-let [on-def-event (get @!callbacks :on-def-event)]
;;   (on-def-event)))


(defn def-query
  [query-id & {:keys [handler]}]
  (defmethod run-query query-id [db query-id args]
    (handler db args)))
