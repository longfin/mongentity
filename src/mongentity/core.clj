(ns mongentity.core
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.query :as query])
  (:use [clojure.string :only [lower-case split]])
  (:import [org.bson.types ObjectId]))

(defprotocol IEntity
  "Protocol of whole entity."
  (add! [e])
  (edit! [e])
  (remove! [e])
  (init [e]))

(defprotocol IDocumentable
  "Representable protocol for mongodb's document name"
  (docname [e]))

(extend-protocol IDocumentable
  java.lang.String
  (docname [e]
    (lower-case e))
  clojure.lang.Symbol
  (docname [e]
    (docname (str e)))
  java.lang.Class
  (docname [e]
    (let [fullname (.getName e)]
      (docname (last (split fullname #"\."))))))

(def entity-fns
  {:init (fn [e] e)
   :add! (fn [e]
           (mc/insert-and-return (docname (type e)) e))
   :edit! (fn [e]
            (mc/update (docname (type e)) {:_id (:_id e)} e))
   :remove! (fn [e]
              (let [id (or (:_id e)
                           (ObjectId. e))]
                (mc/remove-by-id (docname (type e)) id)))})


(defmacro defentity [type args & body]
  (let [name (docname type)
        constructor (symbol (str type "/create"))
        fn-get-by-id (symbol (str "get-" name "-by-id"))
        fn-get (symbol (str "get-" name))
        fn-gets (symbol (str "get-" name "s"))
        mc-with-col (symbol (str "with-" name "s"))]
    `(do
       (defrecord ~type ~args
         ~@body)
       (extend ~type
         IEntity
         entity-fns)
       (let [constructor# (fn [m#]
                            (init (~constructor m#)))]
         (defn ~fn-get [p#]
           (let [m# (mc/find-one-as-map ~name p#)]
             (when m#
               (constructor# m#))))
         (defn ~fn-gets [p#]
           (let [ms# (mc/find-maps ~name p#)]
             (map (fn [m#] (constructor# m#)) ms#)))
         (defn ~fn-get-by-id [id#]
           (try
             (~fn-get {:_id (ObjectId. id#)})
             (catch java.lang.IllegalArgumentException iae#
               nil)))
         (defmacro ~mc-with-col [& body#]
           `(query/with-collection ~~name ~@body#))
         nil))))
