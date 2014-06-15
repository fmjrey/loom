(ns loom.io.clojure
  "Generic serializer using clojure pr-str and read-string."
  {:author "François Rey"}
  (:require [clojure.java.io :refer [as-file]]
            [loom.io.provider :refer [any-format-id any-format-supported
                                      encode decode implementation
                                      IOImplementation IOImplementationProvider
                                      Serializer io-provider]]))

(deftype ClojureSerializer []
  IOImplementation
  (provider [_] :clojure)
  (fmt [_] any-format-id)
  Serializer
  (encode [this g] (pr-str g))
  (encode [this g opts] (encode this g))
  (decode [this s] (read-string s))
  (decode [this s opts] (decode this s))
  (can-encode? [this] true)
  (can-decode? [this] true))

(def clojure-serializer (->ClojureSerializer))
(def clojure-provider
  (reify IOImplementationProvider
    (id [this] :clojure)
    (supported-formats [this impl-type]
      (case impl-type
        :serializer any-format-supported
        nil))
    (implementation [this impl-type]
      (case impl-type
        :serializer clojure-serializer
        nil))
    (implementation [this impl-type _]
      (implementation this impl-type))))

(defmethod io-provider :clojure
  [_]
  clojure-provider)
