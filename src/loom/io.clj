(ns loom.io
  "Renderering, viewing, and serializing graphs. By default graphs are
  rendered to PNG using graphviz,
  viewed using the system default application for for PNG files,
  serialized/deserialized using clojure pr-str and read-string."
  {:author "François Rey, Justin Kramer"}
  (:require [loom.graph :refer [graph?]]
            [loom.io.provider :refer [renderer viewer serializer]]))

;;;
;;; Default renderer, viewer, and serializer
;;;

(require 'loom.io.graphviz :reload)
(def ^:dynamic *default-renderer*
  "Default renderer (:graphviz by default)."
  (renderer :graphviz))

(require 'loom.io.system :reload)
(def ^:dynamic *default-viewer*
  "Default viewer (:system by default)."
  (viewer :system))

(require 'loom.io.clojure :reload)
(def ^:dynamic *default-serializer*
  "Default serializer (:clojure by default)."
  (serializer :clojure))

(defn render
  "Render the given graph using the *default-renderer*."
  [g]
  (loom.io.provider/render *default-renderer* g))

(defn render-to-file
  "Render a graph into a given file using the *default-renderer*."
  [g f]
  (loom.io.provider/render-to-file *default-renderer* g f))

(defn view
  "View the given graph using the *default-viewer* after rendering using
  the *default-renderer*.  g can be a graph instance or its rendered form."
  [g]
  (if (graph? g)
    (loom.io.provider/view *default-viewer* (render g))
    (loom.io.provider/view *default-viewer* g)))

(defn view-file
  "View the given file using the *default-viewer*."
  [f]
  (loom.io.provider/view-file *default-viewer* f))

(defn encode
  "Serialize the given graph using the *default-serializer*."
  [g]
  (loom.io.provider/encode *default-serializer* g))

(defn decode
  "De-serialize using the *default-serializer*. Returns a graph."
  [s]
  (loom.io.provider/decode *default-serializer* s))

