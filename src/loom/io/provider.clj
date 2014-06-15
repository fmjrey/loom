(ns loom.io.provider
  "Data formats, renderers, viewers, and serializers for graphs."
  {:author "François Rey, Justin Kramer"})

;;;
;;; Provider protocol and multimethod
;;;

(defmulti io-provider
  "Returns a IOImplementationProvider instance for a given provider id.
  Providers can be identified uniquely by any value (though preferably
  interned, e.g. :graphviz).
  This method must be implemented by each provider and must take a provider id
  as a single argument (used only for dispatch). It may be called each time
  an implementation provider is requested.
  It is stronly recommended that provider instances and the implementation
  instances they provide have no mutable state so they can be used several times
  and across threads (like a closure). This would  also enable caching or
  memoization."
  identity)
(defmethod io-provider :default
  [id]
  (throw (IllegalArgumentException.
           (str "No I/O provider identified by " id "."))))


(def impl-types
  "Set of possible implementation types."
  #{:renderer :viewer :serializer})

(defprotocol IOImplementationProvider
  (id [this] "Provider id, which can be any unique value (though preferably
  interned, e.g. :graphviz).")
  (supported-formats [this impl-type] "Returns a map of supported format
  descriptors for a given implementation type (:renderer, :viewer, or
  :serializer), or nil if none (rather than an empty map).
  The map should contain FormatDescriptor instances keyed by their id.
  Should the provider support any format, or no specific format in particular,
  the any-format-supported var defined in this namespace can be returned.")
  (implementation [this impl-type] [this impl-type fmt] "Returns an instance of
  IOImplementation for the given implementation type (:renderer, :viewer, or
  :serializer) and for the given format (e.g. :png), or nil if none available.
  When no format is given, the default format for this provider is assumed.
  It is stronly recommended that implementations have no mutable state so they
  can be used several times and across threads (like a closure). This would
  also enable caching or memoization."))

;;;
;;; Format record
;;;

; Record that describe a data format for both clojure and humans
(defrecord FormatDescriptor [ id ; identifier for the format, e.g. :png :svg
                              ^String shortname ; short name for humans
                              ^boolean binary? ; whether it's a binary format
                              ^String desc]) ; short description

(def any-format-id
  "Format id to be used by implementations that work with any format or no
  format in particular."
  :any)

(def any-format-desc
  "Record instance to be used by implementations that work with any format or no
  format in particular."
  (->FormatDescriptor any-format-id"Any" true
                      "Any format."))

(def any-format-supported
  "Map of supported formats for implementations that work with any format or no
  format in particular."
  {any-format-id any-format-desc})

;;;
;;; I/O implementation protocols
;;;

; Base protocol to be implemented by all.
(defprotocol IOImplementation
  (provider [this] "Provider for this implementation (e.g. :graphviz).")
  (fmt [this] "The format id (e.g. :png) handled by this implementation.
  Should the implementation support any format, or no specific format in
  particular, the any-format-id var from this namespace can be returned."))

(defprotocol Renderer
  "Protocol for rendering a graph into a text or binary (e.g. image) format.
  Rendering = converting a graph into a rendered form typically for viewing,
  without necessarily being able to restore a graph from the rendered form."
  (render [this g] [this g opts] "Returns the rendering data of a given graph.")
  (render-to-file [this g f] [this g f opts]
               "Renders a graph into a file. Returns the java.io.File object."))

(defprotocol Viewer
  "Protocol for viewing a graph, typically via or from some form of rendering."
  (view [this g] [this g opts]
        "Displays a given graph, string/byte data, or file.")
  (view-data [this data] [this data opts] "Displays rendered data.")
  (view-file [this f] [this f opts] "Displays rendered data from file."))

(defprotocol Serializer
  "Protocol for serializing a graph into a text or binary format.
  Serialization = saving a graph so it can be restored later on from the
  serialized form."
  (encode [this g] [this g opts] "Returns the serialized form of a graph.")
  (decode [this s] [this s opts] "Builds a graph from a serialized form.")
  (can-encode? [this] "True if able to encode. Useful for testing.")
  (can-decode? [this] "True if able to decode. Useful for testing."))

;;;
;;; Convenience methods for retrieval
;;;

(defn renderer
  "Return the renderer for the given format and from the given provider,
  or nil if none is available. If no format is specified, the provider
  is assumed to support a single format, otherwise an exception is thrown."
  ([provider]
  (implementation (io-provider provider) :renderer))
  ([provider fmt]
  (implementation (io-provider provider) :renderer fmt)))

(defn viewer
  "Return the viewer for the given format and from the given provider,
  or nil if none is available."
  ([provider]
  (implementation (io-provider provider) :viewer))
  ([provider fmt]
  (implementation (io-provider provider) :viewer fmt)))

(defn serializer
  "Return the serializer for the given format and from the given provider,
  or nil if none is available."
  ([provider]
  (implementation (io-provider provider) :serializer))
  ([provider fmt]
  (implementation (io-provider provider) :serializer fmt)))
