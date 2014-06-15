(ns loom.io.system
  "Generic viewer using system/desktop resources."
  {:author "Justin Kramer, François Rey"}
  (:require [clojure.java.io :refer [as-file]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :refer [blank?]]
            [loom.graph :refer [graph?]]
            [loom.io.provider :refer [any-format-id any-format-supported
                                      view-data view-file implementation
                                      IOImplementation IOImplementationProvider
                                      Viewer io-provider]])
  (:import (java.io File FileOutputStream FileWriter)))

;;;
;;; Methods generally useful for os detection, saving and opening files, etc.
;;;

(def os-name
  "Operating system name as returned by java System.getProperty(\"os.name\")"
  (.toLowerCase (System/getProperty "os.name")))

(def os
  "Either :win, :mac, :linux, :unix, or nil."
  (condp #(<= 0 (.indexOf ^String %2 ^String %1)) os-name
    "win" :win
    "mac" :mac
    "nix" :unix
    "nux" :linux
    nil))

(def ^{:doc "Open a file with the default desktop application.
The 'open' method in java.awt.Desktop sometimes hangs on Windows
and turns the process into a GUI process on Max OS X.
Therefore this method invokes directly a shell command."
       :arglists '([f])}
  desktop-open
  ; The 'open' method in java.awt.Desktop sometimes hangs on Windows
  ; and turns the process into a GUI process on Max OS X.
  ; Therefore this method invokes directly a shell command.
  (case os
    :mac #(sh "open" (str %))
    :win #(sh "cmd" (str "/c start " (-> % .toURI .toURL str)))
    (:unix :linux) #(sh "xdg-open" (str %))
    #(throw (UnsupportedOperationException.
              (str "Don't know how to open a file on " os-name \.)))))

(defn test-expected-array-type-fn
  "Generates a function that tests if a value is an array of a given type."
  [t]
  (let [expected-type (type (t []))]
    (fn [arg] (instance? expected-type arg))))

(def byte-array?
  "Test if a given value is an array of a given type."
  (test-expected-array-type-fn byte-array))

(defn save
  "Write the given data (graph, string or bytes) to a given file and returns the
  java.io.File instance.  data can be a graph instance, in which case it will
  be saved as text using pr-str.  f will be converted to a file using
  clojure.java.io/as-file."
  [data f]
  (if (graph? data)
    (save (pr-str data) f)
    (let [fobject (as-file f)]
      (cond
        (string? data) (with-open [w (FileWriter. fobject)]
                         (.write w ^String data))
        (byte-array? data) (with-open [w (FileOutputStream. fobject)]
                             (.write w ^bytes data))
        :else (throw
                (IllegalArgumentException.
                  (str (type data) " isn't a graph, string, or byte array."))))
      fobject)))

(defn open
  "Open the given file or data (string or bytes) in the default application in
  the current desktop environment. When called with a single argument this
  method checks if it is given a java.io.File instance or anything that
  designates an existing file when using clojure.java.io/as-file.
  When data is given, it is first written to a temporary file with the given
  extension (string or keyword, with or without the dot, defaulting to \"tmp\"
  if nil, blank, or none provided). The temporary file is then marked for
  deletion when the JVM terminates. Returns the java.io.File instance."
  ([data]
    (if (instance? File data)
      (do
        (desktop-open data)
        data)
      (if (string? data)
        (let [f (as-file data)]
          (if (.exists f)
            (open f)
            (open data nil)))
        (open data nil))))
  ([data ext]
    (let [ext (if (or (nil? ext) (and (string? ext) (blank? ext)))
                "tmp"
                (name ext))
          ext (if (= \. (first ext)) ext (str \. ext))
          tmp (java.io.File/createTempFile (subs ext 1) ext)
          tmp (save data tmp)]
      (.deleteOnExit tmp)
      (open tmp))))

;;;
;;; Viewer implementation
;;;

(deftype SystemViewer []
  IOImplementation
  (provider [_] :system)
  (fmt [_] any-format-id)
  Viewer
  (view [this g] (cond
                   (graph? g) (throw
                     (IllegalArgumentException.
                       "Graph not rendered or serialized for viewing."))
                   (and (string? g) (.exists (as-file g)))
                     (view-file this g)
                   :else (view-data this g)))
  (view [this g opts] (cond
                        (graph? g) (throw
                          (IllegalArgumentException.
                            "Graph not rendered or serialized for viewing."))
                        (and (string? g) (.isFile (as-file g)))
                          (view-file this g opts)
                        :else (view-data this g opts)))
  (view-data [this data] (open data))
  (view-data [this data ext] (open data ext))
  (view-file [this f] (open f))
  (view-file [this f opts] (view-file this f)))

(def system-viewer (->SystemViewer))
(def desktop-provider
  (reify IOImplementationProvider
    (id [this] :system)
    (supported-formats [this impl-type]
      (case impl-type
        :viewer any-format-supported
        nil))
    (implementation [this impl-type]
      (case impl-type
        :viewer system-viewer
        nil))
    (implementation [this impl-type _]
      (implementation this impl-type))))

(defmethod io-provider :system
  [_]
  desktop-provider)
