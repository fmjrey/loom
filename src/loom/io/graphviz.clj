(ns loom.io.graphviz
  "Graph I/O using GraphViz. Provides a renderer implementations for most
  graphical formats supported by GraphViz. Also provides serializers for
  dot-like text formats, except that decoding is not yet implemented.
  The default renderer format is :png, and the default viewer is :xlib,
  which only works on X Windows systems. The default serializer uses the :gv
  format rather than :dot as the latter is used by MS Word for templates. See
  http://en.wikipedia.org/wiki/DOT_(graph_description_language)
  This implementation supports formats in the form
  \"format[:renderer[:formatter]]\" (e.g. :png:cairo or :png:cairo:gd). They are
  used to specify to graphviz which internal renderer and formatter should be
  used."
  {:author "Justin Kramer, François Rey"}
  (:require [clojure.java.io :refer [as-file]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :refer [escape]]
            [loom.alg :refer [connected? distinct-edges
                              strongly-connected?]]
            [loom.attr :refer [attr attr? attrs]]
            [loom.graph :refer [directed? graph? nodes weight weighted?]]
            [loom.io.provider :refer [encode decode
                                      render render-to-file
                                      view view-data
                                      implementation
                                      ->FormatDescriptor IOImplementation
                                      IOImplementationProvider Renderer
                                      Serializer Viewer io-provider]])
  (:import (java.io File)))

;;;
;;; Graphviz native dot format generation
;;;

(defn- dot-esc
  [s]
  (escape s {\" "\\\"" \newline "\\n"}))

(defn- dot-attrs
  [attrs]
  (when (seq attrs)
    (let [sb (StringBuilder. "[")]
      (doseq [[k v] attrs]
        (when (pos? (.length (str v)))
          (when (< 1 (.length sb))
            (.append sb \,))
          (doto sb
            (.append \")
            (.append (dot-esc (if (keyword? k) (name k) (str k))))
            (.append "\"=\"")
            (.append (dot-esc (if (keyword? v) (name v) (str v))))
            (.append \"))))
      (.append sb "]")
      (str sb))))

(defn dot-str
  "Render graph g as a DOT-format string. Calls (node-label node) and
  (edge-label n1 n2) to determine what labels to use for nodes and edges,
  if any. Will detect graphs that satisfy AttrGraph and include attributes,
  too."
  ([g] (dot-str g {}))
  ([g {:keys [graph-name node-label edge-label]
      :or {graph-name "graph"} :as opts }]
   {:pre [(graph? g)]}
   (let [node-label (if node-label node-label
                        (if (attr? g)
                          #(attr g % :label) (constantly nil)))
         edge-label (if edge-label edge-label
                        (if (attr? g)
                          #(attr g %1 %2 :label) (constantly nil)))
         d? (directed? g)
         w? (weighted? g)
         sb (doto (StringBuilder.
                   (if d? "digraph \"" "graph \""))
              (.append (dot-esc graph-name))
              (.append "\" {\n"))]
     (when (:graph opts)
       (doto sb
         (.append "  graph ")
         (.append (dot-attrs (:graph opts)))))
     (doseq [[n1 n2] (distinct-edges g)]
       (let [n1l (str (or (node-label n1) n1))
             n2l (str (or (node-label n2) n2))
             el (if w? (weight g n1 n2) (edge-label n1 n2))
             eattrs (assoc (if (attr? g)
                             (attrs g n1 n2) {})
                      :label el)]
         (doto sb
           (.append "  \"")
           (.append (dot-esc n1l))
           (.append (if d? "\" -> \"" "\" -- \""))
           (.append (dot-esc n2l))
           (.append \"))
         (when (or (:label eattrs) (< 1 (count eattrs)))
           (.append sb \space)
           (.append sb (dot-attrs eattrs)))
         (.append sb "\n")))
     (doseq [n (nodes g)]
       (doto sb
         (.append "  \"")
         (.append (dot-esc (str (or (node-label n) n))))
         (.append \"))
       (when-let [nattrs (when (attr? g)
                           (dot-attrs (attrs g n)))]
         (.append sb \space)
         (.append sb nattrs))
       (.append sb "\n"))
     (str (doto sb (.append "}"))))))

;;;
;;; Graphviz rendering formats
;;;

(def ^:private render-formats [
  ;http://www.graphviz.org/content/output-formats
  ;id         Name        Bin?  Short description
  [:bmp       "Bmp"       true  "Windows Bitmap Format."                                                                          ]
  [:cgimage   "CGImage"   true  "CGImage bitmap format."                                                                          ]
  [:eps       "Eps"       false "Encapsulated PostScript."                                                                        ]
  [:exr       "Exr"       true  "OpenEXR."                                                                                        ]
  [:fig       "Fig"       false "FIG graphics language."                                                                          ]
  [:gd        "Gd"        true  "Internal GD library format."                                                                     ]
  [:gd2       "Gd2"       true  "Compressed version of 'Gd'."                                                                     ]
  [:gif       "Gif"       true  "Graphics Interchange Format."                                                                    ]
  [:ico       "Ico"       true  "Icon image file format."                                                                         ]
  [:imap      "Imap"      false "Server-side imagemap."                                                                           ]
  [:cmapx     "Cmapx"     false "Client-side imagemap."                                                                           ]
  [:imap_np   "ImapNP"    false "Same as 'Imap', except only rectangles are used as active areas."                                ]
  [:cmapx_np  "CmapxNP"   false "Same as 'Cmapx', except only rectangles are used as active areas."                               ]
  [:jpe       "Jpe"       true  "The JPEG image format."                                                                          ]
  [:jpeg      "Jpeg"      true  "The JPEG image format."                                                                          ]
  [:jpg       "Jpg"       true  "The JPEG image format."                                                                          ]
  [:pct       "Pct"       true  "PICT image format."                                                                              ]
  [:pict      "Pict"      true  "PICT image format."                                                                              ]
  [:pdf       "Pdf"       true  "Portable Document Format."                                                                       ]
  [:pic       "Pic"       false "Kernighan's PIC graphics language."                                                              ]
  [:png       "Png"       true  "Portable Network Graphics format."                                                               ]
  [:pov       "Pov"       false "POV-Ray markup language [prototype]."                                                            ]
  [:ps        "Ps"        false "PostScript."                                                                                     ]
  [:ps2       "Ps2"       true  "PostScript for PDF."                                                                             ]
  [:psd       "Psd"       true  "Adobe PhotoShop PSD file format."                                                                ]
  [:sgi       "Sgi"       true  "SGI image file format."                                                                          ]
  [:svg       "Svg"       false "Scalable Vector Graphics format."                                                                ]
  [:svgz      "SvgZ"      true  "Compressed SVG format."                                                                          ]
  [:tif       "Tif"       true  "Tagged Image File Format."                                                                       ]
  [:tiff      "Tiff"      true  "Tagged Image File Format."                                                                       ]
  [:tga       "Tga"       true  "Truevision TGA or TARGA format."                                                                 ]
  [:tk        "Tk"        false "Text-based TK graphics primitives."                                                              ]
  [:vml       "Vml"       false "Vector Markup Language; 'Svg' is usually preferred."                                             ]
  [:vmlz      "VmlZ"      true  "Compressed VML format; 'SvgZ' is usually preferred."                                             ]
  [:vrml      "Vrml"      false "Virtual Reality Modeling Language format; requires nodes to have a 'Z' attribute."               ]
  [:wbmp      "WBmp"      true  "Wireless BitMap format; monochrome format usually used for mobile computing devices."            ]
  [:webp      "WebP"      true  "Google's WebP format; requires Graphviz >= 2.29.0."                                              ]
])

(def ^:private view-formats [
  ;http://www.graphviz.org/content/output-formats
  ;id         Name        Bin?  Short description
  [:gtk       "Gtk"       true  "GTK canvas."                                                                                     ]
  [:xlib      "Xlib"      true  "Xlib canvas."                                                                                    ]
  [:x11       "X11"       true  "Xlib canvas."                                                                                    ]
])

(def ^:private xdot-versions [
  ;http://www.graphviz.org/content/output-formats#axdot
  ;Xdot version  Graphviz version Change
  [1.0           1.9              "Initial version."]
  [1.1           2.8              "First plug-in version"]
  [1.2           2.13             "Support image operator I"]
  [1.3           2.31             "Add numerical precision"]
  [1.4           2.32             "Add gradient colors"]
  [1.5           2.34             "Fix text layout problem; fix inverted vector in gradient; support version-specific output; new t op for text characteristics"]
  [1.6           2.35             "Add STRIKE-THROUGH bit for t"]
  [1.7           2.37             "Add OVERLINE for t"]
])

(def ^:private serial-formats
  (into [
        ;http://www.graphviz.org/content/output-formats
        ;id         Name        Bin?  Short description
        [:plain     "Plain"     false "Simple text format."                                                                             ]
        [:plain-ext "PlainExt"  false "Same as 'Plain', but provides port names on head and tail nodes when applicable."                ]
        [:canon     "Canon"     false "Pretty-printed Dot output with no layout performed."                                             ]
        [:dot       "Dot"       false "Reproduces the input along with layout information; 'Gv' is now preferred."                      ]
        [:gv        "Gv"        false "Same as 'Dot', preferred graphviz format."                                                       ]
        [:xdot      "XDot"      false "Same as 'Dot', but provides even more information on how the graph is drawn."                    ]
        ]
        (for [[xv gv chg] xdot-versions]
          [(str "xdot" (int (* xv 10)))
           (str "XDot" xv)
           false
           (str "Same as 'XDot', but at version " xv " (" chg
                "); requires graphviz >= " gv)])))

(def ^:private render-format-descriptors
  (into {} (for [f render-formats]
             [(first f) (apply ->FormatDescriptor f)])))

(def ^:private serial-format-descriptors
  (into {} (for [f serial-formats]
             [(first f) (apply ->FormatDescriptor f)])))

(def ^:private view-format-descriptors
  (into {} (for [f view-formats]
             [(first f) (apply ->FormatDescriptor f)])))

;;;
;;; IO protocol implementations
;;;

(defn best-alg-for
  "Determines the best layout algorithm (shell command) for a given graph."
  [g]
  (cond
    (or (nil? g) (not (graph? g))) :dot
    (> (count (nodes g)) 100) (if (directed? g) :neato :dot)
    (directed? g) (if (strongly-connected? g) :circo :neato)
    :else (if (connected? g) :circo :dot)))

(defn- nil-or-file-path [f]
  (when (instance? File f) (.getAbsolutePath f)))

(defn run-graphviz
  [{:keys [id binary?]} in out {:keys [alg out-enc in-enc]
                                :or {alg (best-alg-for in)
                                     out-enc (if binary? :bytes "UTF-8")
                                     in-enc "UTF-8"}
                                :as opts}]
  (let [cmd (name alg)
        t-arg (str "-T" (name id))
        o-arg (when-not (nil? out)
                   (when (instance? File out)
                     (str "-o" (.getAbsolutePath out))))
        file-arg (when-not (nil? in)
                   (when (instance? File in)
                     (.getAbsolutePath in)))
        cmd-line (remove nil? [cmd t-arg o-arg file-arg])
        cmd-line (if file-arg
                   cmd-line
                   (concat cmd-line (list :in in :in-enc in-enc)))
        cmd-line (if o-arg
                   cmd-line
                   (concat cmd-line (list :out-enc out-enc)))]
    (let [{:keys [out exit err] :as result} (apply sh cmd-line)]
      (comment
        (if o-arg
          (println (str "    out=" out)))
        (println (str "    err=" err))
        (println (str "    exit=" exit)))
      (if (and (clojure.string/blank? err) (zero? exit))
        result
        (throw (RuntimeException.
                 (str "Graphviz command failed:"
                      "\n  Command=" (pr-str cmd-line)
                      "\n  err=" err
                      "\n  exit value=" exit)))))))

(defrecord GraphvizRenderer [fmt-desc]
  IOImplementation
  (provider [_] :graphviz)
  (fmt [_] (:id fmt-desc))
  Renderer
  (render [this g] (render this g {}))
  (render [this g opts]
    {:pre [(graph? g)]}
    (let [dot (apply dot-str g (apply concat opts))
          t-arg (str "-T" (name (:id fmt-desc)))]
      (:out (run-graphviz fmt-desc dot nil opts))))
  (render-to-file [this g f] (render-to-file this g f {}))
  (render-to-file [this g f opts]
    (let [dot (apply dot-str g (apply concat opts))
          fobject (as-file f)]
      (run-graphviz fmt-desc dot fobject opts)
      fobject)))

(defrecord GraphvizViewer [fmt-desc]
  IOImplementation
  (provider [_] :graphviz)
  (fmt [_] (:id fmt-desc))
  Viewer
  (view [this g] (view this g {}))
  (view [this g opts]
    {:pre [(graph? g)]}
    (let [dot (apply dot-str g (apply concat opts))]
      (run-graphviz fmt-desc dot nil opts)))
  (view-data [this s] (view-data this s {}))
  (view-data [this s opts]
    {:pre [(string? s)]}
    (run-graphviz fmt-desc s nil opts)))

(defrecord GraphvizSerializer [fmt-desc]
  IOImplementation
  (provider [_] :graphviz)
  (fmt [_] (:id fmt-desc))
  Serializer
  (encode [this g] (encode this g {}))
  (encode [this g opts] (dot-str g opts))
  (decode [this s] (decode this s {}))
  (decode [this s opts] (throw (UnsupportedOperationException.
                                 (str "Parsing of "
                                      (:shortname fmt-desc)
                                      " not implemented yet."))))
  (can-encode? [this] true)
  (can-decode? [this] false))

(def graphviz-provider
  (reify IOImplementationProvider
    (id [this] :graphviz)
    (supported-formats [this impl-type]
      (case impl-type
        :renderer render-format-descriptors
        :viewer view-format-descriptors
        :serializer serial-format-descriptors
        nil))
    (implementation [this impl-type]
      (case impl-type
        :renderer (implementation this impl-type :png)
        :viewer (implementation this impl-type :xlib)
        :serializer (implementation this impl-type :gv)
        nil))
    (implementation [this impl-type fmt]
      (if-let [match (re-matches #"([\w|-]+)(:[\w|-]+(:[\w|-]+)?)?"
                                 (name fmt))]
        (let [[full-fmt sfmt fmt-renderer fmt-formatter] match
               zfmt (if fmt-renderer
                      (keyword sfmt)
                      fmt)]
          (case impl-type
            :renderer (when-let [fd (get render-format-descriptors zfmt)]
                        (if fmt-renderer
                          (->GraphvizRenderer (assoc fd :id (keyword full-fmt)))
                          (->GraphvizRenderer fd)))
            :viewer (when-let [fd (get view-format-descriptors zfmt)]
                        (if fmt-renderer
                          (->GraphvizViewer (assoc fd :id (keyword full-fmt)))
                          (->GraphvizViewer fd)))
            :serializer (when-let [fd (get serial-format-descriptors zfmt)]
                        (if fmt-renderer
                          (->GraphvizSerializer (assoc fd :id (keyword full-fmt)))
                          (->GraphvizSerializer fd)))
            nil))
        (throw (IllegalArgumentException.
                 (str "Format " fmt " not recognized, must be in the form "
                 ":format[:renderer[:formatter]].")))))))

(defmethod io-provider :graphviz
  [_]
  graphviz-provider)
