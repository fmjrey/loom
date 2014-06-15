(ns loom.test.io.provider
  (:require [clojure.test :refer :all]
            [loom.graph :refer :all]
            [loom.io.provider :refer :all]
            [loom.test.io :refer [test-graphs]])
  (:import (java.io File)))

(defn seqable?
  "Returns true if (seq x) will succeed, false otherwise.
  Taken from older clojure.contrib, hasn't been included in core because
  there's no reliable way to cover all the specific cases, but for our
  testing purposes, it should probably be fine."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))

(defn empty-seqable?
  "Test if the given value is nil or an empty seqable (as in seqable?)."
  [value]
  (and (seqable? value) (nil? (seq value))))

(defmulti test-impl
  "Perform generic testing on a given implementation."
  (fn [impl-type impl] impl-type))

(defmethod test-impl :renderer
  [impl-type impl]
  (doseq [g test-graphs
          f [graph digraph weighted-graph weighted-digraph]]
    (let [fg (f g)
          rendered (render impl fg)
          fmt (fmt impl)
          ext (if (or (keyword? fmt) (symbol fmt) (string? fmt))
                (name fmt)
                (str fmt))
          tmp1 (File/createTempFile "loom.test.io" (str \. ext))
          tmp2 (render-to-file impl fg tmp1)]
      (when (seq (nodes fg))
        (is (identical? tmp1 tmp2)
          "render-to-file must return the same file it is given.")
        (when (seqable? rendered)
          (is (not (empty-seqable? rendered))
            "Rendered value for a non-empty graph can't be nil or empty."))
        (is (pos? (.length tmp2))
            "Rendered file for a non-empty graph can't be empty.")
        (is (.delete tmp2)
           "Strange, I'm not able to delete a temporary file.")))))

(defmethod test-impl :viewer
  [impl-type impl]
  ; not sure what we can test here....
  nil)

(defmethod test-impl :serializer
  [impl-type impl]
  (let [can-encode? (can-encode? impl)
        can-decode? (can-decode? impl)]
    (is (or can-encode? can-decode?)
        (str "Serializer for " (fmt impl)
             " must be able to either decode or encode."))
    (doseq [g test-graphs
            f [graph digraph weighted-graph weighted-digraph]]
      (let [fg (f g)
            encoded (when can-encode? (encode impl fg))
            decoded (when (and can-encode? can-decode?) (decode impl encoded))]
        (when (and can-encode? can-decode?)
          (is (= fg decoded)
              (str "Round-trip serialization should yield the same graph.")))
        (when (and can-encode? (seq (nodes fg)) (seqable? encoded))
          (is (not (empty-seqable? encoded))
            "Encoded value for a non-empty graph can't be nil or empty."))))))

(defn test-provider
  "Perform generic testing for a given provider id. To be called within the test
  function of a provider's test suite.  When accept-runtime-exceptions? is true
  the test case will not fail when a RuntimeException is thrown by an
  implementation."
  ([provider] (test-provider provider false))
  ([provider accept-runtime-exceptions?]
  (when (is (contains? (.getMethodTable io-provider) provider)
            (str "Provider " provider
                 " does not implement io-provider."))
    (let [p (io-provider provider)
          fmts (into {} (filter
                          (fn [[impl-type fmts]]
                            (when-not (nil? fmts)
                              (is (not (empty? fmts))
                                  (str "Supported formats for " impl-type
                                       " is an empty map, should be nil.")))
                            (seq fmts))
                          (for [impl-type impl-types]
                            [impl-type (supported-formats p impl-type)])))]
      (is (= provider (id p))
          (str "Mismatching io-provider dispatch value (" provider
               ") and provider id (" (id p) ")."))
      (when (is (> (count fmts) 0)
               "Providers must provide at least one supported format.")
        (let [nb-impls
              (reduce ; counting the number of working implementations
                (fn [n [impl-type fmts]]
                  (reduce
                    (fn [n [fk fd]]
                      (let [fmt (:id fd)
                            impl (implementation p impl-type fmt)
                            impl-provider (loom.io.provider/provider impl)
                            impl-fmt (loom.io.provider/fmt impl)]
                        (is (= fk fmt)
                          (str "Supported formats contains a format descriptor ("
                               fd ") not indexed by its id, but by " fk "."))
                        (is (= provider impl-provider)
                           (str "Mismatching " (name impl-type)
                                " provider (" impl-provider
                                ") and provider used for retrieval ("
                                provider ")."))
                        (is (= fmt impl-fmt)
                            (str "Mismatching " (name impl-type)
                                 " format (" impl-fmt
                                 ") and format used for retrieval ("
                                 fmt ")."))
                        (try
                          (test-impl impl-type impl)
                          (inc n)
                          (catch RuntimeException re
                            (if accept-runtime-exceptions?
                              (println re)
                              (report
                                {:type :error
                                 :expected (list 'test-impl impl-type impl)
                                 :actual re}))
                            n))))
                    n
                    fmts))
                0
                fmts)]
          (is (pos? nb-impls)
             "No working implementation found.")))))))
