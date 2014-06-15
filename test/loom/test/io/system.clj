(ns loom.test.io.system
  (:require [clojure.test :refer :all]
            [loom.graph :refer :all]
            [loom.io.system :refer :all]
            [loom.test.io :refer [test-graphs]]
            [loom.test.io.provider :refer [test-provider]])
  (:import (java.io File)))

(deftest os-detection
  (is os-name "os-name is nil!?!")
  (is os (str "Unrecognized OS: " os-name \.)))

(deftest save-graph
  (doseq [g test-graphs
          f [graph digraph weighted-graph weighted-digraph]]
    (let [fg (f g)
          tmp1 (File/createTempFile "loom.test" ".io")
          tmp2 (save fg tmp1)
          s (slurp tmp2)
          g (read-string s)]
      (is (identical? tmp1 tmp2)
          "save must return the same file instance it is given.")
      (is (pos? (.length tmp2))
          "Saved file can't be empty.")
      (is (not (clojure.string/blank? s))
          "Saving a graph instance should produce a non-blank file.")
      (is (= fg g) "Reading a saved graph should yield an identical graph.")
      (is (.delete tmp2) "Strange, I'm not able to delete a temporary file."))))

(deftest system
  (test-provider :system))
