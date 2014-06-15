(ns loom.test.io.graphviz
  (:require [clojure.test :refer :all]
            [loom.io.graphviz :refer :all]
            [loom.test.io.provider :refer [test-provider]]))


(deftest graphviz
  (test-provider :graphviz true))
