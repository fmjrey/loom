(ns loom.test.io.clojure
  (:require [clojure.test :refer :all]
            [loom.io.clojure :refer :all]
            [loom.test.io.provider :refer [test-provider]]))


(deftest desktop
  (test-provider :clojure))
