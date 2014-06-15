(ns loom.test.io
  (:require [loom.io :refer :all]
            [clojure.test :refer :all]))

(def g0 {})

(def g1
  {:a [:b :c]
   :b [:d]
   :c [:d]
   :d nil})

(def g2
  {:a [:b]
   :b [:a]})

(def g3
  {:a [:b]
   :b [:a :c :d]
   :c [:b :e]
   :d [:b :c :e]
   :e [:c :d :f]
   :f []})

(def g4 ; like g3 with some loops
  {:a [:b]
   :b [:a :c :d]
   :c [:b :c :e]
   :d [:b :c :e]
   :e [:c :d :f]
   :f [:f]})

(def g5 ; like g1 but as an undirected graph
  {:a [:b :c]
   :b [:d :a]
   :c [:a :d]
   :d [:c :b]})

(def g6 ; unconnected with some loops
  {:a [:a]
   :b [:a :c]
   :c [:b :c]
   :d [:e]
   :e [:d :f]
   :f [:f]})

(def test-graphs
  "Vector of test graphs in the adjacency list form (map of neighbors vector
  indexed by node)."
  [g0 g1 g2 g3 g4 g5 g6])

