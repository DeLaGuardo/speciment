(ns speciment.pattern-test
  (:require [speciment.pattern :as sut]
            [clojure.test :refer [deftest testing is are]]))

(deftest matches-test
  (testing "scalar values"
    (are [y] (= {'x y} (sut/matches '?x y))
      1 "qwe" true false)

    (are [x y] (nil? (sut/matches x y))
      1 "1"
      "1" 1
      false true
      true false))

  (testing "vectors"
    (are [x] (nil? (sut/matches [1 2 3] x))
      "qwe"
      false
      true
      [1 2]
      [3 2 1]
      {}
      {"x" 1}
      {"x" [1 2 3]})

    (are [y] (= {'x y} (sut/matches '?x y))
      [1 2 3]
      [1 2 3 4]))

  (testing "bindings"
    (is (= '{x [1 2 3]
             y "qwe"
             z true
             Z false}
           (sut/matches {"foo" ['?x 4 5]
                         "bar" {"qwe" '?y}
                         "baz" {"_" ['?z '?Z]}}
                        {"foo" [[1 2 3] 4 5]
                         "bar" {"qwe" "qwe"}
                         "baz" {"_" [true false]}})))))
