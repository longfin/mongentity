(ns mongentity.core-test
  (:use [clojure.test :only [deftest testing is]]
        [mongentity.core :only [defentity]]))

(defentity User [_id email])
(deftest entity-test
  (testing "Testing existence"
    (is (boolean (ns-resolve 'mongentity.core-test 'User))))
  (testing "Testing it's type"
    (is (instance? java.lang.Class User))))
