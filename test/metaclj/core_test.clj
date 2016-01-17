(ns metaclj.core-test
  (:require [clojure.test :refer :all]
            [metaclj.core :refer :all]
            [metaclj.impl.env :refer [->Env]]))

(deftest env-test
  (is (= (local-env)
         (->Env *ns*)))
  (is (= (let [x 1] (local-env))
         (merge (->Env *ns*) '{x 1})))
  (is (= (let [x 1 x 2] (local-env))
         (merge (->Env *ns*) '{x 2})))
  )
