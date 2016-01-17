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

(defbn my-if [test then else]
  (if test then else))

(defbn vector-bn [& args]
  [args])

(defbn my-and
  ([] true)
  ([x] x)
  ([x & next]
   (let [y x]
     (if y (my-and next) y))))

(deftest by-name-test

  (is (= 1 (my-if true 1 2)))
  (is (= "1" (with-out-str (my-if true (pr 1) (pr 2)))))
  (is (= 2 (my-if false 1 2)))
  (is (= "2" (with-out-str (my-if false (pr 1) (pr 2)))))

  (is (= (vector) (vector-bn)))
  (is (= (vector 1 2 3) (vector-bn 1 2 3)))

  (is (= true (my-and)))
  (is (= 1 (my-and 1)))
  (is (= 2 (my-and 1 2)))
  (is (= 3 (my-and 1 2 3)))
  (is (= 4 (my-and 1 2 3 4)))
  (is (= false (my-and 1 false 3 4)))
  (is (= nil (my-and 1 2 nil 4)))

  )
