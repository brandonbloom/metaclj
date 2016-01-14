(ns metaclj.core
  (:require [metaclj.impl.env :refer [->Env]]
            [metaclj.impl.syntax :refer [syntax*]]))

(defmacro syntax [expr]
  (let [free (keys &env) ;TODO compute minimal set from
        locals (into {} (for [sym free] [(list 'quote sym) sym]))]
    `(syntax* '~expr (->Env ~*ns* ~locals))))

(comment

  (let [x 1
        y [1 2 3]]
    (syntax 1))

  (let [x 1
        y [1 2 3]]
    (syntax x))

  (parse 1 :an-env)

)
