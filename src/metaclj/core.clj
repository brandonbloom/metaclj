(ns metaclj.core
  (:refer-clojure :exclude [eval])
  (:require [metaclj.impl.env :refer [->Env]]
            [metaclj.impl.parse :refer [parse ->Syntax syntax?]]
            [metaclj.impl.transform :refer [transform-in]]))

(defn eval [x]
  ;(fipp.clojure/pprint (transform-in (->Env *ns*) x))
  (run! clojure.core/eval (transform-in (->Env *ns*) x)))

(defmacro local-env []
  `(into (->Env ~*ns*)
         [~@(for [sym (keys &env)]
              [(list 'quote sym) sym])]))

(defmacro syntax [& forms]
  `(->Syntax '~(vec forms) (local-env)))

(defmacro defmeta [name & fn-tail]
  (let [make-binding (fn [param variadic?]
                       (list `->Syntax
                             (list (if variadic? `vec `vector) param)
                             '&env))
        make-bindings (fn [sig]
                        (first
                          (reduce (fn [[bindings variadic?] param]
                                    (if (= param '&)
                                      [bindings true]
                                      [(conj bindings param
                                             (make-binding param variadic?))]))
                                  [[] false]
                                  sig)))
        methods (if (vector? (first fn-tail)) [fn-tail] fn-tail)
        methods (for [[sig & body] methods]
                  (list sig `(let [~@(make-bindings sig)] ~@body)))
        f `(fn [~'&form ~'&env]
               (apply (fn ~@methods) (next ~'&form)))
        ;_ (fipp.clojure/pprint f)
        f (clojure.core/eval f)] ;TODO: meta-eval here?
    `(defmacro ~(vary-meta name assoc :meta-macro f) [~'& ~'args]
       `(eval (~~f '~~'&form (local-env))))))

(defmacro defbn [name & fn-tail]
  (let [methods (if (vector? (first fn-tail)) [fn-tail] fn-tail)]
    `(defmeta ~name ~@(for [[args & body] methods]
                        (list args (list* `syntax body))))))

(comment

  (require 'fipp.edn)
  (require 'fipp.clojure)

  (fipp.clojure/pprint (macroexpand-1 '
  (defmeta blahblah
    ([foo] [foo])
    ([foo & bar] [foo bar]))
  ))

  (fipp.clojure/pprint (macroexpand-1 '
    (blahblah 1 2 3 4)
  ))

  (fipp.edn/pprint
    (let [x 1] (syntax x))
    )

  (let [x 1
        y [1 2 3]]
    (syntax 0))

  (let [x 1
        y [1 2 3]]
    (syntax x y))

)
