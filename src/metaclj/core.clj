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
  (defmacro party [form]
    `(doseq [x# (transform-in (local-env) '~form)]
       (fipp.clojure/pprint x#)))

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

  (party inc)
  (party #'inc)
  (let [x 1] (party (fn f [y] (+ x y))))
  (let [x 1] (party (fn [y] (+ x y))))
  (let [x 1]
    (party (letfn [(f [] (g x))
                   (g [] (inc x))]
             (g 5))))
  (let [x (Exception. "OH NOEZ")] (party (throw x)))

  (deftype Box [x])
  (let [typ Box, x 1] (party (new typ x)))

  (deftype Pair [x y])
  (let [xy (syntax 1 2)] (party (new Pair xy)))

  (let [x (syntax 1 2)] (party (recur x)))

  (let [typ "java.util.regex.Pattern"] (party (clojure.core/import* typ)))

  (party (try))
  (let [a 1, b ArithmeticException, c 2, d 3, e 4]
    (party (try a (catch b ex [ex c]) (catch :default ex d) (finally e))))

  (party (declare x y z))
  (let [x (syntax 1 2)] (party (def y x)))

  (let [x 5 y 2]
    (party (loop [x x, y y] (if (pos? x) (recur (dec x) (inc y)) y))))

  (party (case 1 2 3 4)) ;XXX broken by transform loop

  (def ^:dynamic *x* 1)
  (let [y 3] (party (binding [*x* 2] (set! *x* y))))

  (let [obj :bogus, x 1] (party (set! (.x obj) x)))

  (defprotocol Frobable (frob [this x]))
  (deftype Foo [] Frobable (frob [this x] x))
  (let [x 1] (party (.frob (Foo.) x)))

)
