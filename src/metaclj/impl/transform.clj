(ns metaclj.impl.transform
  (:require [metaclj.impl.env :as env]
            [metaclj.impl.parse :refer [parse head syntax?]]))

(defn rename [sym]
  (gensym (str sym "$")))

(defn meta-macro? [{:keys [origin value]}]
  (and (= origin :namespace) (-> value meta :meta-macro)))

(defn macro? [{:keys [origin value]}]
  (and (= origin :namespace) (-> value meta :macro)))

(defmulti transform #'head)

(defn transform-in [env x]
  (transform (parse x env)))

(defn do-in [env x]
  (let [xs (transform-in env x)]
    (case (count xs)
      0 nil
      1 (first xs)
      (list* 'do xs))))

(defmethod transform :syntax [{:keys [forms env]}]
  (mapcat #(transform-in env %) forms))

(defmethod transform :constant [{:keys [value]}]
  [value])

(defmethod transform :name [{:keys [env sym]}]
  (let [{:keys [origin value] :as resolved} (env/-resolve env sym)]
    (if (macro? resolved)
      (throw (ex-info "Can't take value of a macro" {:sym sym :env env}))
      (let [x (case origin
                nil (throw (ex-info "Undefined" {:sym sym}))
                :locals value
                :host value
                :namespace @value
                (throw (ex-info "Unknown origin" {:origin origin})))]
        (if (syntax? x)
          (transform x)
          [x])))))

(defmethod transform :do [{:keys [env statements ret]}]
  [(concat ['do]
           (mapcat #(transform-in env %) statements)
           (transform-in env ret))])

(defn transform-items [coll env]
  (into (empty coll) (mapcat #(transform-in env %) coll)))

(defmethod transform :collection [{:keys [coll env]}]
  [(transform-items coll env)])

(defn splice-syntax [form env]
  (list* (first form)
         (mapcat (fn [x]
                   (let [y (if (symbol? x) (:value (env/-resolve env x)) x)]
                     (if (syntax? y)
                       (mapcat #(transform-in (:env y) %) (:forms y))
                       [x])))
                 (next form))))

(defmethod transform :invoke [{:keys [env f args form]}]
  (if (symbol? f)
    (let [{:keys [value] :as resolved} (env/-resolve env f)]
      (cond
        (meta-macro? resolved)
        ,,(let [mac (-> value meta :meta-macro)
                subst (mac (splice-syntax form env) env)]
            (mapcat #(transform-in env %) (transform-in env subst)))
        (macro? resolved)
        ,,(let [subst (apply value (list* form env (next form)))]
            (mapcat #(transform-in env %) (transform-in env subst)))
        ;XXX (expansion? value) (transform (assoc ast :f (:expr value)))
        :else [(list* value (mapcat #(transform-in env %) args))]))
    [(concat (transform-in env f) (mapcat #(transform-in env %) args))]))

(defmethod transform :var [{:keys [sym]}]
  [(list 'var sym)])

(defmethod transform :let
  [{:keys [bindings expr env]}]
  (let [[env bindings] (reduce (fn [[env bindings] {:keys [name init]}]
                                 (let [sym (rename name)]
                                   [(assoc env name sym)
                                    (conj bindings sym (do-in env init))]))
                               [env []]
                               bindings)]
    [(list* 'let* bindings (transform-in env expr))]))

(defmethod transform :if
  [{:keys [test then else env]}]
  [(list* 'if (mapcat #(transform-in env %) [test then else]))])

(defmethod transform :meta
  [{:keys [form meta env]}]
  ;XXX use meta
  (transform-in env (with-meta form nil)))

(defn transform-method [{:keys [fixed variadic expr]} env]
  (let [gfixed (map rename fixed)
        gvariadic (when variadic (rename variadic))
        params (concat fixed (when variadic [variadic]))
        gparams (concat gfixed (when gvariadic [gvariadic]))
        sig (vec (concat gfixed (when gvariadic ['& gvariadic])))
        env (into env (map vector params gparams))]
    (list* sig (transform-in env expr))))

(defmethod transform :fn
  [{:keys [name methods env]}]
  (let [code ['fn*]
        gname (when name (rename name))
        [code env] (if name
                     [(conj code name) (assoc env name gname)]
                     [code env])]
    [(concat code (map #(transform-method % env) methods))]))

(defmethod transform :letfn
  [{:keys [bindings expr env]}]
  (let [env (into env (for [[name _] bindings]
                        [name (rename name)]))
        fns (mapv (fn [[name f]]
                    (list* `fn (get env name)
                           (map #(transform-method % env)
                                (:methods f))))
                  bindings)
        bindings (mapcat (fn [[name _] f] [name f]) bindings fns)]
    [(list* 'letfn* (vec bindings) (transform-in env expr))]))

(defmethod transform :throw
  [{:keys [expr env]}]
  [(list 'throw (do-in env expr))])

(defmethod transform :new
  [{:keys [class args env]}]
  [(list* 'new (do-in env class) (mapcat #(transform-in env %) args))])

(defmethod transform :recur
  [{:keys [args env]}]
  [(list* 'recur (mapcat #(transform-in env %) args))])

(defmethod transform :import
  [{:keys [sym env]}]
  [(list* 'clojure.core/import* (transform-in env sym))])

(defmethod transform :try
  [{:keys [try catches default finally env]}]
  [(concat ['try] (transform-in env try)
    (for [{:keys [type name expr]} catches
          :let [rn (rename name)]]
      (list* 'catch (do-in env type) rn
             (transform-in (assoc env name rn) expr)))
    (when-let [{:keys [name expr]} default]
      (let [rn (rename name)]
        [(list* 'catch `Exception rn
                (transform-in (assoc env name rn) expr))]))
    (when finally
      [(list* 'finally (transform-in env finally))]))])

(defmethod transform :declare
  [{:keys [sym]}]
  [(list 'def sym)])

(defmethod transform :define
  [{:keys [sym expr env]}]
  [(list 'def sym (do-in env expr))])

(defmethod transform :loop
  [{:keys [bindings expr env]}]
  (let [[bindings env] (reduce (fn [[bindings env] {:keys [name init]}]
                                 (let [rn (rename name)]
                                   [(conj bindings rn (do-in env init))
                                    (assoc env name rn)]))
                               [[] env]
                               bindings)]
    [(list* 'loop* bindings (transform-in env expr))]))

(defmethod transform :case
  [{:keys [expr cases default env]}]
  [(concat [`case]
     [(do-in env expr)]
     (mapcat (fn [[val expr]]
               [val (do-in env expr)])
             cases)
     [(do-in env default)])])

(defmethod transform :assign-var
  [{:keys [name expr env]}]
  [(list 'set! name (do-in env expr))])

(defmethod transform :assign-field
  [{:keys [object field expr env]}]
  [(list 'set! (list '. (do-in env object) field) (do-in env expr))])

(defmethod transform :interop
  [{:keys [target member args env]}]
  [(list* '. (do-in env target) member (mapcat #(transform-in env %) args))])

(defmethod transform :unquote
  [{:keys [expr env]}]
  (map clojure.core/eval (transform-in env expr)))

;TODO :reify
;TODO :deftype
