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
                                 (let [sym (rename name)
                                       inits (transform-in env init)]
                                   [(assoc env name sym)
                                    (conj bindings sym (list* 'do inits))]))
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

;TODO :throw
;TODO :try
;TODO :new
;TODO :interop
;TODO :declare
;TODO :assign-var
;TODO :assign-field
;TODO :loop
;TODO :recur
;TODO :case
;TODO :import
;TODO :reify
;TODO :deftype
