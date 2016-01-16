(ns metaclj.impl.syntax
  (:require [metaclj.impl.env :as env]
            [metaclj.impl.parse :refer [parse]]))

(defn meta-macro? [{:keys [origin value]}]
  (and (= origin :namespace) (-> value meta :meta-macro)))

(defn macro? [{:keys [origin value]}]
  (and (= origin :namespace) (-> value meta :macro)))

(defrecord Syntax [forms env])

(defn syntax? [x]
  (instance? Syntax x))

(defn head [x]
  (if (syntax? x)
    :syntax
    (:head x)))

(defmulti transform #'head)

(defn transform-in [env x]
  (if (syntax? x)
    (transform x)
    (transform (parse x env))))

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

(defmethod transform :invoke [{:keys [env f args form]}]
  (if (symbol? f)
    (let [{:keys [value] :as resolved} (env/-resolve env f)]
      (cond
        (meta-macro? resolved)
        ,,(let [m (-> value meta :meta-macro)
                subst (apply m (next form))]
            (mapcat #(transform-in env %) (transform-in env subst)))
        (macro? resolved)
        ,,(let [subst (apply value (list* form (:locals env) (next form)))]
            (mapcat #(transform-in env %) (transform-in env subst)))
        ;XXX (expansion? value) (transform (assoc ast :f (:expr value)))
        :else [(list* value (mapcat #(transform-in env %) args))]))
    [(concat (transform-in env f) (mapcat #(transform-in env %) args))]))

(defmethod transform :var [{:keys [sym]}]
  [sym])

(defmethod transform :let
  [{:keys [bindings expr env]}]
  (let [[env bindings] (reduce (fn [[env bindings] {:keys [name init]}]
                                 [(assoc-in env [:locals name] name)
                                  (into (conj bindings name)
                                        (transform-in env init))])
                               [env []]
                               bindings)]
    [(list* 'let* bindings (transform-in env expr))]))

(defmethod transform :if
  [{:keys [test then else env]}]
  [(list* 'if (mapcat #(transform-in env %) [test then else]))])
