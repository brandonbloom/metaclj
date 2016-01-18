(ns metaclj.impl.env
  (:require [metaclj.impl.patch :as patch])
  (:import [clojure.lang Reflector]))


(defrecord Env [namespace])

(defn static-invoke [class member & args]
  (if (zero? (count args))
    (try
      (Reflector/getStaticField class member)
      (catch Exception e
        (Reflector/invokeStaticMethod
          class member clojure.lang.RT/EMPTY_ARRAY)))
    (Reflector/invokeStaticMethod class member (object-array args))))

(defn staticfn [class member]
  (fn [& args]
    (apply static-invoke class member args)))

(defn lookup-var [ns sym]
  (try
    (if-let [x (ns-resolve ns sym)]
      (if (var? x)
        (if-let [patch (patch/vars x)]
          (lookup-var ns patch)
          {:origin :namespace :value x})
        {:origin :host :value x})
      {:origin :host :value (clojure.lang.RT/classForName (name sym))})
    (catch ClassNotFoundException e
      nil)))

;;TODO: These should all be methods on IEnv.

(defn -deref [env ref]
  (deref ref))

(defn -invoke [env f args]
  (apply f args))

(defn -resolve [env sym]
  (or (when-let [[_ value] (find env sym)]
        {:origin :locals :value value})
      (lookup-var (:namespace env) sym)
      (when-let [ns (namespace sym)]
        (let [{:keys [value]} (lookup-var (:namespace env) (symbol ns))
              n (name sym)]
          (when (instance? Class value)
            {:origin :host
             :value (try
                      (.get (.getField value n) value)
                      (catch NoSuchFieldException _
                        (staticfn value n)))})))))

;XXX should the uses of *ns* below be (:namespace env) ?

(defn -declare [env sym]
  (intern *ns* sym))

(defn -define [env sym value]
  (let [var (intern *ns* sym value)]
    (when (-> sym meta :dynamic)
      (.setDynamic var))
    var))

(defn -new [env class args]
  (Reflector/invokeConstructor class (object-array args)))

(defn -interop [env static? object member args]
  (let [s (str member)
        s (if (.startsWith s "-")
            (apply str (next s))
            s)]
    (if static?
      (apply static-invoke object s args)
      (if (zero? (count args))
        (Reflector/invokeNoArgInstanceMember object s)
        (Reflector/invokeInstanceMember s object (object-array args))))))

(defn -assign-var [env var value]
  (var-set var value))

(defn assign-field [env object field value] ;TODO: Test this.
  (let [field (name field)]
    (if (instance? Class object)
      (Reflector/setStaticField object field value)
      (Reflector/setInstanceField object field value))))

(defn -import [env sym]
  (.importClass *ns* (clojure.lang.RT/classForName (name sym))))

(defn -reify [env interfaces methods]
  (clojure.core/eval
    `(reify* ~interfaces
      ~@((for [[name args & body] methods
               :let [expr `'(do ~@body)
                     denv `(-> ~env ~@(for [arg args]
                                        `(assoc '~arg ~arg)))]]
            (list name args `(eclj.core/eval ~expr ~denv)))))))

(defn -deftype [env tagname classname fields implements methods]
  (clojure.core/eval
    `(deftype* ~tagname ~classname ~fields :implements ~implements
       ~@(for [[name args & body] methods
               :let [params (repeatedly (count args) gensym)
                     this (first params)
                     getters (map #(list (symbol (str ".-" %)) this) fields)
                     expr `'(eclj.core/symbol-macrolet
                              [~@(interleave fields getters)]
                              (let [~@(interleave args params)]
                                ~@body))
                     denv `(-> ~env
                             ~@(for [param params]
                                 `(assoc '~param ~param)))]]
             (list name (vec params) `(eclj.core/eval ~expr ~denv))))))
