(ns conditions.free
  (:require [clojure.set :as s]
            [slingshot.slingshot :as slingshot]))

(declare map-free-in-form')

(defn all-symbols [arglist]
  (flatten
   (cond
    (symbol? arglist) [arglist]
    (or (map? arglist) (sequential? arglist)) (keep all-symbols arglist)
    :else nil)))

(defn let-like [f bound [type bindings & forms]]
  (loop [binding-pairs (partition 2 bindings) mapped-bindings [] established-bindings bound]
    (if (seq binding-pairs)
      (let [[new-bindings binding-expr] (first binding-pairs)]
        (recur (rest binding-pairs)
               (conj mapped-bindings [new-bindings (map-free-in-form' f bound binding-expr)])
               (s/union established-bindings (set (all-symbols new-bindings)))))
      `(~type [~@(apply concat mapped-bindings)]
              ~@(doall (map (map-free-in-form' f established-bindings) forms))))))

(defn catch-like [f bound [c cls exc-name & forms]]
  (let [mapped (doall (map (map-free-in-form' f (conj bound exc-name)) forms))]
    `(~c ~cls ~exc-name ~@mapped)))

(defn letrec-like [f bound [type bindings & forms :as expr]]
  ;; all bound names are visible in all binding expressions, no matter
  ;; the order of the bindings. So, first we check all bound symbols;
  ;; if what we're looking for isn't there, we map over all binding
  ;; expressions and the body.
  (let [bound-names (take-nth 2 bindings)
        bound (reduce conj bound (mapcat all-symbols bound-names))
        exprs (take-nth 2 (rest bindings))]
    `(~type [~@(interleave bound-names (map (map-free-in-form' f bound) exprs))]
            ~@(map (map-free-in-form' f bound) forms))))

(defn def-like [f bound [type nm expr]]
  ;; the name being defined is visible in the expression
  `(~type ~nm ~(map-free-in-form' f (conj bound nm) expr)))

(defn fn-like [f bound [type & spec]]
  (let [name (when (symbol? (first spec)) (first spec))
        spec (if name (rest spec) spec)
        [all-arglists bodies] (if (vector? (first spec))
                                [[(first spec)] [(rest spec)]]
                                [(map first spec) (map rest spec)])
        bound (if name (conj bound name) bound)]
    (loop [arglists all-arglists bodies bodies already []]
      (let [arglist (first arglists)
            body (first bodies)]
        (if arglist
          (let [new-bound (reduce conj bound (all-symbols arglist))]
            (recur (rest arglists)
                   (rest bodies)
                   (conj already (doall (map (map-free-in-form' f new-bound) body)))))
          `(~type ~@(when name [name])
                  ~@(doall
                     (map (fn [arglist body] `(~arglist ~@body)) all-arglists already))))))))

(def binding-forms
  {'let* let-like
   'loop* let-like
   'letfn* letrec-like
   'fn* fn-like
   'def def-like
   'catch catch-like})

(defn macro-invokation? [f]
  (and (seq? f)
       (symbol? (first f))
       (-> f first resolve meta :macro)))

(defn expand-macro [bindings f]
  ;; we just have to assume that no one would be so perverse as to
  ;; write a macro that doesn't just see keys are in &env, but
  ;; actually depends on the particular *values*---or at least, we can
  ;; declare that people doing so are just getting what's coming to
  ;; them, given that we don't want to *actually execute* the code
  ;; being inspected (the approach taken by jvm.tools.analyzer).
  (let [result (apply (resolve (first f)) f (zipmap bindings (repeat true)) (rest f))]
    (if (macro-invokation? result)
      (recur bindings result)
      result)))

(defn map-free-in-form'
  ([f bindings] #(map-free-in-form' f bindings %))
  ([f bindings form]
     (cond
      (seq? form) (cond
                   (macro-invokation? form)     (recur f bindings (expand-macro bindings form))
                   (binding-forms (first form)) ((binding-forms (first form)) f bindings form)
                   :else                       (doall (map (map-free-in-form' f bindings) form)))
      (vector? form) (mapv (map-free-in-form' f bindings) form)
      ;; breaks record literals :(
      (map? form) (->> form
                       (map (fn [[key value]] [(map-free-in-form' f bindings key)
                                              (map-free-in-form' f bindings value)]))
                       (into {}))
      (set? form) (into #{} (map (map-free-in-form' f bindings) form))      
      (and (symbol? form)
           (not (contains? bindings form))) (f form)
      :else form)))

(defn map-free-in-form
  "Transform all free symbols in form by the function f. An initial
   environment may be provided. Note that this function macroexpands
   form."
  ([f form]  
     (map-free-in-form #{} f form))
  ([init-env f form]
     (map-free-in-form' f init-env form)))

(defn free-in-form
  "Return a set of all free symbols in form."
  [form]
  (let [a (atom #{})]
    (map-free-in-form (fn [s] (swap! a conj s) s) form)
    @a))

(defn free-in-form-by?
  "Returns true if a symbol for which pred is true is free in form,
   stopping as soon as the first free occurrence is found."  
  [pred form]
  (slingshot/try+
   (doall (map-free-in-form (fn [v] (if (pred v) (slingshot/throw+ true) v)) form))
   false
   (catch true? _ true)))

(defn free-in-form?
  "Returns true if the symbol s is free in form."
  [s form]
  (free-in-form-by? (partial = s) form))

(defn replace-free-in-form
  "Replace all free symbols in form with replacement."
  [replacement form]
  (map-free-in-form (constantly replacement) form))



(defn qualified-symbol? [x]
  (and (symbol? x) (re-find #"/" (str x))))

(defn contains-reference?
  "Returns true if a free symbol not shadowed by env resolves to var in form."
  [env var form]
  (free-in-form-by? (fn [s] (and (not (contains? env s))
                                (= var (resolve s)))) form))

(defn replace-all-reference
  "Replaces free symbols not shadowed by env that resolve to var in
   form with replacement."
  [env var replacement form]
  (map-free-in-form env (fn [s] (if (= var (resolve s))
                                 replacement
                                 s)) form))
