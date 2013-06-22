(ns conditions.free
  (:require [clojure.walk :as w]
            [clojure.set :as s]
            [slingshot.slingshot :as slingshot]))

(declare map-free-in-form')

(defn all-symbols [arglist]
  (flatten
   (cond
    (symbol? arglist) [arglist]
    (or (map? arglist) (sequential? arglist)) (keep all-symbols arglist)
    :else nil)))

(defn let-like [f needle [type bindings & forms]]
  (loop [binding-pairs (partition 2 bindings) mapped-bindings [] established-bindings #{}]
    (cond
     (established-bindings needle)
     `(~type [~@(apply concat (concat mapped-bindings binding-pairs))] ~@forms)
     (seq binding-pairs)
     (let [[bound binding-expr] (first binding-pairs)]
       (recur (rest binding-pairs)
              (conj mapped-bindings [bound (map-free-in-form' f needle binding-expr)])
              (s/union established-bindings (set (all-symbols bound)))))
     :else `(~type [~@(apply concat mapped-bindings)]
                   ~@(map (map-free-in-form' f needle) forms)))))

(defn catch-like [f needle [c cls exc-name & forms :as expr]]
  (if (= exc-name needle)
    expr
    `(~c ~(map-free-in-form' f needle cls) ~exc-name ~@(map (map-free-in-form' f needle) forms))))

(defn letrec-like [f needle [type bindings & forms :as expr]]
  ;; all bound names are visible in all binding expressions, no matter
  ;; the order of the bindings. So, first we check all bound symbols;
  ;; if what we're looking for isn't there, we map over all binding
  ;; expressions and the body.
  (let [bindings (take-nth 2 bindings)
        exprs (take-nth 2 (rest bindings))]
    (if (some #(= needle %) (mapcat all-symbols bindings))
      expr
      `(~type [~@(interleave bindings (map (map-free-in-form' f needle) exprs))]
              ~@(map (map-free-in-form' f needle) forms)))))

(defn fn-like [f needle [type & spec :as expr]]
  (let [name (when (symbol? (first spec)) (first spec))
        spec (if name (rest spec) spec)
        [arglists bodies] (if (vector? (first spec))
                            [[(first spec)] [(rest spec)]]
                            [(map first spec) (map rest spec)])]
    (if (= needle name)
      expr
      (loop [arglists* arglists bodies bodies mapped-already []]
        (let [arglist (first arglists*)
              body (first bodies)]
          (println arglist body mapped-already)
          (if arglist
            (recur (rest arglists*)
                   (rest bodies)
                   (conj mapped-already (if (not-any? #(= needle %) arglist)
                                          (map (map-free-in-form' f needle) body)
                                          body)))
            `(~type ~@(when name [name])
                    ~@(map (fn [al body] `(~al ~@body)) arglists mapped-already))))))))

(def binding-forms
  {'let* let-like
   'loop* let-like
   'letfn* letrec-like
   'fn* fn-like
   'catch catch-like})

(defn map-free-in-form'
  ([f s] #(map-free-in-form' f s %))
  ([f s form]
     (cond
      (seq? form) (cond (not-any? #{s} (all-symbols form)) form
                        (binding-forms (first form)) ((binding-forms (first form)) f s form)
                        :else (doall (map (map-free-in-form' f s) form)))
      (vector? form) (mapv (map-free-in-form' f s) form)
      ;; breaks record literals :(
      (map? form) (->> form
                       (map (fn [[key value]] [(map-free-in-form' f s key)
                                              (map-free-in-form' f s value)]))
                       (into {}))
      (set? form) (into #{} (map (map-free-in-form' f s) form))
      (= form s) (f s)
      :else form)))

(defn map-free-in-form [f s form]  
  (map-free-in-form' f s (w/macroexpand-all form)))

(defn replace-free-in-form [orig replacement form]
  (map-free-in-form (constantly replacement) orig form))

(defn free-in-form? [s form]
  (slingshot/try+ (doall (map-free-in-form (fn [v] (slingshot/throw+ true)) s form))
                  false
                  (catch true? _ true)))

(defn qualified-symbol? [x]
  (and (symbol? x) (re-find #"/" (str x))))

(defn contains-reference?
  "Returns true if a qualified symbol resolves to var in form, or if
   a free unqualified symbol not shadowed by env resolves to var in form."
  [env var form]
  (slingshot/try+ (w/prewalk (fn [x] (if (and (qualified-symbol? x)
                                             (= var (resolve x)))
                                      (slingshot/throw+ true)
                                      x)) form)
                  (w/prewalk (fn [x] (if (and (symbol? x)
                                             (not (contains? env x))
                                             (= var (resolve x))
                                             (free-in-form? x form))
                                      (slingshot/throw+ true)
                                      x)) form)
                  false
                  (catch true? _ true)))

(defn replace-all-reference
  "Replaces all qualified symbols that resolve to var in form with
   replacement, and all free unqualified symbols that resolve to var
   and aren't shadowed in env with replacement."
  [env var replacement form]
  (let [free-vars (atom #{})
        qualified-replaced (w/prewalk (fn [x] (if (and (qualified-symbol? x)
                                                      (= var (resolve x)))
                                               replacement
                                               x)) form)]

    (w/prewalk (fn [x] (when (and (symbol? x)
                                 (= var (resolve x))
                                 (not (contains? env x)))
                        ;; we can't replace it now, because x might
                        ;; resolve, not be shadowed, and be free in
                        ;; form, but not be free in *this*
                        ;; occurrence.
                        (swap! free-vars conj x))
                 x) qualified-replaced)
    (reduce (fn [form v] (replace-free-in-form v replacement form))
            qualified-replaced
            @free-vars)))
