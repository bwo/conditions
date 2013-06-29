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

(defn- mm [f bindings opts form]
  (doall (map (map-free-in-form' f bindings opts) form)))

(defn let-like [f bound opts [type bindings & forms]]
  (loop [binding-pairs (partition 2 bindings) mapped-bindings [] established-bindings bound]
    (if (seq binding-pairs)
      (let [[new-bindings binding-expr] (first binding-pairs)]
        (recur (rest binding-pairs)
               (conj mapped-bindings [new-bindings (map-free-in-form' f bound opts binding-expr)])
               (s/union established-bindings (set (all-symbols new-bindings)))))
      `(~type [~@(apply concat mapped-bindings)]
              ~@(mm f established-bindings opts forms)))))

(defn catch-like [f bound opts [c cls exc-name & forms]]
  (let [mapped (mm f (conj bound exc-name) opts forms)]
    `(~c ~cls ~exc-name ~@mapped)))

(defn letrec-like [f bound opts [type bindings & forms :as expr]]
  ;; all bound names are visible in all binding expressions, no matter
  ;; the order of the bindings. So, first we check all bound symbols;
  ;; if what we're looking for isn't there, we map over all binding
  ;; expressions and the body.
  (let [bound-names (take-nth 2 bindings)
        bound (reduce conj bound (mapcat all-symbols bound-names))
        exprs (take-nth 2 (rest bindings))]
    `(~type [~@(doall (interleave bound-names (mm f bound opts exprs)))]
            ~@(mm f bound opts forms))))

(defn def-like [f bound opts [type nm expr]]
  ;; the name being defined is visible in the expression
  `(~type ~nm ~(map-free-in-form' f (conj bound nm) opts expr)))

(defn fn-like [f bound opts [type & spec]]
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
                   (conj already (mm f new-bound opts body))))
          `(~type ~@(when name [name])
                  ~@(doall
                     (map (fn [arglist body] `(~arglist ~@body)) all-arglists already))))))))

(defn try-like [f bound opts [type & clauses]]
  (let [regular (take-while #(or (not (seq? %))
                                 (not= 'catch (first %))) clauses)
        catches (drop-while #(or (not (seq? %))
                                 (not= 'catch (first %))) clauses)
        finally (let [l (last clauses)]
                  (when (and (seq? l) (= 'finally (first l))) l))]
    `(~type ~@(mm f bound opts regular)
            ~@(doall (map #(catch-like f bound opts %) catches))
            ~@(when finally [(map-free-in-form' f bound opts finally)]))))

(defn do-like [f bound opts [type & clauses]]
  `(~type ~@(mm f bound opts clauses)))

(defn quote-like [_ _ _ expr]
  expr)

(defn case*-like [f bound opts [type ge shift mask default imap & rest]]
  (let [imap (->> imap
                  (map (fn [[k [m expr]]]
                         [k [m (map-free-in-form' f bound opts expr)]]))
                  (into {}))]
    `(~type ~ge ~shift ~mask ~(map-free-in-form' f (conj bound ge) opts default)
            ~imap ~@rest)))

(defn case-like [f bound opts [type expr & clauses]]
  (let [has-default? (odd? (count clauses))
        matches ((if has-default? butlast identity) (take-nth 2 clauses))
        actions (take-nth 2 (rest clauses))
        default (when has-default? (last clauses))]
    `(~type ~(map-free-in-form' f bound expr)
            ~@(doall (interleave matches
                                 (mm f bound opts actions)))
            ~@(when has-default? [default]))))

(defn method-like [f bound opts method]
  (let [[_ name [params & body]] (fn-like f bound opts (cons 'fn* method))]
    `(~name ~params ~@body)))

(defn map-spec [f bound opts spec]
  (if (seq? spec)
    (method-like f bound opts spec)
    spec))

(defn reify-like [f bound opts [type & specs]]
  `(~type ~@(doall (map #(map-spec f bound opts %) specs))))

(defn deftype-like [f bound opts [type name fields & specs]]
  (let [bound (reduce conj bound fields)]
    `(~type ~name ~fields ~@(doall (map #(map-spec f bound opts %) specs)))))

(defn reify*-like [f bound opts [type ifaces & methods]]
  `(~type ~ifaces ~@(doall (map #(method-like f bound opts %) methods))))

(defn deftype*-like [f bound opts [type name class fields impls ifaces & methods]]
  (let [bound (reduce conj bound fields)]
    `(~type ~name ~class ~fields ~impls ~ifaces
            ~@(doall (map #(method-like f bound opts %) methods)))))

(defn new-like [f bound opts [type cls & args]]
  `(~type ~cls ~@(mm f bound opts args)))

(defn dot-like [f bound opts [type obj m]]
  (letfn [(mem-or-meth [m]
            (if (symbol? m)
              (list m)
              (let [[meth & args] m]
                (list (cons meth (mm f bound opts args))))))]
    (if (and (symbol? obj)
             (class? (resolve obj)))
      `(~type ~obj ~@(mem-or-meth m))
      `(~type ~(map-free-in-form' f bound opts obj) ~@(mem-or-meth m)))))

(def binding-forms
  (let [unqualified {
                     '.              dot-like
                     'case           case-like
                     'case*          case*-like
                     'def            def-like
                     'deftype        deftype-like
                     'deftype*       deftype*-like
                     'do             do-like
                     'fn*            fn-like
                     'if             do-like
                     'import*        quote-like
                     'let*           let-like
                     'letfn*         letrec-like
                     'loop*          let-like
                     'monitor-enter  do-like
                     'monitor-exit   do-like
                     'new            new-like
                     'quote          quote-like
                     'recur          do-like
                     'reify          reify-like
                     'reify*         reify*-like
                     'set!           do-like
                     'throw          do-like
                     'try            try-like
                     ;; "var" is weird in that its argument is the
                     ;; *textual* symbol:
                     ;; > ((fn [a] (var a)) 6)
                     ;; #'user/a
                     ;; Consequently I'm not sure how meaningful it is
                     ;; to consider its argument either free or bound.
                     ;; (on the other hand, don't similar
                     ;; considerations suggest treating qualified
                     ;; symbols as *not* free? since no local bindings
                     ;; can be introduced for them.)
                     'var            quote-like
                     }]
    (reduce (fn [m k] (assoc m (symbol "clojure.core" (str k)) (get m k)))
            unqualified (keys unqualified))))

(defn macro-invocation? [f]
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
    (if (macro-invocation? result)
      (recur bindings result)
      result)))

(defn maybe-expand [bindings f]
  (if (macro-invocation? f)
    (expand-macro bindings f)
    f))

(defn qualified? [s]
  (and (not= '/ s)
       (re-find #"/" (str s))))

(defn class-name? [s]
  (and (not (qualified? s))
       (re-find #"\." (str s))))

(defn map-free-in-form'
  ([f bindings opts] #(map-free-in-form' f bindings opts %))
  ([f bindings opts form]
     (cond
      (seq? form) (cond
                   (binding-forms (first form))
                   (maybe-expand bindings ((binding-forms (first form)) f bindings opts form))
                   (macro-invocation? form)
                   (recur f bindings opts (expand-macro bindings form))
                   :else
                   (mm f bindings opts form))
      (vector? form) (mapv (map-free-in-form' f bindings opts) form)
      ;; breaks record literals :(
      (map? form) (->> form
                       (map (fn [[key value]] [(map-free-in-form' f bindings opts key)
                                              (map-free-in-form' f bindings opts value)]))
                       (into {}))
      (set? form) (into #{} (map (map-free-in-form' f opts bindings) form))
      (and (symbol? form)
           (not (contains? bindings form))
           (not (let [s (str form)]
                  (or (.startsWith s ".")
                      (.endsWith s ".")))))
      (cond
       (and (not (:classes-are-free opts)) (class-name? form))  form
       (and (not (:qualified-are-free opts)) (qualified? form)) form
       :else (f form))
      :else form)))

(def default-opts {:classes-are-free false
                   :qualified-are-free false})

(defn map-free-in-form
  "(map-free-in-form f form) transforms all free symbols in form by
   the function f. form is macroexpanded along the way (so that one
   may macroexpand a form by passing in identity as f).

   (map-free-in-form initial-env f form) uses initial-env (a set) as
   an initial environment.

   (map-free-in-form opts initial-env f form) uses initial-env as
   above and opts to control whether qualified class names and
   qualified symbols are treated as free. The default options are
   contained in the default-opts map; with them, neither qualified
   class names (construed as any symbol containing an interior \".\"
   that does not contain a \"/\") nor qualified symbols (any symbol
   other than / containing \"/\") are not considered free, under the
   reasoning that no local bindings can be introduced for them. The
   keys :classes-are-free and :qualified-are-free change the defaults.
   Symbols beginning or ending with a . are never considered free."
  ([f form]  
     (map-free-in-form #{} f form))
  ([init-env f form]
     (map-free-in-form default-opts init-env f form))
  ([opts init-env f form]
     (map-free-in-form' f init-env opts form)))

(defn macroexpand-all
  "Expand all macros in form recursively, propagating information
   about locally bound symbols."
  [form]
  (map-free-in-form identity form))

(defn free-in-form
  "Return a set of all free symbols in form."
  ([form] (free-in-form default-opts form))
  ([opts form]
     (let [a (atom #{})]
       (map-free-in-form opts #{} (fn [s] (swap! a conj s) s) form)
       @a)))

(defn free-in-form-by?
  "Returns true if a symbol for which pred is true is free in form,
   stopping as soon as the first free occurrence is found."
  ([pred form] (free-in-form-by? default-opts pred form))
  ([opts pred form]
     (slingshot/try+
      (map-free-in-form opts #{} (fn [v] (if (pred v) (slingshot/throw+ true) v)) form)
      false
      (catch true? _ true))))

(defn free-in-form?
  "Returns true if the symbol s is free in form."
  ([s form]
     (free-in-form? default-opts s form))
  ([opts s form]
     (free-in-form-by? opts (partial = s) form)))

(defn replace-free-in-form
  "Replace all free symbols in form with replacement."
  [replacement form]
  (map-free-in-form (constantly replacement) form))

(defn contains-reference?
  "Returns true if a free symbol not shadowed by env resolves to var
   in form. Considers qualified symbols free."
  [env var form]
  (free-in-form-by? (assoc default-opts :qualified-are-free true)
                    (fn [s]  (and (not (contains? env s))
                                (= var (resolve s))))
                    form))

(defn replace-all-reference
  "Replaces free symbols not shadowed by env that resolve to var in
   form with replacement. Considers qualified symbols free."
  [env var replacement form]
  (map-free-in-form (assoc default-opts :qualified-are-free true)
                    env
                    (fn [s] (if (= var (resolve s))
                             replacement
                             s))
                    form))
