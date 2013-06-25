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

(defn- mm [f bindings form]
  (doall (map (map-free-in-form' f bindings) form)))

(defn let-like [f bound [type bindings & forms]]
  (loop [binding-pairs (partition 2 bindings) mapped-bindings [] established-bindings bound]
    (if (seq binding-pairs)
      (let [[new-bindings binding-expr] (first binding-pairs)]
        (recur (rest binding-pairs)
               (conj mapped-bindings [new-bindings (map-free-in-form' f bound binding-expr)])
               (s/union established-bindings (set (all-symbols new-bindings)))))
      `(~type [~@(apply concat mapped-bindings)]
              ~@(mm f established-bindings forms)))))

(defn catch-like [f bound [c cls exc-name & forms]]
  (let [mapped (mm f (conj bound exc-name) forms)]
    `(~c ~cls ~exc-name ~@mapped)))

(defn letrec-like [f bound [type bindings & forms :as expr]]
  ;; all bound names are visible in all binding expressions, no matter
  ;; the order of the bindings. So, first we check all bound symbols;
  ;; if what we're looking for isn't there, we map over all binding
  ;; expressions and the body.
  (let [bound-names (take-nth 2 bindings)
        bound (reduce conj bound (mapcat all-symbols bound-names))
        exprs (take-nth 2 (rest bindings))]
    `(~type [~@(doall (interleave bound-names (map (map-free-in-form' f bound) exprs)))]
            ~@(mm f bound forms))))

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
                   (conj already (mm f new-bound body))))
          `(~type ~@(when name [name])
                  ~@(doall
                     (map (fn [arglist body] `(~arglist ~@body)) all-arglists already))))))))

(defn try-like [f bound [type & clauses]]
  (let [regular (take-while #(or (not (seq? %))
                                 (not= 'catch (first %))) clauses)
        catches (drop-while #(or (not (seq? %))
                                 (not= 'catch (first %))) clauses)
        finally (let [l (last clauses)]
                  (when (and (seq? l) (= 'finally (first l))) l))]
    `(~type ~@(mm f bound regular)
            ~@(doall (map #(catch-like f bound %) catches))
            ~@(when finally [(map-free-in-form' f bound finally)]))))

(defn do-like [f bound [type & clauses]]
  `(~type ~@(mm f bound clauses)))

(defn quote-like [f bound expr]
  expr)

(defn case*-like [f bound [type ge shift mask default imap & rest]]
  (let [imap (->> imap
                  (map (fn [[k [m expr]]]
                         [k [m (map-free-in-form' f bound expr)]]))
                  (into {}))]
    `(~type ~ge ~shift ~mask ~(map-free-in-form' f (conj bound ge) default)
            ~imap ~@rest)))

(defn case-like [f bound [type expr & clauses]]
  (let [has-default? (odd? (count clauses))
        matches ((if has-default? butlast identity) (take-nth 2 clauses))
        actions (take-nth 2 (rest clauses))
        default (when has-default? (last clauses))]
    `(~type ~(map-free-in-form' f bound expr)
            ~@(doall (interleave matches
                                 (map (map-free-in-form' f bound) actions)))
            ~@(when has-default? [default]))))

(defn method-like [f bound method]
  (let [[_ name [params & body]] (fn-like f bound (cons 'fn* method))]
    `(~name ~params ~@body)))

(defn map-spec [f bound spec]
  (if (seq? spec)
    (method-like f bound spec)
    spec))

(defn reify-like [f bound [type & specs]]
  `(~type ~@(doall (map #(map-spec f bound %) specs))))

(defn deftype-like [f bound [type name fields & specs]]
  (let [bound (reduce conj bound fields)]
    `(~type ~name ~fields ~@(doall (map #(map-spec f bound %) specs)))))

(defn reify*-like [f bound [type ifaces & methods]]
  `(~type ~ifaces ~@(doall (map #(method-like f bound %) methods))))

(defn deftype*-like [f bound [type name class fields impls ifaces & methods]]
  (let [bound (reduce conj bound fields)]
    `(~type ~name ~class ~fields ~impls ~ifaces
            ~@(doall (map #(method-like f bound %) methods)))))

(defn new-like [f bound [type cls & args]]
  `(~type ~cls ~@(mm f bound args)))

(defn dot-like [f bound [type obj m]]
  (letfn [(mem-or-meth [m]
            (if (symbol? m)
              (list m)
              (let [[meth & args] m]
                (list (cons meth (mm f bound args))))))]
    (if (and (symbol? obj)
             (class? (resolve obj)))
      `(~type ~obj ~@(mem-or-meth m))
      `(~type ~(map-free-in-form' f bound obj) ~@(mem-or-meth m)))))

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

(def ^:dynamic *qualified-are-free* true)
(defn qualified? [s]
  (re-find #"/" (str s)))

(defn map-free-in-form'
  ([f bindings] #(map-free-in-form' f bindings %))
  ([f bindings form]
     (cond
      (seq? form) (cond
                   (binding-forms (first form)) (maybe-expand bindings ((binding-forms (first form)) f bindings form))
                   (macro-invocation? form)     (recur f bindings (expand-macro bindings form))
                   :else                        (mm f bindings form))
      (vector? form) (mapv (map-free-in-form' f bindings) form)
      ;; breaks record literals :(
      (map? form) (->> form
                       (map (fn [[key value]] [(map-free-in-form' f bindings key)
                                              (map-free-in-form' f bindings value)]))
                       (into {}))
      (set? form) (into #{} (map (map-free-in-form' f bindings) form))      
      (and (symbol? form)
           (not (let [nm (name form)]
                  (or (.startsWith nm ".")
                      (.endsWith nm "."))))
           (not (contains? bindings form))
           (or *qualified-are-free* (not (qualified? form)))) (f form)
      :else form)))

(defn map-free-in-form
  "(map-free-in-form f form) transforms all free symbols in form by
   the function f. form is macroexpanded along the way (so that one
   may macroexpand a form by passing in identity as f).

   (map-free-in-form initial-env f form) uses initial-env (a set) as
   an initial environment.

   (map-free-in-form qualified-are-free? initial-env f form) controls
   whether or not qualified symbols are considered free. By default
   they are, reasoning that in an expression such as the following:

      (let [f clojure.core/str] (f 1))

   There is no visible binding for clojure.core/str. However, one
   might also reason that since local bindings *cannot* be introduced
   for qualified symbols, calling them \"free\" is somewhat
   misleading. To never consider qualified symbols free, the
   four-argument form of this function may be used with an initial
   falsy argument."
  ([f form]  
     (map-free-in-form #{} f form))
  ([init-env f form]
     (map-free-in-form true init-env f form))
  ([qualfied-are-free? init-env f form]
     (binding [*qualified-are-free* qualfied-are-free?]
       (map-free-in-form' f init-env form))))

(defn macroexpand-all
  "Expand all macros in form recursively, propagating information
   about locally bound symbols."
  [form]
  (map-free-in-form identity form))

(defn free-in-form
  "Return a set of all free symbols in form."
  ([form] (free-in-form true form))
  ([qualified-are-free? form]
     (let [a (atom #{})]
       (map-free-in-form qualified-are-free? #{} (fn [s] (swap! a conj s) s) form)
       @a)))

(defn free-in-form-by?
  "Returns true if a symbol for which pred is true is free in form,
   stopping as soon as the first free occurrence is found."
  ([pred form] (free-in-form-by? true pred form))
  ([qualified-are-free? pred form]
     (slingshot/try+
      (map-free-in-form qualified-are-free? #{} (fn [v] (if (pred v) (slingshot/throw+ true) v)) form)
      false
      (catch true? _ true))))

(defn free-in-form?
  "Returns true if the symbol s is free in form."
  ([s form]
     (free-in-form? true s form))
  ([qualified-are-free? s form]
     (free-in-form-by? qualified-are-free? (partial = s) form)))

(defn replace-free-in-form
  "Replace all free symbols in form with replacement."
  [replacement form]
  (map-free-in-form (constantly replacement) form))

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
