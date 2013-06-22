(ns conditions.core
  (:require [conditions.support :as s]
            [slingshot.slingshot :as slingshot]))

(def ^:dynamic *handler* (fn [e] (slingshot/throw+ e)))

(def resumable ::resumable)
(def abort-type ::abort)
(def ^:dynamic *abort-depth* 0)

(defn resume
  "Resume processing at the site of the rthrow or rtry invocation with
   value v. Should be called from a catch clause in handle."
  [v]
  {:value v :type resumable})

(defn abort
  "Return the value v from the site of the handle invocation. Should
   be called from a catch clause in handle. \"abort\" causes a return
   from the handle invocation in which it is located, allowing handle
   forms to emulate ordinary try/catch forms."
  [v]
  (slingshot/throw+ {:value v :depth *abort-depth* :type abort-type}))

(def ^{:doc "(catch selector destruct & body)

Catch a resumable exception. The thrown object is matched against
selector (using rules similar to those used by slingshot's catch).

catch has no meaning outside a handle expression (q.v.)."}
  catch (Object.))

(def ^{:doc "(resume-with selector destruct & body)

A resume-with clause in an rtry or rthrow body attempts to match the
value passed to an invocation of resume from a catch clause in handle.
If a match is found (using rules similar to those used by slingshot's
catch), the value is destructed using destruct and the result of
evaluating body within the lexical context thus established is the
result of the rtry or rthrow clause.

resume-with has no meaning outside rtry or rthrow expressions (qq.v.)."}
  resume-with (Object.))

(def ^{:doc "When referred to in a selector in a catch or resume-with
clause, % will be bound to the object being matched.

Note that the mere occurrence of the symbol % does not trigger this!
These are different:

    (require '[conditions.core :as c :refer [%]])

    ;; % refers to conditions.core/%, so the catch clause will match
    ;; the thrown exception.
    (c/handle (c/rthrow {}) (c/catch (map? %) _ (c/resume-with 5)))

    ;; % is shadowed by the let. This will trigger an exception,
    ;; because (map? %) will be interpreted as a function, but its
    ;; actual value is true:
    (let [% {}] (c/handle (c/rthrow {}) (c/catch (map? %) _ (c/resume-with 5))))

    ;; once again, % refers to conditions.core/%.
    (let [% {}] (c/handle (c/rthrow {}) (c/catch (map? c/%) _ (c/resume-with 5))))"}
  % (Object.))

(defn- is-not-resume? [env] #(s/uninteresting-clause? env % #'resume-with))
(defn- is-not-catch? [env] #(s/uninteresting-clause? env % #'catch))

(defmacro handle
  "Run exprs, with the possibility of resuming exceptions thrown by
   rthrow or rtry expressions in their dynamic extent.

   catch-clause => (catch selector destruct & body)

   catch clauses in a handle expression match exceptions in a manner
   similar to slingshot's catch, i.e. by instance checks against
   classes, map value checks against key-value vectors, or according
   to arbitrary predicates. If a match is found for a clause the
   associated exception value is destructed using the pattern in
   destruct and body is evaluated in the resulting context.

   If in the evaluation of exprs an exception is thrown in an rtry or
   rthrow expression, and it matches one of the catch clauses, then
   one of four things may occur in the body of the matched clause:

    - If the last expression evaluated is a call to resume, evaluation
      will continue at the point the exception was raised with an
      attempt to match the argument to resume with a resume-with
      clause. If a matching resume-with clause is found, the result of
      the the rthrow or rtry expression will be the result of
      evaluating the body of the resume-with clause.

    - If the last expression evaluated is a call to abort, evaluation
      unwinds to the present call to handle (i.e. bypassing all
      dynamically nested calls to handle), whose result is the value
      passed to abort. This emulates traditional try/catch behavior.

    - If the last expression evaluated raises an exception, that
      exception is raised from the site of the rtry or rthrow
      expression. Note that it is *not* a resumable exception and will
      not be caught by other handle expressions.

    - Otherwise, the last expression evaluated is the value of the
      rtry or rthrow expression."
  {:arglists '([exprs* catch-exprs*])} 
  [& forms]
  (let [[real-forms catch-forms] (split-with (is-not-catch? &env) forms)
        exn (gensym "exn-")
        old-handler (gensym "old-handler")
        old-depth (gensym "old-depth-")
        depth (gensym "depth-")]
    (assert (every? (complement (is-not-catch? &env)) catch-forms)
            "Only catch clauses can follow catch clauses in a handle expression")
    `(let [~old-handler *handler*
           ~old-depth *abort-depth*
           ~depth (inc ~old-depth)]
       (binding [*abort-depth* ~depth
                 *handler* (fn [~exn]
                             ~(s/exn-match-form
                               &env #'%
                               exn
                               catch-forms
                               `(binding [*abort-depth* ~old-depth]
                                  (~old-handler ~exn))))]
         (slingshot/try+
          (do ~@real-forms)
          (catch (and (= (:type ~'%) abort-type)
                      (= (:depth ~'%) ~depth)) {value# :value} value#))))))

(defmacro rthrow
  "Throw a resumable exception.

   resume-with-clause => (resume-with selector destruct & body),
   similar to the catch-clause of handle.

   If a dynamically enclosing handle expression catches exn, execution
   will resume at the site of the rthrow expression in three cases:

    - If the handle expression calls resume and the value passed
      matches one of the resume-with clauses, the result of the rthrow
      expression will be the result of evaluating the body of the
      relevant resume-with clause.

      If the value passed to resume does not match any resume-with
      clause, it bubbles up and can be handled by enclosing rtry
      blocks.

    - If the handle expression raises an exception, the exception will
      be raised from the site of the rthrow expression. Note that this
      will *not* be a resumable exception and will not be caught by
      handle expressions.

    - If the handle expression returns any other value (i.e. does not
      call resume or abort, or raise an exception), that value will be
      the result of the rthrow expression."
  [exn & resume-with-clauses]
  (assert (every? (complement (is-not-resume? &env)) resume-with-clauses)
          "Only resume-with clauses can follow the exception in rthrow")
  (let [result (gensym "result-")]
    `(do
       (let [~result (*handler* ~exn)]
           (if (= (:type ~result) resumable)
             ~(s/exn-match-form &env #'% `(:value ~result) resume-with-clauses
                                `(slingshot/throw+ ~result))
             ~result)))))

(defmacro rtry
  "Evaluates exprs, catching any non-resumable exceptions thrown and
   throws them using rthrow (q.v.)."
  {:arglists '([exprs* resume-with-clauses*])}
  [& forms]
  (let [[real-forms resume-forms] (split-with (is-not-resume? &env) forms)
        exn (gensym "exn-")
        unwrapped (gensym "unwrapped-")]
    (assert (every? (complement (is-not-resume? &env)) resume-forms)
            "Only resume-with clauses can follow resume-with clauses in rtry")
    `(try (do ~@real-forms)
          (catch Exception ~exn
            (let [~unwrapped ~(s/maybe-unwrap exn)]
              ;; if this is a resumption value, assume that an interior
              ;; rtry failed to handle it and that we should try to
              ;; handle it here. Don't call *handler* b/c resumption
              ;; values aren't exceptions and shouldn't be handled by
              ;; catches in a handle form anyway.
              (if (= (:type ~unwrapped) resumable)
                ~(s/exn-match-form &env #'% `(:value ~unwrapped)
                                   resume-forms
                                   `(slingshot/throw+ ~unwrapped))
                (rthrow ~exn ~@resume-forms)))))))
