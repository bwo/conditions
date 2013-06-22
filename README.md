# conditions

Simple resumable exceptions for clojure. Based loosely on [this ML implementation](http://okmij.org/ftp/ML/resumable.ml).

Resumable exceptions, aka
[conditions](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html),
allow code raising an exception to continue at the point the exception
was raised, according to decisions made by condition handlers. For
instance, in the following example (adapted from
[c2](http://c2.com/cgi/wiki?CommonLispConditionSystem),
`reciprocal-of` throws an error if the divisor passed is zero, and
provides for various responses: returning zero, returning an arbitrary
value, and trying again with a new value. Similarly,
`determine-infinity` allows handlers to instruct it to simply return
`-1` if it throws an error.

```clojure
(require '[conditions.core :as c])
(require '[slingshot.slingshot :as slingshot])

(defn reciprocal-of [v]
  (c/rtry (if (== v 0) (slingshot/throw+ {:type :zerodivisionerror}) (/ 1 v))
          (c/resume-with [:type :return-zero] _ 0)
          (c/resume-with [:type :return-value] {v :value} v)
          (c/resume-with [:type :recalc-with] {v :value} (reciprocal-of v))))

(defn determine-infinity []
  (c/rtry (let [result (reciprocal-of 0)]
            (println "Value:" result)
            result)
        (c/resume-with [:type :continue] _ -1)))
```

If `reciprocal-of` is called within the dynamic extent of a condition
handler, and throws an exception, the handler can determine what
response `reciprocal-of` should take to the error. The error is
handled where it's *raised*, according to a policy determined by where
and how the result would have been *used*. For instance:

```
(defn try-again-with [n]
  (c/handle (determine-infinity)
            (c/catch [:type :zerodivisionerror] _ (c/resume {:type :recalc-with :value n}))))
```

Evaluating `(try-again-with 2)` results in the value `1/2`.
(Evaluating `(try-again-with 0)` is an infinite loop: the handler is
still in place when `reciprocal-of` tries again.)

## Usage

Condition handlers are set using the `handle` macro; conditions can be
signalled explicitly using `rthrow` (for "resumable throw") or `rtry`
(for "resumable try").

### Handling resumable exceptions

A handler consists of several expressions (the body) followed by
several catch clauses describing the conditions the handler will
handle and what to do if they are signalled. The catch clauses are
specified identically to slingshot's catch clauses:

- `(catch ClassName c & body)` -> execute `body` in an implicit do
   with `c` bound to the signalled condition if it is an instance of
   ClassName
    
- `(catch [key value & kvs] destruct & body)` -> execute `body` in an
  implicit do with the names in `destruct` extracted from the
  signalled condition if the condition is a map `m` matching (and (=
  value (get m key)) ...)`. E.g. `(catch [:type :exn] {:keys [value]}
  (* value 2))`.

- `(catch [expression referring to conditions.core/%] destruct &
  body)` -> matches if the expression evaluates truthily when free
  occurrences of symbols that refer to conditions.core/% are
  substituted with the value of the signalled condition. (See the
  section on hygiene at the bottom of this readme for more.)

- `(catch seq-or-symbol destruct & body)` -> matches if the seq or
  symbol, which is assumed to evaluate to a function, returns truthy
  when applied to the condition.

One of four actions may be taken in the body of a handler's catch
clause:

- If the last expression evaluated is a call to resume, evaluation
  will continue at the point the condition was raised with an attempt
  to match the argument to resume with a resume-with clause. If a
  matching resume-with clause is found, the result of the the rthrow
  or rtry expression will be the result of evaluating the body of the
  resume-with clause.

- If the last expression evaluated is a call to abort, evaluation
  unwinds to the present call to handle (i.e. bypassing all
  dynamically nested calls to handle), whose result is the value
  passed to abort. This emulates traditional try/catch behavior.

- If the last expression evaluated raises an exception, that exception
  is raised from the site of the rtry or rthrow expression. Note that
  it is *not* a resumable exception and will not be caught by other
  handle expressions. (But it can be caught by ordinary exception
  handlers.)

- Otherwise, the last expression evaluated is the value of the
  rtry or rthrow expression.

### Throwing and resuming resumable exceptions

`rthrow` takes as its first argument an exception to throw, and
`resume-with` clauses specifying the resumptions it recognizes and the
actions it will take on receiving them as its remaining arguments.
`resume-with` clauses are identical to `handle`'s catch clauses,
except that instead of beginning with `catch` they begin with
`resume-with`. If a `resume-with` clause matches a value that `resume`
was called with in a handler body, the `resume-with` clause's body is
evaluated and its result is the result of the `rthrow` clause. If the
resumption value doesn't match any `resume-with` clause, it proceeds
up the stack until one is found (in an `rtry` expression) that does.

`rtry` is similar to `rthrow` except that it takes any number of
arbitrary expressions as its first arguments, and catches any
exceptions they raise, handling them as if they were thrown as
resumable exceptions with `rthrow`. This allows for recovery from code
that doesn't know about resumable exceptions (note that
`reciprocal-of` throws its exception using slingshot's `throw+`).

### Caveat about control flow

While resumable exceptions don't lead to as many possibilities for
opaque control flow as full-on continuations do, any construct in
which functions that affect control flow are executed in a dynamic
context separate from their textual definition has the potential to be
moderately confusing. Consider, for instance, these definitions:

```clojure
(defn parse-and-divide [n]
  (c/rtry (/ 10 (Integer/parseInt n))
        (c/resume-with map? {:keys [value]} value)))

(defn catcher [n]
  (try (parse-and-divide n)
       (catch ArithmeticException e
         n)))

(defn handler [n]
  (c/handle (catcher n)
            (c/catch ClassCastException _ (throw (ArithmeticException.)))))
```

Evaluating `(handler "5")` results in `2`, with normal control flow:
the parsing and the division proceed normally and everything returns
to its caller in an orderly fashion.

Evaluating `(handler 2)` also results in `2`, but rather more
circuitously:

    - `(handler 2)` calls `(catcher 2)`
    - which calls `(parse-and-divide 2)`
    - which calls `(Integer/parseInt 2)`
    - which raises a ClassCastException
    - which matches the handler defined in `handler`
    - which throws an `ArithmeticException`
    - which is caught in `catcher`
    - which returns 2 to `handler`
    - which returns 2.

This is not really surprising when you consider that the point of the
condition handlers is to be executed in the dynamic context in which
the exception is thrown (and of course you don't need resumable
exceptions for this, just first-class functions and exceptions). But
it is a bit counterintuitive to see a `throw` expression in a function
`foo` which results in an expression being caught in a function called
by foo.

### A note about hygiene

The selector syntax and semantics in `catch` and `resume-with` clauses
in this library are based on slingshot's, and the use of `catch` in a
`handle` expression is derived from Clojure's regular `try`. However,
this library differs from slingshot's selectors (and its `try+`) and
Clojure's `try` in attempting to be as hygienic as possible. In a
regular old `try` expression the *bare symbol* `catch` is enough to
signal a catch clause, so that the following two expressions have the
same semantics:

```clojure
;; this:
(try foo
  (catch Exception e bar))
;; has the same semantics as this:
(let [e 1
      bar 2
      catch (fn [a b c] a)]
  (try foo
    (catch Exception e bar)))
```

Even though one might have expected the second to just evaluate to
`java.lang.Exception` (or to throw an unhandled exception in
evaluating `foo`). Similarly, in slingshot's selectors, the bare
presence of `%` is all that matters:

```clojure
(require '[slingshot.slingshot :as s])

;; (a) the selector here expands to (let [% <the exn>] (map? %))
(s/try+ (s/throw+ {}) (catch (map? %) _ 4))

;; (b) the selector here expands to (let [% <the exn>] (fn [%] (map? %)))
(s/try+ (s/throw+ {}) (catch (fn [%] (map? %)) _ 4))

;; (c) whereas the selector *here* expands to (let [% <the exn>] ((fn [a] (map? a)) %))
(s/try+ (s/throw+ {}) (catch (fn [a] (map? a)) _ 4))

;; (d) the selector here expands to (let [% <the exn>] (fn [x] (= (% x) "100%")))
(let [% (fn [x] (str x "%"))] 
  (s/try+ (s/throw+ "100%") (catch (fn [x] (= (% x) "100%")) _ 4)))

;; (e) whereas the selector here expands to (let [% <the exn>] ((fn [x] (= (perc x) "100%")) %))
(let [perc (fn [x] (str x "%"))]
  (s/try+ (s/throw+ "100%") (catch (fn [x] (= (perc x) "100%")) _ 4))
```

In (c), the symbol `%` is not free in the selector; in (e), it is free
in the selector expression, but has a value in the environment already.

To avoid this kind of thing, this library requires you to actually refer to the `catch`, `resume-with`, and `%` that it provides:

```clojure
(require '[conditions.core :as c :refer [%]])

;; % refers to conditions.core/%, so the catch clause will match
;; the thrown exception.
;; the expansion will be something like (let [G_123 <the exn>] (map? G_123))
(c/handle (c/rthrow {}) (c/catch (map? %) _ (c/resume-with 5)))

;; % is not free in the selector, so it is treated as a function and applied to the exn
;; the expansion will be something like (let [G_123 <the exn>] ((fn [%] (map? %)) G_123))
(c/handle (c/rthrow {}) (c/catch (fn [%] (map? %)) _ (c/resume-with 5)))

;; % is shadowed by the let. 
;; The expansion will be something like (let [G_123 <the exn>] ((map? %) G_123))
;; Consequently this one will trigger an exception, since (map? {}) is not a function!
(let [% {}] (c/handle (c/rthrow {}) (c/catch (map? %) _ (c/resume-with 5))))

;; once again, % refers to conditions.core/%, so this will work the same way as the first example.
(let [% {}] (c/handle (c/rthrow {}) (c/catch (map? c/%) _ (c/resume-with 5))))
```

## License

Copyright © 2013 Ben Wolfson (wolfson at gmail.com)

Distributed under the Eclipse Public License, the same as Clojure.
