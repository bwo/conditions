(ns conditions.core-test
  (:require [conditions.core :as c :refer [catch %]]
            [slingshot.slingshot :as slingshot])
  (:use expectations))

(defn parse-and-divide [n]
  (c/rtry (/ 10 (Integer/parseInt n))
        (c/resume-with map? {:keys [value]} value)))

(defn inner [n]
  (c/handle (parse-and-divide n)
            (c/catch ArithmeticException _ (c/resume {:value -1}))))

(defn outer [n]
  (c/handle (inner n)
            (c/catch ArithmeticException _ (c/resume {:value -2}))
            (c/catch ClassCastException _ (c/resume {:value -3}))))

(defn early [n]
  (c/handle (inner n)
            (c/catch ClassCastException _ "early")))

;; what isn't handled by inner propagates to outer
(expect -3 (outer 1))
;; inner takes precedence
(expect -1 (outer "0"))
(expect -1 (inner "0"))
;; unhandled exceptions propagate
(expect ClassCastException (inner 1))
(expect NumberFormatException (outer "a"))

(expect "early" (early 1))

;; normal operation is uninterfered with
(expect 10 (outer "1"))
(expect 10 (outer "1"))
(expect 10 (early "1"))

(expect ArithmeticException (parse-and-divide "0"))

(defn sling-throw [n]
  (c/rtry (slingshot/throw+ {:type :exn :value n})
          (c/resume-with [:type :resume] {:keys [value]} value)))

(defn sling-throw+ [n]
  (c/rthrow {:type :exn :value n}
            (c/resume-with [:type :resume] {:keys [value]} value)))

(defn sling-handle [n f]
  (c/handle (f n)
            (c/catch [:type :exn] {:keys [value]} (c/resume {:type :resume :value (* 2 value)}))))

(expect 20 (sling-handle 10 sling-throw))
(expect 20 (sling-handle 10 sling-throw+))

(expect clojure.lang.ExceptionInfo (sling-throw+ 10))

(def passed-through (atom []))

(defn abort-on-arith [n]
  (c/handle (parse-and-divide n)
            (c/catch ArithmeticException _ (c/abort -1)))
  (swap! passed-through conj :abort-on-arith))

(defn abort-on-class [n]
  (c/handle (abort-on-arith n)
            (c/catch ClassCastException _ (c/abort -2)))
  (swap! passed-through conj :abort-on-class))

(defn abort-on-format [n]
  (c/handle (abort-on-class n)
            (c/catch NumberFormatException _ (c/abort -3)))
  (swap! passed-through conj :abort-on-format))

;; "abort" jumps back to the correct handler
(expect [:abort-on-format]
        (do (reset! passed-through [])
            (abort-on-format "a")
            @passed-through))

(expect [:abort-on-class :abort-on-format]
        (do (reset! passed-through [])
            (abort-on-format 1)
            @passed-through))

(expect [:abort-on-arith :abort-on-class :abort-on-format]
        (do (reset! passed-through [])
            (abort-on-format "0")
            @passed-through))

(defn perverse [n which]
  (c/handle (if which (perverse n false) (parse-and-divide n))
            (c/catch (if which
                       (instance? ArithmeticException c/%)
                       (instance? ClassCastException c/%))
                e
              (c/abort 1)))
  (swap! passed-through conj which))

(expect [true] (do (reset! passed-through [])
                   (perverse "0" true)
                   @passed-through))


(defn throws-exn [n]
  (c/handle (parse-and-divide n)
            (c/catch ClassCastException _ (throw (ArithmeticException.)))))

(defn calls-throws [n]
  (c/handle (throws-exn n)
            (c/catch ArithmeticException _ (c/resume {:value 5}))))

(expect ArithmeticException (throws-exn 0))
;; it's not thrown as resumable, so ...
(expect ArithmeticException (calls-throws 0))

;; same handler works at different depths
(defn parse [n]
  (c/rtry (Integer/parseInt n)
          (c/resume-with map? {:keys [value]}  value)))

(defn divide [n]
  (c/rtry (/ 10 (parse n))
          (c/resume-with map? {:keys [value]} value)))

(defn handle-both [n]
  (c/handle (divide n)
            (c/catch ClassCastException _ (c/resume {:value 0}))
            (c/catch ArithmeticException _ (c/resume {:value -1}))))

(expect -1 (handle-both 0))

;; resumable exceptions bubble up through nested rtrys

(defn parse-1 [n]
  (c/rtry (Integer/parseInt n)
         (c/resume-with vector? v (first v))))

(defn divide-1 [n]
  (c/rtry (/ 10 (parse-1 n))
          (c/resume-with map? {:keys [value]} value)))

(defn handle-both-1 [n]
  (c/handle (divide-1 n)
            (c/catch ClassCastException _ (c/resume {:value n}))))

;; the resumption isn't a vector, so it gets caught by the resume-with
;; form in divide-1.
(expect 0 (handle-both-1 0))

;; Since an exception thrown in a handle expression's c/catch clause
;; appears to come from the location of the resumption, it can be
;; caught by an exception handler in a function called by the handle
;; expression itself!

(defn catcher [n]
  (try (parse-and-divide n)
       (catch ArithmeticException e
         n)))

(defn spaghetti [n]
  (c/handle (catcher n)
            (c/catch ClassCastException _ (throw (ArithmeticException.)))))

(expect 0 (spaghetti 0))
(expect "0" (spaghetti "0"))



;;;; an adaptation of the example at
;;;; http://c2.com/cgi/wiki?CommonLispConditionSystem

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

(defn continuer []
  (c/handle (determine-infinity)
              (c/catch [:type :zerodivisionerror] _ (c/resume {:type :continue}))))

(defn restarter [n]
  (c/handle (determine-infinity)
            (c/catch [:type :zerodivisionerror] _ (c/resume {:type :recalc-with :value n}))))

(defn return-zeroer-1 []
  (c/handle (determine-infinity)
            (c/catch [:type :zerodivisionerror] _ (c/resume {:type :return-zero}))))

;; shouldn't print
(defn return-zeroer-2 []
  (c/handle (determine-infinity)
            (c/catch [:type :zerodivisionerror] _ (c/abort 0))))

(defn return-zeroer-3 []
  (c/handle (determine-infinity)
            (c/catch [:type :zerodivisionerror] _ 0)))

(defn return-valuer [n]
  (c/handle (determine-infinity)
            (c/catch [:type :zerodivisionerror] _ (c/resume {:type :return-value :value (+ n 2)}))))

(defn multiple-restarts []
  (c/handle (determine-infinity)
            (c/catch [:type :zerodivisionerror] _ (c/resume {:type :recalc-with :value ""}))
            (c/catch ClassCastException _ (c/resume {:type :recalc-wit :value 6}))))

(expect 6 (return-valuer 4))
(expect -1 (continuer))
(expect 0 (return-zeroer-1))
(expect 0 (do (println "nothing should be printed between this line ...")
              (let [r (return-zeroer-2)]
                (println "... and this line")
                r)))
(expect 0 (return-zeroer-3))
(expect 1/2 (restarter 2))
(expect 1/6 (multiple-restarts))


;; we :refer catch, so this is kosher.
(expect 4 (c/handle (determine-infinity)
                    (catch [:type :zerodivisionerror] _ (c/resume {:type :return-value
                                                                   :value 4}))))

;; but this isn't, because the let shadows the catch. we get an
;; exception because "catch" in the catch expression doesn't refer to
;; conditions.core/catch, meaning that expression is considered part
;; of the body, so handle attempts to evaluate it---meaning _ is
;; treated not as a binding form, but as a *use* of the name. Which
;; isn't bound, so ...
;; (for that matter, 5 isn't a function!)
(expect RuntimeException
        (eval '(let [catch 5]
                 (c/handle (determine-infinity)
                           (catch [:type :zerodivisionerror] _ (c/resume {:type :return-value
                                                                          :value 4}))))))

(defn refer-1 []
  ;; handler expands to (map? exn-...)
  (c/handle (c/rthrow {}) (catch (map? %) _ :ok)))

(defn refer-2 []
  ;; handler expands to ((fn [%] (map? %)) exn-...)
  ;; we prove this by *not* catching the exception (if the handler
  ;; were incorrectly expanded to (fn [exn-...] (map? exn-...)), that
  ;; would be truthy, and we'd catch it.
  (c/handle (c/rthrow []) (catch (fn [%] (map? %)) _ :ok)))

(defn refer-fail-1 []
  ;; this fails because we find the reference to c/% and try to invoke
  ;; the passed value, (), as a fn
  (let [s (fn [x] (integer? x))]
    (c/handle (c/rthrow ()) (catch ((fn [] (% 1))) _ :ok))))

(defn refer-fail-2 []
  ;; this fails because we try to invoke a boolean as a function.
  (let [% (fn [x] (integer? x))]
    (c/handle (c/rthrow ()) (catch ((fn [] (% 1))) _ :ok))))

(defn refer-3 []
  ;; this succeeds because we detect that % is already bound, and when
  ;; that's invoked, it returns a function.
  (let [% (fn [x] (constantly (integer? x)))]
    (c/handle (c/rthrow ()) (catch ((fn [] (% 1))) _ :ok))))


(expect :ok (refer-1))
(expect clojure.lang.ExceptionInfo (refer-2))
(expect :ok (refer-3))
(expect ClassCastException (refer-fail-1))
(expect ClassCastException (refer-fail-2))
