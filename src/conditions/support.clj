(ns conditions.support
  (:require [slingshot.slingshot :as slingshot]
            [slingshot.support :as s]))

(defn- contains-symbol? [f sym]
  (cond
   (and (symbol? f) (= f sym)) true
   (sequential? f) (some #(contains-symbol? % sym) (flatten f))
   (map? f) (recur (seq f) sym)
   :else false))

(defn- exn-pred [matcher exn-symbol]
  (cond
   (and (symbol? matcher) (class? (resolve matcher))) `(instance? ~matcher ~exn-symbol)
   (vector? matcher) (if (even? (count matcher))
                       (let [m (gensym "m-")]
                         `(let [~m  ~exn-symbol]
                            (and (map? ~m)
                                 ~@(map (fn [[k v]] `(= ~v (get ~m ~k)))
                                        (partition 2 matcher)))))
                       (throw (Exception.
                               (format "Bad format: %s is not in format [key value & kvs]"
                                       matcher))))
   (contains-symbol? matcher '%) `(let [~'% ~exn-symbol] ~matcher)
   ;; optimistically assume that this is a fn, even though it might not be
   (or (seq? matcher)
       (symbol? matcher))  `(~matcher ~exn-symbol)
   :else (throw (Exception. (format "Do not know how to create a selector from %s" matcher)))))

(defn exn-match-clause [catch-form exn-symbol]
  (let [[matcher binding & body] (rest catch-form)]
    [(exn-pred matcher exn-symbol) `(let [~binding ~exn-symbol] ~@body)]))

(defn maybe-unwrap [exn-symbol]
  `(if-let [unwrapped# (s/unwrap ~exn-symbol)]
     (:object unwrapped#)
     ~exn-symbol))

(defn exn-match-form [exn catch-forms else-form]
  (let [exn-symbol (gensym "exn-")]
    `(let [~exn-symbol ~exn
           ~exn-symbol ~(maybe-unwrap exn-symbol)]
       (cond ~@(mapcat #(exn-match-clause % exn-symbol) catch-forms)
             :else ~else-form))))

(defn uninteresting-clause? [f v]
  (or (not (seq? f))
      (not (symbol? (first f)))
      (not= v (resolve (first f)))))
