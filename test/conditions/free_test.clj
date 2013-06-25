(ns conditions.free-test
  (:require [conditions.free :as f])
  (:use expectations))

(defmacro aif [test then else]
  (let [it (first (filter #(not (contains? &env %))
                          (cons 'it (map #(symbol (str "it-" %)) (iterate inc 1)))))]
    `(let [~it ~test]
       (if ~it ~then ~else))))

(expect [{:y 3} 3] (aif (get {:x {:y 3}} :x)
                        (aif (get it :y)
                             [it it-1]
                             it)
                        nil))

;; friggin' expectations!
(let [r (f/macroexpand-all '(aif (get {:x {:y 3}} :x)
                                 (aif (get it :y)
                                      [it it-1]
                                      it)
                                 nil))]
  (expect '(let* [it (get {:x {:y 3}} :x)]
                 (if it (let* [it-1 (get it :y)]
                              (if it-1 [it it-1] it))
                     nil))
          r))
;; Compare:
;; conditions.free-test> (clojure.pprint/pprint (clojure.walk/macroexpand-all
;;                                               '(aif (get {:x {:y 3}} :x)
;;                                                     (aif (get it :y)
;;                                                          [it it-1]
;;                                                          it)
;;                                                     nil)))
;; (let*
;;  [it (get {:x {:y 3}} :x)]
;;  (if it (let* [it (get it :y)] (if it [it it-1] it)) nil))
;;                ^^ !                ^^ !

(expect '#{z}
        (f/free-in-form '(try (let [x (fn [y] y)]
                                (x z))
                              (catch Exception e e))))

(expect '#{z e}
        (f/free-in-form '(try (let [x (fn [y] y)]
                                (x z))
                              (catch Exception a e))))

(expect '#{z y}
        (f/free-in-form '(try (let [x (fn f [y] (f y))]
                                (x z y))
                              (catch Exception e e))))

(expect '#{z y f}
        (f/free-in-form '(try (let [x (fn g [y] (f y))]
                                (x z y))
                              (catch Exception e e))))

(expect '#{z y f baz}
        (f/free-in-form '(try (let [x (fn g [y] (f y))]
                                (letfn [(bar [baz] (foo baz))
                                        (foo [quux] (bar baz quux))]
                                  (foo z y)))
                              (catch Exception e e))))

(expect '#{that clojure.core/deref c first range rest}
 (f/free-in-form '(let [x (reify IDeref (deref [this] that))]
                    (if @x
                      (throw (loop [a (range 10)]
                               (when-let [af (first a)]
                                 (ArithmeticException.))
                               (recur (rest a))))
                      (do (var expect)
                          (new Exception c))))))

(expect '#{try z fn*}
        (f/free-in-form '(try (let [x (fn [y] y)]
                                (x z try fn*))
                              (catch Exception e e))))

;; note that the above is, strangely, correct:
(expect [1 2 3] (let [z 1
                      try 2
                      fn* 3]
                  (try (let [x (fn* ([a b c] [a b c]))]
                         (x z try fn*))
                       (catch Exception e e))))


(expect '(. kk (startsWith (:x {:x zz})))
        (f/map-free-in-form (fn [s] (symbol (str s s)))
                            '(. k (startsWith (:x {:x z})))))

(expect '(let* [x SystemSystem] (. System (startsWith (:x {:x zz}))))
        (f/map-free-in-form (fn [s] (symbol (str s s)))
                            '(let [x System] (. System (startsWith (:x {:x z}))))))
