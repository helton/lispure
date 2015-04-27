(ns lispure.core
  (:require [lispure.utils :refer :all]
            [clojure.algo.monads :refer :all])) ; Yeah, I'm gonna use it. You'll see.

; ----------------------------------------------
; Environment
; ----------------------------------------------

(def standard-environment 
  (transient {
    '+ +
    '- -
    '* *
    '/ /
    '= =
    '> >
    '< <
    '>= >=
    '<= <=
    'not not
    'car first ;test it!
    'cdr rest  ;test it!
  }))

(defn lispure-evaluate 
  ([expression]
    (lispure-evaluate expression standard-environment))
   
  ([expression environment]
    (defn read-form [expression]
      (let [operator (first expression)
            arg-list (rest expression)]
        {:operator operator
         :arg-list arg-list}))

    (defn get-operator [expression]
      (:operator (read-form expression)))

    (defn get-arg-list [expression]
      (:arg-list (read-form expression)))
   
      (def lispure-number? number?)

    (defn lispure-boolean? [expression]
      (in? expression ['true 'false]))

    (def lispure-symbol? symbol?)

    (defn lispure-is? [sym]
      (fn [expression]
        (= (get-operator expression) sym)))
     
    (def lispure-quote? (lispure-is? 'quote))

    (def lispure-define? (lispure-is? 'define))

    (def lispure-if? (lispure-is? 'if))
   
    (def lispure-lambda? (lispure-is? 'lambda))
   
    (defn evaluate-number [expression] expression)

    (defn evaluate-boolean [expression] (= expression 'true))

    (defn evaluate-symbol [expression] 
      (expression environment))

    (defn evaluate-quote [expression]
      (first (get-arg-list expression)))

    (defn evaluate-define [expression]
      (let [key   (first  (get-arg-list expression))
            value (second (get-arg-list expression))]
        (assoc! environment key value) ; Really? What's the FP? Come on, dude :D
        nil))
   
    (defn evaluate-if [expression]
      (let [condition   (first  (get-arg-list expression))
            consequence (second (get-arg-list expression))
            alternative (third  (get-arg-list expression))]
        (if (lispure-evaluate condition environment)
          (lispure-evaluate consequence environment)
          (lispure-evaluate alternative environment))))  
    
    ;(defn evaluate-lambda [expression]
    ;  (let [args (first  (get-arg-list expression))
    ;        body (second (get-arg-list expression))]
    ;    (fn [args])))
   
    (cond
      (lispure-number?  expression) (evaluate-number  expression)
      (lispure-boolean? expression) (evaluate-boolean expression)
      (lispure-symbol?  expression) (evaluate-symbol  expression)
      (lispure-quote?   expression) (evaluate-quote   expression)
      (lispure-define?  expression) (evaluate-define  expression)
      (lispure-if?      expression) (evaluate-if      expression)
      ;(lispure-lambda?  expression) (evaluate-lambda  expression)
      :else 
        (let [function ((get-operator expression) environment)
              arg-list (get-arg-list expression)]
          (apply function arg-list)))))