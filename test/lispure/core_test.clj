(ns lispure.core-test
  (:require [clojure.test :refer :all]
            [lispure.core :refer :all]))

(deftest test-evaluate-atomic-values-to-themselves
  (testing "Should evaluate integers"
    (is (= 123 (lispure-evaluate '123))))
  (testing "Should evaluate decimals"
    (is (= 3.14 (lispure-evaluate '3.14))))
  (testing "Should evaluate boolean true"
    (is (= true (lispure-evaluate 'true))))
  (testing "Should evaluate boolean false"
    (is (= false (lispure-evaluate 'false)))))

(deftest test-evaluate-function-calls
  (testing "Should function calls on standard environment [+]"
    (is (= 13 (lispure-evaluate '(+ 1 3 (+ 4 5))))))
  (testing "Should function calls on standard environment [-]"
    (is (= -2 (lispure-evaluate '(- 1 3)))))
  (testing "Should function calls on standard environment [*]"
    (is (= 3 (lispure-evaluate '(* 1 3)))))
  (testing "Should function calls on standard environment [/]"
    (is (= 1/3 (lispure-evaluate '(/ 1 3)))))
  (testing "Should function calls on standard environment [=]"
    (is (= false (lispure-evaluate '(= 1 3)))))
  (testing "Should function calls on standard environment [>]"
    (is (= false (lispure-evaluate '(> 1 3)))))
  (testing "Should function calls on standard environment [<]"
    (is (= true (lispure-evaluate '(< 1 3)))))
  (testing "Should function calls on standard environment [>=]"
    (is (= false (lispure-evaluate '(>= 1 3)))))
  (testing "Should function calls on standard environment [<=]"
    (is (= true (lispure-evaluate '(<= 1 3)))))
  (testing "Should function calls on standard environment [not]"
    (is (= false (lispure-evaluate '(not true)))))
  (testing "Should function calls on standard environment [not]"
    (is (= true (lispure-evaluate '(not false))))))

(deftest test-evaluate-quote
  (testing "Should evaluate quote"
    (is (= '123 (lispure-evaluate '(quote 123))))))

(deftest test-evaluate-define
  (testing "Should evaluate define"
    (let [environment   standard-environment
          define-result (lispure-evaluate '(define x (+ 2 (* 11 11))) environment)
          result        (lispure-evaluate 'x environment)]
    	(is (= nil define-result))
        (is (= 123 result)))))

;(deftest test-evaluate-lambda
;  (testing "Should evaluate lambda"
;    (let [environment   standard-environment
;          define-result (lispure-evaluate '(define my-sqr (lambda (x) (* x x)) 10) environment)
;          result        (lispure-evaluate '(my-sqr 11) environment)]
;        (is (= 121 result)))))

(deftest test-evaluate-if
  (testing "Should evaluate if consequence (true condition)"
    (is (= 'ok (lispure-evaluate '(if (< 1 2) (quote ok) (quote nok))))))
  (testing "Should evaluate if alternative (false condition)"
    (is (= 'nok (lispure-evaluate '(if (> 1 2) (quote ok) (quote nok)))))))