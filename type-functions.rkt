#lang typed/racket

(require "types.rkt")
(require "helper-functions.rkt")
(provide (all-defined-out))


; Variant extractor function -------------------------------------------------------------------------------------------------------------------------------------------------------

(: extract-value-variant (Value -> (U Number String)))
(define extract-value-variant
  (lambda (val)
    (cond ((S? val) (S-s val))
          ((N? val) (N-n val))
          (else (error "extract-value-variant: not a value: "val)))))

; Comparison functions for values --------------------------------------------------------------------------------------------------------------------------------------------------

;(: value=? (Value Value -> Boolean))
;(define value=?
;  (lambda (v1 v2)
;    (match v1
;      ((struct S (s1)) (match v2
;                         ((struct S (s2)) (string=? s1 s2))
;                         ((struct N (n)) (error "value=?: cannot compare string to Real: " s1 n))))
;      ((struct N (n1)) (match v2
;                         ((struct N (n2)) (= n1 n2))
;                         ((struct S (s)) (error "value=?: cannot compare Real to string: " n1 s)))))))

(: value>? (Value Value -> Boolean))
(define value>?
  (lambda (v1 v2)
    (match v1
      ((struct S (s1)) (match v2
                         ((struct S (s2)) (string>? s1 s2))
                         ((struct N (n)) (error "value=?: cannot compare string to Real: " s1 n))))
      ((struct N (n1)) (match v2
                         ((struct N (n2)) (> n1 n2))
                         ((struct S (s)) (error "value=?: cannot compare Real to string: " n1 s)))))))

(: value<=? (Value Value -> Boolean))
(define value<=?
  (lambda (v1 v2)
    (not (value>? v1 v2))))


(: value<? (Value Value -> Boolean))
(define value<?
  (lambda (v1 v2)
    (match v1
      ((struct S (s1)) (match v2
                         ((struct S (s2)) (string<? s1 s2))
                         ((struct N (n)) (error "value=?: cannot compare string to Real: " s1 n))))
      ((struct N (n1)) (match v2
                         ((struct N (n2)) (< n1 n2))
                         ((struct S (s)) (error "value=?: cannot compare Real to string: " n1 s)))))))

(: value>=? (Value Value -> Boolean))
(define value>=?
  (lambda (v1 v2)
    (not (value<? v1 v2))))


; Functions to handle boolean and arithmetic operators -----------------------------------------------------------------------------------------------------------------------------

(: negate (Bool-Op -> Bool-Op)) ; returns the opposite of a boolean operator
(define negate
  (lambda (op)
    (cond ((eq? op eql) noteql)
          ((eq? op noteql) eql)
          ((eq? op greater) lesseql)
          ((eq? op lesseql) greater)
          ((eq? op greatereql) less)
          ((eq? op less) greatereql)
          (else (error "negate: boolean operator not known: " op)))))

(: get-comp-function (Bool-Op -> (Value Value -> Boolean))) ; returns the generic function to compare 2 values for a boolean operator
(define get-comp-function
  (lambda (op)
    (cond ((eq? op eql) equal?)
          ((eq? op noteql) (lambda: ((v1 : Value) (v2 : Value)) (not (equal? v1 v2))))
          ((eq? op greater) value>?)
          ((eq? op lesseql) value<=?)
          ((eq? op greatereql) value>=?)
          ((eq? op less) value<?)
          (else (error "get-comp-function: Operator not known: " op)))))

(: get-arith-function (Arith-Op -> (Value Value -> Value))) ; returns the numeric function that performs the requested operation for an arithmetic operator
(define get-arith-function
  (lambda (op)
    (cond ((eq? op _+) add)
          ((eq? op _-) sub)
          ((eq? op _*) mul)
          ((eq? op _/) div)
          (else (error "get-arith-function: Operator not known: " op)))))

(: get-op-function (Operator -> (U (Value Value -> Value)(Value Value -> Boolean)))) ; delegates to the specialized functions to get the operation for an arithmetic or boolean operator
(define get-op-function
  (lambda (op)
    (cond ((Arith-Op? op) (get-arith-function op))
          ((Bool-Op? op) (get-comp-function op)))))


; Functions to evaluate and get the result type of evaluated operands --------------------------------------------------------------------------------------------------------------

(: get-typename (Operand -> Symbol)) ; returns the resulting type of operand evaluation
(define get-typename
  (lambda (o)
    (match o
      ((struct Val (val)) (match val
                            ((struct N (n)) 'N)
                            ((struct S (s)) 'S)))
      ((struct Att (att)) (Attribute-typename att))
      ((struct AppOp (op o1 o2)) (cond ((Arith-Op? op) 'N)
                                       (else (error "get-typename: Not an operand on a basic value: " op))))
      ((struct AppFun (f o1 o2)) (match f
                                   ((struct Substr (p)) 'S)
                                   ((struct StrApp (p)) 'S)
                                   (else (error "get-typename: not a defined function: " f))))
      (_ (error "get-typename: Not a basic value: " o)))))


; Arithmetic functions -------------------------------------------------------------------------------------------------------------------------------------------------------------

(: add (Value Value -> Value))
(define add
  (lambda (x y)
    (match x
      ((struct N (n1)) (match y
                         ((struct N (n2)) (make-N (+ n1 n2)))
                         (_ (error "add: not a number value type: " y))))
      (_ (error "add: not a number value type: " x)))))
      

(: sub (Value Value -> Value))
(define sub
  (lambda (x y)
    (match x
      ((struct N (n1)) (match y
                         ((struct N (n2)) (make-N (- n1 n2)))
                         (_ (error "add: not a number value type: " y))))
      (_ (error "add: not a number value type: " x)))))

(: mul (Value Value -> Value))
(define mul
  (lambda (x y)
    (match x
      ((struct N (n1)) (match y
                         ((struct N (n2)) (make-N (* n1 n2)))
                         (_ (error "add: not a number value type: " y))))
      (_ (error "add: not a number value type: " x)))))

(: div (Value Value -> Value))
(define div
  (lambda (x y)
    (match x
      ((struct N (n1)) (match y
                         ((struct N (n2)) (make-N (/ n1 n2)))
                         (_ (error "add: not a number value type: " y))))
      (_ (error "add: not a number value type: " x)))))


; String functions -----------------------------------------------------------------------------------------------------------------------------------------------------------------

(define substr (make-Substr substring))
(define strapp (make-StrApp string-append))


; Aggregation functions -----------------------------------------------------------------------------------------------------------------------------------------------------------------

(define sum (make-Sum (lambda: ((l : (Listof Number))) (apply + l))))

