#lang typed/racket
;
(require "types.rkt")
;(require "helper-functions.rkt")
(provide (all-defined-out))
;
;
;; Variant extractor function -------------------------------------------------------------------------------------------------------------------------------------------------------
;
;(: extract-value-variant (Value -> (U Number String)))
;(define extract-value-variant
;  (lambda (val)
;    (cond ((S? val) (S-s val))
;          ((N? val) (N-n val))
;          (else (error "extract-value-variant: not a value: "val)))))
;
;; Comparison functions for values --------------------------------------------------------------------------------------------------------------------------------------------------
;
;;(: value=? (Value Value -> Boolean))
;;(define value=?
;;  (lambda (v1 v2)
;;    (match v1
;;      ((struct S (s1)) (match v2
;;                         ((struct S (s2)) (string=? s1 s2))
;;                         ((struct N (n)) (error "value=?: cannot compare string to Real: " s1 n))))
;;      ((struct N (n1)) (match v2
;;                         ((struct N (n2)) (= n1 n2))
;;                         ((struct S (s)) (error "value=?: cannot compare Real to string: " n1 s)))))))
;
(: value>? (Value Value -> Boolean))
(define value>?
  (lambda (v1 v2)
     (cond ((and (real? v1) (real? v2)) (> v1 v2))
          ((or (real? v1) (real? v2)) (error "value>?: cannot compare a number to a not-number: " v1 v2))
          ((and (string? v1) (string? v2)) (string>? v1 v2))
          ((or (string? v1) (string? v2)) (error "value>?: cannot compare a string to a not-string: " v1 v2)))))

(: value<=? (Value Value -> Boolean))
(define value<=?
  (lambda (v1 v2)
    (not (value>? v1 v2))))


(: value<? (Value Value -> Boolean))
(define value<?
  (lambda (v1 v2)
    (cond ((and (real? v1) (real? v2)) (< v1 v2))
          ((or (real? v1) (real? v2)) (error "value<?: cannot compare a number to a not-number: " v1 v2))
          ((and (string? v1) (string? v2)) (string<? v1 v2))
          ((or (string? v1) (string? v2)) (error "value<?: cannot compare a string to a not-string: " v1 v2)))))

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

;(: get-arith-function (Arith-Op -> (Value Value -> Value))) ; returns the numeric function that performs the requested operation for an arithmetic operator
;(define get-arith-function
;  (lambda (op)
;    (cond ((eq? op _+) add)
;          ((eq? op _-) sub)
;          ((eq? op _*) mul)
;          ((eq? op _/) div)
;          (else (error "get-arith-function: Operator not known: " op)))))
;
;(: get-op-function (Operator -> (U (Value Value -> Value)(Value Value -> Boolean)))) ; delegates to the specialized functions to get the operation for an arithmetic or boolean operator
;(define get-op-function
;  (lambda (op)
;    (cond ((Arith-Op? op) (get-arith-function op))
;          ((Bool-Op? op) (get-comp-function op)))))
;
;
; Functions to evaluate and get the result type of evaluated operands --------------------------------------------------------------------------------------------------------------

(: value->real (Value -> (Option Real)))
(define (value->real value)
  (if (real? value)
      value
      #f))

(: value->string (Value -> (Option String)))
(define (value->string value)
  (if (string? value)
      value
      #f))


(: get-type (Operand -> (U (Any -> Boolean : Real) (Any -> Boolean : String)))) ; returns the resulting type of operand evaluation
(define get-type
  (lambda (o)
    (match o
      ((struct Val (val))
       (cond ((string? val) string?)
       ((real? val) real?)))
      ((struct Att (att)) (Attribute-type att))
      ((struct App (f o1 o2)) (cond ((or (eq? f substring) (eq? f string-append)) string?)
                                    ((or (eq? f +) (eq? f -) (eq? f /) (eq? f *)) real?)
                                    (else (error "get-type: not a defined function: " f))))
      (_ (error "get-type: Not an operand: " o)))))

