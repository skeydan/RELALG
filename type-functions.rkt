#lang typed/racket
;
(require "types.rkt")
(require "helper-functions.rkt")
(provide (all-defined-out))

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

(: value>? (Value Value -> Boolean))
(define value>?
  (lambda (v1 v2)
     (cond ((and (real? v1) (real? v2)) (> v1 v2))
          ((or (real? v1) (real? v2)) (error "value>?: cannot compare a number to a not-number: " v1 v2))
          ((and (string? v1) (string? v2)) (string>? v1 v2))
          ((or (string? v1) (string? v2)) (error "value>?: cannot compare a string to a not-string: " v1 v2))
           ((or (Relation? v1) (Relation? v2)) (error "value<?: cannot compare relations: " v1 v2))
          (else (error "value<?: cannot compare values: " v1 v2)))))

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
          ((or (string? v1) (string? v2)) (error "value<?: cannot compare a string to a not-string: " v1 v2))
          ((or (Relation? v1) (Relation? v2)) (error "value<?: cannot compare relations: " v1 v2))
          (else (error "value<?: cannot compare values: " v1 v2)))))

(: value>=? (Value Value -> Boolean))
(define value>=?
  (lambda (v1 v2)
    (not (value<? v1 v2))))


; Functions to handle boolean operators -----------------------------------------------------------------------------------------------------------------------------

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


; Functions to evaluate and get the result type of evaluated operands --------------------------------------------------------------------------------------------------------------

;(: get-type ((U AggOperand Operand) -> (U (Any -> Boolean : Real) (Any -> Boolean : String)))) ; returns the resulting type of operand evaluation
(: get-type (Operand -> (U (Any -> Boolean : Real) (Any -> Boolean : String) (Any -> Boolean : Relation))))
(define get-type
  (lambda (o)
    (match o
      ((struct Val (val)) (cond ((string? val) string?) ((real? val) real?) ((Relation? val) Relation?)))
      ((struct Att (att)) (Attribute-type att))
      ((struct App (f o1 o2)) (cond ((or (eq? f substring) (eq? f string-append)) string?)
                                    ((or (eq? f +) (eq? f -) (eq? f /) (eq? f *)) real?) 
                                    (else (error "get-type: not a defined function: " f))))
      ((struct Agg (f o a)) (cond ((eq? f length) real?)
                                  ((eq? f sum) real?)
                                  ((eq? f max_) real?)
                                  ((eq? f min_) real?)
                                  (else (error "get-type: not a defined aggregating function: " f))))
      ((struct RelX (r)) Relation?)
                      
      (_ (error "get-type: Not an operand: " o)))))