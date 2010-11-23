#lang typed/racket

(provide (all-defined-out))
(require "settings.rkt")

; A "member" function allowing for specification of the equality function to use.
(: contains? (All(A B) ((Listof A) B (A B -> Boolean) -> Boolean)))
(define contains?
  (lambda (lst elem equals-fun?)
    (if (null? lst)
        #f
        (if (equals-fun? (car lst) elem) #t (contains? (cdr lst) elem equals-fun?)))))

; Checks if two lists have the same elements, allowing for specification of the equality function to use.
(: lists-same? (All(A) ((Listof A) (Listof A) (A A -> Boolean) -> Boolean)))
(define lists-same?
  (lambda (l1 l2 equals-fun?)
    (and (= (length l1) (length l2)) (andmap (lambda: ((elem : A)) (contains? l1 elem equals-fun?)) l2) (andmap (lambda: ((elem : A)) (contains? l2 elem equals-fun?)) l1))))

; Given a list, constructs a list consisting of every element contained in its own - nested - list.
(: split-list (All (A) ((Listof A) -> (Listof (Listof A)))))
(define split-list
  (lambda (lst)
    (if (null? lst) '() (cons (list (car lst)) (split-list (cdr lst))))))

(: log (Integer String Any * -> Void))
(define log
  (lambda (level msg . args)
    (when (<= level (trace)) (apply printf msg args))))

(log 1 "~a ~a" 1 2)
(log 1 "~a" '(1))
(log 1 "~a" 1)