#lang typed/racket/no-check

(require rackunit)
(require rackunit/text-ui)
(require "../data/data.rkt")
(require "../types.rkt")
(require "../type-functions.rkt")
(require "../helper-functions.rkt")
(require "../relation-utils.rkt")

(define-test-suite equality-functions

  (test-case
   "Value equality"
   (let: ((s1 : String (make-S "astring")) (s2 : String (make-S "AString")) (s3 : String (make-S "AString")) (n1 : Real (make-N 6)) (n2 : Real (make-N 6.0)) (n3 : Real (make-N 6.01)))
         (check-equal? s2 s3)
         (check-not-equal? s1 s2)
         (check-equal? n1 n2)
         (check-not-equal? n1 n3)))
  
  (test-case 
   "Triple equality"
   (let: ((t1 : Triple (make-Triple "count" 'N (make-N 1))) (t2 : Triple (make-Triple "count" 'N (make-N 1))) (t3 : Triple (make-Triple "count" 'N (make-S "1"))) (t4 : Triple (make-Triple "Count" 'N (make-N 1))) (t5 : Triple (make-Triple "count" 'N (make-N 2))))
         (check-equal? t1 t2)
         (check-equal? t1 t4)
         (check-not-equal? t1 t5)))
  
  (test-case 
   "Tuple equality"
   (let: ((r1 : Relation suppliers) (r2 : Relation (new-relation-from suppliers)))
         (let: ((t1_1 : Tuple (car (Body-tuples (Relation-body r1)))) (t1_2 : Tuple (caddr (Body-tuples (Relation-body r1)))) (t2_1 : Tuple (car (Body-tuples (Relation-body r2)))))
               (check-equal? t1_1 t2_1)
               (check-not-equal? t1_1 t1_2))))
  
  (test-case 
   "Heading equality"
   (let: ((h1 : Heading (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "city" 'S))))
          (h2 : Heading (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'S) (make-Attribute "city" 'S))))
          (h3 : Heading (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "place" 'S)))))
         (check-equal? h1 (Relation-heading suppliers))
         (check-not-equal? h1 h2)
         (check-not-equal? h1 h3))))


(define-test-suite hashcode-functions

  (test-case
   "Value hash code"
   (let: ((s1 : String (make-S "astring")) (s2 : String (make-S "AString")) (s3 : String (make-S "AString")) (n1 : Real (make-N 1/2)) (n2 : Real (make-N 0.5)) (n3 : Real (make-N 0.50000001)))
         (check-eq? (equal-hash-code s2) (equal-hash-code s3))
         (check-not-eq? (equal-hash-code s1) (equal-hash-code s2))
         (check-eq? (equal-hash-code n1) (equal-hash-code n2))
         (check-not-eq? (equal-hash-code n1) (equal-hash-code n3))))
  
  (test-case 
   "Triple hash code"
   (let: ((t1 : Triple (make-Triple "count" 'N (make-N 1))) (t2 : Triple (make-Triple "count" 'N (make-N 1))) (t3 : Triple (make-Triple "count" 'N (make-S "1"))) (t4 : Triple (make-Triple "Count" 'N (make-N 1))) (t5 : Triple (make-Triple "count" 'N (make-N 2))))
         (check-eq? (equal-hash-code t1) (equal-hash-code t2))
         (check-eq? (equal-hash-code t1) (equal-hash-code t4))
         (check-not-eq? (equal-hash-code t1) (equal-hash-code t5))))
  
  (test-case 
   "Tuple hash code"
   (let: ((r1 : Relation suppliers) (r2 : Relation (new-relation-from suppliers)))
         (let: ((t1_1 : Tuple (car (Body-tuples (Relation-body r1)))) (t1_2 : Tuple (caddr (Body-tuples (Relation-body r1)))) (t2_1 : Tuple (car (Body-tuples (Relation-body r2)))))
               (check-eq? (equal-hash-code t1_1) (equal-hash-code t2_1))
               (check-not-eq? (equal-hash-code t1_1) (equal-hash-code t1_2)))))
  
  (test-case 
   "Heading hash code"
   (let: ((h1 : Heading (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "city" 'S))))
          (h2 : Heading (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'S) (make-Attribute "city" 'S))))
          (h3 : Heading (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "place" 'S)))))
         (check-eq? (equal-hash-code h1) (equal-hash-code (Relation-heading suppliers)))
         (check-not-eq? (equal-hash-code h1) (equal-hash-code h2))
         (check-not-eq? (equal-hash-code h1) (equal-hash-code h3)))))

(run-tests equality-functions)
(run-tests hashcode-functions)