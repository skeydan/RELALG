#lang typed/racket

(require typed/rackunit)
(require "../data/data.rkt")
(require "../types.rkt")
(require "../relation-utils.rkt")

;(define-test-suite equality-functions
;  
;  (test-case 
;   "Triple equality"
(let: ((t1 : Triple (Triple "count" real? 1)) (t2 : Triple (Triple "count" real? 1)) (t3 : Triple (Triple "count" string? "1")) (t4 : Triple (Triple "Count" real? 1)) (t5 : Triple (Triple "count" real? 2)))
      (check-equal? t1 t2)
      (check-equal? t1 t4)
      (check-not-equal? t1 t5))

;  (test-case 
;   "Tuple equality"
(let: ((r1 : Relation suppliers) (r2 : Relation (new-relation-from suppliers)))
      (let: ((t1_1 : Tuple (car (Body-tuples (Relation-body r1)))) (t1_2 : Tuple (caddr (Body-tuples (Relation-body r1)))) (t2_1 : Tuple (car (Body-tuples (Relation-body r2)))))
            (check-equal? t1_1 t2_1)
            (check-not-equal? t1_1 t1_2)))
;  (test-case 
;   "Heading equality"
(let: ((h1 : Heading (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?) (Attribute "city" string?))))
       (h2 : Heading (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" string?) (Attribute "city" string?))))
       (h3 : Heading (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?) (Attribute "place" string?)))))
      (check-equal? h1 (Relation-heading suppliers))
      (check-not-equal? h1 h2)
      (check-not-equal? h1 h3))


;(define-test-suite hashcode-functions
;
;  (test-case 
;   "Triple hash code"
(let: ((t1 : Triple (Triple "count" real? 1)) (t2 : Triple (Triple "count" real? 1)) (t3 : Triple (Triple "count" string? "1")) (t4 : Triple (Triple "Count" real? 1)) (t5 : Triple (Triple "count" real? 2)))
      (check-eq? (equal-hash-code t1) (equal-hash-code t2))
      (check-eq? (equal-hash-code t1) (equal-hash-code t4))
      (check-not-eq? (equal-hash-code t1) (equal-hash-code t5)))

;  (test-case 
;   "Tuple hash code"
(let: ((r1 : Relation suppliers) (r2 : Relation (new-relation-from suppliers)))
      (let: ((t1_1 : Tuple (car (Body-tuples (Relation-body r1)))) (t1_2 : Tuple (caddr (Body-tuples (Relation-body r1)))) (t2_1 : Tuple (car (Body-tuples (Relation-body r2)))))
            (check-eq? (equal-hash-code t1_1) (equal-hash-code t2_1))
            (check-not-eq? (equal-hash-code t1_1) (equal-hash-code t1_2))))

;  (test-case 
;   "Heading hash code"
(let: ((h1 : Heading (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?) (Attribute "city" string?))))
       (h2 : Heading (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" string?) (Attribute "city" string?))))
       (h3 : Heading (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?) (Attribute "place" string?)))))
      (check-eq? (equal-hash-code h1) (equal-hash-code (Relation-heading suppliers)))
      (check-not-eq? (equal-hash-code h1) (equal-hash-code h2))
      (check-not-eq? (equal-hash-code h1) (equal-hash-code h3)))

;(run-tests equality-functions)
;(run-tests hashcode-functions)