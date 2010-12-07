#lang typed/racket

(require typed/rackunit)
(require "../data/data.rkt")
(require "../types.rkt")
(require "../relation-utils.rkt")
(require "../eval.rkt")
(require "../type-functions.rkt")
(require "../helper-functions.rkt")
(require "../settings.rkt")

; Custom checks
;(define-binary-check (check lists-same (lambda (x y) (lists-same? x y equal?)) actual expected))

;(define-test-suite general-algorithms
;
;  (test-case
;   "mergesort-tuples sorting for one attribute"
(let: ((t : (Listof (Listof Tuple)) (split-list (Body-tuples (Relation-body suppliers))))
       (a1 : (Listof Attribute) (list (Attribute "city" string?)))
       (a2 : (Listof Attribute) (list (Attribute "status" real?))))
      (let: ((s1 : (Listof Tuple) (mergesort-tuples t a1))
             (s2 : (Listof Tuple) (mergesort-tuples t a2)))
            ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) s1)
            (check-equal? (map (lambda: ((x : Tuple)) (Triple-value (find-triple-for-attribute (Attribute "city" string?) (Tuple-triples x)))) s1) (list "Athens" "London" "London" "Paris" "Paris"))
            ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) s2)
            (check-equal? (map (lambda: ((x : Tuple)) (Triple-value (find-triple-for-attribute (Attribute "status" real?) (Tuple-triples x)))) s2) (list 20 20 30 30 30))))
;  
;  (test-case
;   "mergesort-tuples sorting for two attributes using different orders"
(let: ((t : (Listof (Listof Tuple)) (split-list (Body-tuples (Relation-body shipments))))
       (a1 : (Listof Attribute) (list (Attribute "p#" string?) (Attribute "qty" real?)))
       (a2 : (Listof Attribute) (list (Attribute "qty" real?) (Attribute "p#" string?))))
      (let: ((s1 : (Listof Tuple) (mergesort-tuples t a1))
             (s2 : (Listof Tuple) (mergesort-tuples t a2)))
            ;(for-each (lambda: ((tup : Tuple)) (printf "~a~n" (print-tuple tup))) s1)
            (check-equal? (map (lambda: ((x : Tuple)) (Triple-value (find-triple-for-attribute (Attribute "p#" string?) (Tuple-triples x)))) s1) (list "P1" "P1" "P2" "P2" "P2" "P2" "P3" "P4" "P4" "P5" "P5" "P6"))
            ;(for-each (lambda: ((tup : Tuple)) (printf "~a~n" (print-tuple tup))) s2)
            (check-equal? (map (lambda: ((x : Tuple)) (Triple-value (find-triple-for-attribute (Attribute "p#" string?) (Tuple-triples x)))) s2) (list "P2" "P2" "P2" "P4" "P1" "P1" "P4" "P5" "P6" "P2" "P3" "P5"))))
;
;
;(define-test-suite join-techniques
;
;  (test-case
;   "build-hash"
(let*: ((joining-attrs1 : (Listof Attribute) (list (Attribute "color" string?)))
        (joining-attrs2 : (Listof Attribute) (list (Attribute "pname" string?) (Attribute "color" string?)))
        (t : (Listof Tuple) (Body-tuples (Relation-body parts)))
        (greens : (Listof Tuple) (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute (Attribute "color" string?) (Tuple-triples x))) "Green")) t))
        (reds : (Listof Tuple) (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute (Attribute "color" string?) (Tuple-triples x))) "Red")) t))
        (blue-screws : (Listof Tuple) (filter (lambda: ((x : Tuple)) (and (equal? (Triple-value (find-triple-for-attribute (Attribute "color" string?) (Tuple-triples x))) "Blue") (equal? (Triple-value (find-triple-for-attribute (Attribute "pname" string?) (Tuple-triples x))) "Screw"))) t))
        (red-screws : (Listof Tuple) (filter (lambda: ((x : Tuple)) (and (equal? (Triple-value (find-triple-for-attribute (Attribute "color" string?) (Tuple-triples x))) "Red") (equal? (Triple-value (find-triple-for-attribute (Attribute "pname" string?) (Tuple-triples x))) "Screw"))) t)))
       (let: ((ht1 : (HashTable (Listof Triple) (Listof Tuple)) (build-hash t joining-attrs1))
              (ht2 : (HashTable (Listof Triple) (Listof Tuple)) (build-hash t joining-attrs2)))
             (check-equal? (hash-count ht1) 3)
             (check-eq? (lists-same? (hash-ref ht1 (list (Triple "color" string? "Green")))  greens equal?) #t)
             (check-eq? (lists-same? (hash-ref ht1 (list (Triple "color" string? "Red"))) reds equal?) #t)
             (check-equal? (hash-count ht2) 6)
             (check-eq? (lists-same? (hash-ref ht2 (list (Triple "pname" string? "Screw") (Triple "color" string? "Blue"))) blue-screws equal?) #t)
             (check-eq? (lists-same? (hash-ref ht2 (list (Triple "pname" string? "Screw") (Triple "color" string? "Red"))) red-screws equal?) #t)))

;  (test-case
;   "comparing 3 join methods - 1 join attribute, tuples already sorted"
(let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (s2 : (Listof Tuple) (Body-tuples (Relation-body shipments))) (atts : (Listof Attribute) (list (Attribute "s#" string?))))
      (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2))
              (t2 : (Listof Tuple) (merge-join atts s1 s2))
              (t3 : (Listof Tuple) (nl-join atts s1 s2)))
             ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t1)
             (check-equal? (length (Body-tuples (Relation-body shipments))) (length t1))
             (check-eq? (lists-same? t1 t2 equal?) #t)
             (check-eq? (lists-same? t1 t3 equal?) #t)))

;  (test-case
;   "comparing 3 join methods - 1 join attribute, tuples not sorted"
(let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (s2 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (atts : (Listof Attribute) (list (Attribute "city" string?))))
      (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
             ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
             (check-equal? 9 (length t1))
             (check-eq? (lists-same? t1 t2 equal?) #t)
             (check-eq? (lists-same? t1 t3 equal?) #t)))

;  (test-case
;   "comparing 3 join methods - 2 join attributes, tuples not sorted"
(let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body shipments))) (s2 : (Listof Tuple) (Body-tuples (Relation-body shipments))) (atts : (Listof Attribute) (list (Attribute "p#" string?) (Attribute "qty" real?))))
      (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
             ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
             (check-equal? 20 (length t1))
             (check-eq? (lists-same? t1 t2 equal?) #t)
             (check-eq? (lists-same? t1 t3 equal?) #t)))
; 
;  (test-case
;   "comparing 3 join methods - empty join result"
(let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (s2 : (Listof Tuple) (Body-tuples (Relation-body (build-renamed-relation parts (list (cons (Attribute "pname" string?) "sname")))))) (atts : (Listof Attribute) (list (Attribute "sname" string?))))
      (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
             ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
             (check-equal? 0 (length t1))
             (check-eq? (lists-same? t1 t2 equal?) #t)
             (check-eq? (lists-same? t1 t3 equal?) #t)))
;
;  (test-case
;   "comparing 3 join methods - joining 3 relations"
(let: ((s1 :  (Listof Tuple) (Body-tuples (Relation-body (build-join suppliers shipments)))) (s2 : (Listof Tuple) (Body-tuples (Relation-body (build-projection parts (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?)))))) (atts : (Listof Attribute) (list (Attribute "p#" string?))))
      (let: ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
            ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
            (check-equal? (length (Body-tuples (Relation-body shipments))) (length t1))
            (check-eq? (lists-same? t1 t2 equal?) #t)
            (check-eq? (lists-same? t1 t3 equal?) #t)))

;
;(run-tests general-algorithms)
;(parameterize ((default-join-method
;                 ;'hash))
;                 'sort-merge))
;                 ;'nested-loops))
;  (run-tests join-techniques))
;
; 
; 