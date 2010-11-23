#lang typed/racket/no-check

(require rackunit)
(require rackunit/text-ui)
(require "../data/data.rkt")
(require "../types.rkt")
(require "../relation-utils.rkt")
(require "../eval.rkt")
(require "../type-functions.rkt")
(require "../helper-functions.rkt")
(require "../settings.rkt")

; Custom checks
(define-binary-check (check-lists-same? (lambda (x y) (lists-same? x y equal?)) actual expected))

(define-test-suite general-algorithms

  (test-case
   "mergesort-tuples sorting for one attribute"
   (let: ((t : (Listof (Listof Tuple)) (split-list (Body-tuples (Relation-body suppliers))))
          (a1 : (Listof Attribute) (list (make-Attribute "city" 'S)))
          (a2 : (Listof Attribute) (list (make-Attribute "status" 'N))))
         (let: ((s1 : (Listof Tuple) (mergesort-tuples t a1))
                (s2 : (Listof Tuple) (mergesort-tuples t a2)))
               ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) s1)
               (check-equal? (map (compose extract-value-variant Triple-value) (map (lambda: ((x : Tuple)) (find-triple-for-attribute (make-Attribute "city" 'S) (Tuple-triples x))) s1)) (list "Athens" "London" "London" "Paris" "Paris"))
               ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) s2)
               (check-equal? (map (compose extract-value-variant Triple-value) (map (lambda: ((x : Tuple)) (find-triple-for-attribute (make-Attribute "status" 'N) (Tuple-triples x))) s2)) (list 20 20 30 30 30)))))
  
  (test-case
   "mergesort-tuples sorting for two attributes using different orders"
   (let: ((t : (Listof (Listof Tuple)) (split-list (Body-tuples (Relation-body shipments))))
          (a1 : (Listof Attribute) (list (make-Attribute "p#" 'S) (make-Attribute "qty" 'N)))
          (a2 : (Listof Attribute) (list (make-Attribute "qty" 'N) (make-Attribute "p#" 'S))))
         (let: ((s1 : (Listof Tuple) (mergesort-tuples t a1))
                (s2 : (Listof Tuple) (mergesort-tuples t a2)))
               ;(for-each (lambda: ((tup : Tuple)) (printf "~a~n" (print-tuple tup))) s1)
               (check-equal? (map (compose extract-value-variant Triple-value) (map (lambda: ((x : Tuple)) (find-triple-for-attribute (make-Attribute "p#" 'S) (Tuple-triples x))) s1)) (list "P1" "P1" "P2" "P2" "P2" "P2" "P3" "P4" "P4" "P5" "P5" "P6"))
               ;(for-each (lambda: ((tup : Tuple)) (printf "~a~n" (print-tuple tup))) s2)
               (check-equal? (map (compose extract-value-variant Triple-value) (map (lambda: ((x : Tuple)) (find-triple-for-attribute (make-Attribute "p#" 'S) (Tuple-triples x))) s2)) (list "P2" "P2" "P2" "P4" "P1" "P1" "P4" "P5" "P6" "P2" "P3" "P5"))))))


(define-test-suite join-techniques

  (test-case
   "build-hash"
   (let*: ((joining-attrs1 : (Listof Attribute) (list (make-Attribute "color" 'S)))
           (joining-attrs2 : (Listof Attribute) (list (make-Attribute "pname" 'S) (make-Attribute "color" 'S)))
           (t : (Listof Tuple) (Body-tuples (Relation-body parts)))
           (greens : (Listof Tuple) (filter (lambda: ((x : Tuple)) (equal? (extract-value-variant (Triple-value (find-triple-for-attribute (make-Attribute "color" 'S) (Tuple-triples x)))) "Green")) t))
           (reds : (Listof Tuple) (filter (lambda: ((x : Tuple)) (equal? (extract-value-variant (Triple-value (find-triple-for-attribute (make-Attribute "color" 'S) (Tuple-triples x)))) "Red")) t))
           (blue-screws : (Listof Tuple) (filter (lambda: ((x : Tuple)) (and (equal? (extract-value-variant (Triple-value (find-triple-for-attribute (make-Attribute "color" 'S) (Tuple-triples x)))) "Blue") (equal? (extract-value-variant (Triple-value (find-triple-for-attribute (make-Attribute "pname" 'S) (Tuple-triples x)))) "Screw"))) t))
           (red-screws : (Listof Tuple) (filter (lambda: ((x : Tuple)) (and (equal? (extract-value-variant (Triple-value (find-triple-for-attribute (make-Attribute "color" 'S) (Tuple-triples x)))) "Red") (equal? (extract-value-variant (Triple-value (find-triple-for-attribute (make-Attribute "pname" 'S) (Tuple-triples x)))) "Screw"))) t)))
          (let: ((ht1 : (HashTable (Listof Triple) (Listof Tuple)) (build-hash t joining-attrs1))
                 (ht2 : (HashTable (Listof Triple) (Listof Tuple)) (build-hash t joining-attrs2)))
                (check-equal? (hash-count ht1) 3)
                (check-lists-same? (hash-ref ht1 (list (make-Triple "color" 'S (make-S "Green"))))  greens)
                (check-lists-same? (hash-ref ht1 (list (make-Triple "color" 'S (make-S "Red")))) reds)
                (check-equal? (hash-count ht2) 6)
                (check-lists-same? (hash-ref ht2 (list (make-Triple "pname" 'S (make-S "Screw")) (make-Triple "color" 'S (make-S "Blue")))) blue-screws)
                (check-lists-same? (hash-ref ht2 (list (make-Triple "pname" 'S (make-S "Screw")) (make-Triple "color" 'S (make-S "Red")))) red-screws))))
  
  (test-case
   "comparing 3 join methods - 1 join attribute, tuples already sorted"
   (let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (s2 : (Listof Tuple) (Body-tuples (Relation-body shipments))) (atts : (Listof Attribute) (list (make-Attribute "s#" 'S))))
         (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2))
                 (t2 : (Listof Tuple) (merge-join atts s1 s2))
                 (t3 : (Listof Tuple) (nl-join atts s1 s2)))
                ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t1)
                (check = (length (Body-tuples (Relation-body shipments))) (length t1))
                (check-lists-same? t1 t2)
                (check-lists-same? t1 t3))))
  
  (test-case
   "comparing 3 join methods - 1 join attribute, tuples not sorted"
   (let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (s2 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (atts : (Listof Attribute) (list (make-Attribute "city" 'S))))
         (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
                ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
                (check = 9 (length t1))
                (check-lists-same? t1 t2)
                (check-lists-same? t1 t3))))

  (test-case
   "comparing 3 join methods - 2 join attributes, tuples not sorted"
   (let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body shipments))) (s2 : (Listof Tuple) (Body-tuples (Relation-body shipments))) (atts : (Listof Attribute) (list (make-Attribute "p#" 'S) (make-Attribute "qty" 'N))))
         (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
                ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
                (check = 20 (length t1))
                (check-lists-same? t1 t2)
                (check-lists-same? t1 t3))))
 
  (test-case
   "comparing 3 join methods - empty join result"
   (let: ((s1 : (Listof Tuple) (Body-tuples (Relation-body suppliers))) (s2 : (Listof Tuple) (Body-tuples (Relation-body (build-renamed-relation parts (list (cons (make-Attribute "pname" 'S) "sname")))))) (atts : (Listof Attribute) (list (make-Attribute "sname" 'S))))
         (let:  ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
                ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
                (check = 0 (length t1))
                (check-lists-same? t1 t2)
                (check-lists-same? t1 t3))))

  (test-case
   "comparing 3 join methods - joining 3 relations"
   (let: ((s1 :  (Listof Tuple) (Body-tuples (Relation-body (build-join suppliers shipments)))) (s2 : (Listof Tuple) (Body-tuples (Relation-body (build-projection parts (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N)))))) (atts : (Listof Attribute) (list (make-Attribute "p#" 'S))))
         (let: ((t1 : (Listof Tuple) (hash-join atts s1 s2)) (t2 : (Listof Tuple) (merge-join atts s1 s2)) (t3 : (Listof Tuple) (nl-join atts s1 s2)))
               ;(for-each (lambda: ((tup : Tuple)) (printf "~s~n" (print-tuple tup))) t2)
               (check = (length (Body-tuples (Relation-body shipments))) (length t1))
               (check-lists-same? t1 t2)
               (check-lists-same? t1 t3)))))


(run-tests general-algorithms)
(parameterize ((default-join-method
                 ;'hash))
                 'sort-merge))
                 ;'nested-loops))
  (run-tests join-techniques))

 
 