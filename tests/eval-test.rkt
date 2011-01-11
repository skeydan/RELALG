#lang typed/racket

(require typed/rackunit)
(require "../data/data.rkt")
(require "../types.rkt")
(require "../type-functions.rkt")
(require "../helper-functions.rkt")
(require "../eval.rkt")
(require "../relation-utils.rkt")
(require "../settings.rkt")

;(define-test-suite basic-relational-operators
;  
;  (test-case
;   "build-union"
(let*: ((a : Attribute (Attribute "city" string?))
        (r1 : Relation (Relation (Relation-heading suppliers) (Body (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute a (Tuple-triples x))) "London")) (Body-tuples (Relation-body suppliers))))))
        (r2 : Relation (Relation (Relation-heading suppliers) (Body (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute a (Tuple-triples x))) "Paris")) (Body-tuples (Relation-body suppliers))))))
        (r3 : Relation (Relation (Relation-heading suppliers) (Body (filter (lambda: ((x : Tuple)) (let: ((s : Value (Triple-value (find-triple-for-attribute a (Tuple-triples x))))) (or (equal? s "Paris") (equal? s "Athens")))) (Body-tuples (Relation-body suppliers)))))))
       (let: ((r1_2 : Relation (build-union r1 r2))
              (r1_3 : Relation (build-union r1 r3))
              (r2_3 : Relation (build-union r2 r3)))
             ;(print-relation r1_3)
             (check-equal? 4 (cardinality r1_2))
             (check-equal? 5 (cardinality r1_3))
             (check-equal? 3 (cardinality r2_3))))
;  
;  (test-case
;   "build-intersect"
(let*: ((a : Attribute (Attribute "color" string?)) 
        (r1 : Relation (Relation (Relation-heading parts) (Body (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute a (Tuple-triples x))) "Red")) (Body-tuples (Relation-body parts))))))
        (r2 : Relation (Relation (Relation-heading parts) (Body (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute a (Tuple-triples x))) "Green")) (Body-tuples (Relation-body parts))))))
        (r3 : Relation (Relation (Relation-heading parts) (Body (filter (lambda: ((x : Tuple)) (let: ((s : Value (Triple-value (find-triple-for-attribute a (Tuple-triples x))))) (or (equal? s "Green") (equal? s "Red")))) (Body-tuples (Relation-body parts)))))))
       (let: ((r1_2 : Relation (build-intersect r1 r2))
              (r1_3 : Relation (build-intersect r1 r3))
              (r2_3 : Relation (build-intersect r2 r3)))
             ;(print-relation r1_2)
             (check-equal? 0 (cardinality r1_2))
             (check-equal? 3 (cardinality r1_3))
             (check-equal? 1 (cardinality r2_3))))
;  
;  
;  (test-case
;   "build-difference"
(let*: ((r1 : Relation (Relation (Relation-heading suppliers) (Body (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute (Attribute "city" string?) (Tuple-triples x))) "London")) (Body-tuples (Relation-body suppliers))))))
        (r2 : Relation (build-difference suppliers r1)))
       ;(print-relation r)
       (check-equal? (Relation-heading r2) (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?) (Attribute "city" string?))))
       (check-equal? 3 (cardinality r2)))
;  
;  (test-case
;   "build-product"
(let*: ((r1 : Relation (build-projection shipments (list (Attribute "p#" string?) (Attribute "qty" real?))))
        (r2 : Relation (build-product suppliers r1)))
       ;(print-relation r1)
       (check-equal? (Relation-heading r2) (Heading (append (Heading-attrs (Relation-heading suppliers)) (Heading-attrs (Relation-heading r1)))))
       (check-equal?  (* (cardinality suppliers) (cardinality r1)) (cardinality r2)))

;  (test-case
;   "build-join, joining 2 relations on s#"
(let: ((r : Relation (build-join suppliers shipments)))
      ;(print-relation r)
      (check-equal? (Relation-heading r) (Heading (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?) (Attribute "city" string?) (Attribute "p#" string?) (Attribute "qty" real?))))
      (check-equal? (cardinality shipments) (cardinality r)))
;  
;  (test-case
;   "build-join, joining 3 relations on s#, p#, city in different orders"
(parameterize ((default-join-method
                 ;'hash
                 'sort-merge
                 ;'nested-loops
                 ))
  (let: ((r1 : Relation (build-join parts (build-join suppliers shipments)))
         (r2 : Relation (build-join suppliers (build-join parts shipments)))
         (r3 : Relation (build-join shipments (build-join suppliers parts))))
        ;(print-relation r1)
        (check-equal? (Relation-heading r1) (Heading (list (Attribute "s#" string?) (Attribute "p#" string?) (Attribute "city" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?)  (Attribute "sname" string?) (Attribute "status" real?) (Attribute "qty" real?))))
        (check-equal? (Relation-heading r1) (Relation-heading r2))
        (check-equal? (Relation-heading r1) (Relation-heading r3))
        (check-equal? (cardinality r1) 6)
        (check-equal? (cardinality r2) 6)
        (check-equal? (cardinality r3) 6)))
;  
;  (test-case
;   "build-join, joining 3 relations on s#, p# with city in parts relation projected away"
(parameterize ((default-join-method
                 ;'hash
                 'sort-merge
                 ;'nested-loops
                 ))
  (let: ((r : Relation (build-join (build-projection parts (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?)))(build-join suppliers shipments))))
        ;(print-relation r)
        (check-equal? (Relation-heading r) (Heading (list (Attribute "s#" string?) (Attribute "p#" string?) (Attribute "city" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?)  (Attribute "sname" string?) (Attribute "status" real?) (Attribute "qty" real?))))
        (check-equal? (cardinality r) (cardinality shipments))))
;  
;  (test-case
;   "build-projection"
(let: ((r : Relation (build-projection parts (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?)))))
      ;(print-relation r)
      (check-equal? (cardinality r) 6)
      (check-equal? (Relation-heading r) (Heading (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?)))))
;  
;  (test-case
;   "build-projection, testing for removal of duplicates"
(let: ((r1 : Relation (build-projection parts (list (Attribute "color" string?))))
       (r2 : Relation (build-projection (build-join suppliers shipments) (list (Attribute "sname" string?) (Attribute "status" real?) (Attribute "city" string?)))))
      ;(for-each print-relation (list r1 r2))
      (check-equal? (cardinality r1) 3)
      (check-equal? (cardinality r2) 4)
      (check-equal? (Relation-heading r1) (Heading (list (Attribute "color" string?))))
      (check-equal? (Relation-heading r2) (Heading (list (Attribute "sname" string?) (Attribute "status" real?) (Attribute "city" string?)))))

;  (test-case
;   "build-restriction with simple predicates, testing for different comparison operators"
(let: ((r1 : Relation (build-restriction parts (Is eql (Att (Attribute "city" string?)) (Val "London"))))
       (r2 : Relation (build-restriction parts (Is noteql (Att (Attribute "city" string?)) (Val "London"))))
       (r3 : Relation (build-restriction parts (Is less (Att (Attribute "weight" real?)) (Val 14))))
       (r4 : Relation (build-restriction parts (Is lesseql (Att (Attribute "weight" real?)) (Val 14))))
       (r5 : Relation  (build-restriction parts (Is greater (Att (Attribute "weight" real?)) (Val 14))))
       (r6 : Relation  (build-restriction parts (Is greatereql (Att (Attribute "weight" real?)) (Val 14)))))
      ;(for-each print-relation (list r1 r2 r3 p4 r5 r6))
      (check-equal? (Relation-heading r1) (Relation-heading r2))
      (check-equal? (cardinality r1) 3)
      (check-equal? (cardinality r2) 3)
      (check-equal? (cardinality r3) 2)
      (check-equal? (cardinality r4) 3)
      (check-equal? (cardinality r5) 3)
      (check-equal? (cardinality r6) 4))
;  
;  (test-case
;   "build-restriction with simple predicates, testing Attributes vs. Values as Operands"
(let: ((r1 : Relation (build-restriction parts (Is greatereql (Val 14) (Att (Attribute "weight" real?)))))
       (r2 : Relation (build-restriction parts (Is greatereql (Val 14) (Val 17))))
       (r3 : Relation (build-restriction parts (Is less (Att (Attribute "pname" string?)) (Att (Attribute "color" string?))))))
      ;(for-each print-relation (list r1 r2 r3))
      (check-equal? (cardinality r1) 3)
      (check-equal? (cardinality r2) 0)
      (check-equal? (cardinality r3) 3))
;  
;  (test-case
;   "build-restriction with compound predicates"
(let: ((r1 : Relation (build-restriction parts (And (Is eql (Att (Attribute "city" string?)) (Val "London")) (Is noteql (Att (Attribute "city" string?)) (Val "London")))))
       (r2 : Relation (build-restriction parts (Or (Is eql (Att (Attribute "city" string?)) (Val "London")) (Is noteql (Att (Attribute "city" string?)) (Val "London")))))
       (r3 : Relation (build-restriction parts (And (Is eql (Att (Attribute "city" string?)) (Val "London")) (Is noteql (Att (Attribute "pname" string?)) (Val "Screw")))))
       (r4 : Relation (build-restriction parts (Or (Is noteql (Att (Attribute "city" string?)) (Val "London")) (Is eql  (Att (Attribute "color" string?)) (Val "Red")))))
       (r5 : Relation (build-restriction parts (Not (And (Is eql (Att (Attribute "city" string?)) (Val "London")) (Is noteql (Att (Attribute "pname" string?)) (Val "Screw"))))))
       (r6 : Relation (build-restriction parts (Not (Or (Is noteql (Att (Attribute "city" string?)) (Val "London")) (Is eql (Att (Attribute "color" string?)) (Val "Red")))))))
      ;(for-each print-relation (list r1 r2 r3 r4 r5 r6))
      (check-equal? (cardinality r1) 0)
      (check-equal? (cardinality r2) 6)
      (check-equal? (cardinality r3) 2)
      (check-equal? (cardinality r4) 6)
      (check-equal? (cardinality r5) 4)
      (check-equal? (cardinality r6) 0))
;  
;  (test-case
;   "build-renamed-relation"
(let: ((r1 : Relation (build-renamed-relation parts (list (cons (Attribute "city" string?) "location") (cons (Attribute "color" string?) "howitlooks"))))
       (r2 : Relation (build-renamed-relation parts '())))
      ;(for-each print-relation (list r1 r2))
      (check-equal? (Relation-heading r1) (Heading (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "howitlooks" string?) (Attribute "weight" real?) (Attribute "location" string?))))
      (check-equal? (Relation-heading r2) (Heading (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?) (Attribute "city" string?)))))
;  
;  (test-case
;   "build-theta"
(let: ((r1 : Relation (build-theta (Is greater (Att (Attribute "pname" string?)) (Att (Attribute "sname" string?))) parts (build-projection suppliers (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?)))))
       (r2 : Relation (build-theta (Is greater (Att (Attribute "p#" string?)) (Att (Attribute "s#" string?))) parts (build-projection suppliers (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?))))))
      ; (print-relation r1)
      (check-equal? (cardinality r1) 19)
      (check-equal? (cardinality r2) 0))
;  
;  (test-case
;   "build-theta throws error when there are overlapping attributes"
;   (check-exn exn? (lambda () (build-theta (Is greater (Att (Attribute "pname" string?)) (Att (Attribute "sname" string?))) parts suppliers)))
;  
;  (test-case
;   "build-divide"
(let: ((suppliers_s# : Relation (build-projection suppliers (list (Attribute "s#" string?))))
       (parts_p# : Relation (build-projection parts (list (Attribute "p#" string?))))
       (shipments_s#p# : Relation (build-projection shipments (list (Attribute "s#" string?) (Attribute "p#" string?)))))
      (let: ((r1 : Relation (build-divide suppliers_s# parts_p# shipments_s#p#))
             (r2 : Relation (build-divide suppliers_s# (build-restriction parts_p# (Is eql (Att (Attribute "p#" string?)) (Val "P1"))) shipments_s#p#))
             (r3 : Relation (build-divide suppliers_s# (build-restriction parts_p# (Or (Is eql (Att (Attribute "p#" string?)) (Val "P2")) (Is eql (Att (Attribute "p#" string?)) (Val "P4")))) shipments_s#p#)))
            ;(for-each print-relation (list r1 r2 r3))
            (check-equal? (cardinality r1) 1)
            (check-equal? (cardinality r2) 2)
            (check-equal? (cardinality r3) 2)))
;
;(define-test-suite additional-relational-operators
;  
;  (test-case
;   "build-semijoin"
(let: ((r : Relation (build-semijoin suppliers (build-restriction shipments (Is eql (Att (Attribute "p#" string?)) (Val "P2"))))))
      ;(print-relation r)
      (check-equal? (Relation-heading r) (Relation-heading suppliers))
      (check-equal? (cardinality r) 4))
;  
;  (test-case
;   "build-semiminus"
(let: ((r : Relation (build-semiminus suppliers (build-restriction shipments (Is eql (Att (Attribute "p#" string?)) (Val "P2"))))))
      ;(print-relation r)
      (check-equal? (Relation-heading r) (Relation-heading suppliers))
      (check-equal? (cardinality r) 1))
;  
;  (test-case
;   "build-extension using constant values, attributes, operator applications and function applications"
(let: ((r : Relation (build-extension parts (list (Extension (Val "dummy") "default") (Extension (Att (Attribute "p#" string?)) "id")
                                                  (Extension (App / (Att (Attribute "weight" real?)) (Val 100)) "hectograms")
                                                  (Extension (App string-append (Att (Attribute "city" string?)) (Val "derry")) "newcity")))))
      ;(print-relation r)
      (check-equal? (Relation-heading r) (Heading (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?) (Attribute "city" string?) (Attribute "default" string?) (Attribute "id" string?) (Attribute "hectograms" real?) (Attribute "newcity" string?))))
      (check-equal? "dummy" (Triple-value (find-triple-for-attribute (Attribute "default" string?) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
      (check-equal? "P1" (Triple-value (find-triple-for-attribute (Attribute "id" string?) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
      (check-equal? 0.12 (Triple-value (find-triple-for-attribute (Attribute "hectograms" real?) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
      (check-equal? "Londonderry" (Triple-value (find-triple-for-attribute (Attribute "newcity" string?) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
      (check-equal? (degree r) (+ 4 (degree parts)))
      (check-equal? (cardinality r) (cardinality parts)))

;  (test-case
;   "build-image"
(let: ((r1 : Relation (build-image (car (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute (Attribute "s#" string?) (Tuple-triples x))) "S1"))  (Body-tuples (Relation-body suppliers)))) shipments))
       (r2 : Relation (build-image (car (filter (lambda: ((x : Tuple)) (equal? (Triple-value (find-triple-for-attribute (Attribute "s#" string?) (Tuple-triples x))) "S5"))  (Body-tuples (Relation-body suppliers)))) shipments)))
      ;(print-relation r2)
      (check-equal? (cardinality r1) (cardinality (build-restriction (build-join suppliers shipments) (Is eql (Att (Attribute "s#" string?)) (Val "S1")))))
      (check-equal? (Relation-heading r1) (Heading (list (Attribute "p#" string?) (Attribute "qty" real?))))
      (check-equal? (cardinality r2) 0))

;(test-case
; "build-summarize with one grouping attribute"
(let: ((r1 : Relation (build-summarize shipments (build-projection parts (list (Attribute "p#" string?))) (list (Aggregation (AppAgg length (Attribute "dummy" real?)) "count") (Aggregation (AppAgg sum (Attribute "qty" real?)) "sum") (Aggregation (AppAgg max_ (Attribute "qty" real?)) "max") (Aggregation (AppAgg min_ (Attribute "qty" real?)) "min")))))
      ;(print-relation r1)
      (check-equal? (Relation-heading r1) (Heading (list (Attribute "p#" string?) (Attribute "count" real?) (Attribute "sum" real?) (Attribute "max" real?) (Attribute "min" real?))))
      (for: ((pno : String '("P1" "P2" "P3" "P4" "P5" "P6"))
             (count : Real '(2 4 1 2 2 1))
             (sum : Real '(600 1000 400 500 700 300))
             (max : Real '(300 400 400 300 400 300))
             (min : Real '(300 200 400 200 300 300)))
            (let: ((trs : (Listof Triple) (Tuple-triples (car (Body-tuples (Relation-body (build-restriction r1 (Is eql (Att (Attribute "p#" string?)) (Val pno)))))))))
                  (check-equal? (Triple-value (find-triple-for-attribute (Attribute "count" real?) trs)) count)
                  (check-equal? (Triple-value (find-triple-for-attribute (Attribute "sum" real?) trs)) sum)
                  )))

;(test-case
; "build-summarize with two grouping attributes"
(let*: ((r1 : Relation (let: ((t : (Listof Tuple) (Body-tuples (Relation-body shipments)))) (Relation (Relation-heading shipments) (Body (append t t t)))))
        (r2 : Relation (build-summarize r1 (build-projection shipments (list (Attribute "p#" string?) (Attribute "s#" string?))) (list (Aggregation (AppAgg length (Attribute "dummy" real?)) "count") (Aggregation (AppAgg sum (Attribute "qty" real?)) "sum") (Aggregation (AppAgg max_ (Attribute "qty" real?)) "max") (Aggregation (AppAgg min_ (Attribute "qty" real?)) "min")))))
       (print-relation r2)
       (check-equal? (Relation-heading r2) (Heading (list (Attribute "p#" string?) (Attribute "s#" string?) (Attribute "count" real?) (Attribute "sum" real?) (Attribute "max" real?) (Attribute "min" real?))))
      (for: ((key : (Pairof String String) '(("P1" . "S1") ("P1" . "S2") ("P2" . "S1") ("P2" . "S2") ("P2" . "S3") ("P2" . "S4") ("P3" . "S1") ("P4" . "S1") ("P4" . "S4") ("P5" . "S1") ("P5" . "S4") ("P6" . "S1")))
             (count : Real '(3 3 3 3 3 3 3 3 3 3 3 3))
             (sum : Real '(900 900 600 1200 600 600 1200 600 900 900 1200 900))
             (max : Real '(300 300 200 400 200 200 400 200 300 300 400 900))
             (min : Real '(300 300 200 400 200 200 400 200 300 300 400 900)))
            (let: ((trs : (Listof Triple) (Tuple-triples (car (Body-tuples (Relation-body (build-restriction r2 (And (Is eql (Att (Attribute "p#" string?)) (Val (car key))) (Is eql (Att (Attribute "s#" string?)) (Val (cdr key)))))))))))
                  (check-equal? (Triple-value (find-triple-for-attribute (Attribute "count" real?) trs)) count)
                  (check-equal? (Triple-value (find-triple-for-attribute (Attribute "sum" real?) trs)) sum)
                  )))

;(test-case
; "build-summarize without a grouping attribute (i.e., computing over all tuples"   
(let: ((r1 : Relation (build-summarize shipments empty_rel (list (Aggregation (AppAgg length (Attribute "dummy" real?)) "count") (Aggregation (AppAgg sum (Attribute "qty" real?)) "sum") (Aggregation (AppAgg max_ (Attribute "qty" real?)) "max") (Aggregation (AppAgg min_ (Attribute "qty" real?)) "min")))))
      ;(print-relation r1)
      (check-equal? (Relation-heading r1) (Heading (list (Attribute "count" real?) (Attribute "sum" real?) (Attribute "max" real?) (Attribute "min" real?))))
      (let: ((trs : (Listof Triple) (Tuple-triples (car (Body-tuples (Relation-body r1))))))
            (check-equal? (Triple-value (find-triple-for-attribute (Attribute "count" real?) trs)) 12)
            (check-equal? (Triple-value (find-triple-for-attribute (Attribute "sum" real?) trs)) 3500)
            (check-equal? (Triple-value (find-triple-for-attribute (Attribute "max" real?) trs)) 400)
            (check-equal? (Triple-value (find-triple-for-attribute (Attribute "min" real?) trs)) 200)))

;(parameterize ((default-join-method
;                 ;'hash
;                 ;'sort-merge
;                 'nested-loops))
;  (run-tests basic-relational-operators))
;
;(run-tests additional-relational-operators)


