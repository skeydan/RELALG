#lang typed-scheme/no-check

(require rackunit)
(require rackunit/text-ui)
(require "../data/data.rkt")
(require "../types.rkt")
(require "../type-functions.rkt")
(require "../eval.rkt")
(require "../relation-utils.rkt")
(require "../settings.rkt")

(define-test-suite basic-relational-operators
  
  (test-case
   "build-union"
   (let*: ((a : Attribute (make-Attribute "city" 'S))
           (r1 : Relation (make-Relation (Relation-heading suppliers) (make-Body (filter (lambda: ((x : Tuple)) (equal? (extract-value-variant (Triple-value (find-triple-for-attribute a (Tuple-triples x)))) "London")) (Body-tuples (Relation-body suppliers))))))
           (r2 : Relation (make-Relation (Relation-heading suppliers) (make-Body (filter (lambda: ((x : Tuple)) (equal? (extract-value-variant (Triple-value (find-triple-for-attribute a (Tuple-triples x)))) "Paris")) (Body-tuples (Relation-body suppliers))))))
           (r3 : Relation (make-Relation (Relation-heading suppliers) (make-Body (filter (lambda: ((x : Tuple)) (let: ((s : String (extract-value-variant (Triple-value (find-triple-for-attribute a (Tuple-triples x)))))) (or (equal? s "Paris") (equal? s "Athens")))) (Body-tuples (Relation-body suppliers)))))))
          (let: ((r1_2 : Relation (build-union r1 r2))
                 (r1_3 : Relation (build-union r1 r3))
                 (r2_3 : Relation (build-union r2 r3)))
                ;(print-relation r1_3)
                (check = 4 (cardinality r1_2))
                (check = 5 (cardinality r1_3))
                (check = 3 (cardinality r2_3)))))
  
  (test-case
   "build-intersect"
   (let*: ((a : Attribute (make-Attribute "color" 'S)) 
           (r1 : Relation (make-Relation (Relation-heading parts) (make-Body (filter (lambda: ((x : Tuple)) (equal? (extract-value-variant (Triple-value (find-triple-for-attribute a (Tuple-triples x)))) "Red")) (Body-tuples (Relation-body parts))))))
           (r2 : Relation (make-Relation (Relation-heading parts) (make-Body (filter (lambda: ((x : Tuple)) (equal? (extract-value-variant (Triple-value (find-triple-for-attribute a (Tuple-triples x)))) "Green")) (Body-tuples (Relation-body parts))))))
           (r3 : Relation (make-Relation (Relation-heading parts) (make-Body (filter (lambda: ((x : Tuple)) (let: ((s : String (extract-value-variant (Triple-value (find-triple-for-attribute a (Tuple-triples x)))))) (or (equal? s "Green") (equal? s "Red")))) (Body-tuples (Relation-body parts)))))))
          (let: ((r1_2 : Relation (build-intersect r1 r2))
                 (r1_3 : Relation (build-intersect r1 r3))
                 (r2_3 : Relation (build-intersect r2 r3)))
                ;(print-relation r1_2)
                (check = 0 (cardinality r1_2))
                (check = 3 (cardinality r1_3))
                (check = 1 (cardinality r2_3)))))
  
  
  (test-case
   "build-difference"
   (let*: ((r1 : Relation (make-Relation (Relation-heading suppliers) (make-Body (filter (lambda: ((x : Tuple)) (equal? (extract-value-variant (Triple-value (find-triple-for-attribute (make-Attribute "city" 'S) (Tuple-triples x)))) "London")) (Body-tuples (Relation-body suppliers))))))
           (r2 : Relation (build-difference suppliers r1)))
          ;(print-relation r)
          (check-equal? (Relation-heading r2) (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "city" 'S))))
          (check = 3 (cardinality r2))))
  
  (test-case
   "build-product"
   (let*: ((r1 : Relation (build-projection shipments (list (make-Attribute "p#" 'S) (make-Attribute "qty" 'N))))
           (r2 : Relation (build-product suppliers r1)))
          ;(print-relation r1)
          (check-equal? (Relation-heading r2) (make-Heading (append (Heading-attrs (Relation-heading suppliers)) (Heading-attrs (Relation-heading r1)))))
          (check =  (* (cardinality suppliers) (cardinality r1)) (cardinality r2))))
  
  (test-case
   "build-join, joining 2 relations on s#"
   (let: ((r : Relation (build-join suppliers shipments)))
         ;(print-relation r)
         (check-equal? (Relation-heading r) (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "city" 'S) (make-Attribute "p#" 'S) (make-Attribute "qty" 'N))))
         (check = (cardinality shipments) (cardinality r))))
  
  (test-case
   "build-join, joining 3 relations on s#, p#, city in different orders"
   (let: ((r1 : Relation (build-join parts (build-join suppliers shipments)))
          (r2 : Relation (build-join suppliers (build-join parts shipments)))
          (r3 : Relation (build-join shipments (build-join suppliers parts))))
         ;(print-relation r1)
         (check-equal? (Relation-heading r1) (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "p#" 'S) (make-Attribute "city" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N)  (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "qty" 'N))))
         (check-equal? (Relation-heading r1) (Relation-heading r2))
         (check-equal? (Relation-heading r1) (Relation-heading r3))
         (check = (cardinality r1) 6)
         (check = (cardinality r2) 6)
         (check = (cardinality r3) 6)))
  
  (test-case
   "build-join, joining 3 relations on s#, p# with city in parts relation projected away"
   (let: ((r : Relation (build-join (build-projection parts (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N)))(build-join suppliers shipments))))
         ;(print-relation r)
         (check-equal? (Relation-heading r) (make-Heading (list (make-Attribute "s#" 'S) (make-Attribute "p#" 'S) (make-Attribute "city" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N)  (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "qty" 'N))))
         (check = (cardinality r) (cardinality shipments))))
  
  (test-case
   "build-projection"
   (let: ((r : Relation (build-projection parts (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N)))))
         ;(print-relation r)
         (check = (cardinality r) 6)
         (check-equal? (Relation-heading r) (make-Heading (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N))))))
  
  (test-case
   "build-projection, testing for removal of duplicates"
   (let: ((r1 : Relation (build-projection parts (list (make-Attribute "color" 'S))))
          (r2 : Relation (build-projection (build-join suppliers shipments) (list (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "city" 'S)))))
         ;(for-each print-relation (list r1 r2))
         (check = (cardinality r1) 3)
         (check = (cardinality r2) 4)
         (check-equal? (Relation-heading r1) (make-Heading (list (make-Attribute "color" 'S))))
         (check-equal? (Relation-heading r2) (make-Heading (list (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "city" 'S))))))
  
  (test-case
   "build-restriction with simple predicates, testing for different comparison operators"
   (let: ((r1 : Relation (build-restriction parts (make-Is eql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London")))))
          (r2 : Relation (build-restriction parts (make-Is noteql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London")))))
          (r3 : Relation (build-restriction parts (make-Is less (make-Att (make-Attribute "weight" 'N)) (make-Val (make-N 14)))))
          (r4 : Relation (build-restriction parts (make-Is lesseql (make-Att (make-Attribute "weight" 'N)) (make-Val (make-N 14)))))
          (r5 : Relation  (build-restriction parts (make-Is greater (make-Att (make-Attribute "weight" 'N)) (make-Val (make-N 14)))))
          (r6 : Relation  (build-restriction parts (make-Is greatereql (make-Att (make-Attribute "weight" 'N)) (make-Val (make-N 14))))))
         ;(for-each print-relation (list r1 r2 r3 p4 r5 r6))
         (check-equal? (Relation-heading r1) (Relation-heading r2))
         (check = (cardinality r1) 3)
         (check = (cardinality r2) 3)
         (check = (cardinality r3) 2)
         (check = (cardinality r4) 3)
         (check = (cardinality r5) 3)
         (check = (cardinality r6) 4)))
  
  (test-case
   "build-restriction with simple predicates, testing Attributes vs. Values as Operands"
   (let: ((r1 : Relation (build-restriction parts (make-Is greatereql (make-Val (make-N 14)) (make-Att (make-Attribute "weight" 'N)))))
          (r2 : Relation (build-restriction parts (make-Is greatereql (make-Val (make-N 14)) (make-Val (make-N 17)))))
          (r3 : Relation (build-restriction parts (make-Is less (make-Att (make-Attribute "pname" 'S)) (make-Att (make-Attribute "color" 'S))))))
         ;(for-each print-relation (list r1 r2 r3))
         (check = (cardinality r1) 3)
         (check = (cardinality r2) 0)
         (check = (cardinality r3) 3)))
  
  (test-case
   "build-restriction with compound predicates"
   (let: ((r1 : Relation (build-restriction parts (make-And (make-Is eql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))) (make-Is noteql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))))))
          (r2 : Relation (build-restriction parts (make-Or (make-Is eql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))) (make-Is noteql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))))))
          (r3 : Relation (build-restriction parts (make-And (make-Is eql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))) (make-Is noteql (make-Att (make-Attribute "pname" 'S)) (make-Val (make-S "Screw"))))))
          (r4 : Relation (build-restriction parts (make-Or (make-Is noteql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))) (make-Is eql  (make-Att (make-Attribute "color" 'S)) (make-Val (make-S "Red"))))))
          (r5 : Relation (build-restriction parts (make-Not (make-And (make-Is eql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))) (make-Is noteql (make-Att (make-Attribute "pname" 'S)) (make-Val (make-S "Screw")))))))
          (r6 : Relation (build-restriction parts (make-Not (make-Or (make-Is noteql (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "London"))) (make-Is eql (make-Att (make-Attribute "color" 'S)) (make-Val (make-S "Red"))))))))
         ;(for-each print-relation (list r1 r2 r3 r4 r5 r6))
         (check = (cardinality r1) 0)
         (check = (cardinality r2) 6)
         (check = (cardinality r3) 2)
         (check = (cardinality r4) 6)
         (check = (cardinality r5) 4)
         (check = (cardinality r6) 0)))
  
  (test-case
   "build-renamed-relation"
   (let: ((r1 : Relation (build-renamed-relation parts (list (cons (make-Attribute "city" 'S) "location") (cons (make-Attribute "color" 'S) "howitlooks"))))
          (r2 : Relation (build-renamed-relation parts '())))
         ;(for-each print-relation (list r1 r2))
         (check-equal? (Relation-heading r1) (make-Heading (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "howitlooks" 'S) (make-Attribute "weight" 'N) (make-Attribute "location" 'S))))
         (check-equal? (Relation-heading r2) (make-Heading (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N) (make-Attribute "city" 'S))))))
  
  (test-case
   "build-theta"
   (let: ((r1 : Relation (build-theta (make-Is greater (make-Att (make-Attribute "pname" 'S)) (make-Att (make-Attribute "sname" 'S))) parts (build-projection suppliers (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N)))))
          (r2 : Relation (build-theta (make-Is greater (make-Att (make-Attribute "p#" 'S)) (make-Att (make-Attribute "s#" 'S))) parts (build-projection suppliers (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N))))))
         ; (print-relation r1)
         (check = (cardinality r1) 19)
         (check = (cardinality r2) 0)))
  
  (test-case
   "build-theta throws error when there are overlapping attributes"
   (check-exn exn? (lambda () (build-theta (make-Is greater (make-Att (make-Attribute "pname" 'S)) (make-Att (make-Attribute "sname" 'S))) parts suppliers))))
  
  (test-case
   "build-divide"
   (let: ((suppliers_s# : Relation (build-projection suppliers (list (make-Attribute "s#" 'S))))
          (parts_p# : Relation (build-projection parts (list (make-Attribute "p#" 'S))))
          (shipments_s#p# : Relation (build-projection shipments (list (make-Attribute "s#" 'S) (make-Attribute "p#" 'S)))))
         (let: ((r1 : Relation (build-divide suppliers_s# parts_p# shipments_s#p#))
                (r2 : Relation (build-divide suppliers_s# (build-restriction parts_p# (make-Is eql (make-Att (make-Attribute "p#" 'S)) (make-Val (make-S "P1")))) shipments_s#p#))
                (r3 : Relation (build-divide suppliers_s# (build-restriction parts_p# (make-Or (make-Is eql (make-Att (make-Attribute "p#" 'S)) (make-Val (make-S "P2"))) (make-Is eql (make-Att (make-Attribute "p#" 'S)) (make-Val (make-S "P4"))))) shipments_s#p#)))
               ;(for-each print-relation (list r1 r2 r3))
               (check = (cardinality r1) 1)
               (check = (cardinality r2) 2)
               (check = (cardinality r3) 2)))))

(define-test-suite additional-relational-operators
  
  (test-case
   "build-semijoin"
   (let: ((r : Relation (build-semijoin suppliers (build-restriction shipments (make-Is eql (make-Att (make-Attribute "p#" 'S)) (make-Val (make-S "P2")))))))
         ;(print-relation r)
         (check-equal? (Relation-heading r) (Relation-heading suppliers))
         (check = (cardinality r) 4)))
  
  (test-case
   "build-semiminus"
   (let: ((r : Relation (build-semiminus suppliers (build-restriction shipments (make-Is eql (make-Att (make-Attribute "p#" 'S)) (make-Val (make-S "P2")))))))
         ;(print-relation r)
         (check-equal? (Relation-heading r) (Relation-heading suppliers))
         (check = (cardinality r) 1)))
  
  (test-case
   "build-extension using constant values, attributes, operator applications and function applications"
   (let: ((r : Relation (build-extension parts (list (make-Extension (make-Val (make-S "dummy")) "default") (make-Extension (make-Att (make-Attribute "p#" 'S)) "id") (make-Extension (make-AppOp _/ (make-Att (make-Attribute "weight" 'N)) (make-Val (make-N 100))) "hectograms") (make-Extension (make-AppFun strapp (make-Att (make-Attribute "city" 'S)) (make-Val (make-S "derry"))) "newcity")))))
         ;(print-relation r)
         (check-equal? (Relation-heading r) (make-Heading (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N) (make-Attribute "city" 'S) (make-Attribute "default" 'S) (make-Attribute "id" 'S) (make-Attribute "hectograms" 'N) (make-Attribute "newcity" 'S))))
         (check-equal? (make-S "dummy") (Triple-value (find-triple-for-attribute (make-Attribute "default" 'S) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
         (check-equal? (make-S "P1") (Triple-value (find-triple-for-attribute (make-Attribute "id" 'S) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
         (check-equal? (make-N 0.12) (Triple-value (find-triple-for-attribute (make-Attribute "hectograms" 'N) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
         (check-equal? (make-S "Londonderry") (Triple-value (find-triple-for-attribute (make-Attribute "newcity" 'S) (Tuple-triples (car (Body-tuples (Relation-body r)))))))
         (check = (degree r) (+ 4 (degree parts)))
         (check = (cardinality r) (cardinality parts)))))

(parameterize ((default-join-method
                 ;'hash))
                 'sort-merge))
  ;'nested-loops))
  (run-tests basic-relational-operators))

(run-tests additional-relational-operators)

;(test/gui basic-relational-operators)

; build-summarize
;(test-case
; "build-summarize with one grouping attribute"
; (let ((r (build-summarize shipments (make-Project parts (make-Heading (list (make-Attribute "p#" 'S)))) (make-Agglist (list (make-Aggregation (make-AppAggFun  (make-Sum length) (make-Attribute "totQty" 'N))))))))
;   (print-relation r)
;   (check-equal? (Relation-heading r) (make-Heading (list (make-Attribute "p#" 'S) (make-Attribute "totQty" 'N))))
;   ))
;   

