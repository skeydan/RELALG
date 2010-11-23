#lang typed/racket
(require racket/match)
(require (planet dherman/types:2))
(require "helper-functions.rkt")

(provide (all-defined-out))


; Types ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;(define-datatype Value
;  (S ((s : String)))
;  (N ((n : Real))))
; (define-predicate Value? Value)
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
;

(define-struct: S ((s : String))
  #:property prop:equal+hash
  (list (lambda (s1 s2 equal?-recur)
          (string=? (S-s s1) (S-s s2)))
        (lambda (s hash-recur)
          (hash-recur (S-s s)))
        (lambda (s hash2-recur)
          (hash2-recur (S-s s)))))
 
(define-struct: N ((n : Real))
  #:property prop:equal+hash
  (list (lambda (n1 n2 equal?-recur)
          (= (N-n n1) (N-n n2)))
        (lambda (n hash-recur)
          (hash-recur (exact->inexact (N-n n))))
        (lambda (n hash2-recur)
          (hash2-recur (exact->inexact (N-n n))))))

(define-type Value (U N S)) 

(define-struct: Identifier ((name : Symbol)))

(define-struct: Triple ((name : String) (typename : Symbol) (value : Value))
  #:mutable
  #:property prop:equal+hash
    (list (lambda (t1 t2 equal?-recur) ; Attribute names are case insensitive, attribute type names are not; values are treated individually for each variant
            (and (string=? (string-upcase (Triple-name t1)) (string-upcase (Triple-name t2))) (eq? (Triple-typename t1) (Triple-typename t2)) (equal? (Triple-value t1) (Triple-value t2))))
          (lambda (t hash-recur)
            (+ (hash-recur (string-upcase (Triple-name t))) (* 3 (hash-recur (Triple-typename t))) (* 3 (hash-recur (Triple-value t)))))
           (lambda (t hash2-recur)
            (+ (hash2-recur (string-upcase (Triple-name t))) (* 3 (hash2-recur (Triple-typename t))) (* 3 (hash2-recur (Triple-value t)))))))

(define-type Triplist (Listof Triple))

(define-struct: Tuple ((triples : (Listof Triple)))
  #:property prop:equal+hash
    (list (lambda (t1 t2 equal?-recur)
            (let ((t1 (Tuple-triples t1)) (t2 (Tuple-triples t2))) (lists-same? t1 t2 equal?)))
          (lambda (h hash-recur)
            (apply + (map hash-recur (Tuple-triples h))))
          (lambda (h hash2-recur)
            (apply + (map hash2-recur (Tuple-triples h))))))

(define-type Tuplist (Listof Tuple))

(define-struct: Relation ((heading : Heading) (body : Body)))

(define-struct: Heading ((attrs : (Listof Attribute)))
   #:property prop:equal+hash
    (list (lambda (h1 h2 equal?-recur)
            (let ((attlist1 (Heading-attrs h1)) (attlist2 (Heading-attrs h2))) (lists-same? attlist1 attlist2 equal?)))
          (lambda (h hash-recur)
            (apply + (map hash-recur (Heading-attrs h))))
          (lambda (h hash2-recur)
            (apply + (map hash2-recur (Heading-attrs h))))))

(define-struct: Attribute ((name : String) (typename : Symbol))
  #:mutable
  #:property prop:equal+hash
    (list (lambda (a1 a2 equal?-recur) ; Attribute names are case insensitive, attribute type names are not
            (and (string=? (string-upcase (Attribute-name a1)) (string-upcase (Attribute-name a2))) (eq? (Attribute-typename a1) (Attribute-typename a2))))
          (lambda (a hash-recur)
            (+ (hash-recur (string-upcase (Attribute-name a))) (* 3 (hash-recur (Attribute-typename a)))))
           (lambda (a hash-recur)
            (+ (hash-recur (string-upcase (Attribute-name a))) (* 3 (hash-recur (Triple-typename a)))))))

(define-struct: Body ((tuples : (Listof Tuple))))

(define-datatype TupleExpr
  (Tuplevar ((id : Identifier)))
  (WithT ((t : TupleExpr) (id : Identifier)))
  (Tup ((t : Tuple))))
(define-predicate TupleEpr? TupleExpr)

(define-datatype RelExpr
 (Relvar ((id : Identifier)))
 (With ((rel : RelExpr) (id : Identifier)))
 (Rel ((rel : Relation)))
 (Union ((rel1 : RelExpr) (rel2 : RelExpr)))
 (Intersect ((rel1 : RelExpr) (rel2 : RelExpr)))
 (Difference ((rel1 : RelExpr) (rel2 : RelExpr)))
 (Product ((rel1 : RelExpr) (rel2 : RelExpr)))
 (Project ((rel : RelExpr) (attrs : Heading)))
 (Restrict ((rel : RelExpr) (predicate : Predicate)))
 (Join ((rel1 : RelExpr) (rel2 : RelExpr)))
 (Rename ((rel : RelExpr) (renamings : (Listof (Pair Attribute String)))))
 (Theta-Join ((rel1 : RelExpr) (rel2 : RelExpr) (p : Predicate)))
 (Divide ((rel11 : RelExpr) (rel2 : RelExpr) (rel3 : RelExpr))) ; Date's "Small Divide" only 
 (Semijoin ((rel1 : RelExpr) (rel2 : RelExpr)))
 (Semiminus ((rel1 : RelExpr) (rel2 : RelExpr)))
 (Extend ((r : RelExpr) (e : Extlist)))
 (Summarize ((r1 : RelExpr) (r2 : RelExpr) (a : Agglist)))
 )
(define-predicate RelExpr? RelExpr)

(define-struct: Extension ((operand : Operand) (name : String)))

(define-type Extlist (Listof Extension))

(define-struct: Aggregation ((aggop : AggOperand) (name : String)))

(define-type Agglist (Listof Aggregation))

(define-datatype Bool-Op
  (Greater #:constant greater)
  (GreaterEql #:constant greatereql)
  (Less #:constant less)
  (LessEql #:constant lesseql)
  (Eql #:constant eql)
  (NotEql #:constant noteql))
(define-predicate Bool-Op? Bool-Op)

(define-datatype Arith-Op
  (Plus #:constant _+)
  (Min #:constant _-)
  (Tim #:constant _*)
  (Div #:constant _/))
(define-predicate Arith-Op? Arith-Op)

(define-type Operator (U Arith-Op Bool-Op))
(define-predicate Operator? Operator)

(define-datatype Fun
  (Substr ((p : (case-lambda (String Exact-Nonnegative-Integer -> String) (String Exact-Nonnegative-Integer Exact-Nonnegative-Integer -> String)))))
  (StrApp ((p : (String * -> String)))))

(define-datatype AggFun
  (Count ((p : (All (a) ((Listof a) -> Exact-Nonnegative-Integer)))))
  (Sum ((p : ((Listof Number) -> Number)))))

(define-type Expr (U Predicate RelExpr Value Identifier TupleExpr))

(define-datatype Operand
  (Att ((att : Attribute)))
  (Val ((val : Value)))
  (AppOp ((op : Arith-Op) (o1 : Operand) (o2 : Operand)))
  (AppFun ((f : Fun) (o1 : Operand) (o2 : Operand))))

(define-datatype AggOperand
  (AppAggFun ((f : AggFun) (a : Attribute))))

(define-datatype Predicate
 (Is ((op : Bool-Op) (rand1 : Operand) (rand2 : Operand)))
 (Not ((p : Predicate)))
 (And ((p1 : Predicate) (p2 : Predicate)))
 (Or ((p1 : Predicate) (p2 : Predicate))))
(define-predicate Predicate? Predicate) 


; Constants ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(: empty_rel Relation)
(define empty_rel (make-Relation (make-Heading '()) (make-Body '())))

