#lang typed/racket
(require (planet dherman/types:2))
(require "helper-functions.rkt")

(provide (all-defined-out))


; Types ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define-type Value (U Real String)) 

(struct: Identifier ((name : Symbol)))

(define-struct: Triple ((name : String) (type : (U (Any -> Boolean : Real) (Any -> Boolean : String))) (value : Value))
  #:mutable
  #:property prop:equal+hash
    (list (lambda (t1 t2 equal?-recur) ; Attribute names are case insensitive, attribute type names are not; values are treated individually for each variant
            (and (string=? (string-upcase (Triple-name t1)) (string-upcase (Triple-name t2))) (eq? (Triple-type t1) (Triple-type t2)) (equal? (Triple-value t1) (Triple-value t2))))
          (lambda (t hash-recur)
            (+ (hash-recur (string-upcase (Triple-name t))) (* 3 (hash-recur (Triple-type t))) (* 3 (hash-recur (Triple-value t)))))
           (lambda (t hash2-recur)
            (+ (hash2-recur (string-upcase (Triple-name t))) (* 3 (hash2-recur (Triple-type t))) (* 3 (hash2-recur (Triple-value t)))))))

(struct: Tuple ((triples : (Listof Triple)))
  #:property prop:equal+hash
    (list (lambda (t1 t2 equal?-recur)
            (let ((t1 (Tuple-triples t1)) (t2 (Tuple-triples t2))) (lists-same? t1 t2 equal?)))
          (lambda (h hash-recur)
            (apply + (map hash-recur (Tuple-triples h))))
          (lambda (h hash2-recur)
            (apply + (map hash2-recur (Tuple-triples h))))))

(struct: Relation ((heading : Heading) (body : Body)))

(struct: Heading ((attrs : (Listof Attribute)))
   #:property prop:equal+hash
    (list (lambda (h1 h2 equal?-recur)
            (let ((attlist1 (Heading-attrs h1)) (attlist2 (Heading-attrs h2))) (lists-same? attlist1 attlist2 equal?)))
          (lambda (h hash-recur)
            (apply + (map hash-recur (Heading-attrs h))))
          (lambda (h hash2-recur)
            (apply + (map hash2-recur (Heading-attrs h))))))

(struct: Attribute ((name : String) (type : (U (Any -> Boolean : Real) (Any -> Boolean : String))))
  #:mutable
  #:property prop:equal+hash
    (list (lambda (a1 a2 equal?-recur) ; Attribute names are case insensitive, attribute type names are not
            (and (string=? (string-upcase (Attribute-name a1)) (string-upcase (Attribute-name a2))) (eq? (Attribute-type a1) (Attribute-type a2))))
          (lambda (a hash-recur)
            (+ (hash-recur (string-upcase (Attribute-name a))) (* 3 (hash-recur (Attribute-type a)))))
           (lambda (a hash-recur)
            (+ (hash-recur (string-upcase (Attribute-name a))) (* 3 (hash-recur (Attribute-type a)))))))

(struct: Body ((tuples : (Listof Tuple))))

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

(define-type Expr (U Predicate RelExpr Value Identifier TupleExpr))

(struct: Extension ((operand : Operand) (name : String)))

(define-type Extlist (Listof Extension))

(struct: Aggregation ((aggop : AggOperand) (name : String)))

(define-type Agglist (Listof Aggregation))

(define-datatype Bool-Op
  (Greater #:constant greater)
  (GreaterEql #:constant greatereql)
  (Less #:constant less)
  (LessEql #:constant lesseql)
  (Eql #:constant eql)
  (NotEql #:constant noteql))
(define-predicate Bool-Op? Bool-Op)

(define-datatype Operand
  (Att ((att : Attribute)))
  (Val ((val : Value)))
  (App ((f : Fun) (o1 : Operand) (o2 : Operand))))
(define-predicate Operand? Operand)

(define-datatype AggOperand
  (AppAggFun ((f : AggFun) (a : Attribute))))

(define-datatype Predicate
 (Is ((op : Bool-Op) (rand1 : Operand) (rand2 : Operand)))
 (Not ((p : Predicate)))
 (And ((p1 : Predicate) (p2 : Predicate)))
 (Or ((p1 : Predicate) (p2 : Predicate))))
(define-predicate Predicate? Predicate) 

(define-type Fun (U StringFun ArithFun))

(define-type AggFun
  (U
   (All (a) ((Listof a) -> Nonnegative-Fixnum)) ; length (=> count tuples)
   ))

(define-type StringFun
  (U
   (case-lambda (String Integer -> String) (String Integer Integer -> String)) ; substring
   (String * -> String) ; string-append
   ))

(define-type ArithFun
  (U
   (case-lambda (Integer Integer * -> Integer) (Exact-Rational Exact-Rational * -> Exact-Rational) (Float Float * -> Float) (Float Real * -> Float) (Real Float Real * -> Float) (Inexact-Real Inexact-Real * -> Inexact-Real) (Real Real * -> Real) ((U Inexact-Complex Inexact-Real Exact-Rational) * -> Inexact-Complex) (Inexact-Complex Complex * -> Inexact-Complex) (Complex Inexact-Complex Complex * -> Inexact-Complex) (Complex Complex * -> Complex)) ; -
   (case-lambda (Exact-Positive-Integer Natural * -> Exact-Positive-Integer) (Natural Exact-Positive-Integer Natural * -> Exact-Positive-Integer) (Natural * -> Natural) (Integer * -> Integer) (Exact-Rational * -> Exact-Rational) (Nonnegative-Float * -> Nonnegative-Float) (Float * -> Float) ((U Nonnegative-Float Exact-Positive-Integer Zero) * -> Nonnegative-Float) (Float Real * -> Float) (Real Float Real * -> Float) (Inexact-Real * -> Inexact-Real) (Real * -> Real) ((U Inexact-Complex Inexact-Real Exact-Rational) * -> Inexact-Complex) (Inexact-Complex Complex * -> Inexact-Complex) (Complex Inexact-Complex Complex * -> Inexact-Complex) (Complex * -> Complex)) ; +
   (case-lambda (Exact-Positive-Integer * -> Exact-Positive-Integer) (Natural * -> Natural) (Integer * -> Integer) (Exact-Rational * -> Exact-Rational) (Nonnegative-Float * -> Nonnegative-Float) (Float * -> Float) ((U Nonnegative-Float Exact-Positive-Integer) * -> Nonnegative-Float) ((U Float Exact-Positive-Integer) * -> Float) (Float Inexact-Real * -> Float) (Inexact-Real Float Inexact-Real * -> Float) (Inexact-Real * -> Inexact-Real) (Real * -> Real) ((U Inexact-Complex Float) * -> Inexact-Complex) (Complex * -> Complex)) ; *
   (case-lambda (Integer Integer * -> Exact-Rational) (Exact-Rational Exact-Rational * -> Exact-Rational) (Float Float * -> Float) (Float Real * -> Float) (Inexact-Real Float Inexact-Real * -> Float) (Inexact-Real Inexact-Real * -> Inexact-Real) (Real Real * -> Real) ((U Inexact-Complex Float) (U Inexact-Complex Inexact-Real Exact-Rational) * -> Inexact-Complex) (Inexact-Complex Inexact-Complex * -> Inexact-Complex) (Complex Complex * -> Complex)) ; /
   ))


; Constants ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(: empty_rel Relation)
(define empty_rel (Relation (Heading '()) (Body '())))


