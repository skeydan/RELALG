#lang typed/racket

(require racket/match)
(require racket/list)
(require "types.rkt")
(require "type-functions.rkt")
(require "relation-utils.rkt")
(require "helper-functions.rkt")

(provide (all-defined-out))

(: eval-relexpr (RelExpr -> Rel))
(define eval-relexpr
  (lambda (expr)
    (match expr
      ((struct Rel (r)) (make-Rel r))
      ((struct Union (r1 r2)) (make-Rel (build-union (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Intersect (r1 r2)) (make-Rel (build-intersect (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Difference (r1 r2)) (make-Rel (build-difference (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Product (r1 r2)) (make-Rel (build-product (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Join (r1 r2)) (make-Rel (build-join (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Project (r h)) (make-Rel (build-projection (Rel-rel (eval-relexpr r)) (Heading-attrs h))))
      ((struct Restrict (r p)) (make-Rel (build-restriction (Rel-rel (eval-relexpr r)) p)))
      ((struct Rename (r l)) (make-Rel (build-renamed-relation (Rel-rel (eval-relexpr r)) l)))
      ((struct Theta-Join (r1 r2 p)) (make-Rel (build-theta p (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Divide (r1 r2 r3)) (make-Rel (build-divide (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)) (Rel-rel (eval-relexpr r3)))))
      ((struct Semijoin (r1 r2)) (make-Rel (build-semijoin (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Semiminus (r1 r2)) (make-Rel (build-semiminus (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)))))
      ((struct Extend (r e)) (make-Rel (build-extension (Rel-rel (eval-relexpr r)) e)))
      ;((struct Summarize (r1 r2 a)) (make-Rel (build-summarize (Rel-rel (eval-relexpr r1)) (Rel-rel (eval-relexpr r2)) a)))
      (else (make-Rel empty_rel)))))


; These functions implement the relational algebra operations ----------------------------------------------------------------------------------------------------------------------

; Codd's original eight operators, plus rename -------------------------------------------------------------------------------------------------------------------------------------

; UNION: builds the union of 2 relations, removing any duplicates
(: build-union (Relation Relation -> Relation))
(define build-union
  (lambda (r1 r2)
    (if (not (equal? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use UNION on relations with different headings!" r1 r2)
        (make-Relation (Relation-heading r1) (make-Body (unique-tuples r1 r2))))))

; INTERSECTION: builds the union of 2 relations, removing any duplicates
(: build-intersect (Relation Relation -> Relation))
(define build-intersect
  (lambda (r1 r2)
    (if (not (equal? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use INTERSECT on relations with different headings!" r1 r2)
        (make-Relation (Relation-heading r1) (make-Body (common-tuples r1 r2))))))

; DIFFERENCE: subtracts the 2nd relation's tuples from the first
(: build-difference (Relation Relation -> Relation))
(define build-difference
  (lambda (r1 r2)
    (if (not (equal? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use DIFFERENCE on relations with different headings!" r1 r2)
        (make-Relation (Relation-heading r1) (make-Body (tuples-difference r1 r2))))))

; PRODUCT: builds the cartesian product of 2 relations
(: build-product (Relation Relation -> Relation))
(define build-product
  (lambda (r1 r2)
    (if (not (attribute-names-disjoint? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use PRODUCT on relations with overlapping attribute names!" r1 r2)
        (make-Relation (cartesian-heading (Relation-heading r1) (Relation-heading r2)) (make-Body (cartesian-tuples r1 r2))))))

; JOIN: builds the join of 2 relations
; if r1 and 2 r2 have no attributes in common, the cartesian product is formed; if all attributes are the same, the intersection is formed; otherwise, r1 and r2 are joined on all common attributes.
(: build-join (Relation Relation -> Relation))
(define build-join
  (lambda (r1 r2)
    (let ((h1 (Relation-heading r1)) (h2 (Relation-heading r2)))
      (cond ((attribute-names-disjoint? h1 h2) (build-product r1 r2))
            ((equal? h1 h2) (build-intersect r1 r2))
            (else (let ((joining-attributes (common-attributes h1 h2)))
                    (make-Relation (joined-heading joining-attributes h1 h2) (make-Body (joined-tuples joining-attributes r1 r2)))))))))

; THETA-JOIN: joins two relations on a non-equality operator (though it would also work with equality). The attributes used in the join need to have different names.
; Theta-join is implemented as a restriction (on the given predicate) following a cartesian product.
(: build-theta (Predicate Relation Relation -> Relation))
(define build-theta
  (lambda (p r1 r2)
    (if (not (attribute-names-disjoint? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot make theta-join on relations with overlapping attribute names - rename first!" r1 r2)
        (build-restriction (build-product r1 r2) p))))

; RESTRICT: returns a filtered relation, keeping just those tuples that satisfy the given predicate.
(: build-restriction (Relation Predicate -> Relation))
(define build-restriction
  (lambda (r p)
    (let* ((given-tuples (Body-tuples (Relation-body r)))
          (result 
           (match p
             ((struct Is (op rand1 rand2))
              (filter (lambda: ((x : Tuple)) ((get-comp-function op) (eval-operand x rand1) (eval-operand x rand2))) (Body-tuples (Relation-body r))))
              ((struct Not (p)) (match p
                                  ((struct Is (op att val)) (Body-tuples (Relation-body (build-restriction r (make-Is (negate op) att val)))))
                                  ((struct Not (p3)) (Body-tuples (Relation-body (build-restriction r p3))))
                                  ((struct And (p1 p2)) (Body-tuples (Relation-body (build-restriction r (make-Or (make-Not p1) (make-Not p2))))))
                                  ((struct Or (p1 p2)) (Body-tuples (Relation-body (build-restriction r (make-And (make-Not p1) (make-Not p2))))))))
              ((struct And (p1 p2)) (Body-tuples (Relation-body (build-intersect (build-restriction r p1) (build-restriction r p2)))))
              ((struct Or (p1 p2)) (Body-tuples (Relation-body (build-union (build-restriction r p1) (build-restriction r p2))))))))
      (make-Relation (Relation-heading r) (make-Body result)))))

; DIVIDE: returns a new relation, keeping only those tuples from r1 for which every combination with a tuple from r2 is contained in r3.
; = Date's "small divide"
; originally intended by Codd as an algebraic counterpart of the ALL operator of relational calculus - answers questions like "get all suppliers (r1) wh supply all (r3) parts (r2)
(: build-divide (Relation Relation Relation -> Relation))
(define build-divide
  (lambda (dividend divisor per)
    (let* ((dividend-tuples (Body-tuples (Relation-body dividend))) 
           (divisor-tuples (Body-tuples (Relation-body divisor)))
           (per-tuples (Body-tuples (Relation-body per)))
           (quotient-tuples
            (filter
             (lambda: ((x : Tuple)) (andmap (lambda: ((z : Tuple)) (contains? per-tuples z equal?)) (append-every-tuple-from-list x divisor-tuples)))
             dividend-tuples)))
      (make-Relation (Relation-heading dividend) (make-Body quotient-tuples)))))

; RENAME: returns a new relation with attributes renamed as specified in the given list of attributes & new names.
; Note: the passed-in relation's heading is not modified.
(: build-renamed-relation (Relation (Listof (Pair Attribute String)) -> Relation))
(define build-renamed-relation
  (lambda (r renamings)
    (let ((new-r (new-relation-from r)))
      (begin (replace-attnames renamings (Heading-attrs (Relation-heading new-r)))
             (replace-triplenames renamings (Body-tuples (Relation-body new-r)))
             new-r))))

; PROJECT: projects the given relation on the given set of attributes.
; Removes any duplicates that might have come into existence through this operation.
(: build-projection (Relation (Listof Attribute) -> Relation))
(define build-projection
  (lambda (r atts)
    (make-Relation (make-Heading (filter (lambda: ((x : Attribute)) (contains? atts x equal?)) (Heading-attrs (Relation-heading r))))
                   (make-Body (remove-duplicates (map (lambda: ((x : Tuple)) (make-Tuple (filter (lambda: ((y : Triple)) (contains? atts y triple-attribute=?)) (Tuple-triples x))))  (Body-tuples (Relation-body r))) equal?)))))



; Additional operators as of C. J. Date  -------------------------------------------------------------------------------------------------------------------------------------------

; SEMIJOIN: builds a new relation from r1, retaining only those tuples for which a matching tuple (= a tuple containing the same values in attributes common to both relations) exists.
; Current implementation is a projection following a join - this will reuse any join optimization. TODO: A possible alternative might be just directly hashing the values in common attributes occurring in r2 and probing the r1 tuples against the hashtable (thus avoiding the projection step).
(: build-semijoin (Relation Relation -> Relation))
(define build-semijoin
  (lambda (r1 r2)
    (let ((h1 (Relation-heading r1)) (h2 (Relation-heading r2)))
      (cond ((attribute-names-disjoint? h1 h2) (error "build-semijoin: Makes no sense using semijoin on relations without common attributes!" r1 r2))
            ((equal? h1 h2) (error "build-semijoin: Makes no sense using semijoin on relations with identical attributes, just use intersect here!" r1 r2))
            (else (let ((joining-attributes (common-attributes h1 h2)))
                    (build-projection (make-Relation (joined-heading joining-attributes h1 h2) (make-Body (joined-tuples joining-attributes r1 r2))) (Heading-attrs (Relation-heading r1)))))))))

; SEMIMINUS: builds a new relation from r1, retaining only those tuples for which a matching tuple does not exist in r2.
; Current implementation is a semijoin followed by a difference. TODO: Try out a hashing-only solution, skipping the difference and projection step.
(: build-semiminus (Relation Relation -> Relation))
(define build-semiminus
  (lambda (r1 r2)
    (build-difference r1 (build-semijoin r1 r2))))

; EXTEND: extends a relation, adding new attributes by applying the operations specified in the "extension list" attribute
(: build-extension (Relation Extlist -> Relation))
(define build-extension
  (lambda (r e)
    (let* ((r-tuples (Body-tuples (Relation-body r)))
           (result (map (lambda: ((x : Tuple)) (append-new-triples x e)) r-tuples)))
      (make-Relation (extended-heading r e) (make-Body result)))))


; SUMMARIZE: summarizes r1 as per the distinct value combinations (or values, if r2 has one attribute only) of r2. I.e. for every tuple of r2 (duplicates are not allowed), a summary of certain attributes in r1 is computed. The relation returned contains all attributes of r2, plus the newly requested summarizing attributes. 
;(: build-summarize (Relation Relation Agglist))
    











