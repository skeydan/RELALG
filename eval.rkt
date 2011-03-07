#lang typed/racket

(require racket/match)
(require racket/list)
(require "types.rkt")
(require "type-functions.rkt")
(require "relation-utils.rkt")
(require "helper-functions.rkt")

(provide (all-defined-out))

(: eval-relexpr (RelExpr -> Relation))
(define eval-relexpr
  (lambda (expr)
    (match expr
      ((struct Rel (r)) r)
      ((struct Union (r1 r2)) (build-union (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Intersect (r1 r2)) (build-intersect (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Difference (r1 r2)) (build-difference (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Product (r1 r2)) (build-product (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Join (r1 r2)) (build-join (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Project (r h)) (build-projection (eval-relexpr r) (Heading-attrs h)))
      ((struct Restrict (r p)) (build-restriction (eval-relexpr r) p))
      ((struct Rename (r l)) (build-renamed-relation (eval-relexpr r) l))
      ((struct Theta-Join (r1 r2 p)) (build-theta p (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Divide (r1 r2 r3)) (build-divide (eval-relexpr r1) (eval-relexpr r2) (eval-relexpr r3)))
      ((struct Semijoin (r1 r2)) (build-semijoin (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Semiminus (r1 r2)) (build-semiminus (eval-relexpr r1) (eval-relexpr r2)))
      ((struct Extend (r e)) (build-extension (eval-relexpr r) e))
      ((struct Image (r)) (error "eval: cannot evaluate Image standing alone"))
      ((struct Summarize (r1 r2 a)) (build-summarize (eval-relexpr r1) (eval-relexpr r2) a))
      (else (error "eval: not a relational expression: " expr)))))

(: print-relexpr (RelExpr -> Void))
(define (print-relexpr r)
  (print-relation (eval-relexpr r)))

; These functions implement the relational algebra operations ----------------------------------------------------------------------------------------------------------------------

; Codd's original eight operators, plus rename -------------------------------------------------------------------------------------------------------------------------------------

; UNION: builds the union of 2 relations, removing any duplicates
(: build-union (Relation Relation -> Relation))
(define build-union
  (lambda (r1 r2)
    (if (not (equal? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use UNION on relations with different headings!" r1 r2)
        (Relation (Relation-heading r1) (Body (unique-tuples r1 r2))))))

; INTERSECTION: builds the union of 2 relations, removing any duplicates
(: build-intersect (Relation Relation -> Relation))
(define build-intersect
  (lambda (r1 r2)
    (if (not (equal? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use INTERSECT on relations with different headings!" r1 r2)
        (Relation (Relation-heading r1) (Body (common-tuples r1 r2))))))

; DIFFERENCE: subtracts the 2nd relation's tuples from the first
(: build-difference (Relation Relation -> Relation))
(define build-difference
  (lambda (r1 r2)
    (if (not (equal? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use DIFFERENCE on relations with different headings!" r1 r2)
        (Relation (Relation-heading r1) (Body (tuples-difference r1 r2))))))

; PRODUCT: builds the cartesian product of 2 relations
(: build-product (Relation Relation -> Relation))
(define build-product
  (lambda (r1 r2)
    (if (not (attribute-names-disjoint? (Relation-heading r1) (Relation-heading r2)))
        (error "Cannot use PRODUCT on relations with overlapping attribute names!" r1 r2)
        (Relation (cartesian-heading (Relation-heading r1) (Relation-heading r2)) (Body (cartesian-tuples r1 r2))))))

; JOIN: builds the join of 2 relations
; if r1 and 2 r2 have no attributes in common, the cartesian product is formed; if all attributes are the same, the intersection is formed; otherwise, r1 and r2 are joined on all common attributes.
(: build-join (Relation Relation -> Relation))
(define build-join
  (lambda (r1 r2)
    (let ((h1 (Relation-heading r1)) (h2 (Relation-heading r2)))
      (cond ((attribute-names-disjoint? h1 h2) (build-product r1 r2))
            ((equal? h1 h2) (build-intersect r1 r2))
            (else (let ((joining-attributes (common-attributes h1 h2)))
                    (Relation (joined-heading joining-attributes h1 h2) (Body (joined-tuples joining-attributes r1 r2)))))))))

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
              (filter (lambda: ((x : Tuple)) ((get-comp-function op) (eval-tuple-with-operand x rand1) (eval-tuple-with-operand x rand2))) (Body-tuples (Relation-body r))))
              ((struct Not (p)) (match p
                                  ((struct Is (op att val)) (Body-tuples (Relation-body (build-restriction r (Is (negate op) att val)))))
                                  ((struct Not (p3)) (Body-tuples (Relation-body (build-restriction r p3))))
                                  ((struct And (p1 p2)) (Body-tuples (Relation-body (build-restriction r (Or (Not p1) (Not p2))))))
                                  ((struct Or (p1 p2)) (Body-tuples (Relation-body (build-restriction r (And (Not p1) (Not p2))))))))
              ((struct And (p1 p2)) (Body-tuples (Relation-body (build-intersect (build-restriction r p1) (build-restriction r p2)))))
              ((struct Or (p1 p2)) (Body-tuples (Relation-body (build-union (build-restriction r p1) (build-restriction r p2))))))))
      (Relation (Relation-heading r) (Body result)))))

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
      (Relation (Relation-heading dividend) (Body quotient-tuples)))))

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
    (Relation (Heading (filter (lambda: ((x : Attribute)) (contains? atts x equal?)) (Heading-attrs (Relation-heading r))))
                   (Body (remove-duplicates (map (lambda: ((x : Tuple)) (Tuple (filter (lambda: ((y : Triple)) (contains? atts y triple-attribute=?)) (Tuple-triples x))))  (Body-tuples (Relation-body r))) equal?)))))



; Additional operators as of C. J. Date  -------------------------------------------------------------------------------------------------------------------------------------------

; SEMIJOIN: builds a new relation from r1, retaining only those tuples for which a matching tuple (= a tuple containing the same values in attributes common to both relations) exists.
; Current implementation is a projection following a join - this will reuse any join optimization.
(: build-semijoin (Relation Relation -> Relation))
(define build-semijoin
  (lambda (r1 r2)
    (let ((h1 (Relation-heading r1)) (h2 (Relation-heading r2)))
      (cond ((attribute-names-disjoint? h1 h2) (build-product r1 r2))
            ((equal? h1 h2) (build-intersect r1 r2))
            (else (let ((joining-attributes (common-attributes h1 h2)))
                    (build-projection (Relation (joined-heading joining-attributes h1 h2) (Body (joined-tuples joining-attributes r1 r2))) (Heading-attrs (Relation-heading r1)))))))))

; SEMIMINUS: builds a new relation from r1, retaining only those tuples for which a matching tuple does not exist in r2.
; Current implementation is a semijoin followed by a difference.
(: build-semiminus (Relation Relation -> Relation))
(define build-semiminus
  (lambda (r1 r2)
    (build-difference r1 (build-semijoin r1 r2))))

; EXTEND: extends a relation, adding new attributes by applying the operations specified in the "extension list" attribute
(: build-extension (Relation Extlist -> Relation))
(define build-extension
  (lambda (r e)
    (let* ((r-tuples (Body-tuples (Relation-body r)))
           (result (map (lambda: ((x : Tuple)) (calculate-extension-triples x e)) r-tuples)))
      (Relation (extended-heading r e) (Body result)))))


; IMAGE: for one tuple from a relation r1, returns in a relation all tuples from the given relation r2 that match the tuple on the joining attributes, projected on all the non-common attributes.
(: build-image : (Tuple Relation -> Relation))
(define build-image
  (lambda (t r)
    (let: ((h : Heading (Heading (attlistfromtuple t))))
          (build-projection (build-semijoin r (Relation h (Body (list t)))) (attributes-difference (Relation-heading r) h)))))

; SUMMARIZE: summarizes r1 as per the distinct value combinations (or values, if r2 has one attribute only) of r2. I.e. for every tuple of r2 (duplicates are not allowed), a summary of certain attributes in r1 is computed. The relation returned contains all attributes of r2, plus the newly requested summarizing attributes. 
; When r2 is empty, the summaries are computed over r1 as a whole.
(: build-summarize (Relation Relation Agglist -> Relation)) 
(define build-summarize
  (lambda (r1 r2 alist)
    (Relation (extended-heading r2 alist)
              (Body (if (equal? r2 empty_rel)
                        (list (Tuple (calculate-summary-triples r1 empty_tuple alist)))
                        (for/list: : (Listof Tuple)
                                   ((t : Tuple (Body-tuples (Relation-body r2))))
                                   (let: ((summary-triples : (Listof Triple) (calculate-summary-triples r1 t alist)))
                                         (Tuple (append (Tuple-triples t) summary-triples)))))))))



; Evaluator helper functions --------------------------------------------------------------------------------------------------------------------------------------------------------------

(: eval-tuple-with-operand (Tuple Operand -> Value))
(define eval-tuple-with-operand
  (lambda (t o)
    (match o
      ((struct Val (val)) val)
      ((struct Att (att)) (Triple-value (find-triple-for-attribute att (Tuple-triples t))))
      ((struct App (f o1 o2)) (let: ((o1 : Value (eval-tuple-with-operand t o1)) (o2 : Value (eval-tuple-with-operand t o2)))
                                    (cond ((eq? f string-append) (string-append (assert o1 string?) (assert o2 string?)))
                                          ((eq? f substring) (substring (assert o1 string?) (assert o2 exact-integer?)))
                                          ((eq? f +) (+ (assert o1 real?) (assert o2 real?)))
                                          ((eq? f -) (- (assert o1 real?) (assert o2 real?)))
                                          ((eq? f *) (* (assert o1 real?) (assert o2 real?)))
                                          ((eq? f /) (/ (assert o1 real?) (assert o2 real?)))
                                          (else (error "eval-tuple-with-operand: not a defined function: " f)))))
      ((struct Agg (f o a)) (let: ((r : Relation (assert (eval-tuple-with-operand t o) Relation?)))
                            (cond ((eq? f length) (cardinality r))
                                  ((eq? f sum) (sum (map (lambda: ((x : Tuple)) (assert (Triple-value (find-triple-for-attribute a (Tuple-triples x))) real?)) (Body-tuples (Relation-body r)))))
                                  ((eq? f max_) (max_ (map (lambda: ((x : Tuple)) (assert (Triple-value (find-triple-for-attribute a (Tuple-triples x))) real?)) (Body-tuples (Relation-body r)))))
                                  ((eq? f min_) (min_ (map (lambda: ((x : Tuple)) (assert (Triple-value (find-triple-for-attribute a (Tuple-triples x))) real?)) (Body-tuples (Relation-body r)))))
                                  (else (error "eval-tuple-with-operand: not a defined aggregating function: " f)))))
      ((struct RelX (r)) (match r
                           ((struct Project (r a)) (match r
                                                     ((struct Image (r)) (build-projection (build-image t (eval-relexpr r)) (Heading-attrs a)))
                                                     (_ (build-projection (eval-relexpr r) (Heading-attrs a)))))
                           ((struct Image (r)) (build-image t (eval-relexpr r)))
                           (_ (error "eval-tuple-with-operand: not implemented with RelX:" r)))))))

; Given a tuple and a list of extensions, builds a new tuple, constructing new triples from the names and operations specified in the extension list and appending them to the existing triples.
(: calculate-extension-triples (Tuple Extlist -> Tuple))
(define calculate-extension-triples
  (lambda (t elist)
    (Tuple (append (Tuple-triples t) (map (lambda: ((e : Extension)) (Triple (Extension-name e) (get-type (Extension-operand e)) (eval-tuple-with-operand t (Extension-operand e)))) elist)))))

; calculates summary triples for tuples of a relation matching a given tuple in all its triples
(: calculate-summary-triples (Relation Tuple Agglist -> (Listof Triple)))
(define calculate-summary-triples
  (lambda (r t alist)
    (for/list: : (Listof Triple) ((a : Aggspec alist))
               (let: ((matching-tuples : (Listof Tuple)
                                       (if (equal? t empty_tuple)
                                           (Body-tuples (Relation-body r))
                                           (filter (lambda: ((tup : Tuple)) (andmap (lambda: ((trip : Triple)) (equal? (Triple-value trip) (Triple-value (find-corresponding-triple trip (Tuple-triples tup))))) (Tuple-triples t))) (Body-tuples (Relation-body r))))))
                     (Triple (Aggspec-name a) (get-type (Agg (Aggspec-f a) (Val empty_rel) (Aggspec-a a))) (eval-tuple-with-operand empty_tuple (Agg (Aggspec-f a) (Val (tuples->relation matching-tuples)) (Aggspec-a a))))))))

