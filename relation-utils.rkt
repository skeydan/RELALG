#lang typed/racket
(require racket/match)
(require (planet dherman/types:2))
(require "types.rkt")
(require "helper-functions.rkt")
(require "type-functions.rkt")
(require "convert.rkt")
(require "settings.rkt")

(provide (all-defined-out))

; Display functions for relations and their components -----------------------------------------------------------------------------------------------------------------------------

(: print-relation (Relation -> Void))
(define print-relation
  (lambda (r)
    (printf (format "~a~n~a" (print-heading (Relation-heading r)) (print-body (Relation-body r))))))

(: print-heading (Heading -> String))
(define print-heading
  (lambda (h)
    (foldr (lambda: ((x : Attribute) (y : String)) (string-append (format (if (null? (string->list y)) "~a" "~a, ") (print-attribute x)) y)) "" (Heading-attrs h))))

(: print-attribute (Attribute -> String))
(define print-attribute
  (lambda (a)
    (format "~a(~a)" (Attribute-name a) (cond ((eq? (Attribute-type a) real?) "number")
                                              ((eq? (Attribute-type a) string?) "string")))))
 
(: print-body (Body -> String))
(define print-body
  (lambda (b)
    (print-tuples (Body-tuples b))))

(: print-tuples ((Listof Tuple) -> String))
(define print-tuples
  (lambda (t)
    (foldr (lambda: ((x : Tuple) (y : String)) (string-append (format "~a~n" (print-tuple x)) y)) "" t)))

(: print-tuple (Tuple -> String))
(define print-tuple
  (lambda (t)
    (foldr (lambda: ((x : Triple) (y : String)) (string-append (format (if (null? (string->list y)) "~a" "~a, ") (Triple-value x)) y)) "" (Tuple-triples t))))


; Comparison functions for relations and their components --------------------------------------------------------------------------------------------------------------------------

(: triple-attribute=? (Attribute Triple -> Boolean))
(define triple-attribute=?
  (lambda (a t)
    (and (string=? (string-upcase (Triple-name t)) (string-upcase (Attribute-name a))) (eq? (Triple-type t) (Attribute-type a)))))


; Utility functions on relations ---------------------------------------------------------------------------------------------------------------------------------------------------

(: cardinality (Relation -> Integer)) 
(define cardinality
  (lambda (r)
    (length (Body-tuples (Relation-body r)))))

(: degree (Relation -> Integer))
(define degree
  (lambda (r)
    (length (Heading-attrs (Relation-heading r)))))

; copies everything besides the Value, which is reused
(: new-relation-from (Relation -> Relation))
(define new-relation-from
  (lambda (r)
    (Relation (Heading (map (lambda: ((a :  Attribute)) (Attribute (Attribute-name a) (Attribute-type a))) (Heading-attrs (Relation-heading r)))) (Body (map (lambda: ((tup : Tuple)) (Tuple (map (lambda: ((trip : Triple)) (Triple (Triple-name trip) (Triple-type trip) (Triple-value trip))) (Tuple-triples tup))))  (Body-tuples (Relation-body r)))))))

; constructs a relation from a given list of tuples
(: tuples->relation ((Listof Tuple) -> Relation))
(define tuples->relation 
  (lambda (tlist)
    (Relation (Heading (attlistfromtuple (car tlist))) (Body tlist))))

; Utility functions on tuples ------------------------------------------------------------------------------------------------------------------------------------------------------

; given 2 tuples and a list of attributes to (hierarchically) sort for, indicates whether the first tuple comes first in sort order.
(: precedes? (Tuple Tuple (Listof Attribute) -> Boolean))
(define precedes?
  (lambda (t1 t2 atts)
    (let loop ((tr1 (joining-triples atts t1)) (tr2 (joining-triples atts t2)) (atts atts))
      (cond ((null? atts) #f) ; when no comparison attributes left, return #f (arbitrary)
            ((value<? (Triple-value (find-triple-for-attribute (car atts) tr1)) (Triple-value (find-triple-for-attribute (car atts) tr2))) #t)
            ((value>? (Triple-value (find-triple-for-attribute (car atts) tr1)) (Triple-value (find-triple-for-attribute (car atts) tr2))) #f)
            (else (loop tr1 tr2 (cdr atts)))))))     
                                                                                                                                           
; builds the union of 2 relations, removing duplicates. Used in UNION,
(: unique-tuples (Relation Relation -> (Listof Tuple))) 
(define unique-tuples
  (lambda (r1 r2)
    (let ((t1 (Body-tuples (Relation-body r1))) (t2 (Body-tuples (Relation-body r2))))
      (remove-duplicates (append t1 t2) equal?))))

; builds the intersection of 2 relations, removing duplicates. Used in INTERSECT,
(: common-tuples (Relation Relation -> (Listof Tuple)))
(define common-tuples
  (lambda (r1 r2)
    (let ((t1 (Body-tuples (Relation-body r1))) (t2 (Body-tuples (Relation-body r2))))
      (remove-duplicates (filter (lambda: ((x : Tuple)) (contains? t2 x equal?)) t1) equal?))))

; builds the difference of 2 relations, removing any duplicates in the first. Used in DIFFERENCE.
(: tuples-difference (Relation Relation -> (Listof Tuple)))
(define tuples-difference
  (lambda (r1 r2)
    (let ((t1 (Body-tuples (Relation-body r1))) (t2 (Body-tuples (Relation-body r2))))
      (remove-duplicates (filter (lambda: ((x : Tuple)) (not (contains? t2 x equal?))) t1)))))

; builds a cartesian product by appending every tuple from relation 1 to every tuple from relation 2
(: cartesian-tuples (Relation Relation -> (Listof Tuple)))
(define cartesian-tuples
  (lambda (r1 r2)
    (let ((t1 (Body-tuples (Relation-body r1))) (t2 (Body-tuples (Relation-body r2))))
      (foldl (lambda: ((x : (Listof Tuple)) (y : (Listof Tuple))) (append x y)) '() (map (lambda: ((x : Tuple)) (append-every-tuple-from-list x t2)) t1)))))

; builds a new list of tuples by appending a tuple's triples to every tuple in a list
(: append-every-tuple-from-list (Tuple (Listof Tuple) -> (Listof Tuple)))
(define append-every-tuple-from-list
  (lambda (t tlist)
    (map (lambda: ((x : Tuple)) (Tuple (append (Tuple-triples t) (Tuple-triples x)))) tlist)))

; determine adequate join method
(: determine-join-method ((Listof Attribute) Relation Relation -> Symbol))
(define determine-join-method
  (lambda (joining-attrs r1 r2)
    (default-join-method)))

; Builds a list of all joinable tuples for 2 relations. For the order of the triples in the newly built tuples, see append-joinable-tuples.
; Dispatches to nl-join, merge-join or hash-join according to given flag.
(: joined-tuples ((Listof Attribute) Relation Relation -> (Listof Tuple)))
(define joined-tuples
  (lambda (joining-attrs r1 r2)
    (let ((t1 (Body-tuples (Relation-body r1))) (t2 (Body-tuples (Relation-body r2))) (method (determine-join-method joining-attrs r1 r2)))
      (cond ((eq? method 'nested-loops) (nl-join joining-attrs t1 t2))
            ((eq? method 'sort-merge) (merge-join joining-attrs t1 t2))
            ((eq? method 'hash) (hash-join joining-attrs t1 t2))
            (else (error "joined-tuples: not a join method: " method))))))

; Performs the nested loop join, appending every matching tuple from list 1 to every tuple from list 2.
(: nl-join ((Listof Attribute) (Listof Tuple) (Listof Tuple) -> (Listof Tuple)))
(define nl-join
  (lambda (joining-attrs t1 t2)
    (foldl (lambda: ((x : (Listof Tuple)) (y : (Listof Tuple))) (append x y)) '() (map (lambda: ((x : Tuple)) (append-joinable-tuples joining-attrs x t2)) t1))))

; Performs the merge join after first sorting both lists.
(: merge-join ((Listof Attribute) (Listof Tuple) (Listof Tuple) -> (Listof Tuple)))
(define merge-join
  (lambda (joining-attrs t1 t2)
    (mergejoin-tuples (mergesort-tuples (split-list t1) joining-attrs) (mergesort-tuples (split-list t2) joining-attrs) joining-attrs)))

; Performs the hash join, deciding which list to load as a hash map into memory and then probing every tuple from the other list against it.
(: hash-join ((Listof Attribute) (Listof Tuple) (Listof Tuple) -> (Listof Tuple)))
(define hash-join
  (lambda (joining-attrs t1 t2)
    (let-values: ((((build-t : (Listof Tuple)) (probe-t : (Listof Tuple))) (determine-build-tuples t1 t2)))
                 (let: ((hash-t : (HashTable (Listof Triple) (Listof Tuple)) (build-hash build-t joining-attrs)))
                       (foldl (lambda: ((x : (Listof Tuple)) (y : (Listof Tuple))) (append x y)) '() (probe-hash hash-t probe-t joining-attrs))))))

; Decides which input tuple list to use as a hash table. Currently just checks for the length of the lists, choosing the shorter one as input for hashing.
(: determine-build-tuples ((Listof Tuple) (Listof Tuple) -> (values (Listof Tuple) (Listof Tuple))))
(define determine-build-tuples
  (lambda (t1 t2)
    (if (> (length t1) (length t2)) (values t2 t1) (values t1 t2))))

; Builds a hash table from the given tuple list, using as a key a list of the triples involved in the join (listing them in the order they have in the passed-in list of joining attributes), and as value  a list of all tuples containing those triples.
(: build-hash ((Listof Tuple) (Listof Attribute) -> (HashTable (Listof Triple) (Listof Tuple))))
(define build-hash
  (lambda (tlist joining-attrs)
    (for/fold: : (HashTable (Listof Triple) (Listof Tuple)) ((ht : (HashTable (Listof Triple) (Listof Tuple)) (ann (make-immutable-hash '()) (HashTable (Listof Triple) (Listof Tuple)))))
               ((t : Tuple tlist))
               (let: ((key : (Listof Triple) (joining-triples joining-attrs t)))
                     (if (hash-has-key? ht key)
                         (hash-set ht key (append (hash-ref ht key) (list t)))
                         (hash-set ht key (list t)))))))
               
; Constructs joined tuples from the specified build table and a list of tuples to be probed. 
(: probe-hash ((HashTable (Listof Triple) (Listof Tuple)) (Listof Tuple) (Listof Attribute) -> (Listof (Listof Tuple))))
(define probe-hash
  (lambda (ht tlist joining-attrs)
    (for/list: : (Listof (Listof Tuple)) ((t : Tuple tlist)
                                 #:when (hash-has-key? ht (joining-triples joining-attrs t)))
               (map (lambda: ((x : Tuple)) (compose-tuple x t joining-attrs)) (hash-ref ht (joining-triples joining-attrs t))))))

; Builds a new tuple from two tuples matching in join criteria. For every new tuple, the triples will be in the order 1. common triples 2. triples from first tuple 3. triples from second tuple.                
(: compose-tuple (Tuple Tuple (Listof Attribute) -> Tuple))
(define compose-tuple
  (lambda (t1 t2 joining-attrs)
    (Tuple (append (joining-triples joining-attrs t1) (all-but-joining-triples joining-attrs t1) (all-but-joining-triples joining-attrs t2)))))

; Given two sorted lists of tuples, puts together tuples with matching values in the joining attributes.
(: mergejoin-tuples ((Listof Tuple) (Listof Tuple) (Listof Attribute) -> (Listof Tuple)))
(define mergejoin-tuples
  (lambda (t1 t2 atts)
    (log 3 "---------------MERGEJOIN-TUPLES------------------~n~a~n~a~n" (print-tuples t1) (print-tuples t2))
    (define: (process-inner (t1 : (Listof Tuple)) (t2 : (Listof Tuple)) (last-match : (Listof Tuple)) (result : (Listof Tuple)))
      : (Listof Tuple)
      (let: loop-inner : (Listof Tuple) ((t1 : (Listof Tuple) t1) (t2 : (Listof Tuple) t2) (last-match : (Listof Tuple) last-match) (result : (Listof Tuple) result)) 
         (log 3 "-------------- LOOP-INNER-------------------: ~nt1:~n~a~nt2:~n~a~nlast-match:~n~a~nresult:~n~a~n" (print-tuples (take t1 (min (length t1) 2))) (print-tuples (take t2 (min (length t2) 2))) (print-tuples (take last-match (min (length last-match) 2))) (print-tuples (take result (min (length result) 2))))
        (cond ((null? t2) (process-outer (cdr t1) last-match result))
              ;((joining-values-match atts  (car t1) (car t2)) (process-inner t1 (cdr t2) t2 (cons (Tuple (append (joining-triples atts (car t1)) (all-but-joining-triples atts (car t1)) (all-but-joining-triples atts (car t2)))) result))) ; match found - try next item from inner list, keeping this match als re-entry point for subsequent outer loop
              ((joining-values-match atts  (car t1) (car t2)) (process-inner t1 (cdr t2) last-match (cons (Tuple (append (joining-triples atts (car t1)) (all-but-joining-triples atts (car t1)) (all-but-joining-triples atts (car t2)))) result))) ; match found - try next item from inner list, keeping this match als re-entry point for subsequent outer loop
              ((precedes? (car t1) (car t2) atts) (process-outer (cdr t1) last-match result)) ; matching values passed over in inner list - continue outer loop with last match from inner as reentry point
              ((precedes? (car t2) (car t1) atts) (process-inner t1 (cdr t2) last-match result)) ; no match yet in inner - move forward in inner
              (else (error "mergejoin-tuples/process-inner: no condition matched for (t1 first item - t2 first item - last match): " (print-tuple (car t1)) (print-tuple (car t2)) (print-tuple (car last-match))))))) 
    (define: (process-outer (t1 : (Listof Tuple)) (t2 : (Listof Tuple)) (result : (Listof Tuple)))
      : (Listof Tuple)
      (let: loop-outer : (Listof Tuple) ((t1 : (Listof Tuple) t1) (t2 : (Listof Tuple) t2) (result : (Listof Tuple) result))
            (log 3 "-------------- LOOP-OUTER-------------------: ~nt1:~n~a~nt2:~n~a~nresult:~n~a~n" (print-tuples (take t1 (min (length t1) 2))) (print-tuples (take t2 (min (length t2) 2))) (print-tuples (take result (min (length result) 2))))
        (cond ((null? t1) result)
              ((joining-values-match atts  (car t1) (car t2)) (process-inner t1 t2 t2 result)) ; should only be possible if very first tuples match 
              ((precedes? (car t1) (car t2) atts) (process-outer (cdr t1) t2 result)) ; no matches possible for first item of outer list - move on in outer list
              ((precedes? (car t2) (car t1) atts) (process-inner t1 (cdr t2) t2 result)) ; matches possible in inner list - move forward in inner
              (else (error "mergejoin-tuples/process-outer : no condition matched for (t1 first item - t2 first item): " (print-tuple (car t1)) (print-tuple (car t2)))))))
    (process-outer t1 t2 '())))
            

; Sorts a list of tuples according to a specified set of attributes. With helper functions, implements the mergesort algorithm for tuples.
(: mergesort-tuples ((Listof (Listof Tuple)) (Listof Attribute) -> (Listof Tuple)))
(define mergesort-tuples
  (lambda (tlist atts)
    (cond ((null? (cdr tlist)) (car tlist))
          (else (mergesort-tuples (merge-successive tlist atts) atts)))))

; Sorts pairs of neighbors in a list. Part of mergesort for tuples.
(: merge-successive ((Listof (Listof Tuple)) (Listof Attribute) -> (Listof (Listof Tuple))))
(define merge-successive
  (lambda (tlist atts)
    (cond ((null? tlist) '())
          ((null? (cdr tlist)) tlist)
          (else (cons (merge-2-listsof-tuples (car tlist) (cadr tlist) atts) (merge-successive (cddr tlist) atts))))))

; Merges two lists of tuples, sorting by the specified set of attributes. Part of mergesort for tuples.
(: merge-2-listsof-tuples ((Listof Tuple) (Listof Tuple) (Listof Attribute) -> (Listof Tuple)))
(define merge-2-listsof-tuples
  (lambda (t1 t2 atts)
    (cond ((null? t1) t2)
          ((null? t2) t1)
          ((precedes? (car t1) (car t2) atts) (cons (car t1) (merge-2-listsof-tuples (cdr t1) t2 atts)))
          (else (cons (car t2) (merge-2-listsof-tuples (cdr t2) t1 atts))))))

; Given a tuple and a list of tuples, if the corresponding values match, builds a new list of tuples by combining all matching tuples from the list with the single tuple.
; This implements the inner loop of the nested loops method.
(: append-joinable-tuples ((Listof Attribute) Tuple (Listof Tuple) -> (Listof Tuple)))
(define append-joinable-tuples
  (lambda (joining-attrs t tlist)
    (for/list: : (Listof Tuple) ((x : Tuple tlist)
                                 #:when (joining-values-match joining-attrs x t))
               (Tuple (append (joining-triples joining-attrs t) (all-but-joining-triples joining-attrs t) (all-but-joining-triples joining-attrs x))))))

; Checks if 2 tuples match (= can be joined) on a set of joining attributes.
(: joining-values-match ((Listof Attribute) Tuple Tuple -> Boolean))
(define joining-values-match
  (lambda (joining-attrs t1 t2)
    (andmap (lambda: ((x : Attribute)) (equal? (Triple-value (find-triple-for-attribute x (Tuple-triples t1))) (Triple-value (find-triple-for-attribute x (Tuple-triples t2))))) joining-attrs)))


; Utility functions on triples -----------------------------------------------------------------------------------------------------------------------------------------------------

; For one tuple, gets all triples corresponding to one of the specified joining attributes (= all triples whose values will have to match those from another relation).
(: joining-triples ((Listof Attribute) Tuple -> (Listof Triple)))
(define joining-triples
  (lambda (joining-attrs tuple)
    (map (lambda: ((x : Attribute)) (find-triple-for-attribute x (Tuple-triples tuple))) joining-attrs)))

; For one tuple, gets all triples not contained in the list of specified joining attributes.
(: all-but-joining-triples ((Listof Attribute) Tuple -> (Listof Triple)))
(define all-but-joining-triples
  (lambda (joining-attrs tuple)
    (filter (lambda: ((x : Triple)) (not (contains? (map Attribute-name joining-attrs) (Triple-name x) string=?))) (Tuple-triples tuple))))

; Renames triples in a list of tuples according to a given specification.
(: replace-triplenames ((Listof (Pair Attribute String)) (Listof Tuple) -> Void))
(define replace-triplenames
  (lambda (renamings tuples)
    (for-each (lambda: ((mapping : (Pair Attribute String))) (for-each (lambda: ((t : Triple)) (set-Triple-name! t (cdr mapping))) (map (lambda: ((tup : Tuple)) (find-triple-for-attribute (car mapping) (Tuple-triples tup))) tuples))) renamings)))

(: find-corresponding-triple (Triple (Listof Triple) -> Triple))
(define find-corresponding-triple
  (lambda (t tlist)
    (cond ((null? tlist) (error "Corresponding triple not found in list: " t tlist))
          ((and (equal? (Triple-name t) (Triple-name (first tlist))) (equal? (Triple-type t) (Triple-type (first tlist)))) (first tlist))
          (else (find-corresponding-triple t (rest tlist))))))


; Utility functions on headings ----------------------------------------------------------------------------------------------------------------------------------------------------

; Builds the heading for a cartesian product by appending the 2nd relation's attributes to the 1st's
(: cartesian-heading (Heading Heading -> Heading))
(define cartesian-heading
  (lambda (h1 h2)
    (Heading (append (Heading-attrs h1) (Heading-attrs h2)))))

; Builds the heading for a join, concatenating - in this order - the attribute(s) we're joining on, the attributes contained in relation1 only and those contained in rel. 2 only
(: joined-heading ((Listof Attribute) Heading Heading -> Heading))
(define joined-heading
  (lambda (joining-attrs h1 h2)
    (Heading (remove-duplicates (append joining-attrs (Heading-attrs h1) (Heading-attrs h2)) equal?))))

; Builds the heading for an extension, adding to the existing attributes the ones specified in the "extension list".
(: extended-heading (Relation (U Extlist Agglist) -> Heading))
(define extended-heading
  (lambda (r lst)
    (Heading (append (Heading-attrs (Relation-heading r))
                     (cond ((Extlist? lst) (map (lambda: ((e : Extension)) (Attribute (Extension-name e) (get-type (Extension-operand e)))) (assert lst Extlist?)))
                           ((Agglist? lst) (map (lambda: ((a : Aggspec)) (Attribute (Aggspec-name a) (get-type (Agg (Aggspec-f a) (Val empty_rel) (Aggspec-a a)))))  (assert lst Agglist?))))))))


; Utility functions on attributes --------------------------------------------------------------------------------------------------------------------------------------------------

; Returns the triple matching an attribute specification
(: find-triple-for-attribute (Attribute (Listof Triple) -> Triple))
(define find-triple-for-attribute
  (lambda (a tlist)
    (cond ((null? tlist) (error "Joining attribute not found in Tuple: " (print-attribute a) (print-tuple (Tuple tlist))))
          ((triple-attribute=? a (first tlist)) (first tlist))
          (else (find-triple-for-attribute a (rest tlist))))))

; Returns all attributes 2 headings have in common
(: common-attributes (Heading Heading -> (Listof Attribute)))
(define common-attributes
  (lambda (h1 h2)
    (let ((a1 (Heading-attrs h1)) (a2 (Heading-attrs h2)))
      (filter (lambda: ((x : Attribute)) (contains? a2 x equal?)) a1))))

; Returns all attributes 2 headings have in common
(: attributes-difference (Heading Heading -> (Listof Attribute)))
(define attributes-difference
  (lambda (h1 h2)
    (for/list: : (Listof Attribute) ((a : Attribute (Heading-attrs h1))
                                     #:when (not (member a (Heading-attrs h2))))
               a)))

; Checks that 2 headings don't have any attributes in common
(: attribute-names-disjoint? (Heading Heading -> Boolean))
(define attribute-names-disjoint?
  (lambda (h1 h2)
    (let ((l1 (map Attribute-name (Heading-attrs h1))) (l2 (map Attribute-name (Heading-attrs h2))))
      (not (ormap (lambda: ((name : String)) (member name l2)) l1)))))

; Renames members in a list of attributes according to a given specification.
(: replace-attnames ((Listof (Pair Attribute String)) (Listof Attribute) -> Void))
(define replace-attnames
  (lambda (renamings attrs)
    (for-each (lambda: ((mapping : (Pair Attribute String))) (for-each (lambda: ((a : Attribute)) (set-Attribute-name! a (cdr mapping))) (filter (lambda: ((att : Attribute)) (equal? att (car mapping))) attrs))) renamings)))

;
(: attlistfromtuple (Tuple -> (Listof Attribute)))
(define attlistfromtuple
  (lambda (t)
    (for/list: : (Listof Attribute) ((tr : Triple (Tuple-triples t)))
               (Attribute (Triple-name tr) (Triple-type tr)))))


