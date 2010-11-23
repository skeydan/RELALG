#lang typed/racket

(require "../types.rkt")

(provide (all-defined-out))

(: suppliers Relation)
(define suppliers
  (make-Relation
   (make-Heading 
    (list (make-Attribute "s#" 'S) (make-Attribute "sname" 'S) (make-Attribute "status" 'N) (make-Attribute "city" 'S)))
   (make-Body 
    (list (make-Tuple (list (make-Triple "s#" 'S (make-S "S1")) (make-Triple "sname" 'S (make-S "Smith")) (make-Triple "status" 'N (make-N 20)) (make-Triple "city" 'S (make-S "London"))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S2")) (make-Triple "sname" 'S (make-S "Jones")) (make-Triple "status" 'N (make-N 30)) (make-Triple "city" 'S (make-S "Paris"))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S3")) (make-Triple "sname" 'S (make-S "Blake")) (make-Triple "status" 'N (make-N 30)) (make-Triple "city" 'S (make-S "Paris"))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S4")) (make-Triple "sname" 'S (make-S "Clark")) (make-Triple "status" 'N (make-N 20)) (make-Triple "city" 'S (make-S "London"))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S5")) (make-Triple "sname" 'S (make-S "Adams")) (make-Triple "status" 'N (make-N 30)) (make-Triple "city" 'S (make-S "Athens"))))))))

(: parts Relation)
(define parts
  (make-Relation
   (make-Heading 
    (list (make-Attribute "p#" 'S) (make-Attribute "pname" 'S) (make-Attribute "color" 'S) (make-Attribute "weight" 'N) (make-Attribute "city" 'S)))
   (make-Body 
    (list (make-Tuple (list (make-Triple "p#" 'S (make-S "P1")) (make-Triple "pname" 'S (make-S "Nut")) (make-Triple "color" 'S (make-S "Red")) (make-Triple "weight" 'N (make-N 12.0)) (make-Triple "city" 'S (make-S "London"))))
          (make-Tuple (list (make-Triple "p#" 'S (make-S "P2")) (make-Triple "pname" 'S (make-S "Bolt")) (make-Triple "color" 'S (make-S "Green")) (make-Triple "weight" 'N (make-N 17.0)) (make-Triple "city" 'S (make-S "Paris"))))
          (make-Tuple (list (make-Triple "p#" 'S (make-S "P3")) (make-Triple "pname" 'S (make-S "Screw")) (make-Triple "color" 'S (make-S "Blue")) (make-Triple "weight" 'N (make-N 17.0)) (make-Triple "city" 'S (make-S "Oslo"))))
          (make-Tuple (list (make-Triple "p#" 'S (make-S "P4")) (make-Triple "pname" 'S (make-S "Screw")) (make-Triple "color" 'S (make-S "Red")) (make-Triple "weight" 'N (make-N 14.0))  (make-Triple "city" 'S (make-S "London"))))
          (make-Tuple (list (make-Triple "p#" 'S (make-S "P5")) (make-Triple "pname" 'S (make-S "Cam")) (make-Triple "color" 'S (make-S "Blue")) (make-Triple "weight" 'N (make-N 12.0)) (make-Triple "city" 'S (make-S "Paris"))))
          (make-Tuple (list (make-Triple "p#" 'S (make-S "P6")) (make-Triple "pname" 'S (make-S "Cog")) (make-Triple "color" 'S (make-S "Red")) (make-Triple "weight" 'N (make-N 19.0)) (make-Triple "city" 'S (make-S "London"))))))))

(: shipments Relation)
(define shipments
  (make-Relation
   (make-Heading 
    (list (make-Attribute "s#" 'S) (make-Attribute "p#" 'S) (make-Attribute "qty" 'N)))
   (make-Body 
    (list (make-Tuple (list (make-Triple "s#" 'S (make-S "S1")) (make-Triple "p#" 'S (make-S "P1")) (make-Triple "qty" 'N (make-N 300))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S1")) (make-Triple "p#" 'S (make-S "P2")) (make-Triple "qty" 'N (make-N 200))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S1")) (make-Triple "p#" 'S (make-S "P3")) (make-Triple "qty" 'N (make-N 400))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S1")) (make-Triple "p#" 'S (make-S "P4")) (make-Triple "qty" 'N (make-N 200))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S1")) (make-Triple "p#" 'S (make-S "P5")) (make-Triple "qty" 'N (make-N 300))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S1")) (make-Triple "p#" 'S (make-S "P6")) (make-Triple "qty" 'N (make-N 300))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S2")) (make-Triple "p#" 'S (make-S "P1")) (make-Triple "qty" 'N (make-N 300))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S2")) (make-Triple "p#" 'S (make-S "P2")) (make-Triple "qty" 'N (make-N 400))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S3")) (make-Triple "p#" 'S (make-S "P2")) (make-Triple "qty" 'N (make-N 200))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S4")) (make-Triple "p#" 'S (make-S "P2")) (make-Triple "qty" 'N (make-N 200))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S4")) (make-Triple "p#" 'S (make-S "P4")) (make-Triple "qty" 'N (make-N 300))))
          (make-Tuple (list (make-Triple "s#" 'S (make-S "S4")) (make-Triple "p#" 'S (make-S "P5")) (make-Triple "qty" 'N (make-N 400))))))))


