#lang typed/racket

(require "../types.rkt")

(provide (all-defined-out))

(: suppliers Relation)
(define suppliers
  (Relation
   (Heading 
    (list (Attribute "s#" string?) (Attribute "sname" string?) (Attribute "status" real?) (Attribute "city" string?)))
   (Body 
    (list (Tuple (list (Triple "s#" string? "S1") (Triple "sname" string? "Smith") (Triple "status" real? 20) (Triple "city" string? "London")))
          (Tuple (list (Triple "s#" string? "S2") (Triple "sname" string? "Jones") (Triple "status" real? 30) (Triple "city" string? "Paris")))
          (Tuple (list (Triple "s#" string? "S3") (Triple "sname" string? "Blake") (Triple "status" real? 30) (Triple "city" string? "Paris")))
          (Tuple (list (Triple "s#" string? "S4") (Triple "sname" string? "Clark") (Triple "status" real? 20) (Triple "city" string? "London")))
          (Tuple (list (Triple "s#" string? "S5") (Triple "sname" string? "Adams") (Triple "status" real? 30) (Triple "city" string? "Athens")))))))

(: parts Relation)
(define parts
  (Relation
   (Heading 
    (list (Attribute "p#" string?) (Attribute "pname" string?) (Attribute "color" string?) (Attribute "weight" real?) (Attribute "city" string?)))
   (Body 
    (list (Tuple (list (Triple "p#" string? "P1") (Triple "pname" string? "Nut") (Triple "color" string?  "Red") (Triple "weight" real? 12.0) (Triple "city" string? "London")))
          (Tuple (list (Triple "p#" string? "P2") (Triple "pname" string? "Bolt") (Triple "color" string?  "Green") (Triple "weight" real? 17.0) (Triple "city" string? "Paris")))
          (Tuple (list (Triple "p#" string? "P3") (Triple "pname" string? "Screw") (Triple "color" string?  "Blue") (Triple "weight" real? 17.0) (Triple "city" string? "Oslo")))
          (Tuple (list (Triple "p#" string? "P4") (Triple "pname" string? "Screw") (Triple "color" string?  "Red") (Triple "weight" real? 14.0)  (Triple "city" string? "London")))
          (Tuple (list (Triple "p#" string? "P5") (Triple "pname" string? "Cam") (Triple "color" string?  "Blue") (Triple "weight" real? 12.0) (Triple "city" string? "Paris")))
          (Tuple (list (Triple "p#" string? "P6") (Triple "pname" string? "Cog") (Triple "color" string?  "Red") (Triple "weight" real? 19.0) (Triple "city" string? "London")))))))

(: shipments Relation)
(define shipments
  (Relation
   (Heading 
    (list (Attribute "s#" string?) (Attribute "p#" string?) (Attribute "qty" real?)))
   (Body 
    (list (Tuple (list (Triple "s#" string? "S1") (Triple "p#" string? "P1") (Triple "qty" real? 300)))
          (Tuple (list (Triple "s#" string? "S1") (Triple "p#" string? "P2") (Triple "qty" real? 200)))
          (Tuple (list (Triple "s#" string? "S1") (Triple "p#" string? "P3") (Triple "qty" real? 400)))
          (Tuple (list (Triple "s#" string? "S1") (Triple "p#" string? "P4") (Triple "qty" real? 200)))
          (Tuple (list (Triple "s#" string? "S1") (Triple "p#" string? "P5") (Triple "qty" real? 300)))
          (Tuple (list (Triple "s#" string? "S1") (Triple "p#" string? "P6") (Triple "qty" real? 300)))
          (Tuple (list (Triple "s#" string? "S2") (Triple "p#" string? "P1") (Triple "qty" real? 300)))
          (Tuple (list (Triple "s#" string? "S2") (Triple "p#" string? "P2") (Triple "qty" real? 400)))
          (Tuple (list (Triple "s#" string? "S3") (Triple "p#" string? "P2") (Triple "qty" real? 200)))
          (Tuple (list (Triple "s#" string? "S4") (Triple "p#" string? "P2") (Triple "qty" real? 200)))
          (Tuple (list (Triple "s#" string? "S4") (Triple "p#" string? "P4") (Triple "qty" real? 300)))
          (Tuple (list (Triple "s#" string? "S4") (Triple "p#" string? "P5") (Triple "qty" real? 400)))))))


