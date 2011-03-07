#lang typed/racket

(require typed/rackunit)
(require "../data/data.rkt")
(require "../types.rkt")
(require "../type-functions.rkt")
(require "../helper-functions.rkt")
(require "../eval.rkt")
(require "../relation-utils.rkt")
(require "../settings.rkt")

; Set operators (analogously: Difference)
(print-relexpr (Union (Restrict (Rel suppliers) (Is eql (Att (Attribute "city" string?)) (Val "London")))
                      (Restrict (Rel suppliers) (Is eql (Att (Attribute "city" string?)) (Val "Athens")))))

(print-relexpr (Intersect (Rel shipments) (Restrict (Rel shipments) (Is eql (Att (Attribute "qty" real?)) (Val 300)))))

; Joins, theta join and cartesian product
(print-relexpr (Product (Project (Rel suppliers) (Heading (list (Attribute "s#" string?))))
                        (Project (Rel parts) (Heading (list (Attribute "p#" string?))))))

(print-relexpr (Join (Rel suppliers) (Rel shipments)))

(print-relexpr (Join (Project (Rel parts) (Heading (list (Attribute "p#" string?) (Attribute "pname" string?)))) (Join (Rel suppliers) (Rel shipments))))

(print-relexpr (Theta-Join (Project (Rel suppliers) (Heading (list (Attribute "s#" string?) (Attribute "sname" string?)))) (Rel parts) (Is greater (Att (Attribute "sname" string?)) (Att (Attribute "pname" string?)))))

; Projection
(print-relexpr (Project (Rel parts) (Heading (list (Attribute "color" string?)))))

; Restriction
(print-relexpr (Restrict (Rel parts) (Or (Is noteql (Att (Attribute "city" string?)) (Val "London")) (Is eql  (Att (Attribute "color" string?)) (Val "Red")))))

; Rename
(print-relexpr (Rename (Rel parts) (list (cons (Attribute "city" string?) "location") (cons (Attribute "color" string?) "appearance"))))

; Divide
(print-relexpr (Divide (Project (Rel suppliers) (Heading (list (Attribute "s#" string?))))
                       (Project (Rel parts)(Heading (list (Attribute "p#" string?))))
                       (Project (Rel shipments)(Heading (list (Attribute "s#" string?) (Attribute "p#" string?))))))
; Semijoin
(print-relexpr (Semijoin (Rel suppliers) (Restrict (Rel shipments) (Is eql (Att (Attribute "p#" string?)) (Val "P2")))))

; Semiminus
(print-relexpr (Semijoin (Rel suppliers) (Restrict (Rel shipments) (Is eql (Att (Attribute "p#" string?)) (Val "P2")))))

; Extend
(print-relexpr (Extend (Rel parts) (list (Extension (App / (Att (Attribute "weight" real?)) (Val 100)) "hectograms")
                                         (Extension (Att (Attribute "p#" string?)) "id"))))

; Image
(print-relexpr (Extend (Rel suppliers) (list (Extension (Agg sum (RelX (Image (Rel shipments))) (Attribute "qty" real?)) "totq") (Extension (Agg max_ (RelX (Image (Rel shipments)))  (Attribute "qty" real?)) "maxq"))))

(print-relexpr (Restrict (Rel suppliers) (Is eql (RelX (Project (Image (Rel shipments)) (Heading (list (Attribute "p#" string?)))))  (RelX (Project (Rel parts) (Heading (list (Attribute "p#" string?))))))))

; Summarize
(print-relexpr (Summarize (Rel shipments) (Project (Rel parts) (Heading (list (Attribute "p#" string?)))) (list (Aggspec length (Attribute "dummy" real?) "count") (Aggspec sum (Attribute "qty" real?) "sum") (Aggspec max_ (Attribute "qty" real?) "max") (Aggspec min_ (Attribute "qty" real?) "min"))))