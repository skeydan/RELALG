#lang typed/racket

(provide (all-defined-out))

(define: trace : (Parameterof Integer) (make-parameter 0))

(define: default-join-method : (Parameterof Symbol) (make-parameter 'hash))
