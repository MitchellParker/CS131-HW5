#lang racket

(provide (all-defined-out))

; returns the expression using % to pick between x or y
; should NOT be called when they're the same
(define (different-expr x y)
  (if (and (boolean? x) (boolean? y))
    (if x '% '(not %)) ; special cases for #t and #f
    `(if % ,x ,y)))

; simplest subproblem to expr-compare
; if x and y are different, returns an expression which
; uses % to select the right one, otherwise returns x
; also if either are bound variables, replaces them with the approprate thing
(define (compare x y bound-vars)
  (let (
      (x (if (symbol? x)
        (let ((bx (lookup x cadr car bound-vars))) (if bx bx x))
        x))
      (y (if (symbol? y)
        (let ((by (lookup y caddr car bound-vars))) (if by by y))
        y)))
    (if (eqv? x y)
      x
      (different-expr x y))))

; helper to expr-compare which compares non-special forms
; x and y assumed to be lists of equal length
; uses expr-compare to expand subexpressions
(define (subexprs-compare x y bound-vars)
  (if (null? x)
    '()
    (cons
      (expr-compare (car x) (car y) bound-vars)
      (subexprs-compare (cdr x) (cdr y) bound-vars))))

(define (special-compare x y bound-vars)
  ; bound values for car x and car y, if any
  (let ((bx (lookup (car x) cadr car bound-vars))
      (by (lookup (car y) caddr car bound-vars)))
  ; booleans representing if x or y is special
  (let ((sx (and (not bx) (memv (car x) '(if lambda λ quote))))
      (sy (and (not by) (memv (car y) '(if lambda λ quote)))))
  (if (or sx sy)
    (if (and sx sy (eqv? (car x) (car y)))
      (if (memv (car x) '(lambda λ))
        (different-expr x y bound-vars) ; TODO handle lambdas
      (if (eqv? (car x) 'if)
        (subexprs-compare x y bound-vars)
      ; else must be quote
        (different-expr x y)))
      (different-expr x y)) ; only one is special, or they don't match
    #f))))

; finds the right symbols to use for x and y
; e.g. if x=c, f=cadr, r=car bv=((a!b a b) (c!d c d))
;      then (lookup x f r bv) ==> c!d
(define (lookup key finder retriever list)
    (let find ((list list))
      (if (null? list)
        #f
        (if (eqv? key (finder (car list)))
          (retriever (car list))
          (find (cdr list))))))

; compares two expressions x and y, and returns an expression where
; common subexpressions have been unified and
; differing subexpressions have the property that:
; when % is bound to #t, behaves like x
; when % is bound to #f, behaves like y
(define (expr-compare x y (bound-vars '()))
  (if (and (list? x) (list? y) (eqv? (length x) (length y)))
    (let ((sf (special-compare x y bound-vars)))
      (if sf 
        sf ; special form spotted, do special thing
        (subexprs-compare x y bound-vars))) ; expand recursively as usual
    (compare x y bound-vars))) ; compare without expanding
