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

; helper to special-compare, handles lambda expressions
; x and y must both be lambda expressions
(define (lmda-compare x y bound-vars)
  (if (not (eqv? (length (cadr x)) (length (cadr y))))
    (different-expr x y) ; different number of formals, comparison not possible
    (cons
      (if (or (eqv? (car x) 'λ) (eqv? (car y) 'λ))
        'λ
        'lambda)
      (let ((formals (process-formals (cadr x) (cadr y))))
        (cons
          formals
          ; loop through the processed formals to put them into bound-vars
          (let bind-vars ((formals formals) (xf (cadr x)) (yf (cadr y)) (bound bound-vars))
            (if (null? formals)
              (subexprs-compare (cddr x) (cddr y) bound) ; exit loop here
              (bind-vars (cdr formals) (cdr xf) (cdr yf)
                (cons (list (car formals) (car xf) (car yf)) bound)))
          )
        )
      )

    )))

(define (process-formals xf yf)
  (if (null? xf)
    '()
    (cons
      (if (eqv? (car xf) (car yf))
        (car xf)
        (string->symbol (string-append (symbol->string (car xf)) "!" (symbol->string (car yf)))))
      (process-formals (cdr xf) (cdr yf)))))

(define (special-compare x y bound-vars)
  ; bound values for car x and car y, if any
  (let ((bx (lookup (car x) cadr car bound-vars))
      (by (lookup (car y) caddr car bound-vars)))
  ; booleans representing if x or y is special
  (let ((sx (and (not bx) (memv (car x) '(if lambda λ quote))))
      (sy (and (not by) (memv (car y) '(if lambda λ quote)))))
  (if (or sx sy)
    (if (and (memv (car x) '(lambda λ)) (memv (car y) '(lambda λ)))
      (lmda-compare x y bound-vars) ; lambdas need special treatment
    (if (and (eqv? (car x) 'if) (eqv? (car y) 'if))
      (subexprs-compare x y bound-vars) ; ifs can be compared element-wise
    (if (and (eqv? (car x) 'quote) (eqv? (car y) 'quote))
      (different-expr x y) ; quotes don't get expanded
    ; else, only one is special, or they don't match
      (different-expr x y))))
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
        (subexprs-compare x y bound-vars))) ; compare recursively as usual
    (compare x y bound-vars))) ; compare without expanding
