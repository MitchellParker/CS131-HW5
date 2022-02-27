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
        (cons formals
        ; loop through the processed formals to put them into bound-vars
        (let bind-vars ((formals formals) (xf (cadr x)) (yf (cadr y)) (bound bound-vars))
          (if (null? formals)
            (subexprs-compare (cddr x) (cddr y) bound) ; exit loop here
            (bind-vars (cdr formals) (cdr xf) (cdr yf)
              (cons (list (car formals) (car xf) (car yf)) bound)))))))))

; helper to lmda-compare which combines the lists of formals xf and yf
; when they agree, keeps the same identifier
; when x has X and y has Y, instead uses X!Y
(define (process-formals xf yf)
  (if (null? xf)
    '()
    (cons
      (if (eqv? (car xf) (car yf))
        (car xf)
        (string->symbol (string-append (symbol->string (car xf)) "!" (symbol->string (car yf)))))
      (process-formals (cdr xf) (cdr yf)))))

; helper to expr-compare specifically for handling special forms
; returns #f if neither are special
; otherwise, returns the proper expression for comparing x and y
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
    #f)))) ; neither are special

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
; common subexpressions are unchanged and
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

; like eqv? except it recurses through multi-level lists
(define (deep-eqv? a b)
  (if (and (pair? a) (pair? b))
    (and (deep-eqv? (car a) (car b)) (deep-eqv? (cdr a) (cdr b)))
    (eqv? a b)))

; finds the expression from doing (expr-compare x y)
; tests if evaluating that expression in a context
; where % is #t or #f is same as (eval x) or (eval y) respectively
(define (test-expr-compare x y)
  (let ((cmp (expr-compare x y)))
    (and
      (deep-eqv? (eval x) (eval `(let ((% #t)) ,cmp)))
      (deep-eqv? (eval y) (eval `(let ((% #f)) ,cmp))))))

(define test-expr-x
  '(list
    #t
    #t
    #f
    #f
    (list 1 2 3)
    (list 1 2 3)
    ((lambda (a b) (a b)) list 'b)
    ((lambda (a b) (a b)) list 'b)
    ((λ (a b) (a b)) list 'b)
    ((λ (a b) (a b)) list 'b)
    ((lambda (a b) (a b)) list 'b)
    ((lambda (a b) (a b)) list 'b)
    ((lambda (if a b c) (if a b c)) + 1 2 3)
    (if (eqv? 1 2) #t #f)
    (if (eqv? 1 2) #t #f)
    (if (eqv? 1 2) #t #f)
    '(a b)
    '(a b)
    (quote (a b))))

(define test-expr-y
  '(list
    #t ; no change
    #f ; #t->#f
    #t ; #f->#t
    #f ; no change
    (list 1 2 3) ; no change
    (list 3 2 1) ; 1->3 and 3->1
    ((lambda (a b) (a b)) list 'b) ; no change
    ((λ (a b) (a b)) list 'b) ; lambda->λ
    ((lambda (a b) (a b)) list 'b) ; λ->lambda
    ((λ (a b) (a b)) list 'b) ; no change
    ((lambda (a c) (a c)) list 'c) ; b->c
    ((lambda (b a) (a b)) 'b list) ; a->b and b->a
    ((lambda (quote a b c) (quote a b c)) + 1 2 3) ; if->quote
    (if (eqv? 1 2) #t #f) ; no change
    (list (eqv? 1 2) #t #f) ; if->list
    (if (= 1 1) #t #f) ; eqv?->= and 2->1
    '(a c) ; b->c
    '(a b) ; no change, but inside of quotes shouldn't be looked at
    (quote (a c)))) ; b->c