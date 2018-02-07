(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests)
)

(define (zip pairs)
  (cond
    ((null? (car pairs)) nil)
    (else
      (define lists_list1 (cons (zip_helper_group_first pairs) nil))
      (define lists_rest (zip (zip_helper_remove_first pairs)))
      (append lists_list1 lists_rest)
    )
  )
)

(define (zip_helper_group_first pairs)
  (cond
    ((null? pairs) nil)
    (else
      (cons (car (car pairs)) (zip_helper_group_first (cdr pairs)))
    )
  )
)

(define (zip_helper_remove_first pairs)
  (map (lambda (x) (cdr x)) pairs)
)
;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerate_helper s i)
    (cond
      ((null? s) nil)
      (
        (define val (car s))
        (define list_elem (list i val))
        (cons list_elem(enumerate_helper (cdr s) (+ i 1)))
      )
      )
    )
  (enumerate_helper s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
    (cond
      ((null? denoms) nil)
      ((= total 0) cons (cons nil nil))
      (( > (car denoms) total) (list-change total (cdr denoms)))
      (else (append
        (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
        (list-change total (cdr denoms))))
      )
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons 'lambda (cons params (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (define after_zip_values (zip values))
           (define args (car after_zip_values))
           (define values (cadr after_zip_values))
           (define after_body (map let-to-lambda body))
           (define after_values (map let-to-lambda values))
           (cons (cons 'lambda (cons args after_body)) after_values)
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
