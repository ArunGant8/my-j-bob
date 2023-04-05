;;; We redefine if to operate on 't and 'nil rather
;;; than #t and #f, and define the missing built-in
;;; operators, and change some existing operators to
;;; be TOTAL: to return a value no mater what input
;;; they are given.

(define s.car car)
(define s.cdr cdr)
(define s.+ +)
(define s.< <)
(define (num x) (if (number? x) x 0))

(define (if/nil Q A E)
  (if (equal? Q 'nil) (E) (A)))

;;; Making functions total:
(define (atom x) (if (pair? x) 'nil 't))
(define (car x) (if (pair? x) (s.car x) '()))
(define (cdr x) (if (pair? x) (s.cdr x) '()))
(define (equal x y) (if (equal? x y) 't 'nil))
(define (+ x y) (s.+ (num x) (num y)))
(define (< x y)
  (if (s.< (num x) (num y)) 't 'nil))

;;; Defining the natp predicate for natural numbers:
(define (natp x)
  (if (integer? x) (if (< x 0) 'nil 't) 'nil))

;;; Making the new if syntax
(define-syntax if
  (syntax-rules ()
    ((_ Q A E)
     (if/nil Q (lambda () A) (lambda () E)))))

(define-syntax defun
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(define-syntax dethm
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(defun size (x)
  (if (atom x)
    '0
    (+ '1 (size (car x)) (size (cdr x)))))
