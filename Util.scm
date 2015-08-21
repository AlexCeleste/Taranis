
;; General Utilities:
;; This file defines simple, generic macros and procedures that are of use
;; in more than one Ravioli module.

(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...) #f))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if test #f (begin body ...)))))

(define-syntax while    ;Procedural-style While loop
  (syntax-rules ()
    ((_ condition action ...) (let while-loop ()
                                (if condition
                                    (begin action ... (while-loop))
                                    #f)))))

;; These were appearing surprisingly often as lambdas - this is shorter
(define (id x) x)
(define (fst one two) one)
(define (snd one two) two)
(define (mid one two three) two)

;;  (define-syntax setr!    ;Set and return (similar to = in C) (not clear)
;;    (syntax-rules ()
;;      [(_ var val) (begin (set! var val) var)]))

(define (remp pred lst)      ;Useful func from R6RS
  (define (r-inner p src dst)
    (if (eq? src '())
        dst
        (if (p (car src))
            (r-inner p (cdr src) dst)
            (r-inner p (cdr src) (append dst (cons (car src) '()))))))
  (r-inner pred lst '()))

(define (string-downcase s)  ;Destructive string-to-lower function
  (let ((l (string-length s)))
    (do ((i 0 (fx+ 1 i)))
        ((fx= i l) s)
        (string-set! s i (char-downcase (string-ref s i))))))

