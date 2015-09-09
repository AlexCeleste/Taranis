
;; General Utilities:
;; This file defines simple, generic macros and procedures that are of use
;; in more than one compiler module.

(define-macro (when c . b)
  `(if ,c (begin ,@b #!void) #!void) )
(define-macro (unless c . b)
  `(if ,c #!void (begin ,@b #!void)) )

(define-macro (while c . b)    ;procedural-style while loop
  `(let *while-loop* ()
    (if ,c
      (begin ,@b (*while-loop*))
      #!void) ))

(define-macro (-> v . f)    ;Pass the return value of each call to the next item (l -> r)
  (define (wrapl val funs)
    (if (null? funs)
        val
        (wrapl (cons (car funs) val) (cdr funs)) ))
  (wrapl v f) )

(define-macro (catch action handler)  ;;force all exceptions coming through to be nonresumable
  `(with-exception-catcher
    ,handler
    (lambda () (with-exception-catcher (lambda (e) (abort e)) (lambda () ,action))) ))

;; These were appearing surprisingly often as lambdas - this is shorter
(define (id x) x)
(define (fst one two) one)
(define (snd one two) two)
(define (mid one two three) two)

(define (remp pred lst)      ;Useful func from R6RS
  (define (r-inner p src dst)
    (if (eq? src '())
        dst
        (if (p (car src))
            (r-inner p (cdr src) dst)
            (r-inner p (cdr src) (append dst (cons (car src) '()))))))
  (r-inner pred lst '()))

(define (list-tryref l k)   ;k-th element of l, or #f if l is shorter
  (cond ((null? l) #f)
        ((fx= k 0) (car l))
        (else (list-tryref (cdr l) (fx- k 1))) ))

(define (string-downcase s)  ;Destructive string-to-lower function
  (let ((l (string-length s)))
    (do ((i 0 (fx+ 1 i)))
        ((fx= i l) s)
        (string-set! s i (char-downcase (string-ref s i))))))

(define (show . x)
  (for-each (lambda (v) (pp v)(display " ")) x)
  (newline) )
