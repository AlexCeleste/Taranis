
;; Parser rule implementation
;; This file defines the rule macro and its associated helpers

(define-macro (rule name body)
  (define (expand-rule r errbox)
    (cond
      ((and (pair? r) (eq? (car r) '->)) r)
      ((eq? r '!)
        (set-box! errbox #t) '(set-error-point!) )
      ((symbol? r)
        (let ((sr (symbol->string r)))
          (if (char=? (string-ref sr 0) #\%)
              (if (char=? (string-ref sr 1) #\!)
                  `(expect-ttype (quote ,(string->symbol (substring sr 2 (string-length sr)))))
                  `(check-ttype (quote ,(string->symbol (substring sr 1 (string-length sr))))) )
              (list r) )))
      (else
        (cons
          (case (car r)
            ((/) 'alt-rule)
            ((?) 'opt-rule)
            ((*) 'star-rule)
            ((+) 'plus-rule)
            (else 'cat-rule) )
          (map
            (lambda (s) (expand-rule s errbox))
            (case (car r) ((/ ? * +) (cdr r)) (else r)) )))))
  (let* ((errbox (box #f))
         (body (expand-rule body errbox)) )
    `(define (,name)
      ,(if (unbox errbox)
           `(let* ((*errp (push-err-point)) (*res ,body)) (pop-err-point *errp ,name) *res)
           body) )))

;; Token stream storage and accessor functions
(define current-token-stream #f) ;whole stream
(define current-token-index 0)   ;index of the current token
(define token-stream-length 0)

(define (get-token)    ;Get the current top token from stream (no increment)
 (when (>= current-token-index token-stream-length)  ;Error if it doesn't exist
   (general-err "Encountered unexpected end-of-file while parsing module") )
 (vector-ref current-token-stream current-token-index) )

(define (peek-token)   ;Get the current top token, or #f if doesn't exist
 (if current-token-stream
    (vector-ref current-token-stream current-token-index)
    #f) )

;; Increment the token stream
(define (next-token) (set! current-token-index (+ current-token-index 1)))

;; Get and set current stream position (only for easy maintenance)
(define (get-position) current-token-index)
(define (set-position! p) (set! current-token-index p))

;; error point handling
(define (set-error-point!)
  (set-car! current-error-point (get-position)) )
(define (push-err-point)
  (get-position) )
(define (pop-err-point p r)
  (when (> p (get-position))
    (parse-err (token-line (get-token)) (token-file (get-token))
               (string-append "error trying to complete " (expanded-rule-name (symbol->string r))) )))

(define (set-token-stream! s)
	(set! current-token-stream s)
  (set! current-token-index 0)
  (set! token-stream-length (vector-length s)) )

(define (check-ttype t)    ;See if top token matches type, if so return type
  (let ((tok (peek-token)))
    (if (and tok (eq? (token-type tok) t))
        tok
        #f) ))
(define (expect-ttype t)    ;Exception if top token doesn't match type
  (let ((tok (get-token)))
    (if (and tok (eq? (token-type tok) t))
        tok
        (other-type-err t) )))

;; "Expecting-other-type exception"
(define (other-type-err t)
 (parse-err (token-line (get-token)) (token-file (get-token))
            (string-append "Expecting token type '"
                           (symbol->string t) "'; found token type '"
                           (symbol->string (token-type (get-token))) "':'"
                           (token-value (get-token)) "'")))

;; Main rule macro implementations

(define-macro (opt-rule . r)
  `(<opt-rule>
    ,(if (> (length r) 1)
         (cons 'cat-rule r)
         r) ))

(define-macro (star-rule . r)
  `(<star-rule>
    ,(if (> (length r) 1)
         (cons 'cat-rule r)
         r) ))

(define-macro (plus-rule . r)
  `(<plus-rule>
    ,(if (> (length r) 1)
         (cons 'cat-rule r)
         r) ))
