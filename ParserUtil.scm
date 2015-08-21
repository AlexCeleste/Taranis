
;; Parser Utilities:
;; This file defines generic helper functions that help with parsing, but
;; don't have any specific connection to a specific language or grammar.


;; Token stream storage and accessor functions
(define current-token-stream '())	;Store the current token here
(define current-error-point '())	;Point to report errors from

(define (get-token)    ;Get the current top token from stream (no increment)
  (when (null? current-token-stream)  ;Error if it doesn't exist
    (general-err "Encountered unexpected end-of-file while parsing module"))
  (car current-token-stream))
(define (get-tvalue) (token-value (get-token)))

(define (peek-token)   ;Get the current top token, or #f if doesn't exist
  (if (null? current-token-stream) #f (car current-token-stream)))

;; Increment the token stream
(define (next-token) (set! current-token-stream (cdr current-token-stream)))
;; Get and set current stream position (only for easy maintenance)
(define (get-position) current-token-stream)
(define (set-position! p) (set! current-token-stream p))
(define (set-error-point! p) (set! current-error-point p))
(define (set-parse-start! p)
	(set-position! p) (set-error-point! p) )

;; Base procedure for the check/expect accessor functions
(define (check-expect-base test cmp fail-action)
  (let ((t-tok (get-token)))
    (if (test t-tok cmp)      ;Test the top token somehow
        (begin
          (next-token) t-tok) ;If matched, increment stream & return match
        (fail-action))))

(define (check-ttype t)    ;See if top token matches type, if so return type
  (let ((t-tok (check-expect-base is-type? t return-false)))
    (if t-tok (token-type t-tok) #f)))
(define (expect-ttype t)    ;Exception if top token doesn't match type
  (token-type
   (check-expect-base is-type? t (lambda () (other-type-err t)))))

;; "Expecting-other-type exception"
(define (other-type-err t)
  (parse-err (token-line (get-token)) (token-file (get-token))
             (string-append "Expecting token type '"
                            (symbol->string t) "'; found token type '"
                            (symbol->string (token-type (get-token))) "':'"
                            (token-value (get-token)) "'")))

;; Helper functions to check token types and values
(define (is-type? t type)
  (eq? (token-type t) type))
(define (is-sep? t)
  (or (is-type? t 'newline) (is-type? t 'colon)))
(define (has-val? t val) (string=? (token-value t) val))

;; Utilities for the metaparser
(define (filter-void l)
	(let loop ((acc '()) (l l))
		(if (null? l) acc (loop (if (eq? (car l) #!void) acc (append acc (list (car l)))) (cdr l))) ))
(define (compose f g)
	(lambda (x) (f (g x))) )

(define (<parser-errpoint>)
	(set-error-point! (get-position)) )


;; Grammar production rules.

;; Choice operator: defines a list of patterns that can match a parsing rule.
;; If used at the outermost level, on success, "succ" is returned, or after a
;; complete failure, "fail"; if not, the results of the first matching rule are
;; returned, or #f. In both cases, the token stream is reset to its previous
;; position, after each failed match.
;; Note that since this uses the cond-apply form, any lists will be applied as
;; a single argument to the success procedure - may give unexpected results.
(define-syntax <parser-alt>
  (syntax-rules (=>)
    [(_ rule0 rulek ... => succ fail)   ;Outermost has a destination
     (let ((pos (get-position)))
       (cond (rule0 => succ)    ;Don't need to reset on first rule 
             ((begin (set-position! pos) rulek) => succ) ...
             (else (begin (set-position! pos) fail))))]
    [(_ rule0 rulek ...)
     (let ((pos (get-position)))
       (or rule0
           (begin (set-position! pos) rulek) ...
           (begin (set-position! pos) #f)))]))

;; Concat operator: tests a list of rule elements in order, building a list of
;; the results of each element check. If this is used as the outermost part of
;; a match attempt, it will reset the stream position on fail, but otherwise
;; will not (it will invariably be surrounded by something that can).
(define-syntax <parser-cat>
  (syntax-rules (=>)
    [(_ tok ... => succ fail)   ;Outermost operation
     (let ((tlist '()) (pos (get-position)))
       (if (and (let ((t tok))
                  (if t (begin (set! tlist (append tlist (list t))) tlist) #f))
                ...)
           (apply succ tlist)
           (begin (set-position! pos) fail)))]
    [(_ tok ...)
     (let ((tlist '()))
       (if (and (let ((t tok))
                  (if t (begin (set! tlist (append tlist (list t))) tlist) #f))
                ...)
           tlist #f))]))

;; Option operator: This wraps an optional element in a <++> list, returning
;; '() instead of #f in the event of a failed match. This passes a null value
;; to the receiving function, but doesn't halt the match the way #f would.
(define-syntax <parser-opt>
  (syntax-rules ()    ;This is never used at the outermost match level
    [(_ tok) (let ((t tok) (pos (get-position)))
               (if t t (begin (set-position! pos) '() )))]))

;; Star operator: This attempts to match an element in a <++> list. Upon fail
;; the stream position is reset; upon success, the stream position is updated
;; and the match is attempted again, repeating until this fails. Since this
;; never forms the outermost part of a match (and cannot return #f), it has no
;; success or failure procedure arguments.
(define-syntax <parser-rep>
  (syntax-rules ()
    [(_ tok) (let <rep>-loop ((pos (get-position)) (tlist '() ))
               (let ((t tok))  ;Re-evaluate tok here each time
                 (if t
                     (<rep>-loop (get-position) (append tlist (list t)))
                     (begin (set-position! pos) tlist))))]))

;; Plus operator: Similar to the star operator, except that this one will fail
;; if the pattern does not match at least once. This one is also never used at
;; the outer match level by Ravioli, and doesn't reset or have a succ/fail.
(define-syntax <parser-plus>
  (syntax-rules ()
    [(_ tok) (let ((t1 tok))  ;t1 and t2 represent the same pattern..clearer
               (if t1
                   (let <plus>-loop ((pos (get-position)) (tlist (list t1)))
                     (let ((t2 tok))  ;Re-evaluate tok here each time
                       (if t2
                           (<plus>-loop (get-position) (append tlist (list t2)))
                           (begin (set-position! pos) tlist))))
                   #f))]))

