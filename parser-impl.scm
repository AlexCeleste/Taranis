
;; Parser rule implementation
;; This file defines the rule macro and its associated helpers

(define-macro (rule name body)
  (define (expand-rule r errbox)
    (cond
      ((and (pair? r) (or (eq? (car r) '->) (eq? (car r) '-->))) r)
      ((eq? r '!)
        (set-box! errbox #t) r )
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
           `(let* ((*errp (push-err-point)) (*res ,body)) (pop-err-point *errp (quote ,name)) *res)
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

;; Grammar production rules.

;; Choice operator: defines a list of patterns that can match a parsing rule.
;; Returns the first matching pattern, or #f.
(define-macro (<alt-rule> . r)
  `(let ((pos (get-position)))
    (or ,(car r)
      ,@(map (lambda (e) `(begin (set-position! pos) ,e)) (cdr r))
      (begin (set-position! pos) #f) )))

;; Concat operator: tests a list of rule elements in order, building a list of
;; the results of each element check. Returns #f on the first failing element.
;; The final element can be a filter function to apply to a successful match:
;; the form (-> (x y) x) returns a list of the expressions in the body, while
;; the form (--> (x y) (list x)) is a straightforward lambda (no auto list).
(define-macro (<cat-rule> . r)
  (define (expand-filter f)
    (list 'lambda (cadr filter)
      (if (eq? (car f) '->) (cons 'list (cddr filter)) (cddr filter)) ))
  (define filter #f)
  (let ((rev (reverse r)))
    (if (and (pair? (car rev)) (eq? (caar rev) '->))
      (begin
          (set! r (reverse (cdr rev)))
          (set! filter (car rev)) )))
  `(let ((tlist '()) (pos (get-position)))
    (if (and
      ,@(map (lambda (e)
          (if (eq? e '!)
              '(begin (set! *errp (get-position)) #t)
              `(let ((t ,e)) (if t (begin (set! tlist (cons t tlist)) #t) #f)) ))
          r) )
      ,(if filter
        `(apply ,(expand-filter filter) (reverse tlist))
        '(reverse tlist) )
      (begin (set-position! pos) #f) )))

;; Option operator: This wraps an optional element in a cat list, returning
;; '() instead of #f in the event of a failed match. This passes a null value
;; to the receiving function, but doesn't halt the match the way #f would.
(define-macro (<opt-rule> r)
  `(let ((t ,r) (pos (get-position)))
    (if t t (begin (set-position! pos) '())) ))

;; Star operator: This attempts to match an element in a cat list. Upon fail
;; the stream position is reset; upon success, the stream position is updated
;; and the match is attempted again, repeating until this fails. This operator
;; never returns #f, and will return an empty list if there is no match.
(define-macro (<star-rule> r)
  `(let <star>-loop ((tlist '()) (pos (get-position)))
    (let ((t ,r))  ;re-evaluate r here every time
      (if t
        (<star>-loop (cons t tlist) (get-position))
        (begin (set-position! pos) (reverse tlist)) ))))

;; Plus operator: Similar to the star operator, except that this one will fail
;; if the pattern does not match at least once.
(define-macro (<plus-rule> r)
  `(let ((t ,r))
    (if t
      (let <plus>-loop ((tlist '()) (pos (get-position)))
        (let ((t ,r))
          (if t
            (<plus>-loop (cons t tlist) (get-position))
            (begin (set-position! pos) (reverse tlist)) )))
      #f) ))


;; helpers: expand multi-component forms to nested-cat forms

(define-macro (cat-rule . r) (cons '<cat-rule> r))
(define-macro (alt-rule . r) (cons '<alt-rule> r))

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
