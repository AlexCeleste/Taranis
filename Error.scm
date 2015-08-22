
;; Simple error handling system

;; These procedures set the command line flags to disable warnings, or to
;; turn warnings into full errors.
(define (no-warnings) (set! emit-warning (lambda r #!void)))
(define (warning->error) (set! emit-warning raise))

(define emit-warning display)

;; generate a location for errors and warnings
(define (location-msg file lno cno)
  (if lno
    (string-append " on line " (number->string lno)
      (if cno (string-append ":" (number->string cno)) "")
      (if file (string-append " of '" file "'") "") )
    (if file (string-append " in '" file "'") "") ))

;; Lexer standard error message
(define (lex-err msg #!optional file lno cno)
  (raise (cons 'scanner-error
    (string-append "Error" (location-msg file lno cno) ":\n  " msg "\n") )))

;; Lexer standard warning message
(define (lex-warn msg #!optional file lno cno)
  (emit-warning
    (string-append "Warning: " msg "\n" (location-msg file lno cno) "\n") ))

;; Parser standard error message
(define (parse-err msg #!optional file lno cno)
  (raise (cons 'parser-error
    (string-append "Error" (location-msg file lno cno) ":\n  " msg "\n") )))

;; Struct-Parser scoping error message
(define (parse-scope-err msg)
  (display "Parser error: \n")
  (display msg)
  (newline)
  (raise 'parser-error))

;; General error message
(define (general-err msg)
  (display "General error: \n")
  (display msg)
  (newline)
  (raise 'general-error))

;; General warning message
(define (general-warn msg)
  (emit-warning
    (string-append "General warning: \n" msg "\n") ))
