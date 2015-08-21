
;; Simple error handling system

;; These procedures set the command line flags to disable warnings, or to
;; turn warnings into full errors.
(define (no-warning) (set! no-warn-flag #t))
(define (w-error) (set! w-error-flag #t))

;; These are the actual flags (cannot export mutable variables)
(define no-warn-flag #f)
(define w-error-flag #f)

;; Lexer standard error message
(define (lex-err lno file msg)
  (if lno
      (display (string-append "Scanner error on line " (number->string lno)
                              " of file \"" file "\":\n"))
      (if file
          (display (string-append "Scanner error in file \"" file "\":\n"))
          (display "Scanner error:\n")))
  (display (string-append "    " msg "\n"))
  (newline)
  (raise 'scanner-error))

;; Lexer standard warning message
(define (lex-warn lno file msg)
  (unless no-warn-flag                          ;Potentially disable this
    (when w-error-flag (lex-err lno file msg))  ;Pass it to full err proc

    (display (string-append "Warning: " msg "\n"))
    (if lno
        (display (string-append " on line " (number->string lno)
                                " of file \"" file "\":\n"))
        (display (string-append " in file \"" file "\":\n")))
    (newline)))

;; Struct-Parser standard error message
(define (parse-err lno file msg)
  (if lno
      (display (string-append "Parser error on line " (number->string lno)
                              " of file \"" file "\":\n"))
      (display (string-append "Parser error in file \"" file "\":\n")))
  (display (string-append "    " msg "\n"))
  (newline)
  (raise 'parser-error))

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
  (unless no-warn-flag
    (when w-error-flag (general-err msg))

    (display "General warning: \n")
    (display msg)
    (newline)))

