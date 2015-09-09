
;; Taranis
;; a BlitzBasic compiler mostly backward-compatible with BlitzPlus/Blitz3D

(declare
  (block)
  (constant-fold)
  (lambda-lift)
  (inline)
  (inline-primitives)
  (standard-bindings)
  (extended-bindings)
  (not run-time-bindings)
  (not debug)
  (not generative-lambda)
  (optimize-dead-local-variables)
  (fixnum)
)

(include "util.scm")
(include "error.scm")
(include "lexer.scm")
;(include "parser.scm")
;...

;; This is applied to each element of the argument list by remp; if the arg
;; is a flag, its setter method is immediately called, so we don't need to
;; either store the flags, or have a second canonical flag list.
(define (check-flag flag)
  (cond ((or (equal? flag "-?") (equal? flag "-help"))
         (set! show-help-flag #t) #t)
        ((equal? flag "-nowarn") (no-warnings) #t)   ;Suppress warnings
        ((equal? flag "-werr") (warning->error) #t)        ;Warnings to errors
        ((equal? flag "-dll")
         (flag-not-supported flag) #t)     ;Not ready yet
        (else #f)))    ;Assume it's a filename then

;; Setting this flag to true will cause compiler usage help to be printed
(define show-help-flag #f)

;; This procedure actually prints said compiler usage help
(define (show-help)
  (display "Usage: BlitzBasic source file [options] ...\n")
  (display "Current compiler version only supports one input file.\n")
  (display "Options:\n")
  (display "    -? or -help   Display this information\n")
  (display "    -nowarn       Suppress warning messages\n")
  (display "    -werr         Convert warnings to errors\n")
  (display "    -shared       Produce a shared library\n")
  (newline))

;; Print a message indicating that the requested function isn't ready yet
(define (flag-not-supported flag)
  (display (string-append "The \"" flag "\" flag is not supported by the "
                          "current version of the compiler.\n")))

(define (list-tokens tl)
  (for-each (lambda (t) (display t) (newline)) tl))

(define (display-ast ast)
  (pretty-print ast))

(catch
  ;; The first arg will always be the compiler itself, so get rid of it
  (let ((cmd-args (cdr (command-line))))
    (when (null? cmd-args)
      (set! show-help-flag #t))   ;Set the -help flag

    (let ((args (remp check-flag cmd-args)))

      (when (eq? show-help-flag #t) (show-help))

      (when (and (null? args) (eq? show-help-flag #f))
        (general-err "No filename arguments supplied to compiler."))

      (when (> (length args) 1)
        (general-warn "Current compiler version only accepts single filename argument."))

      (unless (null? args)    ;Just do the first one for now
        (-> ;(car args)
          "tests/Test0.bb"
          scan-source-file
          list-tokens
          ;run include/import
          ;normalise-stream
          ;build-ast
          ;display-ast
          )))

      (display "done.\n"))

  (lambda (err)
    (if (pair? err)
      (case (car err) ;-> we're only using compile-error atm
        ((scanner-error parser-error compile-error)
          (display (cdr err))
          (display "Compilation halted.\n") )
        (else
          (raise err) ))
      (raise err) )))

;;(exit)
