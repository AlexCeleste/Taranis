
;; Bits Basic compiler
;; v0.1 Backward-compatible with Blitz3D

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

;(include "Util.scm")
(include "Error.scm")
(include "Lexer.scm")
(include "Parser.scm")
;(include "Printer.scm")
;Rewriter/checker/optimiser
;(include "Writer.ss")


;; This is applied to each element of the argument list by remp; if the arg
;; is a flag, its setter method is immediately called, so we don't need to
;; either store the flags, or have a second canonical flag list.
(define (check-flag flag)
  (cond [(or (equal? flag "-?") (equal? flag "-help"))
         (set! show-help-flag #t) #t]
        [(equal? flag "-nowarn") (no-warning) #t]   ;Suppress warnings
        [(equal? flag "-werr") (w-error) #t]        ;Warnings to errors
        [(equal? flag "-dll")
         (flag-not-supported flag) #t]     ;Not ready yet
        [else #f]))    ;Assume it's a filename then

;; Setting this flag to true will cause compiler usage help to be printed
(define show-help-flag #f)

;; This procedure actually prints said compiler usage help
(define (show-help)
  (display "Usage: Basic source file [options] ...\n")
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
                          "current version of the Bits compiler.\n")))

(define (list-tokens tl)
  (for-each (lambda (t) (display t) (newline)) tl))

(define (display-ast ast)
  (pretty-print ast))

(define-syntax ->    ;Pass the return value of each call to the next item (l -> r) 
  (syntax-rules ()
    ((_ val f) (f val))
    ((_ val f g ...) (-> (f val) g ...))))

;; Outer handler (messages are printed prior to this, this is just to catch
;; the actual exception as we exit the compiler)
(with-exception-catcher
 (lambda (err) (cond [(or (eq? err 'scanner-error)
                          (eq? err 'parser-error)
                          (eq? err 'general-error))
                      (display "\nCompilation halted\n")]
                     [else (raise err)]))    ;Re-raise; not our exception

 (lambda ()
   ;; The first arg will always be the compiler itself, so get rid of it
   (let ((cmd-args (cdr (command-line))))
     (when (null? cmd-args)
       (set! show-help-flag #t))   ;Set the -help flag

     (let ((args (remp check-flag cmd-args)))

       (when (eq? show-help-flag #t) (show-help))

       (when (and (null? args) (eq? show-help-flag #f))
         (general-err "No filename arguments supplied to compiler."))

       (when (> (length args) 1)
         (general-warn (string-append"Current compiler version only "
                                     "accepts single filename argument.")))

       (unless (null? args)    ;Just do the first one for now
         (-> ;(car args)
             "Test0.bb"
             scan-source-file
             normalise-separators
             ;list-tokens
             build-ast
             display-ast
             ))))

   (display "done\n")))


;;(exit)

