
;; Lexical scanner:
;; This file defines a simple tokeniser to convert a text source file into
;; a token stream (a list of token objects).
;; Exports: scan-source-file (accepts a source filename and returns a
;; token stream), accessors for the token objects

(include "irregex-0.8.3/irregex.scm")  ;Portable regexen, by Alex Shinn

;; The token stream is made up of token objects
(define-structure token type value line file)
(define (new-token ttype) (make-token ttype #f #f #f)) ;Just for export

(define scan-table #f)    ;Store the scan table here between invocations

;; The scan table is made up of rules
(define-structure scan-rule mode regex action arg)

;; Loaded file: name, content, current char and current line
(define-structure sfile dir name stream cptr cline)


;; Helpers for initialising the scan table - not exported

(define-syntax init-scan-table    ;The scan table contains a mode, regex,
  (syntax-rules ()                ;action and an argument for each rule.
    [(_ rule ...)
     (set! scan-table (list (expand-rule rule) ...))]))

(define-syntax expand-rule        ;Helper for init-scan-table
  (syntax-rules ()                ;Allows mode and type values to be omitted
    [(_ (a b)) (make-scan-rule 'start (rule->regex a) b 'nil)]
    [(_ (a b c)) (make-scan-rule 'start (rule->regex a) b c)]
    [(_ (m a b c)) (make-scan-rule m (rule->regex a) b c)]))


;; The main exported function to convert source code to tokens
;; Accepts one filename argument, and returns unprocessed token list
;; Can raise: file-not-found, recursive-include-directives, 
;; invalid-include-directive, illegal-character excpetions.
;; The scan table is defined here in loosely flex-inspired terms.
(define (scan-source-file filename)

  (let ((include-stack '())    ;A stack of files loaded via Include
        (working-dir (current-directory))
        (cfile #f)             ;The current source file
        (scan-mode 'start)     ;Current scan mode (start = default)
        (prev-mode 'start)     ;Previous scan mode (only for comments)
        (comment-lvl 0)        ;Current nested comment level
        (match-value #f)       ;The best token match string
        (match-rule #f)        ;Rule that generated the above string
        (token-stream '()))    ;The output token list

    ;; Load a file and add it to the include stack
    (define (load-source-file name)
      (set! name (path-expand name))
      (verify-source-file name)  ;Check file exists, is not recursive
      ;; Load file into string, return sfile object
      (let ((text (normalise-newlines (read-all-text name))))
        (set! cfile (make-sfile (path-directory name) name text 0 1))
        (set! include-stack (cons cfile include-stack)) ;Push to stack
        (current-directory (sfile-dir cfile))
        cfile))

    ;; Ensure a file is not included recursively, and actually exists
    (define (verify-source-file name)
      (if (file-exists? name)
          (let loop ((check-stack include-stack))
            (unless (eq? check-stack '())    ;No other files
              (if (eqv? (sfile-name (car check-stack)) name)
                  (lex-err (sfile-cline cfile) (sfile-name cfile)
                           (string-append "Recursively includes file " name))
                  (loop (cdr check-stack)))))
          (lex-err #f #f (string-append "File not found: " name))))

    ;; These are the default actions for rules - other actions can be
    ;; specified anonymously, or added here. Actions are applied after a
    ;; match is chosen.
    (define (store t)    ;Add the matched string as the next token
      (add-token t match-value))
    (define (type t)     ;Add the token but discard its value
      (add-token t ""))
    (define (set-mode m)   ;Change scan mode (not portable - but there
      (set! prev-mode scan-mode)    ;can only be one previous mode anyway)
      (set! scan-mode m))
    (define (skip x) 'nil)   ;No-op, to fill its space in the scan table
    (define (increment-line-count t)  ;Increment line, possibly store too
      (unless (eq? t 'nil)
        (add-token t (if (eq? t 'newline) "" match-value)))
      (sfile-cline-set! cfile (fx+ 1 (sfile-cline cfile))))
    
    ;; Unimplemented language features get this rule
    (define (unimplemented t)
      (lex-err (sfile-cline cfile) (sfile-name cfile)
               (string-append "'" t "' is an unfinished extension feature"
                              " not yet available for use with Bits Basic.")))
    
    (define (add-token type val)      ;Helper
      (set! token-stream
            (cons
             (make-token type val (sfile-cline cfile) (sfile-name cfile))
             token-stream)))


    ;; The rule table. Defined here to allow it to close over local defs
    (init-scan-table
     ;; In order to avoid leaning toothpick syndrome, # represents \
     ;; However, if a " needs to appear it must still be escaped normally

     ;; Numbers
     ("[0-9]+" store 'integer)
     ("\\%[01]+" store 'integer)         ;Binary (ints only)
     ("\\$[0-9a-f]+" store 'integer)    ;Hexadecimal (ints only)
     ("[0-9]*\\.[0-9]+([eE]-?[0-9]+)?" store 'float)
     ("(([0-9]+)|(0x[0-9a-f]+)|([0-9]*\\.[0-9]+([eE]-?[0-9]+)?))[a-z_]"
      (lambda (err) (lex-err (sfile-cline cfile) (sfile-name cfile) err))
      "number identifier conflict err")      ;Easiest way to spot
              ;;numbers running into words (that's Mono-friendly, anyway)

     ;; Strings
     ("\"\"" type 'string)     ;Empty string - store nothing
     ("\"" set-mode 'inString)
     ('inString "[^\\n\"]*" store 'string)  ;Value string (no escapes)
     ('inString "\"" set-mode 'start)
     ('inString "\\n" (lambda (m)        ;Generate a warning here:
                        (lex-warn (sfile-cline cfile) (sfile-name cfile)
                                  "String with unescaped newline")
                        (increment-line-count 'newline)
                        (set-mode m))
       'start)    ;Strings are forced to end on an unescaped newline

     ;; Comments
     (";[^\\n]*\\n" increment-line-count 'newline)
     
     ;; Newline, whitespace
     ("\\n" increment-line-count 'newline)
     ("[[:blank:]]" skip)

     ;; Include directive
     ("include" set-mode 'include)
     ('include "[[:blank:]]*\"[^\\n]*\""    ;Try to load file
       (lambda (m)
         (load-source-file (strip-quotes match-value)) (set-mode m))
       'start)
     ('include "\\n|[^\\n]"    ;Any other match is an error here 
       (lambda (x)
         (lex-err (sfile-cline cfile) (sfile-name cfile)
                  (string-append "illegal include path: " match-value)))
       'nil)

     ;; Keywords
     ("abs" type 'abs)
     ("after" type 'after)
     ("and" type 'and)
     ("before" type 'before)
     ("case" type 'case)
     ("const" type 'const)
     ("default" type 'default)
     ("delete" type 'delete)
     ("data" type 'data)
     ("dim" type 'dim)
     ("each" type 'each)
     ("else" type 'else)
     ("else[[:blank:]]*if" type 'elseif)
     ("end" type 'end)
     ("end[[:blank:]]+function" type 'endfunction)
     ("end[[:blank:]]*if" type 'endif)
     ("end[[:blank:]]+select" type 'endselect)
     ("end[[:blank:]]+type" type 'endtype)
     ("exit" type 'exit)
     ("false" type 'false)
     ("field" type 'field)
     ("first" type 'first)
     ("float" type 'tofloat)
     ("for" type 'for)
     ("forever" type 'forever)
     ("function" type 'function)
     ("global" type 'global)
     ("gosub" type 'gosub)
     ("goto" type 'goto)
     ("handle" type 'handle)
     ("if" type 'if)
     ("insert" type 'insert)
     ("int" type 'toint)
     ("last" type 'last)
     ("local" type 'local)
     ("mod" type 'mod)
     ("new" type 'new)
     ("next" type 'next)
     ("not" type 'not)
     ("null" type 'null)
     ("or" type 'or)
     ("object" type 'object)
     ("pi" type 'pi)
     ("repeat" type 'repeat)
     ("restore" type 'restore)
     ("return" type 'return)
     ("sar" type 'sar)
     ("select" type 'select)
     ("sgn" type 'sgn)
     ("shl" type 'shl)
     ("shr" type 'shr)
     ("step" type 'step)
     ("stop" type 'stop)
     ("str" type 'tostr)
     ("then" type 'then)
     ("to" type 'to)
     ("true" type 'true)
     ("type" type 'type)
     ("until" type 'until)
     ("wend" type 'wend)
     ("while" type 'while)
     ("xor" type 'xor)
     
     ;; Extension keywords
     ("@attribute" unimplemented)
     ("@call" unimplemented)
     ("@continue" unimplemented)
     ("@export" unimplemented)
     ("@end export" unimplemented)
     ("@strict" unimplemented)
     ("@try" unimplemented)
     ("@catch" unimplemented)
     ("@finally" unimplemented)
;;     ("class, end class" unimplemented)
;;     ("aspect, end aspect, around, pointcut" unimplemented)
     ("@rem" unimplemented)
     ("@end rem" unimplemented)
     ("@gc" unimplemented)

     ;; Punctuation
     ("<=" type 'leq)
     ("=<" type 'leq)
     (">=" type 'geq)
     ("=>" type 'geq)
     ("<>" type 'neq)
     ("," type 'comma)
     ("\\*" type 'mul)
     ("/" type 'div)
     ("\\(" type 'lparen)
     ("\\)" type 'rparen)
     ("-" type 'sub)
     ("\\+" type 'add)
     ("=" type 'equals)
     ("\\[" type 'lbracket)
     ("\\]" type 'rbracket)
     ("\\." type 'ttag)    ;Option to switch for colon
     ("<" type 'lt)
     (">" type 'gt)
     ("\\^" type 'pow)
     ("%" type 'intsig)
     ("#" type 'flosig)
     ("\\$" type 'strsig)
     (":" type 'colon)    ;Option to switch for semi
     ("\\\\" type 'member)    ;Option to switch for dot
     ("~" type 'bitnot)

     ;; Extension punctuation
     ("->" unimplemented)
     ("{" unimplemented)
     ("}" unimplemented)
     ("&" unimplemented)
     ("'" unimplemented)
     ("\\.\\." unimplemented)
     ("!" unimplemented)
     ("%%" unimplemented)
     ("\\[compound assigmnent operators\\]" unimplemented)   ;Type 'em out later

     ;; Identifiers (may not begin with a number, but this is caught above)
     ("[a-z_][a-z0-9_]*"
      (lambda (i) (string-downcase match-value) (store i)) 'identifier)

     ;; Unrecognised character (none of the above)
     ("[^\\n]" (lambda (x)
                 (display (char->integer (string-ref match-value 0)))(newline)
                 (lex-err (sfile-cline cfile) (sfile-name cfile)
                          "unrecognised character"))))


    (load-source-file filename)  ;Start by loading up the requested file
    
    ;; When an included file ends, pop it and resume the previous one
    (let file-loop ()
      ;; The main test/match loop, for each file
      (while (fx< (sfile-cptr cfile) (string-length (sfile-stream cfile)))
             (set! match-value "")    ;Reset to the empty string
             (let rule-loop ((rule-list scan-table))  ;For each rule...
               (when (eq? scan-mode (scan-rule-mode (car rule-list)))
                 (let ((try-value (try-regex
                                   (scan-rule-regex (car rule-list))
                                   (sfile-stream cfile)
                                   (sfile-cptr cfile))))
                   (when (fx> (string-length try-value)
                              (string-length match-value))
                     (set! match-value try-value)    ;Set to longest match
                     (set! match-rule (car rule-list)))))    ;Update rule
               (when (pair? (cdr rule-list))
                 (rule-loop (cdr rule-list))))  ;Loop if not done
             (sfile-cptr-set! cfile (fx+ (sfile-cptr cfile)
                                         (string-length match-value)))
             ;; Note that none of the rules can safely depend on cptr
             ((scan-rule-action match-rule) (scan-rule-arg match-rule)))

      (when (not (eq? scan-mode 'start))  ;Don't stay in comment mode
        (lex-warn (sfile-cline cfile) (sfile-name cfile)
                  (string-append "File ended while still in \""
                                 (symbol->string scan-mode)
                                 "\" mode"))
        (set! scan-mode 'start)
        (set! prev-mode 'start))

      (set! include-stack (cdr include-stack))  ;Pop the completed file
      (when (pair? include-stack)    ;As long as files remain, loop
        (set! cfile (car include-stack))
        (current-directory (sfile-dir cfile))
        (file-loop)))

    ;; Restore the working directory to what it was when we started
    (current-directory working-dir)
    ;; Append a final separator token if not present (simplifies things)
    (unless (and (pair? token-stream)    ;Don't crash on an empty file 
                 (eq? (token-type (car token-stream)) 'newline))
      (add-token 'newline ""))
    token-stream))   ;Return the list still reversed (better for next step)
    ;;(reverse token-stream)))


;; Read all text from a source file into a string
(define (read-all-text name)
  (let* ((sz (file-info-size (file-info name)))
         (s (make-string (fx+ 1 sz) #\newline)))  ;Room for a final newline
    (call-with-input-file name (lambda (p)
                                 (read-substring s 0 sz p) s))))

;; Replace CR and CRLF line endings in a string with LF
(define (normalise-newlines str)
  (irregex-replace/all "\r" (irregex-replace/all "\r\n" str "\n") "\n"))

;; Count the number of newlines in a string
(define (count-newlines str)
  (let ((count 0))
    (let loop ((charlist (string->list str)))
      (if (eq? charlist '())
          count
          (begin
            (set! count (fx+ count (if (char=? (car charlist) #\xA) 1 0)))
            (loop (cdr charlist)))))))

;; Replace escape sequences in strings with their literal values
;(define (replace-esc-seqs str)
;  (define-syntax esc   ;Unfortunately the macro is necessary as we need the
;    (syntax-rules ()   ;static types of the arguments to String.Replace
;      [(_ s (from to) ...) (begin
;                             (set! s (irregex-replace/all from s to))
;                             ...)]))
;  (esc str    ;A list of escape sequences and their literal equivalents 
;       ("\\\"" "\"")   ;Escaped double quote
;       ("\\n" "\n")    ;Newline (written as \n)
;       ("\\\n" ""))    ;An actual newline, escaped with \
;  str)

;; Remove the surrounding quotes from an included filename
(define (strip-quotes name)
  (irregex-replace/all "[[:blank:]]*\"[[:blank:]]" name ""))

(define (rule->regex rule)   ;Convenience function to create regex objects
  (irregex (string-append "^" rule) 'i))   ;Abandon use of # as escape character for now

(define (try-regex rule str start)    ;Convenience because pregexp-match returns #f on fail
  (let ((m (irregex-search rule str start)))
    (if m (irregex-match-substring m) "")))

;(display "Lexer loaded OK\n")
