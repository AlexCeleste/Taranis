
;; Lexical scanner implementation
;; This file defines procedures for building a simple tokeniser to convert a
;; text source file into a token stream (a vector of token objects).

(include "irregex-0.9.3/irregex.scm")  ;Portable regexen, by Alex Shinn


;; The token stream is made up of token objects
(define-structure token
  value type file line col)

;; The scan table is made up of rules
(define-structure scan-rule
  regex action arg mode)
(define (new-scan-rule regex action . arg)  ;need to add utf8 support here (needs byte-strings)
  (make-scan-rule (irregex `(: bos ,(string->sre regex)) 'i) action (list-tryref arg 0) (list-tryref arg 1)) )

;; Loaded file: name, content, current char and current line
(define-structure source-file
  dir name stream cptr cline ccol)
(define (load-source-file name)
  (let ((fullname (path-expand name)))
    (unless (file-exists? name)
      (lex-err (string-append "File not found: " name)))
    (let ((text (normalise-newlines (read-all-text fullname))))
      (make-source-file (path-directory name) name text 0 1 1) )))

;; Lexer object
(define-structure lexer
  rules out)
(define (new-lexer . rules)
  (make-lexer rules '()) )


;; The main exported function to convert source code to tokens
;; Accepts one filename argument, and returns a token stream (list)
;; Can raise compile-error
(define (scan-source-file lx filename)
  (let* ((match-value #f) ;The best token match string
         (match-rule #f)  ;The output token list
         (rule-list (lexer-rules lx))
         (cfile (load-source-file filename))  ;current file
         (file-len (string-length (source-file-stream cfile))) )   ;file stream length
    (lexer-out-set! lx '())
    (while (fx< (source-file-cptr cfile) file-len)
      (set! match-value "")    ;Reset to empty
      (for-each
        (lambda (rule)
          (let ((try-value
                  (try-regex (scan-rule-regex rule)
                             (source-file-stream cfile)
                             (source-file-cptr cfile))))
            (when (fx> (string-length try-value) (string-length match-value))
            (set! match-value try-value)    ;Set to longest match
            (set! match-rule rule) )))    ;Update rule
        rule-list)
      (if (fx> (string-length match-value) 0)
        (begin
          (source-file-cptr-set! cfile
            (fx+ (source-file-cptr cfile) (string-length match-value)))
          (let ((nlc (count-newlines match-value)))
            (if (fx> nlc 0)
              (begin
                (source-file-cline-set! cfile (fx+ (source-file-cline cfile) nlc))
                (source-file-ccol-set! cfile
                  (fx- (string-length match-value) (find-last-newline match-value))) )
              (source-file-ccol-set! cfile
                (fx+ (source-file-ccol cfile) (string-length match-value)) )))
          ;; Note that none of the rules can safely depend on cptr
          ((scan-rule-action match-rule) lx match-value (scan-rule-arg match-rule) cfile) )
        (source-file-cptr-set! cfile (fx+ (source-file-cptr cfile) 1)) ))

    ;; Append a final separator token if not present (simplifies things)
    (let ((out (lexer-out lx)))
      (unless (and (pair? out) (eq? (token-type (car out)) 'newline))
        (set! out (cons (make-token "" 'newline #f #f #f) out)) )
      (reverse out) )))

;; Lexer actions
(define (lexer-store-value lx val rule-arg f)  ;can't be bothered with any others right now
  (lexer-out-set! lx (cons
    (make-token val rule-arg (source-file-name f) (source-file-cline f) (source-file-ccol f))
    (lexer-out lx) )))
(define (lexer-match-error lx val rule-arg f)
  (lex-err (string-append "unrecognized symbol " val)
    (source-file-name f) (source-file-cline f) (source-file-ccol f) ))


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
  (let ((count 0) (slen (string-length str)))
    (do ((i 0 (fx+ i 1)))
      ((fx= i slen) count)
      (when (char=? (string-ref str i) #\xA) (set! count (fx+ count 1))) )))
;; Get index of last newline in a string
(define (find-last-newline str)
  (do ((i (fx- (string-length str) 1) (fx- i 1)))
    ((char=? (string-ref str i) #\xA) i)
    #!void) )

;; Replace escape sequences in strings with their literal values
(define (replace-esc-seqs str)
  (for-each
    (lambda (esc) (set! s (irregex-replace/all (car esc) str (cdr esc))))
    '(("\\\"" . "\"")   ;Escaped double quote
    ;  ("\\n"  . "\n")   ;Newline (written as \n)
    ;  ("\\\n" . "")     ;An actual newline, escaped with \
    )))

;; Remove the surrounding quotes from an included filename
(define (strip-quotes name)
  (irregex-replace/all "[[:blank:]]*\"[[:blank:]]" name ""))

(define (rule->regex rule)   ;Convenience function to create regex objects
  (irregex (string-append "^" rule) 'i))   ;Abandon use of # as escape character for now

(define (try-regex rule str start)
  (let ((m (irregex-search rule str start)))
    (if m (irregex-match-substring m) "")))

;(display "Lexer loaded OK\n")
