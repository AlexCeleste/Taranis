
;; Taranis lexer
;; Regex-rule based lexical scanner


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
