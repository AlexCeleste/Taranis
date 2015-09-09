
;; Taranis lexer
;; Regex-rule based lexical scanner (powered by irregex)

(include "lexer-impl.scm")

(define (get-lexer)
  (define (R pat . typ)
    (cond
      ((null? typ) (new-scan-rule pat lexer-store-value (string->symbol pat)))
      ((eq? (car typ) 'error) (new-scan-rule pat lexer-match-error (cadr typ)))
      (else (new-scan-rule pat lexer-store-value (car typ))) ))
  (new-lexer
    ;; Numbers
    (R "[0-9]+" 'integer)
    (R "\\%[01]+" 'integer)      ;Binary (ints only)
    (R "\\$[0-9a-f]+" 'integer)  ;Hexadecimal (ints only)
    (R "[0-9]*\\.[0-9]+([eE]-?[0-9]+)?" 'float)
    (R "(([0-9]+)|(0x[0-9a-f]+)|([0-9]*\\.[0-9]+([eE]-?[0-9]+)?))[a-z_]"
    'error "number identifier conflict err") ;Spot numbers running into words

    ;; Strings
    (R "\"\"" 'string)     ;Empty string - store nothing
    (R "\"[^\\n\"]*\"" 'string)  ;Value string (no escapes)

    ;; Comments
    (R ";[^\\n]*\\n" 'newline)
    (R ";\\{[^}]*\\}" 'ignore)

    ;; Newline
    (R "\\n" 'newline)
    (R "[ \\t]+" 'ignore)

    ;; Keywords
    (R "abs")
    (R "after")
    (R "and")
    (R "before")
    (R "case")
    (R "const")
    (R "default")
    (R "delete")
    (R "data")
    (R "dim")
    (R "each")
    (R "else")
    (R "else[[:blank:]]*if" 'elseif)
    (R "end")
    (R "end[[:blank:]]*function" 'endfunction)
    (R "end[[:blank:]]*if" 'endif)
    (R "end[[:blank:]]*select" 'endselect)
    (R "end[[:blank:]]*type" 'endtype)
    (R "exit")
    (R "false")
    (R "field")
    (R "first")
    (R "float" 'tofloat)
    (R "for")
    (R "forever")
    (R "function")
    (R "global")
    (R "gosub")
    (R "goto")
    (R "handle")
    (R "if")
    (R "include")
    (R "insert")
    (R "int")
    (R "last")
    (R "local")
    (R "mod")
    (R "new")
    (R "next")
    (R "not")
    (R "null")
    (R "or")
    (R "object")
    (R "pi")
    (R "repeat")
    (R "restore")
    (R "return")
    (R "sar")
    (R "select")
    (R "sgn")
    (R "shl")
    (R "shr")
    (R "step")
    (R "stop")
    (R "str" 'tostr)
    (R "then")
    (R "to")
    (R "true")
    (R "type")
    (R "until")
    (R "wend")
    (R "while")
    (R "xor")

    ;; Punctuation
    (R "<=" 'leq)
    (R "=<" 'leq)
    (R ">=" 'geq)
    (R "=>" 'geq)
    (R "<>" 'neq)
    (R "," 'comma)
    (R "\\*" 'mul)
    (R "/" 'div)
    (R "\\(" 'lparen)
    (R "\\)" 'rparen)
    (R "-" 'sub)
    (R "\\+" 'add)
    (R "=" 'equals)
    (R "\\[" 'lbracket)
    (R "\\]" 'rbracket)
    (R "\\." 'ttag)    ;Option to switch for colon
    (R "<" 'lt)
    (R ">" 'gt)
    (R "\\^" 'pow)
    (R "%" 'intsig)
    (R "#" 'flosig)
    (R "\\$" 'strsig)
    (R ":" 'colon)    ;Option to switch for semi
    (R "\\\\" 'member)    ;Option to switch for dot
    (R "~" 'bitnot)

    ;; Extension punctuation
;    (R "->" unimplemented)
;    (R "{" unimplemented)
;    (R "}" unimplemented)
;    (R "&" unimplemented)
;    (R "'" unimplemented)
;    (R "\\.\\." unimplemented)
;    (R "!" unimplemented)
;    (R "%%" unimplemented)
;    (R "\\[compound assigmnent operators\\]" unimplemented)   ;Type 'em out later

    ;; Identifiers (may not begin with a number, but this is caught above)
    (R "[a-z_][a-z0-9_]*" 'identifier)

    ;; Unrecognised character (none of the above)
    (R "[^\\n]" 'error "unrecognised symbol") ))
