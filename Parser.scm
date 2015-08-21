
;; Parser:
;; This file defines a simple recursive descent parser to convert a linear
;; token stream into an abstract syntax tree. The parser structure closely
;; mirrors the language grammar.


(include "ParserUtil.scm")


;; These are the types of object that make up the AST. This doesn't quite
;; match up to the grammar as these represent desired nodes, not syntax.

(define-structure variable scope name type init)  ;Type can include array size
(define-structure constant name type value)
(define-structure array name type dimensions)

(define-structure datatype name fields)      ;Fields are variables
(define-structure function name rtype params locals stmts)

(define-structure var-type datatype size)    ;C-style array type
(define-structure var-ref name type)    ;Useful for catching$ tagged# names%

(define-structure while-stmt expr stmts)
(define-structure repeat-stmt expr stmts)
(define-structure for-int-stmt var start finish step stmts)
(define-structure for-each-stmt var lst stmts)
(define-structure select-stmt expr cases default)
(define-structure if-stmt expr then else)
(define-structure return-stmt expr)
(define-structure goto-stmt lbl)
(define-structure gosub-stmt lbl)
(define-structure read-stmt var)
(define-structure restore-stmt lbl)
(define-structure end-stmt)
(define-structure exit-stmt)
(define-structure stop-stmt)

(define-structure case-block exprs stmts)
(define-structure label name)

(define-structure assignment-stmt lvalue rvalue)
(define-structure insert-stmt pos val rel)
(define-structure delete-stmt expr)

(define-structure d-array-access arr exprs)
(define-structure c-array-access arr expr)
(define-structure member-access obj mems)

(define-structure binary-expr op l r)  ;Since these are all left-associative
(define-structure unary-expr op val)
(define-structure list-expr op val)
(define-structure type-expr op type)

(define-structure function-call func args)
(define-structure object-cast type)

(define-structure data-stmt datums)
(define-structure b3d-integer val)
(define-structure b3d-float val)
(define-structure b3d-string val)


;; The main parse function: build an AST out of a linear token stream
;; Returns a list containing the top-level statements of one compilation unit
(define (build-ast stream)
  (set-position! stream)    ;Pass the token list to the streaming functions
  (let loop ((stmts '()))
    (if (peek-token)      ;Tokens remain to be read
      (if (check-sep)
          (loop stmts)  ;Don't append separators to the statement list
          (loop (cons [</> (global-line-declaration)
                           (block-declaration)
                           (control-statement)
                           (label-statement)
                           (expression-statement)
                           (data-statement)
                        => id
                           ((lambda ()
                              (parse-err-lf
                               (string-append
                                "Expecting expression statement; found '"
                                (symbol->string
                                 (token-type (peek-token))) "'"))))]
                      stmts)))
    ;; Do the reverse manually and expand any list declarations
    (reverse stmts))))  ;(It's backwards)


;; Rule functions: procedures that loosely correspond to grammar rules 

(define (global-line-declaration)
  [</> (var-declaration) (const-declaration) (array-declaration)])

(define (block-declaration)
  [</> (type-declaration) (function-declaration)])

(define (var-declaration)
  [<++> (</> [check-ttype 'local] [check-ttype 'global])
        (var-decl-body) (<*> [<++> (check-ttype 'comma) (var-decl-body)
                                => snd  ;Drop the comma
                                   #f])
     => (lambda (scope d dl)
          (map (lambda (v) (variable-scope-set! v scope) v) (cons d dl)))
        #f])

(define (var-decl-body)
  [</> (<++> [var-name][check-ttype 'lbracket]
             [expression][expect-ttype 'rbracket]
          => (lambda (var _l e _r)
               (var-type-size-set! (variable-type var) e) var)
             #f)
       (<++> [var-name][<?> (<++> [check-ttype 'equals][expression]
                               => snd #f)]
          => (lambda (var e) (variable-init-set! var e) var)
             #f)])

(define (const-declaration)
  [<++> (check-ttype 'const)(const-decl-body)
        (<*> [<++> (check-ttype 'comma)(const-decl-body)
                => snd #f])
     => (lambda (_c d dl) (cons d dl))
        #f])

(define (const-decl-body)
  [<++> (var-name)(expect-ttype 'equals)(expression)
     => (lambda (v _e e)
          (make-constant (variable-name v)
                         (var-type-datatype (variable-type v))
                         e))
        #f])

(define (array-declaration)
  [<++> (check-ttype 'dim)(array-decl-body)
        (<*> [<++> (check-ttype 'comma)(array-decl-body)
                => snd #f])
     => (lambda (_d d dl) (cons d dl))
        #f])

(define (array-decl-body)
  [<++> (var-name)(check-ttype 'lparen)(expression)
        (<*> [<++> (check-ttype 'comma)(expression)
                => snd #f])
        (expect-ttype 'rparen)
     => (lambda (v _l e el _r)
          (make-array (variable-name v)
                      (var-type-datatype (variable-type v))
                      (cons e el)))
        #f])

(define (type-declaration)
  [<++> (check-ttype 'type)(check-tval 'identifier)
        (member-list)(expect-ttype 'endtype)  ;Should not need newline check
     => (lambda (_t n ml _e) (make-datatype n ml))
        #f])

(define (member-list)
  (let loop ((members '()))
    (cond ((check-ttype 'newline) (loop members))
          ((field-declaration) => (lambda (f) (loop (cons f members))))
          (else (reverse members)))))

(define (field-declaration)
  [<++> (check-ttype 'field)
        (var-decl-body) (<*> [<++> (check-ttype 'comma) (var-decl-body)
                                => snd  ;Drop the comma
                                   #f])
     => (lambda (scope d dl)
          (map (lambda (v) (variable-scope-set! v scope) v) (cons d dl)))
        #f])

(define (function-declaration)
  [<++> (check-ttype 'function)(check-tval 'identifier)(<?> [type-tag])
        (func-param-list)(local-statement-list)(expect-ttype 'endfunction)
     => (lambda (_f n rt pl lsl _e) (make-function n rt pl '() lsl))
        #f])

(define (func-param-list)
  [<++> (check-ttype 'lparen)
        (<?> [<++> (var-decl-body)
                   (<*> [<++> (check-ttype 'comma) (var-decl-body)
                           => snd #f])
                => (lambda (d dl)
                     (map 
                      (lambda (v) (variable-scope-set! v 'local) v)
                      (cons d dl)))
                   #f])
        (expect-ttype 'rparen)
     => mid #f])

(define (local-statement-list)
  (let loop ((stmts '()))
    (cond ((check-sep) (loop stmts))
          ((local-statement) => (lambda (f) (loop (cons f stmts))))
          (else (reverse stmts)))))

(define (local-statement)
  [</> (var-declaration)(array-declaration)
       (control-statement)(label-statement)(expression-statement)])

(define (var-name . is-def)
  [<++> (check-tval 'identifier)(<?> [type-tag])
     => (lambda (n t) (if (pair? is-def)  ;Just name, or definition?
                          (make-var-ref n t)
                          (make-variable #f n (make-var-type t #f) #f)))
        #f])

(define (type-tag)
  [</> (check-ttype 'intsig)
       (check-ttype 'flosig)
       (check-ttype 'strsig)
       (<++> [check-ttype' ttag][expect-tval 'identifier]
          => snd #f)])

(define (control-statement)
  [</> (while-stmt)(repeat-stmt)(for-e-stmt)(for-i-stmt)(select-stmt)(if-stmt)
       (return-stmt)(goto-stmt)(gosub-stmt)(read-stmt)(restore-stmt)(end-stmt)
       (exit-stmt)(stop-stmt)])

(define (while-stmt)
  [<++> (check-ttype 'while)(expression)(local-statement-list)
        (expect-ttype 'wend)
     => (lambda (_w e sl _e) (make-while-stmt e sl))
        #f])

(define (repeat-stmt)
  [<++> (check-ttype 'repeat)(local-statement-list)(repeat-block-end)
     => (lambda (_r sl e) (make-repeat-stmt e sl))
        #f])

(define (repeat-block-end)
  [</> (check-ttype 'forever)
       (<++> [expect-ttype 'until][expression]
          => snd #f)])

(define (for-e-stmt)
  [<++> (check-ttype 'for)(var-name #f)(expect-ttype 'equals)(check-ttype 'each)
        (expect-tval 'identifier)(local-statement-list)(expect-ttype 'next)
     => (lambda (_f i _eq _e tl sl _n) (make-for-each-stmt i tl sl))
        #f])

(define (for-i-stmt)
  [<++> (check-ttype 'for)(var-name #f)(expect-ttype 'equals)
        (expression)(expect-ttype 'to)(expression)
        (<?> [<++> (check-ttype 'step)(expression)
                => snd #f])
        (local-statement-list)(expect-ttype 'next)
     => (lambda (_f i _eq e1 _t e2 s sl _n)(make-for-int-stmt i e1 e2 s sl))
        #f])

(define (select-stmt)
  [<++> (check-ttype 'select)(expression)
        (<*> [case-block])
        (<?> [default-block])
        (expect-ttype 'endselect)
     => (lambda (_s e cl d _e) (make-select-stmt e cl d))
        #f])

(define (case-block)
  [<++> (<?> [check-sep])(check-ttype 'case)
        (expression)(<*> [<++> (check-ttype 'comma)(expression)
                            => snd #f])
        (local-statement-list)
     => (lambda (_s _c e el sl) (make-case-block (cons e el) sl))
        #f])

(define (default-block)
  [<++> (<?> [check-sep])(check-ttype 'default)(local-statement-list)
     => (lambda (_s _d sl) sl)
        #f])

(define (if-stmt)
  [<++> (check-ttype 'if)(expression)(<?> [check-ttype 'then])
        (</> [<++> (check-ttype 'newline)(if-block-body)(expect-ttype 'endif)
                => mid #f]
             [if-line-body])  ;These return an if-stmt object
     => (lambda (_i e _t b) (if-stmt-expr-set! b e) b)
        #f])

(define (if-line-body)
  [<++> (if-line-list)
        (<?> [</> (<++> [check-ttype 'elseif][expression]
                        [<?> (check-ttype 'then)][if-line-body]
                     => (lambda (_e e _t b) (if-stmt-expr-set! b e) b)
                        #f)
                  (<++> [check-ttype 'else][if-line-list]
                     => snd #f)])
     => (lambda (t e) (make-if-stmt #f t e))
        #f])

(define (if-line-list)
    (let loop ((stmts '()))  ;Yes, really - an inline If can contain blocks
      (cond ((check-ttype 'colon) (loop stmts))
            ((local-statement) => (lambda (f) (loop (cons f stmts))))
            (else (reverse stmts)))))

(define (if-block-body)
  [<++> (local-statement-list)
        (<?> [</> (<++> [check-ttype 'elseif][expression]
                        [<?> (check-ttype 'then)][if-block-body]
                     => (lambda (_e e _t b) (if-stmt-expr-set! b e) b)
                        #f)
                  (<++> [check-ttype 'else][local-statement-list]
                     => snd #f)])
        ;(expect-ttype 'endif)
     => (lambda (t e) (make-if-stmt #f t e))
        #f])

(define (return-stmt)
  [<++> (check-ttype 'return)(expression)
     => (lambda (_r e) (make-return-stmt e))
        #f])

(define (goto-stmt)
  [<++> (check-ttype 'goto)(expect-tval 'identifier)
     => (lambda (_g l) (make-goto-stmt l))
        #f])

(define (gosub-stmt)
  [<++> (check-ttype 'gosub)(expect-tval 'identifier)
     => (lambda (_r l) (make-gosub-stmt l))
        #f])

(define (read-stmt)
  [<++> (check-ttype 'read)(var-name #f)
     => (lambda (_r v) (make-read-stmt v))
        #f])

(define (restore-stmt)
  [<++> (check-ttype 'restore)(check-tval 'identifier)
     => (lambda (_r l) (make-restore-stmt l))
        #f])

(define (end-stmt)
  (if (check-ttype 'end) (make-end-stmt) #f)) 

(define (exit-stmt)
  (if (check-ttype 'exit) (make-exit-stmt) #f))

(define (stop-stmt)
  (if (check-ttype 'end) (make-stop-stmt) #f))

(define (label-statement)
  [<++> (check-ttype 'ttag)(expect-tval 'identifier) ;Should rename it dot
     => (lambda (_d i) (make-label i))
        #f])

(define (expression-statement)
  [</> (assignment-statement)(command-statement)(list-statement)])

(define (assignment-statement)
  [<++> (l-value)(check-ttype 'equals)(expression)
     => (lambda (l _e e) (make-assignment-stmt l e))
        #f])

(define (command-statement)
  [</> (<++> [check-tval 'identifier][<?> (type-tag)]
             [<?> (<++> [expression][<*> (<++> [check-ttype 'comma][expression]
                                            => snd #f)]
                     => cons #f)]
          => (lambda (f _t al) (make-function-call f al))
             #f)
       (function-call)])

(define (list-statement)
  (</> [<++> (check-ttype 'insert)(expression)(list-operator)(expression)
          => (lambda (_i e1 pos e2) (make-insert-stmt pos e1 e2))
             #f]
       [<++> (check-ttype 'delete)(expression)
          => (lambda (_d e) (make-delete-stmt e))
             #f]))

(define (function-call)
  [<++> (check-tval 'identifier)(<?> [type-tag])
        (check-ttype 'lparen)
        (<?> [<++> (expression)(<*> [<++> (check-ttype 'comma)(expression)
                                       => snd #f])
                     => cons #f])
        (expect-ttype 'rparen)
          => (lambda (f _l _t al _r) (make-function-call f al))
             #f])

(define (l-value)
  [<++> (var-name #f)(<*> [</> (dim-array-elem)(c-array-elem)(member-access)])
     => (lambda (v rl)
          (let loop ((e v) (acc rl)) ;Avoid left recursion by looping
            (if (pair? acc)  ;Operations to attach
              (begin (cond ((d-array-access? (car acc))
                            (d-array-access-arr-set! (car acc) e))
                           ((c-array-access? (car acc))
                            (c-array-access-arr-set! (car acc) e))
                           ((member-access? (car acc))
                            (member-access-obj-set! (car acc) e)))
                (loop (car acc) (cdr acc)))
              e)))  ;Whew!
        #f])

(define (dim-array-elem)
  [<++> (check-ttype 'lparen)(expression)
        (<*> [<++> (check-ttype 'comma)(expression)
                => snd #f])
        (expect-ttype 'rparen)
     => (lambda (_l e el _r) (make-d-array-access #f (cons e el)))
        #f])

(define (c-array-elem)
  [<++> (check-ttype 'lbracket)(expression)(expect-ttype 'rbracket)
     => (lambda (_l e _r) (make-c-array-access #f e))
        #f])

(define (member-access)
  [<++> (check-ttype 'member)(var-name #f)
     => (lambda (_m m) (make-member-access #f m))
     #f])


;; Helper to make all undery expressions identical
(define u-helper
  (lambda (nl e)
    (let loop ((ns nl) (ne e))
      (if (pair? ns)
          (loop (cdr ns) (make-unary-expr (car ns) ne))
          ne))))

(define (expression)
  [<++> (<*> [check-ttype 'not])(bitwise-expr)
     => u-helper #f])

;; Helpers to make all left-associative binary expressions identical
(define b-helper-1 (lambda (op r) (make-binary-expr op #f r)))
(define b-helper-2
  (lambda (e be)
    (for-each (lambda (o) (binary-expr-l-set! o e) (set! e o)) be)
    e))

(define-syntax <bin-exp>
  (syntax-rules ()
    ((_ exp-l op exp-r) [<++> exp-l (<*> [<++> op exp-r
                                             => b-helper-1 #f])
                           => b-helper-2 #f])))

(define (bitwise-expr)
  (<bin-exp> [comp-expr]
             [</> (check-ttype 'and)(check-ttype 'or)(check-ttype 'xor)]
             [comp-expr]))

(define (comp-expr)
  (<bin-exp> [sum-expr]
             [</> (check-ttype 'lt)(check-ttype 'gt)(check-ttype 'equals)
                  (check-ttype 'neq)(check-ttype 'leq)(check-ttype 'geq)]
             [sum-expr]))

(define (sum-expr)
  (<bin-exp> [shift-expr]
             [</> (check-ttype 'add)(check-ttype 'sub)][shift-expr]))

(define (shift-expr)
  (<bin-exp> [mul-expr]
             [</> (check-ttype 'shl)(check-ttype 'shr)(check-ttype 'sar)]
             [mul-expr]))

(define (mul-expr)
  (<bin-exp> [pow-expr]
             [</> (check-ttype 'mul)(check-ttype 'div)(check-ttype 'mod)]
             [pow-expr]))

(define (pow-expr)
  (<bin-exp> [unary-expr][check-ttype 'pow][unary-expr]))


(define (unary-expr)
  [<++> (<*> [</> (unary-operator)(cast-operator)(list-operator)])
        (atomic-value)
     => u-helper #f])

(define (atomic-value)
  [</> (literal-value)(check-ttype 'null)(type-expr)(function-call)
       (<++> [check-ttype 'lparen][expression][expect-ttype 'rparen]
          => mid #f)
       (l-value)])

(define (literal-value)
  [</> (check-ttype 'true)(check-ttype 'false)(check-ttype 'pi)
       (get-literal 'integer make-b3d-integer)
       (get-literal 'float make-b3d-float)
       (get-literal 'string make-b3d-string)])

(define (get-literal t c)
  (let ((tok (peek-token))) (if (check-ttype t) (c (token-value tok)) #f))) 

(define (type-expr)
  [<++> (</> [check-ttype 'new][check-ttype 'first][check-ttype 'last])
        (expect-tval 'identifier)
     => (lambda (op type) (make-type-expr op type))
        #f])

(define (unary-operator)
  [</> (check-ttype 'add)(check-ttype 'sub)(check-ttype 'bitnot)
       (check-ttype 'abs)(check-ttype 'sgn)])

(define (cast-operator)
  [</> (check-ttype 'toint)(check-ttype 'tofloat)(check-ttype 'tostr)
       (check-ttype 'handle)
       (<++> [check-ttype 'object][check-ttype 'ttag]
             [expect-tval 'identifier]
          => (lambda (_o _d i) (make-object-cast i))
             #f)])

(define (list-operator)
  [</> (check-ttype 'before)(check-ttype 'after)])

(define (data-statement)
  [<++> (check-ttype 'data)(expression)(<*> [<++> (check-ttype 'comma)
                                                  (expression)
                                               => snd #f])
     => (lambda (_d e el) (make-data-stmt (cons e el)))
        #f])
                                           


;; Simplification of main error handling function
(define (parse-err-lf msg)
  (parse-err (token-line (get-token)) (token-file (get-token)) msg))


;; Where more than one newline or colon appear in a row, strip out the extra
;; separators. Takes a reversed token list.
(define (normalise-separators stream)
  (let loop ((head (cdr stream)) (out (list (car stream))))
    (if (pair? head)
        (cond ((and (is-sep? (car head)) (is-type? (car out) 'newline))
               (loop (cdr head) out))
              ((and (is-sep? (car head)) (is-type? (car out) 'colon))
               (set-car! out (car head))
               (loop (cdr head) out))
              (else (loop (cdr head) (cons (car head) out))))
        (if (is-sep? (car out))  ;Remove leading separator if present
            (cdr out) out))))


