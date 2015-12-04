#lang plai
(print-only-errors #t)

;---------------------------------------
; data definitions
;---------------------------------------

(define-type Expr
  [num (n number?)]
  [id (v symbol?)]
  [bool (b boolean?)]
  [bin-num-op (op procedure?) (lhs Expr?) (rhs Expr?)]
  [iszero (e Expr?)]
  [bif (test Expr?) (then Expr?) (else Expr?)]
  [with (bound-id symbol?) (bound-body Expr?) (body Expr?)]
  [fun (arg-id symbol?)
       (arg-type Type?) (result-type Type?)
       (body Expr?)]
  [app (fun-expr Expr?) (arg-expr Expr?)]
  [nempty]
  [ncons (first Expr?) (rest Expr?)]
  [nfirst (e Expr?)]
  [nrest (e Expr?)]
  [isnempty (e Expr?)])

(define-type Type
  [t-num]
  [t-bool]
  [t-nlist]
  [t-fun (arg Type?) (result Type?)])

;---------------------------------------
; helper functions
;---------------------------------------

;---------------------------------------
; main functions
;---------------------------------------

; parse-type : s-expression -> Type
; Converts the given s-expression into a Type
;   by parsing it according to this grammar:
;   type ::=       number
;          |       boolean
;          |       nlist
;          |       (type -> type)
(define (parse-type s-exp)
  (match s-exp
    ['number
      (t-num)]
    ['boolean
      (t-bool)]
    ['nlist
      (t-nlist)]
    [(list type_a '-> type_b)
      (t-fun (parse-type type_a)
             (parse-type type_b))]
    [_
      (error 'parse-type "Syntax Error: Invalid Type declaration")]))

  ; tests
    (test (parse-type 'number) (t-num))
    (test (parse-type 'boolean) (t-bool))
    (test (parse-type 'nlist) (t-nlist))
    (test (parse-type '{number -> boolean}) (t-fun (t-num) (t-bool)))
    (test (parse-type '{{number -> number} -> {boolean -> nlist}})
          (t-fun (t-fun (t-num) (t-num)) (t-fun (t-bool) (t-nlist))))

; parse : s-expression -> Expr
; Converts the given s-expression into a Expr
;   by parsing it according to this grammar:
;   expr ::=       num
;          |       true
;          |       false
;          |       (+ expr expr)
;          |       (- expr expr)
;          |       (* expr expr)
;          |       (iszero expr)
;          |       (bif expr expr expr)
;          |       id
;          |       (with (id expr) expr)
;          |       (fun (id : type) : type expr)
;          |       (expr expr)
;          |       nempty
;          |       (ncons expr expr)
;          |       (nempty? expr)
;          |       (nfirst expr)
;          |       (nrest expr)
;
;   type ::=       number
;          |       boolean
;          |       nlist
;          |       (type -> type)
; where num is a Racket number and id is an identifier not otherwise mentioned in the grammar.
(define (parse s-exp)
  (match s-exp

    ; |   num
    [(? number? s-exp)
      (num s-exp)]

    ; |   true
    ['true
      (bool true)]

    ; |   false
    ['false
      (bool false)]

    ; |   (+ expr expr)
    [(list '+ lhs rhs)
      (bin-num-op +
                  (parse lhs)
                  (parse rhs))]

    ; |   (- expr expr)
    [(list '- lhs rhs)
      (bin-num-op -
                  (parse lhs)
                  (parse rhs))]

    ; |   (* expr expr)
    [(list '* lhs rhs)
      (bin-num-op *
                  (parse lhs)
                  (parse rhs))]

    ; |   (iszero expr)
    [(list 'iszero expr)
      (iszero (parse expr))]

    ; |   (bif expr expr expr)
    [(list 'bif pred true-branch false-branch)
      (bif (parse pred)
           (parse true-branch)
           (parse false-branch))]
    [(list-rest 'bif rst)
     (error 'try-parse-bif "Syntax Error")]

    ; |   with
    [(list 'with (list bound-id bound-expr) body)
      (with bound-id
            (parse bound-expr)
            (parse body))]
    [(list-rest 'with rst)
     (error 'parse "Syntax Error")]

    ; |   (fun (id : type) : type expr)
    [(list 'fun (list param-id ': param-type) ': result-type body)
      (fun param-id
           (parse-type param-type)
           (parse-type result-type)
           (parse body))]
    [(list-rest 'fun rst)
     (error 'try-parse-fun "Syntax Error")]

    ; |   nempty
    ['nempty
      (nempty)]

    ; |   (ncons expr expr)
    [(list 'ncons fst rst)
      (ncons (parse fst)
             (parse rst))]

    ; |   (nempty? expr)
    [(list 'nempty? expr)
      (isnempty (parse expr))]

    ; |   (nfirst expr)
    [(list 'nfirst expr)
      (nfirst (parse expr))]

    ; |   (nrest expr)
    [(list 'nrest expr)
      (nrest (parse expr))]

    ; |   id
    ; NOTE: order matters here;
    ;   if this is earlier we might accidentally catch reserved constants (i.e., nempty, true, and false)
    [(? symbol? s-exp)
      (id s-exp)]

    ; |   (expr expr)
    ; NOTE: order matters here;
    ;   if this is earlier we might accidentally catch 2-length constructs (i.e., nempty?, nfirst, and nrest)
    [(list func-expr arg-expr)
      (app (parse func-expr)
           (parse arg-expr))]

    [_
      (error 'parse "Syntax error: Not an Expr")]))

  ; tests
    (test (parse '1) (num 1))
    (test (parse 'true) (bool true))
    (test (parse 'false) (bool false))
    (test (parse '{+ 1 2}) (bin-num-op + (num 1) (num 2)))
    (test (parse '{+ 1 {+ 2 3}}) (bin-num-op + (num 1) (bin-num-op + (num 2) (num 3))))
    (test (parse '{- 1 2}) (bin-num-op - (num 1) (num 2)))
    (test (parse '{- 1 {- 2 3}}) (bin-num-op - (num 1) (bin-num-op - (num 2) (num 3))))
    (test (parse '{* 1 2}) (bin-num-op * (num 1) (num 2)))
    (test (parse '{* 1 {* 2 3}}) (bin-num-op * (num 1) (bin-num-op * (num 2) (num 3))))
    (test (parse '{iszero 0}) (iszero (num 0)))
    (test (parse '{iszero {+ 1 2}}) (iszero (bin-num-op + (num 1) (num 2))))
    (test (parse '{bif true 10 20}) (bif (bool true) (num 10) (num 20)))
    (test (parse '{bif (iszero 10) {+ 1 2} {- 1 2}}) (bif (iszero (num 10)) (bin-num-op + (num 1) (num 2)) (bin-num-op - (num 1) (num 2))))
    (test (parse 'x) (id 'x))
    (test (parse '{with {x 2} x}) (with 'x (num 2) (id 'x)))
    (test (parse '{with {x {+ 1 2}} {+ x 3}}) (with 'x (bin-num-op + (num 1) (num 2)) (bin-num-op + (id 'x) (num 3))))
    (test (parse '{with {x {with {x 1} {+ x 2}}} {with {y 3} {+ x y}}})
          (with 'x (with 'x (num 1) (bin-num-op + (id 'x) (num 2))) (with 'y (num 3) (bin-num-op + (id 'x) (id 'y)))))
    (test (parse '{fun {x : number} : number x})
          (fun 'x (t-num) (t-num) (id 'x)))
    (test (parse '{fun {x : number} : boolean {iszero x}})
          (fun 'x (t-num) (t-bool) (iszero (id 'x))))
    (test (parse '{f 0}) (app (id 'f) (num 0)))
    (test (parse '{{f 0} {g 1}}) (app (app (id 'f) (num 0)) (app (id 'g) (num 1))))
    (test (parse '{fun {f : (number -> number)} : (number -> number)
                       {fun {y : number} : number {f {+ y 1}}}})
          (fun 'f (t-fun (t-num) (t-num)) (t-fun (t-num) (t-num))
               (fun 'y (t-num) (t-num) (app (id 'f) (bin-num-op + (id 'y) (num 1))))))
    (test (parse 'nempty) (nempty))
    (test (parse '{ncons 1 nempty}) (ncons (num 1) (nempty)))
    (test (parse '{ncons {ncons 1 nempty} {ncons 2 {ncons 3 nempty}}}) (ncons (ncons (num 1) (nempty)) (ncons (num 2) (ncons (num 3) (nempty)))))
    (test (parse '{nempty? nempty}) (isnempty (nempty)))
    (test (parse '{nempty? (ncons 1 nempty)}) (isnempty (ncons (num 1) (nempty))))
    (test (parse '{nfirst nempty}) (nfirst (nempty)))
    (test (parse '{nfirst (ncons 1 nempty)}) (nfirst (ncons (num 1) (nempty))))
    (test (parse '{nrest nempty}) (nrest (nempty)))
    (test (parse '{nrest (ncons 1 nempty)}) (nrest (ncons (num 1) (nempty))))

; type-of : Expr -> Type
; Returns what the type of the given program is,
;   or throws an error if there are type errors
; TODO: don't we also need to pass in a type environment?
(define (type-of expr)
  (match expr
    [(num (? number? n))
      (t-num)]
    [(num _) (error 'type-of "Type Error: Bad num value")]
    [(id (? symbol? sym))
      (error 'type-of "unimplemented")
      ; TODO: what do we do here? we need a type environment, right?
      ]
    [(id _) (error 'type-of "Type Error: Bad id value")]
    [(bool (? boolean? b))
      (t-bool)]
    [(bool _) (error 'type-of "Type Error: Bad bool value")]
    [(bin-num-op (? procedure? op) (? Expr? lhs) (? Expr? rhs))
      (unless (and (equal? (type-of lhs) (t-num))
                   (equal? (type-of rhs) (t-num)))
        (error 'type-of "Type Error: Non-num binop arguments"))
        (t-num)]
    [(iszero (? Expr? expr))
      (unless (equal? (type-of expr) (t-num))
        (error 'type-of "Type Error: Non-num iszero argument"))
      (t-bool)]
    [(bif (? Expr? bif-test) (? Expr? bif-then) (? Expr? bif-else))
      (unless (equal? (type-of bif-test) (t-bool))
        (error 'type-of "Type Error: Non-bool bif condition"))
      (let ([t-then (type-of bif-then)]
            [t-else (type-of bif-else)])
        (unless (equal? t-then t-else)
          (error 'type-of "Type Error: Non-matching bif branches"))
        t-then)]
    [(with (? symbol? bound-id) (? Expr? bound-body) (? Expr? body))
      (error 'type-of "unimplemented")
      ; TODO: extend the environment and recurse
      ]
    [(fun (? symbol? arg-id) (? Type? arg-type) (? Type? result-type) (? Expr? body))
      (error 'type-of "unimplemented")
      ; TODO: extend the environment and recurse
      ]
    [(app (? Expr? fun-expr) (? Expr? arg-expr))
      (match (type-of fun-expr) ; TODO: we need to extend the function's environment to know it's own type and it's parameter's type I think
        [(t-fun t-input t-output)
          (unless (equal? (type-of arg-expr) (t-input))
            (error 'type-of "Type Error: Applying function to bad input type"))
          t-output]
        [_
          (error 'type-of "Type Error: Function doesn't have a function-like type")]) ; TODO: will this error ever be thrown?
      ]
    [(nempty)
      (t-nlist)]
    [(ncons (? Expr? fst) (? Expr? rst))
      (unless (equal? (type-of fst) (t-num))
        (error 'type-of "Type Error: Non-num in cons:first"))
      (unless (equal? (type-of rst) (t-nlist))
        (error 'type-of "Type Error: Non-list in cons:rest"))
      (t-nlist)]
    [(nfirst (? Expr? expr))
      (unless (equal? (type-of expr) (t-nlist))
        (error 'type-of "Type Error: nfirst called on a non-list"))
      (t-num)]
    [(nrest (? Expr? expr))
      (unless (equal? (type-of expr) (t-nlist))
        (error 'type-of "Type Error: nrest called on a non-list"))
      (t-nlist)]
    [(isnempty (? Expr? expr))
      (unless (equal? (type-of expr) (t-nlist))
        (error 'type-of "Type Error: nempty? called on a non-list"))
      (t-bool)]))

  ; tests
    (define run (compose type-of parse))

    (test (run '1) (t-num))
    (test (run 'true) (t-bool))
    (test (run 'false) (t-bool))
    (test (run '{+ 1 2}) (t-num))
    (test (run '{+ 1 {+ 2 3}}) (t-num))
    (test (run '{- 1 2}) (t-num))
    (test (run '{- 1 {- 2 3}}) (t-num))
    (test (run '{* 1 2}) (t-num))
    (test (run '{* 1 {* 2 3}}) (t-num))
    (test (run '{iszero 0}) (t-bool))
    (test (run '{iszero {+ 1 2}}) (t-bool))
    (test (run '{bif true true false}) (t-bool))
    (test (run '{bif (iszero 10) {+ 1 2} {- 1 2}}) (t-num))
    ; (test (run 'x) (???)) ; TODO: what's expected here? an error?
    (test (run '{with {x 2} x}) (t-num))
    (test (run '{with {x {+ 1 2}} {+ x 3}}) (t-num))
    (test (run '{with {x {with {x 1} {+ x 2}}} {with {y 3} {+ x y}}}) (t-num))
    (test (run '{fun {x : number} : number x}) (t-fun (t-num) (t-num)))
    (test (run '{fun {x : number} : boolean {iszero x}}) (t-fun (t-num) (t-bool)))
    ; (test (run '{f 0}) (???)) ; TODO: what should this be? an error since it doesn't know f?
    ; (test (run '{{f 0} {g 1}}) (app (app (id 'f) (num 0)) (???))) ; TODO: what should this be? an error since it doesn't know f?
    (test (run '{fun {f : (number -> number)} : (number -> number)
                       {fun {y : number} : number {f {+ y 1}}}})
          (t-fun (t-fun (t-num) (t-num)) (t-fun (t-num) (t-num))))
    (test (run 'nempty) (t-nlist))
    (test (run '{ncons 1 nempty}) (ncons (num 1) (t-nlist)))
    (test (run '{ncons {ncons 1 nempty} {ncons 2 {ncons 3 nempty}}}) (t-nlist))
    (test (run '{nempty? nempty}) (n-bool))
    (test (run '{nempty? (ncons 1 nempty)}) (n-bool))
    (test (run '{nfirst nempty}) (t-num))
    (test (run '{nfirst (ncons 1 nempty)}) (t-num))
    (test (run '{nrest nempty}) (t-nlist))
    (test (run '{nrest (ncons 1 nempty)}) (t-nlist))



    ; TODO: error tests

    ; TODO: do we only need to test run, or type-of as well?

    (test (type-of (bif (iszero (bin-num-op + (num 1) (num 2))) (num 1) (num 0))) (t-num))


