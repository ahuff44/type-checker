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

(define-type TypeEnv
  [mtEnv]
  [anEnv (name symbol?)
         (value Type?)
         (env TypeEnv?)])

;---------------------------------------
; helper functions
;---------------------------------------

; lookup : symbol TypeEnv -> (or Type error)
; looks up an identifier in the given type environment
;   and returns the type bound to it
; If no match is found in the environment, a "Type Error: Unbound identifier" is thrown
(define (lookup name env)
  (type-case TypeEnv env
    [mtEnv ()
      (error 'lookup "Type Error: Unbound identifier")]
    [anEnv (current-name current-value next-env)
      (if (equal? name current-name)
        current-value
        (lookup name next-env))]))

  ; test data
    (define env1 (anEnv 'x (t-num) (mtEnv)))
    (define env1a (anEnv 'y (t-nlist) env1))
    (define env1ai (anEnv 'x (t-bool) env1a))
    (define env1aii (anEnv 'z (t-fun (t-num) (t-bool)) env1a))

    (define env2 (anEnv 'x (t-nlist) (mtEnv)))

  ; tests
    (test/exn (lookup 'x (mtEnv)) "Type Error: Unbound identifier")
    (test (lookup 'x env1) (t-num))
    (test (lookup 'x env1a) (t-num))
    (test (lookup 'x env1ai) (t-bool))
    (test (lookup 'x env1aii) (t-num))
    (test (lookup 'x env2) (t-nlist))

    (test (lookup 'y env1a) (t-nlist))
    (test (lookup 'y env1ai) (t-nlist))
    (test (lookup 'y env1aii) (t-nlist))
    (test/exn (lookup 'y env2) "Type Error: Unbound identifier")

    (test/exn (lookup 'z env1ai) "Type Error: Unbound identifier")
    (test (lookup 'z env1aii) (t-fun (t-num) (t-bool)))
    (test/exn (lookup 'z env2) "Type Error: Unbound identifier")

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
      (unless (symbol? bound-id)
        (error 'parse "Syntax Error: Non-symbol bind target"))
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
(define (type-of expr)
  (type-of-helper (mtEnv) expr))

; type-of-helper : TypeEnv Expr -> Type
; Returns what the type of the given program is,
;   or throws an error if there are type errors
; Uses the given environment when evaluating the type of the given program
(define (type-of-helper env expr)
  (match expr
    [(num (? number? n))
      (t-num)]
    [(id (? symbol? sym))
      (lookup sym env)]
    [(bool (? boolean? b))
      (t-bool)]
    [(bin-num-op (? procedure? op) (? Expr? lhs) (? Expr? rhs))
      (unless (and (equal? (type-of-helper env lhs) (t-num))
                   (equal? (type-of-helper env rhs) (t-num)))
        (error 'type-of-helper "Type Error: Non-num binop argument"))
        (t-num)]
    [(iszero (? Expr? expr))
      (unless (equal? (type-of-helper env expr) (t-num))
        (error 'type-of-helper "Type Error: Non-num iszero argument"))
      (t-bool)]
    [(bif (? Expr? bif-test) (? Expr? bif-then) (? Expr? bif-else))
      (unless (equal? (type-of-helper env bif-test) (t-bool))
        (error 'type-of-helper "Type Error: Non-bool bif condition"))
      (let ([t-then (type-of-helper env bif-then)]
            [t-else (type-of-helper env bif-else)])
        (unless (equal? t-then t-else)
          (error 'type-of-helper "Type Error: Non-matching bif branches"))
        t-then)]
    [(with (? symbol? bound-id) (? Expr? bound-body) (? Expr? with-body))
      (type-of-helper
        (anEnv bound-id (type-of-helper env bound-body) env)
        with-body)]
    [(fun (? symbol? arg-id) (? Type? arg-type) (? Type? result-type) (? Expr? body))
      (unless (equal? result-type (type-of-helper (anEnv arg-id arg-type env) body))
        (error 'type-of-helper "Type Error: Actual and declared function return types don't match"))
      (t-fun arg-type result-type)]
    [(app (? Expr? fun-expr) (? Expr? arg-expr))
      (match (type-of-helper env fun-expr)
        [(t-fun t-input t-output)
          (unless (equal? (type-of-helper env arg-expr) t-input)
            (error 'type-of-helper "Type Error: Applying function to bad input type"))
          t-output]
        [_
          (error 'type-of-helper "Type Error: Trying to apply a non-function")])
      ]
    [(nempty)
      (t-nlist)]
    [(ncons (? Expr? fst) (? Expr? rst))
      (unless (equal? (type-of-helper env fst) (t-num))
        (error 'type-of-helper "Type Error: Non-num in cons:first"))
      (unless (equal? (type-of-helper env rst) (t-nlist))
        (error 'type-of-helper "Type Error: Non-list in cons:rest"))
      (t-nlist)]
    [(nfirst (? Expr? expr))
      (unless (equal? (type-of-helper env expr) (t-nlist))
        (error 'type-of-helper "Type Error: nfirst called on a non-list"))
      (t-num)]
    [(nrest (? Expr? expr))
      (unless (equal? (type-of-helper env expr) (t-nlist))
        (error 'type-of-helper "Type Error: nrest called on a non-list"))
      (t-nlist)]
    [(isnempty (? Expr? expr))
      (unless (equal? (type-of-helper env expr) (t-nlist))
        (error 'type-of-helper "Type Error: nempty? called on a non-list"))
      (t-bool)]
    [_
      (error 'type-of-helper "***********Internal Assertion Error: Type Error: unknown cause")]))

  ; tests
    (define run (compose type-of parse))

    ; basic tests to show it works properly
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

      (test (run '{with {x 2} x}) (t-num))
      (test (run '{with {x {+ 1 2}} {+ x 3}}) (t-num))
      (test (run '{with {x {with {x 1} {+ x 2}}} {with {y 3} {+ x y}}}) (t-num))
      (test (run '{fun {x : number} : number x}) (t-fun (t-num) (t-num)))
      (test (run '{fun {x : number} : boolean {iszero x}}) (t-fun (t-num) (t-bool)))
      (test (run '{fun {f : (number -> number)} : (number -> number)
                       {fun {y : number} : number
                            {f {+ y 1}}}})
            (t-fun (t-fun (t-num) (t-num)) (t-fun (t-num) (t-num))))

      (test (run '{{fun {x : number} : number {+ x 3}} 1}) (t-num))
      (test (run '{with {f {fun {x : number} : number {+ x 3}}} {f 1}}) (t-num))
      (test (run '{{fun {x : number} : boolean {iszero x}} 1}) (t-bool))
      (test (run '{with {f {fun {x : number} : boolean {iszero x}}} {f 1}}) (t-bool))
      (test (run '{{fun {x : number} : nlist {ncons x nempty}} 1}) (t-nlist))
      (test (run '{with {f {fun {x : number} : nlist {ncons x nempty}}} {f 1}}) (t-nlist))

      (test (run 'nempty) (t-nlist))
      (test (run '{ncons 1 nempty}) (t-nlist))
      (test (run '{ncons 1 {ncons 2 {ncons 3 nempty}}}) (t-nlist))
      (test (run '{nempty? nempty}) (t-bool))
      (test (run '{nempty? (ncons 1 nempty)}) (t-bool))
      (test (run '{nfirst nempty}) (t-num))
      (test (run '{nfirst (ncons 1 nempty)}) (t-num))
      (test (run '{nrest nempty}) (t-nlist))
      (test (run '{nrest (ncons 1 nempty)}) (t-nlist))

      ; type env is preserved
        (test (run '{with {x 1} {with {y 2} {+ x y}}}) (t-num))
        (test (run '{with {x {fun {x : number} : nlist {ncons x nempty}}}
                          {with {y {fun {x : nlist} : boolean {nempty? x}}}
                                {y {x 1}}}}) (t-bool))
      ; shadowing
        (test (run '{with {x {fun {x : number} : nlist {ncons x nempty}}}
                          {with {x {fun {x : nlist} : boolean {nempty? x}}}
                                {x nempty}}}) (t-bool))

    ; errors
      ; id
        (test/exn (run 'x) "Type Error: Unbound identifier")
      ; bin-num-op
        (test/exn (run '{+ 1 false}) "Type Error: Non-num binop argument")
        (test/exn (run '{+ nempty 2}) "Type Error: Non-num binop argument")
        (test/exn (run '{+ {ncons 1 nempty} 2}) "Type Error: Non-num binop argument")
        (test/exn (run '{+ 3 {fun {x : number} : boolean true}}) "Type Error: Non-num binop argument")
        (test/exn (run '{- 1 false}) "Type Error: Non-num binop argument")
        (test/exn (run '{- nempty 2}) "Type Error: Non-num binop argument")
        (test/exn (run '{- {ncons 1 nempty} 2}) "Type Error: Non-num binop argument")
        (test/exn (run '{- 3 {fun {x : number} : boolean true}}) "Type Error: Non-num binop argument")
        (test/exn (run '{* 1 false}) "Type Error: Non-num binop argument")
        (test/exn (run '{* nempty 2}) "Type Error: Non-num binop argument")
        (test/exn (run '{* {ncons 1 nempty} 2}) "Type Error: Non-num binop argument")
        (test/exn (run '{* 3 {fun {x : number} : boolean true}}) "Type Error: Non-num binop argument")
      ; iszero
        (test/exn (run '{iszero true}) "Type Error: Non-num iszero argument")
        (test/exn (run '{iszero nempty}) "Type Error: Non-num iszero argument")
        (test/exn (run '{iszero {ncons 1 nempty}}) "Type Error: Non-num iszero argument")
        (test/exn (run '{iszero {fun {x : number} : number 1}}) "Type Error: Non-num iszero argument")
      ; bif
        (test/exn (run '{bif 1 true false}) "Type Error: Non-bool bif condition")
        (test/exn (run '{bif nempty true false}) "Type Error: Non-bool bif condition")
        (test/exn (run '{bif {ncons 1 nempty} true false}) "Type Error: Non-bool bif condition")
        (test/exn (run '{bif {fun {x : number} : number 2} true false}) "Type Error: Non-bool bif condition")

        (test/exn (run '{bif true 1 nempty}) "Type Error: Non-matching bif branches")
        (test/exn (run '{bif true 1 {ncons 1 nempty}}) "Type Error: Non-matching bif branches")
        (test/exn (run '{bif true 1 {fun {x : number} : number 2}}) "Type Error: Non-matching bif branches")
        (test/exn (run '{bif true nempty false}) "Type Error: Non-matching bif branches")
        (test/exn (run '{bif true {ncons 1 nempty} false}) "Type Error: Non-matching bif branches")
        (test/exn (run '{bif true {fun {x : number} : number 2} false}) "Type Error: Non-matching bif branches")
        (test/exn (run '{bif true nempty {fun {x : number} : number 2}}) "Type Error: Non-matching bif branches")

        (test/exn (run '{bif true {fun {x : number} : number 2} {fun {x : number} : boolean true}}) "Type Error: Non-matching bif branches")
        (test/exn (run '{bif true {fun {x : nlist} : number 2} {fun {x : number} : number 2}}) "Type Error: Non-matching bif branches")
      ; with
        (test/exn (run '{with {x y} 1}) "Type Error: Unbound identifier")
        (test/exn (run '{with {x 1} y}) "Type Error: Unbound identifier")
        (test/exn (run '{with {1 2} 1}) "Syntax Error: Non-symbol bind target")

        (test/exn (run '{with {x 1} {bif x 1 2}}) "Type Error: Non-bool bif condition")
        (test/exn (run '{with {x 1} {nempty? x}}) "Type Error: nempty? called on a non-list")
        (test/exn (run '{with {x 1} {x 5}}) "Type Error: Trying to apply a non-function")

        (test/exn (run '{with {x true} {+ 1 x}}) "Type Error: Non-num binop argument")
        (test/exn (run '{with {x true} {nempty? x}}) "Type Error: nempty? called on a non-list")
        (test/exn (run '{with {x true} {x 5}}) "Type Error: Trying to apply a non-function")

        (test/exn (run '{with {x nempty} {+ 1 x}}) "Type Error: Non-num binop argument")
        (test/exn (run '{with {x nempty} {bif x 1 2}}) "Type Error: Non-bool bif condition")
        (test/exn (run '{with {x nempty} {x 5}}) "Type Error: Trying to apply a non-function")

        (test/exn (run '{{with {x 1} x} 0}) "Type Error: Trying to apply a non-function")
      ; fun
        (test/exn (run '{fun {x : boolean} : number {+ x 1}}) "Type Error: Non-num binop argument")
        ; constant return values
          (test/exn (run '{fun {x : number} : number true}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : number nempty}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : number {ncons 1 nempty}}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : number {fun {x : number} : number x}}) "Type Error: Actual and declared function return types don't match")

          (test/exn (run '{fun {x : number} : boolean 1}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : boolean nempty}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : boolean {ncons 1 nempty}}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : boolean {fun {x : number} : number x}}) "Type Error: Actual and declared function return types don't match")

          (test/exn (run '{fun {x : number} : nlist 1}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : nlist true}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : nlist {fun {x : number} : number x}}) "Type Error: Actual and declared function return types don't match")

          (test/exn (run '{fun {x : number} : (number -> nlist) 1}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : (number -> nlist) true}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : (number -> nlist) nempty}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : (number -> nlist) {ncons 1 nempty}}) "Type Error: Actual and declared function return types don't match")

          ; higher order functions
            (test/exn (run '{fun {x : number} : (number -> nlist) {fun {x : number} : boolean true}}) "Type Error: Actual and declared function return types don't match")
            (test/exn (run '{fun {x : number} : (number -> nlist) {fun {x : nlist} : number 1}}) "Type Error: Actual and declared function return types don't match")
            (test/exn (run '{fun {x : number} : (number -> number) {fun {x : number} : nlist nempty}}) "Type Error: Actual and declared function return types don't match")

        ; non-constant return values
          (test/exn (run '{fun {x : number} : number (iszero x)}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : number} : boolean x}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : boolean} : number {bif x true false}}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : nlist} : number {nempty? x}}) "Type Error: Actual and declared function return types don't match")
          (test/exn (run '{fun {x : nlist} : nlist {nfirst x}}) "Type Error: Actual and declared function return types don't match")
      ; app
        (test/exn (run '{f 0}) "Type Error: Unbound identifier")
        (test/exn (run '{{f 0} {g 1}}) "Type Error: Unbound identifier")

        ; applying non-function
          (test/exn (run '{1 2}) "Type Error: Trying to apply a non-function")
          (test/exn (run '{true 1}) "Type Error: Trying to apply a non-function")
          (test/exn (run '{nempty 1}) "Type Error: Trying to apply a non-function")
          (test/exn (run '{{ncons 1 nempty} 2}) "Type Error: Trying to apply a non-function")

        ; bad input
          (test/exn (run '{{fun {x : number} : number 1} true}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : number} : number 1} nempty}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : number} : number 1} {ncons 1 nempty}}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : number} : number 1} {fun {x : number} : number 1}}) "Type Error: Applying function to bad input type")

          (test/exn (run '{{fun {x : boolean} : number 1} 1}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : boolean} : number 1} nempty}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : boolean} : number 1} {ncons 1 nempty}}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : boolean} : number 1} {fun {x : boolean} : number 1}}) "Type Error: Applying function to bad input type")

          (test/exn (run '{{fun {x : nlist} : number 1} 1}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : nlist} : number 1} true}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : nlist} : number 1} {fun {x : nlist} : number 1}}) "Type Error: Applying function to bad input type")

          (test/exn (run '{{fun {x : (number -> number)} : (number -> number) x} 1}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : (number -> number)} : (number -> number) x} true}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : (number -> number)} : (number -> number) x} nempty}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : (number -> number)} : (number -> number) x} {ncons 1 nempty}}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : (number -> number)} : (number -> number) x} {fun {x : number} : boolean true}}) "Type Error: Applying function to bad input type")
          (test/exn (run '{{fun {x : (number -> number)} : (number -> number) x} {fun {x : nlist} : number 1}}) "Type Error: Applying function to bad input type")
      ; ncons
        (test/exn (run '{ncons nempty nempty}) "Type Error: Non-num in cons:first")
        (test/exn (run '{ncons {ncons 1 nempty} nempty}) "Type Error: Non-num in cons:first")
        (test/exn (run '{ncons true nempty}) "Type Error: Non-num in cons:first")
        (test/exn (run '{ncons {fun {x : number} : number x} nempty}) "Type Error: Non-num in cons:first")

        (test/exn (run '{ncons 1 1}) "Type Error: Non-list in cons:rest")
        (test/exn (run '{ncons 1 false}) "Type Error: Non-list in cons:rest")
        (test/exn (run '{ncons 1 {fun {x : number} : number x}}) "Type Error: Non-list in cons:rest")
      ; nfirst
        (test (run '{nfirst nempty}) (t-num)) ; Runtime error, so we let it pass here
        (test/exn (run '{nfirst 1}) "Type Error: nfirst called on a non-list")
        (test/exn (run '{nfirst true}) "Type Error: nfirst called on a non-list")
        (test/exn (run '{nfirst {fun {x : nlist} : nlist nempty}}) "Type Error: nfirst called on a non-list")
      ; nrest
        (test (run '{nrest nempty}) (t-nlist))
        (test/exn (run '{nrest 1}) "Type Error: nrest called on a non-list")
        (test/exn (run '{nrest true}) "Type Error: nrest called on a non-list")
        (test/exn (run '{nrest {fun {x : nlist} : nlist nempty}}) "Type Error: nrest called on a non-list")
      ; nempty?
        (test/exn (run '{nempty? 1}) "Type Error: nempty? called on a non-list")
        (test/exn (run '{nempty? true}) "Type Error: nempty? called on a non-list")
        (test/exn (run '{nempty? {fun {x : nlist} : nlist nempty}}) "Type Error: nempty? called on a non-list")
