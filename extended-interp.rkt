#lang plai
(print-only-errors #t)

;
; Data definitions
;

; internal representation of binding a symbol to an expression
(define-type Binding
  [binding (name symbol?) (expr CFWAE?)])

; abstract syntax
(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?)
         (lhs CFWAE?)
         (rhs CFWAE?)]
  [with (lob (listof Binding?))
        (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?)
       (t CFWAE?)
       (e CFWAE?)]
  [fun (params (listof symbol?))
       (body CFWAE?)]
  [app (fun-expr CFWAE?)
       (args (listof CFWAE?))])

; internal representation of possible return values
(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])

; internal representation of an environment
(define-type Env
  [mtEnv]
  [anEnv (name symbol?)
         (value CFWAE-Value?)
         (env Env?)])

  ; test data
    (define my-closure (closureV (list 'x 'y)
                                 (binop + (id 'x) (id 'y))
                                 (mtEnv)))

    (define env1 (anEnv 'x (numV 1) (mtEnv)))
    (define env12 (anEnv 'y (numV 2) env1))
    (define env12c (anEnv 'x my-closure env12))
    (define env2 (anEnv 'x (numV 2) (mtEnv)))

;
; Helper functions
;

; flip : (a b -> c) -> (b a -> c)
; Returns a function equivalent to the input function,
;   except that it accepts its two arguments in the opposite order
; (this function does no error checking)
  (define (flip func)
    (lambda (x y)
            (func y x)))

  ; tests
    (test ((flip -) 4 2) -2)
    (test ((flip -) 2 4) 2)
    (test (expt 3 2) 9)
    (test ((flip expt) 2 3) 9)

; has-duplicates? : list -> boolean
; Returns whether lst contains any element more than once
; Implemented using an inefficient algorithm
  (define (has-duplicates? lst)
    (not (= (length lst)
            (length (remove-duplicates lst)))))

    ; tests
      (test (has-duplicates? empty) false)
      (test (has-duplicates? (list 1)) false)
      (test (has-duplicates? (list 1 2)) false)
      (test (has-duplicates? (list 1 2 3)) false)
      (test (has-duplicates? (list 1 2 3 3.0)) false)
      (test (has-duplicates? (list 1 2 3 3)) true)
      (test (has-duplicates? (list 1 2 3 1)) true)
      (test (has-duplicates? (list 'a 'b)) false)
      (test (has-duplicates? (list 'a 'b 'a)) true)
      (test (has-duplicates? (list 0 'a 1 'b 2 3 4 'a 5 7)) true)

; length-checker : number -> (list -> boolean)
; Returns a function that returns whether its argument (assumed to be a list) has the specified length
  (define (length-checker len)
    (lambda (lst)
            (= (length lst) len)))

  ; tests
    (test ((length-checker 0) empty) true)
    (test ((length-checker 1) empty) false)
    (test ((length-checker 0) (list 1)) false)
    (test ((length-checker 1) (list 1)) true)
    (test ((length-checker 2) (list 1)) false)
    (test ((length-checker 2) (list (list 1 2))) false)
    (test ((length-checker 4) (list 'b 2 2 (list 2 3 3 4))) true)

; ensure : Alpha procedure String -> (or true error)
; Returns true if (predicate value) is true; otherwise, errors with the specified message
  (define (ensure value predicate error-message)
    (if (predicate value)
        true
        (error error-message)))

  ; tests
    (define msg "generic error message")

    (test (ensure empty (length-checker 0) msg) true)
    (test/exn (ensure empty (length-checker -1) msg) msg)
    (test/exn (ensure empty (length-checker 1) msg) msg)
    (test (ensure (list 1) (length-checker 1) msg) true)
    (test (ensure (list 100) (length-checker 1) msg) true)
    (test (ensure (list 100 5 -13) (length-checker 3) msg) true)
    (test (ensure (list 2 2 'v (list 1 2 3 4 5)) (length-checker 4) msg) true)

    (test (ensure (list 0) list? msg) true)
    (test/exn (ensure (num 0) list? msg) msg)
    (test (ensure (list (list 0) (list 1 2)) (listof list?) msg) true)
    (test/exn (ensure (list 1 2) (listof list?) msg) msg)
    (test (ensure (list (id 'x) (num 0)) (compose id? first) msg) true)
    (test/exn (ensure (list (num 3) (num 0)) (compose id? first) msg) msg)

; ensure-all : Alpha (listof cons?) -> (or true error)
; Sequentially tests that each of the given predicates are true for the given value
; If any predicate fails, this method errors with the specifiec error message
; If all predicates pass, this method returns true
; pred-msg-list is assumed to be a list of (cons <predicate> <error-string-if-predicate-fails>) pairs
  (define (ensure-all value pred-msg-list)
    (andmap (match-lambda [(cons pred error-msg)
                            (ensure value
                                    pred
                                    error-msg)])
            pred-msg-list))

  ; tests (non-exhaustive; mainly here just to be usage examples)
    (define my-value (list (list 0 1) (list 3 4 5) (list 'a 'b)))
    (test (ensure-all my-value
                       (list (cons list? "Not a list")
                             (cons (listof list?) "Not a list of lists")
                             (cons (lambda (lst) (> (length lst) 1)) "Lists are too short")))
          true)
    (test/exn (ensure-all my-value
                           (list (cons list? "Not a list")
                                 (cons (listof list?) "Not a list of lists")
                                 (cons (length-checker 2) "Lists are not all pairs")
                                 (cons (listof (listof number?)) "Lists are non-numeric")))
          "Lists are not all pairs")

;
; Main functions
;

; lookup : symbol Env -> (or CFWAE-Value error)
; looks up an identifier in an environment and returns the value
;   bound to it (or reports error if not found)
  (define (lookup name env)
    (type-case Env env
      [mtEnv ()
             (error (string-append "Runtime error: Unbound identifier: " (symbol->string name)))]
      [anEnv (bound-name bound-value rest-env)
             (if (symbol=? name bound-name)
                 bound-value
                 (lookup name rest-env))]))

  ; tests
    (test/exn (lookup 'x (mtEnv)) "Runtime error: Unbound identifier")
    (test (lookup 'x env1) (numV 1))
    (test (lookup 'x env12) (numV 1))
    (test (lookup 'y env12) (numV 2))
    (test (lookup 'x env12c) my-closure)
    (test (lookup 'y env12c) (numV 2))
    (test/exn (lookup 'z env12c) "Runtime error: Unbound identifier")
    (test (lookup 'x env2) (numV 2))
    (test/exn (lookup 'y env2) "Runtime error: Unbound identifier")

; extend-Env : (list binding) Env -> Env
; Extends the given environment by adding in each of the given bindings
  (define (extend-Env params args env)
    (foldl anEnv
           env
           params args))

  ; tests
    (test (extend-Env empty empty (mtEnv)) (mtEnv))
    (test (extend-Env empty empty env1) env1)
    (test (extend-Env empty empty env12c) env12c)
    (test (extend-Env (list 'x)
                      (list (numV 1))
                      (mtEnv))
          env1)
    (test (extend-Env (list 'y)
                      (list (numV 2))
                      env1)
          env12)
    (test (extend-Env (list 'x 'y)
                      (list (numV 1) (numV 2))
                      (mtEnv))
          env12) ; depends on implementation (foldr v. foldl)

; div : number number -> number
; Returns n / d, or throws a custom error if d is 0
  (define (div n d)
    (and (ensure d
                 (compose not zero?)
                 "Runtime error: Division by zero")
         (/ n d)))

; A lookup table (symbol -> procedure) of arithmetic operations
(define op-table
  [list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ div)])

; lookup-op : symbol -> boolean
; Returns the procedure that corresponds to the given operator.
; Returns false if no matching procedure is found.
  (define (lookup-op op)
    (let [(result (assoc op op-table))]
      (if (equal? result false)
        false
        (second result))))

  ; tests
    (test (lookup-op '+) +)
    (test (lookup-op '-) -)
    (test (lookup-op '*) *)
    (test/pred (lookup-op '/) procedure?)

    ; sanity check
      (test ((lookup-op '+) 3 11) 14)
      (test ((lookup-op '-) 3 11) -8)
      (test ((lookup-op '*) 3 11) 33)
      (test ((lookup-op '/) 3 11) 3/11)

    ; returns false when appropriate
    (test (lookup-op 'a) false)
    (test (lookup-op '&) false)
    (test (lookup-op '++) false)
    (test (lookup-op 'a+) false)
    (test (lookup-op 'with) false)
    (test (lookup-op 'fun) false)
    (test (lookup-op 'if0) false)

; reserved? : symbol -> boolean
; Returns whether the given symbol is a reserved word
  (define/match (reserved? sym)
    [('with) true]
    [('fun) true]
    [('if0) true]
    [(sym)
     (if (equal? (lookup-op sym) false)
       false
       true)])

  ; tests
    (test (reserved? '+) true)
    (test (reserved? '-) true)
    (test (reserved? '*) true)
    (test (reserved? '/) true)
    (test (reserved? 'with) true)
    (test (reserved? 'fun) true)
    (test (reserved? 'if0) true)

    (test (reserved? 'f) false)
    (test (reserved? 'hello) false)
    (test (reserved? '+-) false)
    (test (reserved? 'a+) false)
    (test (reserved? 'if) false)
    (test (reserved? 'if1) false)
    (test (reserved? 'func) false)
    (test (reserved? 'app) false)
    (test (reserved? 'w) false)
    (test (reserved? 'wih) false)
    (test (reserved? 'with*) false)

; parse : s-exp -> CFWAE
; Converts the given s-expression into a CFWAE
;   by parsing according to this grammar:
;   CFWAE   =   number
;           |   (+ CFWAE CFWAE)
;           |   (- CFWAE CFWAE)
;           |   (* CFWAE CFWAE)
;           |   (/ CFWAE CFWAE)
;           |   id
;           |   (if0 CFWAE CFWAE CFWAE)
;           |   (with ([id CFWAE] ...) CFWAE)
;           |   (fun (id ...) CFWAE)
;           |   (CFWAE CFWAE ...)
; where `number` is a Racket number and `id` is not '+, '-, '*, '/, 'with, 'if0, or 'fun.
;
; (the last line of the BNF represents function application)
  (define (parse s-exp)
    (or (try-parse-id s-exp)
        (try-parse-num s-exp)
        (try-parse-from-list s-exp)
        (error "Syntax error: Not a CFWAE")))

  ; try-parse-id : s-expression -> (or CFWAE false)
  ; Blindly tries to parse s-exp to get an id token
  ; Returns false if it fails to parse
  (define (try-parse-id s-exp)
    (if (symbol? s-exp)
        (and (ensure s-exp
                     (compose not reserved?)
                     "Syntax error: Symbol is a reserved identifier")
             (id s-exp))
        false))

  ; try-parse-num : s-expression -> (or CFWAE false)
  ; Blindly tries to parse s-exp to get a num token
  ; Returns false if it fails to parse
  (define (try-parse-num s-exp)
    (if (number? s-exp)
        (num s-exp)
        false))

  ; try-parse-from-list : s-expression -> (or CFWAE false)
  ; Blindly tries to parse s-exp as if it's a list.
  ; Returns false if it fails to parse
  (define (try-parse-from-list s-exp)
    (if (list? s-exp)
        (and (ensure s-exp
                     (lambda (lst) (> (length lst) 0))
                     "Syntax error: Bad arity")
             (or (try-parse-binop s-exp) ; TODO: maybe change this into a switch on (First s-exp) and make s/try-parse-x/parse-x/g (for these 4 x)
                 ; e.g. [(cond (CFWAE-operator? fst))
                 ;       (parse-binop op-proc (rest s-exp))]
                 (try-parse-with s-exp)
                 (try-parse-fun s-exp)
                 (try-parse-if0 s-exp)
                 (parse-app s-exp)))
        false))

  ; try-parse-binop : (and s-expression non-empty-list) -> (or CFWAE false)
  ; Blindly tries to parse s-exp to get a binop token
  ; Returns false if it fails to parse
  ; Errors if it parses an invalid binop token
  (define (try-parse-binop s-exp-as-list)
    (if (symbol? (first s-exp-as-list))
        (let ([op-proc (lookup-op (first s-exp-as-list))])
            (if (not (equal? op-proc false)) ; TODO make this a func?)
                (and (ensure s-exp-as-list
                            (length-checker 3)
                            "Syntax error: Bad arity")
                     (binop op-proc
                            (parse (second s-exp-as-list))
                            (parse (third s-exp-as-list))))
                false))
        false))

  ; try-parse-with : (and s-expression non-empty-list) -> (or CFWAE false)
  ; Blindly tries to parse s-exp to get a with token
  ; Returns false if it fails to parse
  ; Errors if it parses an invalid with token
  (define (try-parse-with s-exp-as-list)
    (if (and (symbol? (first s-exp-as-list))
             (symbol=? (first s-exp-as-list) 'with))
        (and (ensure s-exp-as-list
                     (length-checker 3)
                     "Syntax error: Bad arity")
             (ensure-all (second s-exp-as-list)
                          (list (cons list? "Syntax error: Bindings is not a list of lists")
                                (cons (listof list?) "Syntax error: Bindings is not a list of lists")
                                (cons (listof (length-checker 2)) "Syntax error: Bad binding arity")
                                (cons (listof (compose symbol? first)) "Syntax error: Non-symbol bind target")
                                (cons (listof (compose not reserved? first)) "Syntax error: Bind target is a reserved identifier")
                                (cons (compose not has-duplicates? ((curry map) first)) "Syntax error: Multiple identical bind targets")))
             (with (map (lambda (bind-pair)
                                (binding (first bind-pair)
                                         (parse (second bind-pair))))
                        (second s-exp-as-list))
                   (parse (third s-exp-as-list))))
        false))

  ; try-parse-fun : (and s-expression non-empty-list) -> (or CFWAE false)
  ; Blindly tries to parse s-exp to get a fun token
  ; Returns false if it fails to parse
  ; Errors if it parses an invalid fun token
  (define (try-parse-fun s-exp-as-list)
    (if (and (symbol? (first s-exp-as-list))
             (symbol=? (first s-exp-as-list) 'fun))
        (and (ensure s-exp-as-list
                     (length-checker 3)
                     "Syntax error: Bad arity")
             (ensure-all (second s-exp-as-list)
                          (list (cons list? "Syntax error: Function parameters are not a list")
                                (cons (listof symbol?) "Syntax error: Non-symbol function parameters")
                                (cons (compose not has-duplicates?) "Syntax error: Multiple identical function parameters")
                                (cons (listof (compose not reserved?)) "Syntax error: Function parameters are reserved symbols")))
             (fun (second s-exp-as-list)
                  (parse (third s-exp-as-list))))
        false))

  ; try-parse-if0 : (and s-expression non-empty-list) -> (or CFWAE false)
  ; Blindly tries to parse s-exp to get an if0 token
  ; Returns false if it fails to parse
  ; Errors if it parses an invalid if0 token
  (define (try-parse-if0 s-exp-as-list)
    (if (and (symbol? (first s-exp-as-list))
             (symbol=? (first s-exp-as-list) 'if0))
        (and (ensure s-exp-as-list
                     (length-checker 4)
                     "Syntax error: Bad arity")
             (if0 (parse (second s-exp-as-list))
                  (parse (third s-exp-as-list))
                  (parse (fourth s-exp-as-list))))
        false))

  ; parse-app : (and s-expression non-empty-list) -> CFWAE
  ; Parses s-exp to get an app token
  ; Errors if parsing is impossible
    (define (parse-app s-exp-as-list)
      (let ([func-expr (parse (first s-exp-as-list))])
        (and (ensure-all func-expr
                         ; We're sure these two types of CFWAE cannot produce functions, so fail:
                         (list (cons (compose not num?) "Syntax error: Cannot apply a non-function")
                               (cons (compose not binop?) "Syntax error: Cannot apply a non-function")))
             (app func-expr
                  (map parse (rest s-exp-as-list))))))

  ; tests
    ; basic building blocks
      ; id
        (test {parse 'x} (id 'x))

      ; num
        (test {parse '5} (num 5))

      ; binop
        (test (parse '{+ 2 3})
                      (binop + (num 2)
                               (num 3)))
        (test (parse '{- 2 3})
                      (binop - (num 2)
                               (num 3)))
        (test (parse '{* 2 3})
                      (binop * (num 2)
                               (num 3)))
        (test (parse '{/ 2 3})
                      (binop div (num 2)
                                 (num 3)))
        (test (parse '{+ 1 {* 2 3}})
                      (binop + (num 1)
                               (binop * (num 2)
                                        (num 3))))

      ; with
        (test (parse '{with {} 1})
              (with empty (num 1)))
        (test (parse '{with {[x 1]} x})
              (with (list (binding 'x (num 1))) (id 'x)))
        (test (parse '{with {[x 1] [y 2]}
                            {+ x y}})
              (with (list (binding 'x (num 1))
                          (binding 'y (num 2)))
                    (binop + (id 'x) (id 'y))))
        (test (parse '{with {[x 1] [y x] [z z] [w {+ 2 x}]}
                            {+ x {* y z}}})
              (with (list (binding 'x (num 1))
                          (binding 'y (id 'x))
                          (binding 'z (id 'z))
                          (binding 'w (binop + (num 2) (id 'x))))
                    (binop + (id 'x)
                             (binop * (id 'y) (id 'z)))))

      ; fun
        (test (parse '{fun {} 1})
              (fun empty
                   (num 1)))
        (test (parse '{fun {x} x})
              (fun (list 'x)
                   (id 'x)))
        (test (parse '{fun {x y} x})
              (fun (list 'x 'y)
                   (id 'x)))
        (test (parse '{fun {x y &} {+ x &}})
              (fun (list 'x 'y '&)
                   (binop + (id 'x) (id '&))))

      ; app
        (test (parse 'f) ; not technically an `app` test
              (id 'f))
        (test (parse '{f})
              (app (id 'f) empty))
        (test (parse '{{f}})
              (app (app (id 'f) empty) empty))

        (test (parse '{{fun {} 1}})
              (app (fun empty
                        (num 1))
                   empty))
        (test (parse '{f 3})
              (app (id 'f)
                   (list (num 3))))
        (test (parse '{{with {[f {fun {x} x}]}
                             f}
                       3})
              (app (with (list (binding 'f (fun (list 'x)
                                                (id 'x))))
                         (id 'f))
                   (list (num 3))))
        (test (parse '{{if0 0
                            {fun {x y} x}
                            {fun {x y} y}}
                       3})
              (app (if0 (num 0)
                        (fun (list 'x 'y) (id 'x))
                        (fun (list 'x 'y) (id 'y)))
                   (list (num 3))))

      ; if0
        (test (parse '{if0 x y z})
              (if0 (id 'x) (id 'y) (id 'z)))
        (test (parse '{if0 {- 1 1} x {- 0 x}})
              (if0 (binop - (num 1) (num 1))
                   (id 'x)
                   (binop - (num 0) (id 'x))))

      ; binding a function with `with`
      (test (parse '{with {[f {fun {x}
                                   {* 2 x}}]}
                          {f 3}})
            (with [list (binding 'f (fun (list 'x)
                                         (binop * (num 2) (id 'x))))]
                  (app (id 'f) (list (num 3)))))

    ; misc exceptions
      (test/exn (parse true) "Syntax error: Not a CFWAE")
      (test/exn (parse empty) "Syntax error: Bad arity")

    ; id exceptions
      (test/exn (parse '+) "Syntax error: Symbol is a reserved identifier")
      (test/exn (parse '-) "Syntax error: Symbol is a reserved identifier")
      (test/exn (parse '*) "Syntax error: Symbol is a reserved identifier")
      (test/exn (parse '/) "Syntax error: Symbol is a reserved identifier")
      (test/exn (parse 'with) "Syntax error: Symbol is a reserved identifier")
      (test/exn (parse 'fun) "Syntax error: Symbol is a reserved identifier")
      (test/exn (parse 'if0) "Syntax error: Symbol is a reserved identifier")

    ; binop exceptions
      (test/exn (parse '{+}) "Syntax error: Bad arity")
      (test/exn (parse '{-}) "Syntax error: Bad arity")
      (test/exn (parse '{*}) "Syntax error: Bad arity")
      (test/exn (parse '{/}) "Syntax error: Bad arity")

      (test/exn (parse '{+ 1}) "Syntax error: Bad arity")
      (test/exn (parse '{- 1}) "Syntax error: Bad arity")
      (test/exn (parse '{* 1}) "Syntax error: Bad arity")
      (test/exn (parse '{/ 1}) "Syntax error: Bad arity")

      (test/exn (parse '{+ 1 2 3}) "Syntax error: Bad arity")
      (test/exn (parse '{- 1 2 3}) "Syntax error: Bad arity")
      (test/exn (parse '{* 1 2 3}) "Syntax error: Bad arity")
      (test/exn (parse '{/ 1 2 3}) "Syntax error: Bad arity")

    ; with exceptions
      (test/exn (parse '{with}) "Syntax error: Bad arity")
      (test/exn (parse '{with {[x 1]}}) "Syntax error: Bad arity")
      (test/exn (parse '{with {[x 1]} x x}) "Syntax error: Bad arity")

      (test/exn (parse '{with [x 1] x}) "Syntax error: Bindings is not a list of lists")
      (test/exn (parse '{with {+ 3 4} x}) "Syntax error: Bindings is not a list of lists")

      (test/exn (parse '{with {[]} x}) "Syntax error: Bad binding arity")
      (test/exn (parse '{with {[x]} x}) "Syntax error: Bad binding arity")
      (test/exn (parse '{with {[x 1 3]} x}) "Syntax error: Bad binding arity")

      (test/exn (parse '{with {[4 x]} x}) "Syntax error: Non-symbol bind target")
      (test/exn (parse '{with {[x +]} x}) "Syntax error: Symbol is a reserved identifier")
      (test/exn (parse '{with {[+ 7]} x}) "Syntax error: Bind target is a reserved identifier")

      (test/exn (parse '{with {[x 0] [x 10]} x}) "Syntax error: Multiple identical bind targets")
      (test/exn (parse '{with {[y 1] [x 0] [z 2] [x 10] [w 3]} w}) "Syntax error: Multiple identical bind targets")

    ; fun exceptions
      (test/exn (parse '{fun}) "Syntax error: Bad arity")
      (test/exn (parse '{fun {x}}) "Syntax error: Bad arity")
      (test/exn (parse '{fun {x} x x}) "Syntax error: Bad arity")

      (test/exn (parse '{fun x x}) "Syntax error: Function parameters are not a list")

      (test/exn (parse '{fun {x x} x}) "Syntax error: Multiple identical function parameters")
      (test/exn (parse '{fun {x y z y} x}) "Syntax error: Multiple identical function parameters")

      (test/exn (parse '{fun {1} 1}) "Syntax error: Non-symbol function parameters")
      (test/exn (parse '{fun {x 1 y} 1}) "Syntax error: Non-symbol function parameters")
      (test/exn (parse '{fun {{x}} 1}) "Syntax error: Non-symbol function parameters")
      (test/exn (parse '{fun {x {y} z} 1}) "Syntax error: Non-symbol function parameters")
      (test/exn (parse '{fun {x {+ 1 2} z} 1}) "Syntax error: Non-symbol function parameters")

      (test/exn (parse '{fun {x + y} x}) "Syntax error: Function parameters are reserved symbols")
      (test/exn (parse '{fun {x - y} x}) "Syntax error: Function parameters are reserved symbols")
      (test/exn (parse '{fun {x * y} x}) "Syntax error: Function parameters are reserved symbols")
      (test/exn (parse '{fun {x / y} x}) "Syntax error: Function parameters are reserved symbols")
      (test/exn (parse '{fun {x with y} x}) "Syntax error: Function parameters are reserved symbols")
      (test/exn (parse '{fun {x if0 y} x}) "Syntax error: Function parameters are reserved symbols")
      (test/exn (parse '{fun {x fun y} x}) "Syntax error: Function parameters are reserved symbols")

    ; app exceptions
      (test/exn (parse '{0 0}) "Syntax error: Cannot apply a non-function")
      (test/exn (parse '{{+ 0 0} 0}) "Syntax error: Cannot apply a non-function")
      (test/exn (parse '{{} 3}) "Syntax error: Bad arity") ; not technically an `app` test

    ; if0 exceptions
      (test/exn (parse '{if0}) "Syntax error: Bad arity")
      (test/exn (parse '{if0 x}) "Syntax error: Bad arity")
      (test/exn (parse '{if0 x y}) "Syntax error: Bad arity")
      (test/exn (parse '{if0 x y z w}) "Syntax error: Bad arity")

    ; long, ugly recursion
    (test (parse '{+ 2 {* x1 {with {[y_sub 5] [x1 121]} {/ {with {[zebra 10] [web-gun 33]} {- y_sub zebra}} {if0 zebra {{fun {} 2}} {{fun {x} {- 0 x}} zebra}}}}}})
          (binop + (num 2)
                   (binop * (id 'x1)
                            (with [list (binding 'y_sub (num 5))
                                        (binding 'x1 (num 121))]
                                  (binop div (with [list (binding 'zebra (num 10))
                                                         (binding 'web-gun (num 33))]
                                                   (binop - (id 'y_sub) (id 'zebra)))
                                             (if0 (id 'zebra)
                                                  (app (fun empty
                                                            (num 2) )
                                                       empty)
                                                  (app (fun (list 'x)
                                                            (binop - (num 0) (id 'x)))
                                                       (list (id 'zebra)))))))))

; interp : CFWAE Env -> CFWAE-Value
; Interprets the given CFWAE with respect to the given environment
  (define (interp expr env)
    (type-case CFWAE expr
      [num (n)
           (numV n)]
      [binop (op lhs rhs)
             (let ([lhs-value (interp lhs env)]
                   [rhs-value (interp rhs env)])
                  (and (ensure lhs-value
                               numV?
                               "Runtime error: Argument to a binop is not a number")
                       (ensure rhs-value
                               numV?
                               "Runtime error: Argument to a binop is not a number")
                       (numV (op (numV-n lhs-value) (numV-n rhs-value)))))]
      [with (lob body)
            (interp body
                    (extend-Env (map binding-name lob)
                                (map (compose ((curry (flip interp)) env) ; interpret the expression in every binding
                                              binding-expr)
                                     lob)
                                env))]
      [id (name)
          (lookup name env)]
      [if0 (c t e)
           (let ([cond-value (interp c env)])
                (and (ensure cond-value
                             numV?
                             "Runtime error: Conditional expression is not a number")
                     (interp (if (zero? (numV-n cond-value))
                                 t
                                 e)
                             env)))]
      [fun (params body)
           (closureV params body env)]
      [app (fun-expr args)
           (let ([fun-value (interp fun-expr env)])
                (and (ensure fun-value
                             closureV?
                             "Runtime error: Cannot apply a non-function")
                     (let ([params (closureV-params fun-value)]
                           [body (closureV-body fun-value)]
                           [closure-env (closureV-env fun-value)])
                      (and (ensure params
                                   (length-checker (length args))
                                   "Runtime error: Argument/parameter arity mismatch")
                           (interp body
                                   (extend-Env params ; TODO: implement by using `with`? The code duplication here smells...
                                               (map ((curry (flip interp)) env)
                                                    args)
                                               closure-env))))))]))

  ; interp is implicitly tested later in the `run` tests

; run : s-expression -> CFWAE-Value
; A helper function that parses and then evaluates an s-expression in the CFWAE language
  (define (run s-exp)
    (interp (parse s-exp) (mtEnv)))

  ; tests
    ; basic tests
      ; basic arithmetic
        (test (run '5) (numV 5))
        (test (run '{+ 4 2}) (numV 6))
        (test (run '{- 4 2}) (numV 2))
        (test (run '{* 4 2}) (numV 8))
        (test (run '{/ 4 2}) (numV 2))

      ; with
        (test (run '{with {} 4}) (numV 4))
        (test (run '{with {[x 5]} x}) (numV 5))
        (test (run '{with {[x 5] [y 11]} {+ x y}}) (numV 16))
        (test (run '{with {[x 5] [y 11]} {- x y}}) (numV -6))
        (test (run '{with {[x 5] [y 11]} {* x y}}) (numV 55))
        (test (run '{with {[x 5] [y 11]} {/ x y}}) (numV 5/11))
        ; Multiple substitution
        (test (run '{with {[x 5]}
                          {+ x x}})
              (numV 10))
        ; Nested withs
        (test (run '{with {[x 5]}
                          {with [{y 6}]
                                {+ x y}}})
              (numV 11))
        ; substituting into deeper with bindings
        ; also, binding to an expression
        (test (run '{with {[x 5]}
                          {with {[y {+ x 1}]}
                                {- y x}}})
              (numV 1))
        ; shadowing tests are later

      ; if0
        (test (run '{if0 0 1 2})
              (numV 1))
        (test (run '{if0 10 1 2})
              (numV 2))
        (test (run '{if0 {- 10 10} 1 2})
              (numV 1))
        (test (run '{if0 {+ 10 10} 1 2})
              (numV 2))
      ; fun/app
        (test (run '{{fun {} 1}})
              (numV 1))
        (test (run '{{fun {x} x} 3})
              (numV 3))
        (test (run '{{{fun {x} {fun {y} {+ x y}}} 3} 11})
              (numV 14))
        (test (run '{{fun {x y} {/ y x}} 3 11})
              (numV 11/3))
        (test (run '{with {[f {fun {x} x}]}
                          {f 3}})
              (numV 3))
        (test (run '{with {[f {fun {x} x}]}
                          {{fun {x} {f {* 2 x}}} 3}})
              (numV 6))
        ; shadowing tests are later

      ; can return closureV objects
        (test (run '{fun {} 1}) (closureV empty (num 1) (mtEnv)))
        (test (run '{with {[x 2]}
                          {fun {y} {+ x y}}})
              (closureV (list 'y)
                        (binop + (id 'x) (id 'y))
                        (anEnv 'x (numV 2) (mtEnv))))

    ; more complicated tests:
      ; make sure args->params transition always works:
        (test (run '{with {[f {fun {x} x}]}
                          {with {[g {fun {y} {f y}}]}
                                {f 1}}})
              (numV 1))
        (test (run '{with {[f {fun {x} x}]}
                          {with {[g {fun {x} {f x}}]}
                                {f 1}}})
              (numV 1))
        (test (run '{with {[f {fun {x} x}]}
                              {with {[g {fun {y} {f y}}]}
                                    {g 1}}})
              (numV 1))
        (test (run '{with {[f {fun {x} x}]}
                          {with {[g {fun {x} {f x}}]}
                                {g 1}}})
              (numV 1))
      ; shadowing
        ; basic function shadowing/scoping
        (test (run '{{{fun {x}
                           {fun {x} x}}
                      5}
                     6})
              (numV 6))
        ; shadowing of `with` params within `with` body
          (test (run '{with {[x 5]}
                            {with {[x 6]}
                                  x}})
                (numV 6))
          (test (run '{with {[x 5]}
                            {with {[x {+ x 1}]}
                                  x}})
                (numV 6))
        ; shadowing of `fun` params within `fun` body
          (test (run '{{fun {x}
                            {{fun {x} x}
                             6}}
                        5})
                (numV 6))
          (test (run '{{fun {x}
                            {{fun {x} x}
                             {+ x 1}}}
                        5})
                (numV 6))
        ; shadowing of `fun` params within `with` body
          (test (run '{with {[x 5]}
                            {{fun {x} {+ 1 x}} 1}})
                (numV 2))
          (test (run '{with {[x 5]}
                            {{fun {x} {+ 1 x}} {* x 10}}})
                (numV 51))
        ; shadowing of `with` params within `fun` body
          (test (run '{{fun {x} {with {[x 1]} {+ 1 x}}}
                       5})
                (numV 2))
          (test (run '{{fun {x}
                            {with {[x {* x 10}]}
                                  {+ 1 x}}}
                       5})
                (numV 51))
        ; shadowing of `with` params within `with` initialization
          (test (run '{with {[x 2]}
                            {with {[x {with {[x 3]}
                                            {* x x}}]}
                                  x}})
                (numV 9))

      ; static scoping (this is the Ben Bitdiddle problem from the Substitution assignment)
      (test (run '{with {[x 100]}
                        {with {[x 4]}
                              {with {[f {fun {y} {+ x y}}]}
                                    {with {[x 5]}
                                          {f 10}}}}})
            (numV 14))

      ; variables that depend on previous variables
      (test (run '{with {[a 1]}
                        {with {[b {+ a 10}]}
                              {with {[f {fun {c} {+ a {+ b c}}}]}
                                    {with {[g {fun {c d} {+ c {f d}}}]}
                                          {g 100 1000}}}}})
            (numV 1112))

    ; Runtime errors
      (test/exn (run '{{with {[x 1]} x} 0}) "Runtime error: Cannot apply a non-function")

      (test/exn (run 'x) "Runtime error: Unbound identifier")
      (test/exn (run '{double 3}) "Runtime error: Unbound identifier")
      (test/exn (run '{with {[x y]} 1}) "Runtime error: Unbound identifier")
      (test/exn (run '{with {[x 1]} y}) "Runtime error: Unbound identifier")
      ; correct scope closing
      (test/exn (run '{+ {with {[f {fun {y} y}]}
                               {f 3}}
                         {f 4}})
                "Runtime error: Unbound identifier")

      (test/exn (run '{/ 1 0}) "Runtime error: Division by zero")
      (test/exn (run '{/ 0 0}) "Runtime error: Division by zero")
      (test/exn (run '{/ 1 {- 3 3}}) "Runtime error: Division by zero")
      (test/exn (run '{/ 1 {with {} 0}}) "Runtime error: Division by zero")
      (test/exn (run '{/ 1 {{fun {x} x} 0}}) "Runtime error: Division by zero")
      (test/exn (run '{/ 1 {if0 0 0 1}}) "Runtime error: Division by zero")
      (test/exn (run '{/ 1 {if0 1 1 0}}) "Runtime error: Division by zero")
      (test/exn (run '{with {[x 0]} {/ 1 x}}) "Runtime error: Division by zero")
      (test/exn (run '{{fun {x} {/ 1 x}} 0}) "Runtime error: Division by zero")

      (test/exn (run '{+ 1 {fun {} 1}}) "Runtime error: Argument to a binop is not a number")
      (test/exn (run '{+ {fun {} 1} 1}) "Runtime error: Argument to a binop is not a number")
      (test/exn (run '{- 1 {fun {} 1}}) "Runtime error: Argument to a binop is not a number")
      (test/exn (run '{- {fun {} 1} 1}) "Runtime error: Argument to a binop is not a number")
      (test/exn (run '{* 1 {fun {} 1}}) "Runtime error: Argument to a binop is not a number")
      (test/exn (run '{* {fun {} 1} 1}) "Runtime error: Argument to a binop is not a number")
      (test/exn (run '{/ 1 {fun {} 1}}) "Runtime error: Argument to a binop is not a number")
      (test/exn (run '{/ {fun {} 1} 1}) "Runtime error: Argument to a binop is not a number")

      (test/exn (run '{{fun {} 1} 13}) "Runtime error: Argument/parameter arity mismatch")
      (test/exn (run '{{fun {x} x}}) "Runtime error: Argument/parameter arity mismatch")
      (test/exn (run '{{fun {x} x} 0 1}) "Runtime error: Argument/parameter arity mismatch")
      (test/exn (run '{{fun {x} x} 0 1 2}) "Runtime error: Argument/parameter arity mismatch")
      (test/exn (run '{{fun {x y} x}}) "Runtime error: Argument/parameter arity mismatch")
      (test/exn (run '{{fun {x y} x} 0}) "Runtime error: Argument/parameter arity mismatch")

      (test/exn (run '{if0 {fun {} 1} 1 2}) "Runtime error: Conditional expression is not a number")
      (test/exn (run '{if0 {with {} {fun {} 1}} 1 2}) "Runtime error: Conditional expression is not a number")

      ; runtime errors from un-evaluated functions and `if0` branches are NOT thrown
        (test (run '{if0 0 1 {/ 1 0}}) (numV 1)) ; Division by zero
        (test (run '{if0 1 {/ 1 0} 1}) (numV 1)) ; Division by zero (test other branch)
        (test (run '{if0 0 1 {+ {fun {} 1} 1}}) (numV 1)) ; Argument to a binop is not a number
        (test (run '{if0 1 {+ {fun {} 1} 1} 1}) (numV 1)) ; Argument to a binop is not a number (test other branch)
        (test (run '{if0 0 1 {{fun {} x}}}) (numV 1)) ; Unbound identifier
        (test (run '{if0 1 {{fun {} x}} 1}) (numV 1)) ; Unbound identifier (test other branch) (sanity check)
        (test (run '{if0 0 1 {{fun {x} 1}}}) (numV 1)) ; Argument/parameter arity mismatch
        (test (run '{if0 0 1 {{fun {} 1} 2}}) (numV 1)) ; Argument/parameter arity mismatch
        (test (run '{if0 0 1 {if0 {fun {} 1} 1 2}}) (numV 1)) ; Conditional expression is not a number

        (test (run '{fun {} {/ x 0}}) (closureV empty (binop div (id 'x) (num 0)) (mtEnv))) ; Division by zero
        (test (run '{fun {} x}) (closureV empty (id 'x) (mtEnv))) ; Unbound identifier
        (test (run '{fun {} {{fun {} 0} 1}})
              (closureV empty
                        (app (fun empty
                                  (num 0))
                             (list (num 1)))
                        (mtEnv))) ; Argument/parameter arity mismatch
        (test (run '{fun {} {{fun {x} 0}}})
              (closureV empty
                        (app (fun (list 'x)
                                  (num 0))
                             empty)
                        (mtEnv))) ; Argument/parameter arity mismatch
        (test (run '{fun {} {if0 {fun {} 1} 1 2}})
              (closureV empty
                        (if0 (fun empty (num 1))
                             (num 1)
                             (num 2))
                        (mtEnv))) ; Conditional expression is not a number
