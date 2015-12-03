#lang plai
(print-only-errors #t)

; A binding of a symbol to an arithmetic expression
(define-type Binding
  [binding (name symbol?) (named-expr WAE?)]) ; TODO add a monad-like function that maps functions onto the named-expr. would simplify a few places in calc and subst I think

; A "'with' arithmetic expression"; i.e. an arithmetic expression that may contain 'with' bindings
(define-type WAE
  [num (n number?)]
  [binop (op procedure?) ; TODO what about checking the arity of op?
         (lhs WAE?)
         (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

; fmap : (A -> B) (or binding ...) -> B
; Applies func to the contents of the context functor
; (A hacked together version of haskell's fmap)
; The supported contexts include:
;   binding
;   num
;   number
;   string
; (note that this comment is probably outdated, let's be honest)
  (define (fmap func context)
    (cond [(binding? context)
          ; custom data types
           (binding (binding-name context)
                    (func (binding-named-expr context)))]
          [(num? context)
           (num (func (num-n context)))]
          ; racket data types
          [(list? context)
           (map func context)]
          [(number? context)
           (func context)]
          [(string? context)
           (func context)]
          [else ; I COULD replace this with (func context), but that would mean silent failures... no thanks
           (error "Runtime error: Unknown context passed to `fmap`")]))

  ; tests
    (define (inc x) (+ x 1))
    (define (double x) (* x 2))
    (define (square x) (* x x))

    (define my-binding (binding 'x (num 11)))

    (test (fmap identity my-binding) my-binding)
    (test (fmap ((curry fmap) inc) my-binding) (binding 'x (num 12)))
    (test (fmap ((curry fmap) double) my-binding) (binding 'x (num 22)))
    (test (fmap ((curry fmap) square) my-binding) (binding 'x (num 121)))

    (test (fmap identity (num 5)) (num 5))
    (test (fmap inc (num 5)) (num 6))
    (test (fmap double (num 5)) (num 10))
    (test (fmap square (num 5)) (num 25))

    (test (fmap identity empty) empty)
    (test (fmap identity (list 1 3 'a)) (list 1 3 'a))
    (test (fmap inc (list 17)) (list 18))
    (test (fmap double (list 1 3 7)) (list 2 6 14))
    (test (fmap square (list 1 3 7)) (list 1 9 49))

    (test (fmap identity 5) 5)
    (test (fmap inc 5) 6)
    (test (fmap double 5) 10)
    (test (fmap square 5) 25)

    (test (fmap identity "hi") "hi")
    (test (fmap ((curry string-append) "hello") " world") "hello world")

    (test/exn (fmap identity 't) "Runtime error: Unknown context passed to `fmap`")
    (test/exn (fmap identity +) "Runtime error: Unknown context passed to `fmap`")

; A lookup table (symbol -> procedure) of arithmetic operations
(define op-table
  [list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)])

; lookup-op : symbol -> boolean
; Returns the procedure that corresponds to the given operator.
; Returns false if no matching procedure is found.
  (define (lookup-op op)
    (let [(result (assoc op op-table))]
      (if (equal? false result)
        false
        (second result))))

  ; tests
    (test (lookup-op '+) +)
    (test (lookup-op '-) -)
    (test (lookup-op '*) *)
    (test (lookup-op '/) /)

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

; reserved? : symbol -> boolean
; Returns whether the given symbol is a reserved word
  (define/match (reserved? sym)
    [('with) true]
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
    (test (reserved? 'a) false)
    (test (reserved? 'hello) false)
    (test (reserved? '+-) false)
    (test (reserved? 'a+) false)
    (test (reserved? 'wih) false)
    (test (reserved? 'w) false)
    (test (reserved? 'with*) false)
    (test (reserved? 'fun) false)
    (test (reserved? 'if0) false)

; parse : s-expression -> WAE
; Parses s-exp into a WAE according to this grammar:
;   WAE = number
;       | (+ WAE WAE)
;       | (- WAE WAE)
;       | (* WAE WAE)
;       | (/ WAE WAE)
;       | (with ([id WAE] [id WAE] ...) WAE)
;       | id
  (define (parse s-exp)
    (cond [(list? s-exp)
           (if (not (= 3 (length s-exp)))
              [error "Illegal syntax: Bad arity"]
              [let [(fst (first s-exp))
                    (snd (second s-exp))
                    (thd (third s-exp))]
                (cond [(and (symbol? fst)
                            (symbol=? fst 'with))
                       (cond [(not ((listof list?) snd))
                              (error "Illegal syntax: Bindings is not a list of lists")]
                             [(not (andmap (lambda (bind-pair)
                                                   (= 2 (length bind-pair)))
                                           snd))
                              (error "Illegal syntax: Bad binding arity")]
                             [(not (andmap (lambda (bind-pair)
                                                   (symbol? (first bind-pair)))
                                           snd))
                              (error "Illegal syntax: Non-symbol bind target")]
                             [(not (andmap (lambda (bind-pair)
                                                   (not (reserved? (first bind-pair))))
                                           snd))
                              (error "Illegal syntax: Bind target is a reserved identifier")]
                             [else
                              (with (map (lambda (bind-pair)
                                                 (binding (first bind-pair) (parse (second bind-pair)))) ; we know (first bind-pair) is a symbol at this point
                                         snd)
                                    (parse thd))])]
                      [(equal? (lookup-op fst) false)
                       (error "Illegal syntax: Unknown operator")]
                      [else
                       (binop (lookup-op fst) (parse snd) (parse thd))])])]
          [(number? s-exp)
           (num s-exp)]
          [(symbol? s-exp)
           (if (reserved? s-exp)
              (error "Illegal syntax: Symbol is a reserved identifier")
              (id s-exp))]
          [else
           (error "Illegal syntax: Not a WAE")]))

  ; tests
    ; basic building blocks
      (test {parse 'x} (id 'x))
      (test {parse '5} (num 5))
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
                    (binop / (num 2)
                             (num 3)))
      (test (parse '{with {} 1})
            (with empty (num 1)))
      (test (parse '{with {[x 1]} x})
            (with (list (binding 'x (num 1))) (id 'x)))
      (test (parse '{with {[x 1] [y 2]}
                          {+ x y}})
            (with (list (binding 'x (num 1))
                        (binding 'y (num 2)))
                  (binop + (id 'x) (id 'y))))

    ; operation composition
    (test (parse '{+ 1 {* 2 3}})
                  (binop + (num 1)
                           (binop * (num 2)
                                    (num 3))))

    (test/exn (parse true) "Illegal syntax: Not a WAE")
    (test/exn (parse '{& 1 2}) "Illegal syntax: Unknown operator")
    (test/exn (parse '{1 2 3}) "Illegal syntax: Unknown operator")
    (test/exn (parse '{}) "Illegal syntax: Bad arity")
    (test/exn (parse '{1 2}) "Illegal syntax: Bad arity")

    (test/exn (parse '{+}) "Illegal syntax: Bad arity")
    (test/exn (parse '{-}) "Illegal syntax: Bad arity")
    (test/exn (parse '{*}) "Illegal syntax: Bad arity")
    (test/exn (parse '{/}) "Illegal syntax: Bad arity")

    (test/exn (parse '{+ 1}) "Illegal syntax: Bad arity")
    (test/exn (parse '{- 1}) "Illegal syntax: Bad arity")
    (test/exn (parse '{* 1}) "Illegal syntax: Bad arity")
    (test/exn (parse '{/ 1}) "Illegal syntax: Bad arity")

    (test/exn (parse '{+ 1 2 3}) "Illegal syntax: Bad arity")
    (test/exn (parse '{- 1 2 3}) "Illegal syntax: Bad arity")
    (test/exn (parse '{* 1 2 3}) "Illegal syntax: Bad arity")
    (test/exn (parse '{/ 1 2 3}) "Illegal syntax: Bad arity")

    (test/exn (parse '{+ with 3}) "Illegal syntax: Symbol is a reserved identifier")
    (test/exn (parse '{+ + 3}) "Illegal syntax: Symbol is a reserved identifier")
    (test/exn (parse '{+ - 3}) "Illegal syntax: Symbol is a reserved identifier")
    (test/exn (parse '{+ * 3}) "Illegal syntax: Symbol is a reserved identifier")
    (test/exn (parse '{+ / 3}) "Illegal syntax: Symbol is a reserved identifier")
    (test/exn (parse '{with {[x 1]}}) "Illegal syntax: Bad arity")
    (test/exn (parse '{with {[x 1]} x x}) "Illegal syntax: Bad arity")
    (test/exn (parse '{with [x 1] x}) "Illegal syntax: Bindings is not a list of lists")
    (test/exn (parse '{with {+ 3 4} x}) "Illegal syntax: Bindings is not a list of lists")
    (test/exn (parse '{with {[4 x]} x}) "Illegal syntax: Non-symbol bind target")
    (test/exn (parse '{with {[]} x}) "Illegal syntax: Bad binding arity")
    (test/exn (parse '{with {[x]} x}) "Illegal syntax: Bad binding arity")
    (test/exn (parse '{with {[x 1 3]} x}) "Illegal syntax: Bad binding arity")
    (test/exn (parse '{with {[x +]} x}) "Illegal syntax: Symbol is a reserved identifier")
    (test/exn (parse '{with {[+ 7]} x}) "Illegal syntax: Bind target is a reserved identifier")

    ; an awkward but parsable 'with' binding:
    (test (parse '{with {[x 1] [x x] [x y] [x &] [x &] [x (+ 2 x)]} {+ x 3}})
          (with (list (binding 'x (num 1))
                      (binding 'x (id 'x))
                      (binding 'x (id 'y))
                      (binding 'x (id '&))
                      (binding 'x (id '&))
                      (binding 'x (binop + (num 2) (id 'x))))
                (binop + (id 'x) (num 3))))

    ; long, ugly recursion
    (test (parse '{+ 2 {* x1 {with {[y_sub 5] [x1 121]} {/ {with {[zebra 10] [web-gun 33]} {- y_sub zebra}} 3}}}})
          (binop + (num 2)
                   (binop * (id 'x1)
                            (with [list (binding 'y_sub (num 5))
                                        (binding 'x1 (num 121))]
                                  (binop / (with [list (binding 'zebra (num 10))
                                                       (binding 'web-gun (num 33))]
                                                 (binop - (id 'y_sub) (id 'zebra)))
                                           (num 3))))))

; subst : Binding WAE -> WAE
; Substitutes bdg into body
; bdg must be a binding to a num
; Test this function alongside subst*
  (define (subst bdg expr)
    (let [(bdg-name (binding-name bdg))
          (bdg-val (if (num? (binding-named-expr bdg))
                       (num-n (binding-named-expr bdg))
                       (error "Runtime error: non-number binding")))]
      (type-case WAE expr
        [num (n)
             expr]
        [binop (op lhs rhs)
               (binop op (subst bdg lhs) (subst bdg rhs))]
        [with (lob w-body)
              (with (map ((curry fmap) ((curry subst) bdg))
                         lob) ; substitute into the bodies of each binding
                    (if (ormap (lambda (w-bdg)
                                  (symbol=? bdg-name (binding-name w-bdg)))
                               lob) ; if there's a shadow variable, don't recurse the substitution
                        w-body
                        (subst bdg w-body)))]
        [id (sym)
            (if (symbol=? bdg-name sym)
                (num bdg-val) ; We assume bdg is a binding to a num
                expr)])))

  ; tests
    ; no substitution
    (test (subst (binding 'x (num 0))
                 (num 1))
          (num 1))
    ; single substitution
    (test (subst (binding 'x (num 0))
                 (id 'x))
          (num 0))
    ; doesn't substitute into the wrong variables
    (test (subst (binding 'x (num 4))
                 (binop + (id 'y) (id 'x)))
          (binop + (id 'y) (num 4)))
    ; multiple substitution
    (test (subst (binding 'x (num 4))
                 (binop + (id 'x) (id 'x)))
          (binop + (num 4) (num 4)))

    ; nested substitution
    (test (subst (binding 'x (num 4))
                 (binop + (id 'x)
                          (binop * (num 3)
                                   (with [list (binding 'y (num 5))
                                               (binding 'z (num 1))]
                                         (binop / (id 'x) (id 'z))))))
          (binop + (num 4)
                   (binop * (num 3)
                            (with [list (binding 'y (num 5))
                                        (binding 'z (num 1))]
                                  (binop / (num 4) (id 'z))))))

    ; Substitutes into 'with' bindings
    (test (subst (binding 'x (num 1))
                 (with [list (binding 'y (binop + (id 'x) (num 2)))]
                       (binop + (id 'x) (id 'y))))
          (with [list (binding 'y (binop + (num 1) (num 2)))]
                (binop + (num 1) (id 'y))))
    ; Doesn't substitute shadowed variables in with-body
    (test (subst (binding 'hello (num 10))
                 (with [list (binding 'x (num 4))
                             (binding 'hello (num 15))]
                       (binop + (id 'hello) (id 'x))))
          (with [list (binding 'x (num 4))
                       (binding 'hello (num 15))]
                 (binop + (id 'hello) (id 'x))))
    ; Does substitute in with-bindings-list even if variable is shadowed (but doesn't substitute into the with-body)
    (test (subst (binding 'x (num 1))
                 (with [list (binding 'y (id 'x))
                             (binding 'x (binop + (id 'x) (num 2)))
                             (binding 'z (id 'x))]
                       (binop + (id 'x) (id 'y))))
          (with [list (binding 'y (num 1))
                       (binding 'x (binop + (num 1) (num 2)))
                       (binding 'z (num 1))]
                 (binop + (id 'x) (id 'y))))

    (test/exn (subst (binding 'x (id 'y))
                 (num 1))
              "Runtime error: non-number binding")

    ; There are more tests later under "wholistic tests"

; has-duplicates? : list -> boolean
; Returns whether lst contains any element more than once
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

; subst* : (listof Binding) WAE -> WAE
; Substitutes for all of the bindings in lob inside body simultaneously
; Assumes that lob only contains binding to num objects
; Test this function alongside subst
  (define (subst* lob body)
    (if (has-duplicates? (map binding-name lob))
        (error "Runtime error: Duplicate binding")
        (foldr subst
               body
               lob)))

  ; tests
    ; no substitution
    (test (subst* empty
                  (num 1))
          (num 1))
    (test (subst* empty
                  (id 'x))
          (id 'x))
    ; single substitution
    (test (subst* [list (binding 'x (num 1))]
                  (id 'x))
          (num 1))
    ; doesn't substitute into the wrong variables
    (test (subst* [list (binding 'x (num 4))]
                  (binop + (id 'y) (id 'x)))
          (binop + (id 'y) (num 4)))
    ; multiple substitution
    (test (subst* [list (binding 'x (num 1))
                        (binding 'y (num 2))]
                  (binop * (binop + (id 'y) (id 'x))
                           (id 'x)))
          (binop * (binop + (num 2) (num 1))
                            (num 1)))

    (test/exn (subst* [list (binding 'x (num 1))
                            (binding 'x (num 2))]
                      (binop + (id 'x) (id 'x)))
          "Runtime error: Duplicate binding")

    ; There are more tests later under "wholistic tests"

; calc : WAE -> number
; Consumes a WAE representation of an expression
;   and computes the corresponding numerical result, eagerly.
  (define (calc expr)
    (type-case WAE expr
      [num (n)
           n]
      [binop (op lhs rhs)
             (let [(rhs-value (calc rhs))]
               (if (and (equal? op /)
                        (equal? rhs-value 0))
                   (error "Runtime error: Division by zero")
                   (op (calc lhs) rhs-value)))] ; the parser takes care of unknown operators, so we don't need to verify op
      [with (w-lob w-body)
            ; run calc on each binding first, then subst* results and calc
            (calc (subst* (map ((curry fmap) (compose num calc))
                               w-lob)
                          w-body))]
      [id (name)
          (error "Runtime error: Unbound variable")]))

  ; tests
    ; basic building blocks
      (test (calc (num 1)) 1)

      (test (calc (binop + (num 1) (num 2))) 3)
      (test (calc (binop - (num 1) (num 2))) -1)
      (test (calc (binop * (num 1) (num 2))) 2)
      (test (calc (binop / (num 1) (num 2))) 1/2)

      (test (calc (with [list (binding 'x (num 1))]
                        (id 'x)))
            1)
      (test (calc (with [list (binding 'x (num 1))
                              (binding 'y (num 2))]
                        (binop + (id 'x) (id 'y))))
            3)

    ; nested 'with' expressions
    (test (calc (with [list (binding 'x (num 1))
                            (binding 'y (num 2))]
                      (with [list (binding 'x (num 10))
                                  (binding 'z (num 30))]
                            (binop + (id 'x)
                                     (binop * (id 'z) (id 'y))))))
          70)
    ; shadowed variables DO substitute into sub-bindings, but NOT sub-bodies
    (test (calc (parse '{with {[x 5]}
                              {with {[x {+ x 1}]}
                                    {+ x 1}}}))
          7)
    ; shadowed variables DO substitute into sub-bindings, but NOT sub-bodies
    ; (another, similar, test to the one right above this)
    (test (calc (parse '{with {[x 1]}
                              {with {[x 10]
                                     [y {+ x 1}]}
                                    {* x y}}}))
          20)

    (test/exn (calc (id 'x))
              "Runtime error: Unbound variable")
    (test/exn (calc (with [list (binding 'x (id 'y))]
                          (num 1)))
             "Runtime error: Unbound variable")
    (test/exn (calc (with [list (binding 'x (num 1))]
                          (id 'y)))
              "Runtime error: Unbound variable")
    (test/exn (calc (binop / (num 1) (num 0)))
              "Runtime error: Division by zero")
    (test/exn (calc (binop / (num 0) (num 0)))
              "Runtime error: Division by zero")
    (test/exn (calc (binop / (num 1)
                             (binop - (num 3) (num 3))))
              "Runtime error: Division by zero")
    (test/exn (calc (binop / (num 1)
                             (with empty (num 0))))
              "Runtime error: Division by zero")
    (test/exn (calc (with [list (binding 'x (num 0))]
                          (binop / (num 1) (id 'x))))
              "Runtime error: Division by zero")

    (test/exn (calc (with [list (binding 'x (num 1))
                                (binding 'x (num 2))]
                          (num 1)))
              "Runtime error: Duplicate binding")
    (test/exn (calc (parse '{f 3 7})) ; The parser takes care of catching unknown operators
              "Illegal syntax: Unknown operator")

    ; There are more tests later under "wholistic tests"


; wholistic tests
  (test (calc (parse '5)) 5)
  (test (calc (parse '{+ 4 2})) 6)
  (test (calc (parse '{- 4 2})) 2)
  (test (calc (parse '{* 4 2})) 8)
  (test (calc (parse '{/ 4 2})) 2)
  (test (calc (parse '{with {} 4})) 4)
  (test (calc (parse '{with {[x 5]} x})) 5)
  (test (calc (parse '{with {[x 5] [y 11]} {+ x y}})) 16)
  (test (calc (parse '{with {[x 5] [y 11]} {- x y}})) -6)
  (test (calc (parse '{with {[x 5] [y 11]} {* x y}})) 55)
  (test (calc (parse '{with {[x 5] [y 11]} {/ x y}})) 5/11)
  (test (calc (parse '{with {[x 5]}
                            {+ x x}}))
        10)
  (test (calc (parse '{with {[x 5]}
                            {with [{y 6}]
                                  {+ x y}}}))
        11)
  (test (calc (parse '{with {[x 5]}
                            {with {[y {+ x 1}]}
                                  {- y x}}}))
        1)
  (test (calc (parse '{with {[x 5]}
                            {with {[x 6]}
                                  x}}))
        6)

  ; shotgun testing
    (define a1-as-sexp '{* {with {[x {with {[x 5] [y x]}
                                           {- variable {* x y}}}]
                                  [y 7]}
                                 {with {[x x] [z {/ variable y}]}
                                       {+ z {/ x y}}}}
                           {with {[y 11]}
                                 x}})
    (define a1-as-wae (binop * (with [list (binding 'x (with [list (binding 'x (num 5))
                                                                   (binding 'y (id 'x))]
                                                             (binop - (id 'variable)
                                                                      (binop * (id 'x) (id 'y)))))
                                           (binding 'y (num 7))]
                                     (with [list (binding 'x (id 'x))
                                                 (binding 'z (binop / (id 'variable) (id 'y)))]
                                           (binop + (id 'z) (binop / (id 'x) (id 'y)))))
                               (with [list (binding 'y (num 11))]
                                     (id 'x))))

    (define a2-as-wae (with [list (binding 'x (num 2))
                                  (binding 'variable (num 3))]
                            a1-as-wae))
    (define a2-with-first-level-substitution (binop * (with [list (binding 'x (with [list (binding 'x (num 5))
                                                                                          (binding 'y (num 2))]
                                                                                    (binop - (num 3)
                                                                                             (binop * (id 'x) (id 'y)))))
                                                                  (binding 'y (num 7))]
                                                            (with [list (binding 'x (id 'x))
                                                                        (binding 'z (binop / (num 3) (id 'y)))]
                                                                  (binop + (id 'z) (binop / (id 'x) (id 'y)))))
                                                      (with [list (binding 'y (num 11))]
                                                            (num 2))))
    (define a2-as-num -8/7)
      ; derivation: (starting after the initial substitution)
      ;
      ; {* {with {[x {with {[x 5] [y 2]}
      ;                    {- 3 {* x y}}}]
      ;           [y 7]}
      ;          {with {[x x] [z {/ 3 y}]}
      ;                {+ z {/ x y}}}}
      ;    {with {[y 11]}
      ;          2}}

      ; =>

      ; {* {with {[x -7]
      ;           [y 7]}
      ;          {with {[x x] [z {/ 3 y}]}
      ;                {+ z {/ x y}}}}
      ;    2}

      ; =>

      ; {* with {[x -7] [z 3/7]}
      ;         {+ z {/ x 7}}}
      ;    2}

      ; =>

      ; {* {+ 3/7 -1}}
      ;    2}

      ; =>

      ; {* -4/7
      ;    2}

      ; =>

      ; -8/7


    (test (parse a1-as-sexp) a1-as-wae)
    (test (subst* [list (binding 'x (num 2))
                        (binding 'variable (num 3))]
                  a1-as-wae)
          a2-with-first-level-substitution)
    (test (calc a2-as-wae) a2-as-num)
    (test (calc a2-with-first-level-substitution) a2-as-num) ; sanity check

  ; nested substitution
    (define b1-as-sexp '{+ x
                           {* 3
                              {with {[y 5] [z 1]}
                                    {/ x z}}}})
    (define b1-as-wae (binop + (id 'x)
                               (binop * (num 3)
                                        (with [list (binding 'y (num 5))
                                                    (binding 'z (num 1))]
                                              (binop / (id 'x) (id 'z))))))
    (define b2-as-wae (with [list (binding 'x (num 4))]
                            b1-as-wae))
    (define b2-with-first-level-substitution (binop + (num 4)
                                                      (binop * (num 3)
                                                               (with [list (binding 'y (num 5))
                                                                           (binding 'z (num 1))]
                                                                     (binop / (num 4) (id 'z))))))
    (define b2-as-num 16)

    (test (parse b1-as-sexp) b1-as-wae)
    (test (subst* [list (binding 'x (num 4))]
                  b1-as-wae)
          b2-with-first-level-substitution)
    (test (calc b2-as-wae) b2-as-num)
    (test (calc b2-with-first-level-substitution) b2-as-num)

  ; Does substitute in with-bindings-list even if variable is shadowed (but doesn't substitute into the with-body)
    (define c1-as-sexp '{with {[x 1]}
                              {with {[y x]
                                     [x (* x 10)]
                                     [z x]}
                                    {+ x y}}})
    (define c1-as-wae (with [list (binding 'x (num 1))]
                             (with [list (binding 'y (id 'x))
                                         (binding 'x (binop * (id 'x) (num 10)))
                                         (binding 'z (id 'x))]
                                   (binop + (id 'x) (id 'y)))))
    (define c1-as-num 11)
    (test (parse c1-as-sexp) c1-as-wae)
    (test (calc c1-as-wae) c1-as-num)