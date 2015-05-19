;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eval) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; importing required teachpacks
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require racket/trace)
(define TIME-ON-TASK 25)

; providing required functions
(provide eval)
(provide lambda?)
(provide errstr?)
(provide subst)
(provide expr->expr/no-var)
(provide expr=?)

;ArithOp Constants
(define ADD '+)
(define SUB '-)
(define MULTIPLY '*)
(define DIVIDE '/)
;CmpOp Constants
(define EQUALS '=)
(define GREATER-THAN '>)
(define LESS-THAN '<)
;BoolOp Constants
(define OR 'or)
(define AND 'and)

; A UniqueListOf<X> is a ListOf<X>
; WHERE: none of the elements of the list are equal? to each other
; EXAMPLE:
(define UNIQUE-LST '(1 2 3 4))

; A 2ListOf<X> is a (cons X (cons X ListOf<X>))
; Represents a list of 2 or more elements.
;TEMPLATE:
; 2ListOf<X> -> ???
;(define (2listofx-fn 2lox)
;  (cond
;    [(empty? (rest (rest 2lox)))(...(first 2lox)...(second 2lox))]
;    [else (...(first 2lox)...(second 2lox)...(2listofx-fn (rest 2lox))...)]))
; EXAMPLE : 
(define LST-2X '(2 3))

; A Program is a:
; - empty
; - (cons Expr Program)
; - (cons Def Program)
; Represents a PDPLang program consisting of function defs and expressions.
; WHERE: no two Defs have the same name
; TEMPLATE :
; Program -> ???
;(define (program-fn pgm)
;  (cond
;    [(empty? pgm) ...]
;    [(def? (first pgm)) (...(def-fn (first pgm))...(program-fn (rest pgm))...)]
;    [else (...(expr-fn (first pgm))...(program-fn (rest pgm))...)]))
; EXAMPLE : 
(define SAMPLE-PROGRAM '(make-arith ADD '(2 3)))

; An Arith is (make-arith ArithOp 2ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : op represents the operator symbol and the args 
; represents the expressions
(define-struct arith (op args))
; TEMPLATE :
; Arith -> ???
(define (arith-fn arith)
  (...(arith-op arith)...(2listofx-fn (arith-args arith))...))
; EXAMPLE :
(define SAMPLE-ARITH (make-arith ADD '(2 3)))

; An Bool is (make-bool BoolOp 2ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : op represents the boolean operator symbol and the args 
; represents the expressions
(define-struct bool (op args))
; TEMPLATE :
; Bool -> ???
(define (bool-fn bool)
  (...(bool-op bool)...(2listofx-fn (bool-args bool))...))
; EXAMPLE :
(define SAMPLE-BOOL (make-bool OR '(#t #f)))

; An Comp is (make-cmp CmpOp 2ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : op represents the comparator operator symbol and the args 
; represents the expressions
(define-struct cmp (op args))
; TEMPLATE :
; Comp -> ???
(define (cmp-fn cmp)
  (...(cmp-op cmp)...(2listofx-fn (cmp-args cmp))...))
; EXAMPLE :
(define SAMPLE-CMP (make-cmp LESS-THAN '(2 3)))

; An IfExp is (make-if-exp X X X)
; WHERE : X can be Expr or ExprNoVar
; INTERP : test represents the test part of if and the branch1 branch2 
; represents the different branches of IfExp 
(define-struct if-exp (test branch1 branch2))
;TEMPLATE : 
; IfExp -> ???
(define (if-exp-fn ifexp)
  (...(expr-fn (if-exp-test expr))...
      (expr-fn (if-exp-branch1 expr))...
      (expr-fn (if-exp-branch2 expr))...))
; EXAMPLE :
(define SAMPLE-IF-EXP (make-if-exp (make-cmp LESS-THAN '(2 3)) 2 3))

; An Call is (make-call Expr ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : fn represents the function name that is being called and args 
; represents the aruments passed. 
(define-struct call (fn args))
; TEMPLATE :
; Call -> ???
(define (call-func call)
  (...(expr-fn (call-fn expr))...(loe-fn (call-args expr))...))
; EXAMPLE :
(define SAMPLE-CALL (make-call 'f '(2 3)))

; An Expr is one of:
; - Number
; - Boolean
; - Var
; - ErrString
; - Lambda
; - (make-arith ArithOp 2ListOf<Expr>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<Expr>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<Expr>)     ; a comparison expression
; - (make-if-exp Expr Expr Expr) ; an if conditional
; - (make-call Expr ListOf<Expr>) ; a function call
; Represents a PDPLang expression.
; TEMPLATE : 
; Expr -> ???
;
;(define (expr-fn expr)
;  (cond
;    [(number? expr)...]
;    [(boolean? expr)...]
;    [(symbol? expr)...]
;    [(string? expr)...]
;    [(lam? expr)...]
;    [(arith? expr) (...(arith-op expr)...(2listofx-fn (arith-args expr))...)]
;    [(bool? expr)  (...(bool-op expr)...(2listofx-fn (bool-args expr))...)]
;    [(cmp? expr)  (...(cmp-op expr)...(2listofx-fn (cmp-args expr))...)]
;    [(if-exp? expr)  (...(expr-fn (if-exp-test expr))...
;                         (expr-fn (if-exp-branch1 expr))...
;                         (expr-fn (if-exp-branch2 expr))...)]
;    [(call? expr)
;          (...(expr-fn (call-fn expr))...(loe-fn (call-args expr))...)]))
; EXAMPLE :
(define SAMPLE-EXPR (make-arith ADD '(2 3)))


; A Var is a Symbol, representing PDPLang variable.

; An ErrString is a String, representing a PDPLang error message.

; A Lambda is a (make-lam UniqueListOf<Param> Expr)
; Represents a lambda expression in PDPLang
(define-struct lam (params body))
; TEMPLATE :
; Lambda -> ???
(define (lam-fn lam)
  (...(lop-fn (lam-params lam))...(expr-fn (lam-expr lam))...))
; EXAMPLE :
(define SAMPLE-LAM (make-lam '(a b) (make-arith ADD '(2 3))))

; A Param is a Var, representing a function parameter.

; An ArithOp is one of:
; - ADD
; - SUB
; - MULTIPLY
; - DIVIDE
; Represents an arithmetic operation in PDPLang
; TEMPLATE :
; ArithOp -> ???

;(define (arithop-fn op)
;  (cond
;    [(eq? op ADD)...]
;    [(eq? op SUB)...]
;    [(eq? op MULTIPLY)...]
;    [(eq? op DIVIDE)...]))

; A BoolOp is one of:
; - AND
; - OR
; Represents a boolean operation in PDPLang
; TEMPLATE :
; BoolOp -> ???
;(define (boolop-fn op)
;  (cond
;    [(eq? op AND)...]
;    [(eq? op OR)...]))

; A CmpOp is one of:
; - EQUALS
; - LESS-THAN
; - GREATER-THAN
; Represents a comparison operation in PDPLang
; TEMPLATE :
; CmpOp -> ???
;(define (cmpop-fn op)
;  (cond
;    [(eq? op EQUALS)...]
;    [(eq? op LESS-THAN)...]
;    [(eq? op GREATER-THAN)...]))

; A Def is a (make-def FnName UniqueListOf<Param> Expr)
; Represents the definition of a function with the specified parameters.
(define-struct def (name params body))
; TEMPLATE :
; Def -> ???
(define (def-fn def)
  (...(def-name def)...(loe-fn (def-params))...(expr-fn (def-body def))...))
; EXAMPLE : 
(define SAMPLE-DEF (make-def 'add '(a b) (make-arith ADD '(2 3))))

; A FnName is a Var, representing a function name.

; A Result is a:
; - Number
; - Boolean
; - ErrString
; - Lambda
; TEMPLATE :
; Result -> ???

;(define (result-fn res)
;  (cond
;    [(number? res)...]
;    [(boolean? res)...]
;    [(string? res)...]
;    [(lambda? res)...]))

; An ExprNoVar is one of:
; - Number
; - Boolean
; - StaticDist
; - ErrString
; - LamNoVar
; - (make-arith ArithOp 2ListOf<ExprNoVar>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<ExprNoVar>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<ExprNoVar>)     ; a comparison expression
; - (make-if-exp ExprNoVar ExprNoVar ExprNoVar) ; an if conditional
; - (make-call ExprNoVar ListOf<ExprNoVar>) ; a function call
; Represents an Expr without explicit variables.
; TEMPLATE : 
; ExprNoVar -> ???

;(define (exprnovar-fn exprnovar)
;  (cond
;    [(number? exprnovar)...]
;    [(boolean? exprnovar)...]
;    [(cons? exprnovar) (...(first exprnovar)...
;                                 (second exprnovar)...)]
;    [(string? exprnovar)...]
;    [(lam/no-var? exprnovar) (...(lam/no-var-body exprnovar)...)]
;    [(arith? exprnovar) 
;     (...(arith-op exprnovar)...(2listofx-fn (arith-args exprnovar))...)]
;    [(bool? exprnovar)  
;     (...(bool-op exprnovar)...(2listofx-fn (bool-args exprnovar))...)]
;    [(cmp? exprnovar)  
;     (...(cmp-op exprnovar)...(2listofx-fn (cmp-args exprnovar))...)]
;    [(if-exp? exprnovar)  
;     (...(exprnovar-fn (if-exp-test exprnovar))...
;         (exprnovar-fn (if-exp-branch1 exprnovar))...
;         (exprnovar-fn (if-exp-branch2 exprnovar))...)]
;    [(call? exprnovar)
;     (...(exprnovar-fn (call-fn exprnovar))...
;         (lox-fn (call-args exprnovar))...)]))

; A StaticDist is a (list Depth Index)
; Represents a variable reference
; where depth is number of additional lambdas between this var ref and the
; lambda for which this variable is a parameter,
; and index is the (0-based) position of this variable in that lambda's
; parameter list.
; TEMPLATE :
; StaticDist -> ???
(define (staticdist-fn sd)
  (...(first sd)...(second sd)...))

; A Param-StaticDist is a (list Param Depth Index)
; Represents the Param and its StaticDist.
; TEMPLATE :
; StaticDist -> ???
(define (param-staticdist-fn psd)
  (...(first psd)...(rest psd)))

; A Depth is a Natural
; An Index is a Natural

; A LamNoVar is a (make-lam/no-var ExprNoVar)
(define-struct lam/no-var (body))
; TEMPLATE :
; LamNoVar -> ???
(define (lamnovar-fn lnv)
  (...(lam/no-var-body lnv)...))

; Sample Definitions
(define MK-ADD-DEF
  (make-def
   'mk-add '(n)
   (make-lam '(m) (make-arith ADD '(n m)))))
(define ADD5-DEF
  (make-def
   'add5 '(x)
   (make-call (make-call 'mk-add '(5)) '(x))))
; add5-or-6 : adds 5 to y if y it's positive, else adds 6
(define ADD5-OR-6-DEF
  (make-def
   'add5-or-6 '(y)
   (make-if-exp (make-cmp GREATER-THAN '(y 0))
                (make-call 'add5 '(y))
                (make-call (make-call 'mk-add '(6)) '(y)))))

(define SAMPLE-PGM (list
                    MK-ADD-DEF
                    ADD5-DEF
                    ADD5-OR-6-DEF
                    (make-call 'add5 '(10))
                    (make-call 'add5-or-6 '(200))
                    (make-call 'add5-or-6 '(-100))))


(define SAMPLE-LODEF (list
                      (make-def
                       'mk-add
                       (list 'n)
                       (make-lam (list 'm) (make-arith '+ (list 'n 'm))))
                      (make-def 'add5 (list 'x) (make-arith '+ (list 5 'x)))
                      (make-def
                       'add5-or-6
                       (list 'y)
                       (make-if-exp
                        (make-cmp '> (list 'y 0))
                        (make-arith '+ (list 5 'y))
                        (make-arith '+ (list 6 'y))))))

; eval : Program -> ListOf<Result>
; Evaluates a PDPLang program to a list of Results.
; Specifically, evaluates the Exprs in p, in the context of the given Defs.
; WHERE: A function may be called before it is defined.
; WHERE: The produced results are in the same relative order as their 
; originating Exprs in p.
; EXAMPLE : 
(begin-for-test(check-equal?
                (eval
                 (list
                  MK-ADD-DEF
                  ADD5-DEF
                  ADD5-OR-6-DEF
                  (make-call 'add5 '(10))
                  (make-call 'add5-or-6 '(200))
                  (make-call 'add5-or-6 '(-100))))
                (list 15 205 -94)
                "call-fn evaluates to a function")
               (check-equal?
                (eval 
                 (list 
                  (make-def 
                   'count '(n) 
                   (make-if-exp (make-cmp GREATER-THAN (list 'n 0))
                                (make-call 'count 
                                           (list (make-arith SUB (list 'n 1))))
                                #true))
                  (make-call 'count (list 10))))
                (list true)
                "a def with a recursive call"))
; STRATEGY : Function Composition
(define (eval pgm0)
  (local(; eval/a : Program Natural ListOf<Def> ListOf<Result> -> ListOf<Result>
         ; WHERE : 
         ; parser selects which step to run on the program
         ; lodef represents the list of definition found in pgm
         ; lor represents the list of result after evaluating the pgm 
         ; STRATEGY : Generative Recursion
         ; HALTING MEASURE : decreasing parser by 1
         ; TERMINATING ARGUMENT : after each step the parser decreases by one 
         ; and the function terminates when parser becomes zero.
         (define (eval/a pgm parser lodef lor)
           (cond
             [(zero? parser) lor]
             [(eq? parser 1) 
              (eval/a pgm (sub1 parser) lodef (eval-pgm pgm lodef))]
             [(eq? parser 2) 
              (eval/a pgm (sub1 parser) 
                      (final-simplified-defs (compile-pgm pgm)) lor)])))
    (eval/a pgm0 2 '() '())))

; eval-pgm : Program ListOf<Def> -> ListOf<Result>
; Evaluates a PDPLang program to a list of Results.
; Specifically, evaluates the Exprs in p, in the context of the given Defs.
; WHERE: A function may be called before it is defined.
; WHERE: The produced results are in the same relative order as their 
; originating Exprs in p.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (eval-pgm '() (list MK-ADD-DEF ADD5-DEF))
   '()
   "Empty program list")
  (check-equal?
   (eval-pgm (list
              MK-ADD-DEF
              ADD5-DEF
              ADD5-OR-6-DEF
              (make-call 'add5 '(10))
              (make-call 'add5-or-6 '(200))
              (make-call 'add5-or-6 '(-100))) 
             SAMPLE-LODEF)
   (list 15 205 -94)
   "given a valid program")
  (check-equal?
   (eval-pgm (list
              MK-ADD-DEF
              ADD5-DEF
              ADD5-OR-6-DEF
              (make-call 'add5 '(10))
              (make-call 'add5-or-6 '(200))
              (make-call 'add5-or-6 '(-100))) (list MK-ADD-DEF))
   '("Error: No definition found"
     "Error: No definition found"
     "Error: No definition found")
   "Error messages since the required definitions were not found"))
; STRATEGY : Data Decomposition on pgm : Program
(define (eval-pgm pgm lodef)
  (cond
    [(empty? pgm) '()]
    [(def? (first pgm)) (eval-pgm (rest pgm) lodef)]
    [else 
     (append (list (eval-expr (first pgm) lodef))(eval-pgm (rest pgm) lodef))]))

; eval-expr : Expr ListOf<Def> -> Result
; Evaluates an expression and returns a list of result.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (eval-expr 5 SAMPLE-LODEF)
   5
   "The list of results")
  (check-equal?
   (eval-expr true SAMPLE-LODEF)
   #t
   "The boolean result")
  (check-equal?
   (eval-expr "hello" SAMPLE-LODEF)
   "hello"
   "The string in the expression")
  (check-equal?
   (eval-expr (make-lam '(m) (make-arith '+ '(n m))) ADD5-DEF)
   (make-lam '(m) (make-arith '+ '(n m)))
   "The lamba expression")
  (check-equal?
   (eval-expr (make-bool 'and '(1 2)) ADD5-DEF)
   "and:question result is not true or false"
   "The result")) 
; STRATEGY : Data Decomposition on expr : Expr
(define (eval-expr expr lodef)
  (cond
    [(number? expr) expr]
    [(boolean? expr)expr]
    [(symbol? expr) (get-var-result expr lodef)] 
    [(string? expr) expr]
    [(lam? expr)    expr]
    [(arith? expr) (get-arith-res (arith-op expr) 
                                  (arith-args expr) lodef)]
    [(bool? expr)  (get-bool-res (bool-op expr)(bool-args expr) lodef)]
    [(cmp? expr)   (get-cmp-res (cmp-op expr)(cmp-args expr) lodef)]
    [(if-exp? expr)(get-if-exp-res (if-exp-test expr) 
                                   (if-exp-branch1 expr) 
                                   (if-exp-branch2 expr) lodef)]
    [(call? expr)  (get-call-res (call-fn expr)(call-args expr) lodef)])) 

; get-var-result : Var ListOf<Def> -> Result
; Returns the result of a Var expression.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-var-result 5 (list ADD5-DEF))
   "err: undefined var"
   "The variable is not defined")
  (check-equal?
   (get-var-result 'add5 SAMPLE-LODEF)
   (make-lam (list 'x) (make-arith '+ (list 5 'x)))
   "given a defined variable"))
; STRATEGY : Function Composition
(define (get-var-result expr lodef)
  (if (ormap (λ (x) (eq? expr (def-name x))) lodef)
      (make-lam (get-params-of-fn expr lodef)
                (get-body-of-fn expr lodef))
      "err: undefined var"))

; get-call-res : Symbol Expr ListOf<Def> -> Expr
; returns the results of the call statement.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-call-res 'min 10 (list ADD5-DEF))
   "Error: No definition found"
   "The variable is not defined")
  (check-equal?
   (get-call-res 'add5 (list 10) SAMPLE-LODEF)
   15
   "given a defined variable a valid parameter"))
; STRATEGY : Function Composition
(define (get-call-res sym expr lodef)
  (if (or (lam? sym) (check-if-valid-fn-name sym lodef))
      (eval-expr (subst-list (get-params-of-fn sym lodef) 
                             (eval-pgm expr lodef) 
                             (get-body-of-fn sym lodef)) lodef)
      "Error: No definition found")) 

;(trace get-call-res)

; subst-list : ListOf<Param> ListOf<Result> Expr -> Expr/ErrString
; Replaces all the params in the expr with corresponding element in lov
; EXAMPLE :
(begin-for-test
  (check-equal?
   (subst-list '(x y) (list 10 20) (make-arith ADD '(x y)))
   (make-arith '+ (list 10 20))
   "The arithmetic expression")
  (check-equal?
   (subst-list '(x y) (list 10 20 30) (make-arith ADD '(x y)))
   "expected 2 number of arguments, given 3"
   "The arithmetic expression"))
; STRATEGY : Function Composition
(define (subst-list lop lov expr)
  (if (= (length lop) (length lov))
      (replace-var-with-value lop lov expr)
      (string-append "expected " (number->string (length lop)) 
                     " number of arguments, given " 
                     (number->string(length lov)))))

; replace-var-with-value : ListOf<Param> ListOf<Result> Expr -> Expr/ErrString
; Replaces all the params in the expr with corresponding element in lov.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (replace-var-with-value '(x y) (list 10 20) (make-arith ADD '(x y)))
   (make-arith '+ (list 10 20))
   "The arithmetic expression"))
; STRATEGY : Data Decoomposition on lop : ListOf<Param>
(define (replace-var-with-value lop lov expr)
  (cond
    [(empty? lop) expr]
    [else (replace-var-with-value (rest lop) (rest lov)  
                                  (subst (first lov) (first lop) expr))]))

; get-arith-res : ArithOp 2ListOf<Expr> ListOf<Def> -> Result
; Evaluates an arithmatic expression.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (get-arith-res '+ '(2 3) SAMPLE-LODEF)
   5
   "The result of the arithmetic expression")
  (check-equal?
   (get-arith-res '+ '(x y) SAMPLE-LODEF)
   "+:expects a number as arguments"
   "The addition does not get a number"))
; STRATEGY : Function Composition
(define (get-arith-res op args lodef)
  (if (andmap number? (map (λ (x) (eval-expr x lodef)) args)) 
      (eval-arith op args lodef)
      (string-append (symbol->string op) 
                     ":expects a number as arguments")))

; eval-arith : ArithOp 2ListOf<Expr> ListOf<Def> -> Result
; WHERE : all the Expr in args evaluate to a Number
; Evaluates an arithmatic expression.
(begin-for-test
  (check-equal?
   (eval-arith '+ '( 2 3) SAMPLE-LODEF)
   5
   "The addition arithmetic expression")
  (check-equal?
   (eval-arith '- '(2 3) SAMPLE-LODEF)
   -1
   "The subtraction arithmetic expression")
  (check-equal?
   (eval-arith '* '(2 3) SAMPLE-LODEF)
   6
   "The multiplication arithmetic expression")
  (check-equal?
   (eval-arith '/ '(6 3) SAMPLE-LODEF)
   2
   "The division arithmetic expression")
  (check-equal?
   (eval-arith '/ (list (make-arith ADD '(10 10)) 5 4) SAMPLE-LODEF)
   1
   "The division arithmetic expression with an addition inside it"))
; STRATEGY : Data Decomposition on op : ArithOp
(define (eval-arith op args lodef)
  (cond
    [(eq? op ADD) (foldl + 0 (map (λ (x) (eval-expr x lodef)) args))]
    [(eq? op SUB) (- (eval-expr (first args) lodef) 
                     (foldl + 0 (map (λ (x) (eval-expr x lodef)) (rest args))))]
    [(eq? op MULTIPLY) (foldl * 1 (map (λ (x) (eval-expr x lodef)) args))]
    [(eq? op DIVIDE) 
     (/ 
      (eval-expr (first args) lodef)
      (foldl * 1 (map (λ (x) (eval-expr x lodef)) (rest args))))]))

; get-bool-res : BoolOp 2ListOf<Expr> ListOf<Def> -> Result
; Evaluates a boolean expression.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-bool-res 'and '(2 3) (list ADD5-DEF))
   "and:question result is not true or false"
   "The result does not evaluates to a false for and statements")
  (check-equal?
   (get-bool-res 'or (list #true #false) (list ADD5-DEF))
   #true
   "The result evaluates to a false for or statements"))
; STRATEGY : Function Composition
(define (get-bool-res op args lodef)
  (if (andmap boolean? (map (λ (x) (eval-expr x lodef)) args))
      (eval-bool op args lodef)
      (string-append (symbol->string op) 
                     ":question result is not true or false")))

; eval-bool : BoolOp 2ListOf<Expr> -> Result
; WHERE : all the Expr in args evaluates to a Boolean.
; Evaluates a boolean expression.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (eval-bool 'and  (list true false) (list ADD5-DEF))
   #f
   "The result evaluates to a false for and statements")
  (check-equal?
   (eval-bool 'or (list true false) (list ADD5-DEF))
   #true
   "The result evaluates to a false for or statements")
  (check-equal?
   (eval-bool 'or (list true false true) (list ADD5-DEF))
   #true
   "The result evaluates to a false for or statements")
  (check-equal?
   (eval-bool 'and (list true true (make-cmp '< '(2 3))) (list ADD5-DEF))
   #true
   "given an expression with comparison in it"))
; STRATEGY  : Data Decomposition on op : BoolOp
(define (eval-bool op args lodef)
  (cond
    [(eq? op AND) (andmap (λ (x) (eq? x #true)) 
                          (map (λ (x) (eval-expr x lodef)) args))]
    [(eq? op OR)  (ormap  (λ (x) (eq? x #true)) 
                          (map (λ (x) (eval-expr x lodef)) args))]))

; get-cmp-res : CmpOp 2ListOf<Expr> ListOf<Def> -> Result
; Evaluates a comparison expression.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-cmp-res '< '(2 3) (list SAMPLE-CMP))
   #t
   "The compare result evaluates to a true")
  (check-equal?
   (get-cmp-res '< '(param1 param2) (list ADD5-DEF))
   "<:expects a real as argument"
   "The compare expects an argument but what is given is a variable"))
; STRATEGY : Function Composition
(define (get-cmp-res op args lodef)
  (if (andmap number? (map (λ (x) (eval-expr x lodef)) args))
      (eval-comp op args lodef)
      (string-append (symbol->string op) 
                     ":expects a real as argument")))

; eval-comp : CmpOp 2ListOf<Expr> ListOf<Def> -> Result
; WHERE : all the Expr in args evaluates to a Number.
; Evaluates a comparison expression.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (eval-comp '= '(2 2) (list SAMPLE-CMP))
   #t
   "The result evaluates to a true")
  (check-equal?
   (eval-comp '< '(2 3) (list SAMPLE-CMP))
   #t
   "The result evaluates to a true")
  (check-equal?
   (eval-comp '> '(2 3) (list SAMPLE-CMP))
   #f
   "The result evaluates to a false")
  (check-equal?
   (eval-comp '> (list (make-arith '+ '(3 3)) 
                       (make-arith '+ '(2 3)) 2) (list SAMPLE-CMP))
   #true
   "given an expression that has an arith exp in it"))
; STRATEGY : Data Decomposition on op : CmpOp
(define (eval-comp op args lodef)  
  (cond
    [(eq? op EQUALS)
     (compare-a-list = (map (λ (x) (eval-expr x lodef)) args))]
    [(eq? op LESS-THAN)
     (compare-a-list < (map (λ (x) (eval-expr x lodef)) args))]
    [(eq? op GREATER-THAN)
     (compare-a-list > (map (λ (x) (eval-expr x lodef)) args))])) 

; compare-a-list : CmpOp 2ListOf<Expr> -> Boolean
; Returns true if all the Expr in lst evaluate positively to the compare test.
; EXAMPLE : Refer test cases above.
; STRATEGY : Data Decompositon on lst : 2ListOf<Expr>
(define (compare-a-list cmpop lst)
  (cond
    [(empty? (rest (rest lst))) (cmpop (first lst) (second lst))]
    [else (and (cmpop (first lst) (second lst)) 
               (compare-a-list cmpop (rest lst)))]))

; get-if-exp-res :Expr Expr Expr ListOf<Def> -> Result
; Evaluates a comparison expression.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-if-exp-res 3 4 5 (list ADD5-DEF))
   "if:question result is not true or false"
   "The result is not true or false")
  (check-equal?
   (get-if-exp-res #true 4 5 (list ADD5-DEF))
   4
   "test is true hence branch1 is returned"))
; STRATEGY : Function Composition
(define (get-if-exp-res test branch1 branch2 lodef)
  (if (boolean? (eval-expr test lodef))
      (eval-if-exp test branch1 branch2 lodef)
      "if:question result is not true or false"))

; eval-if-exp : Expr Expr Expr ListOf<Def> -> Result
; WHERE : (eval-expr test) is a Boolean
; Evaluates an if expression.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (eval-if-exp #true 4 5 (list ADD5-DEF))
   4
   "test is true hence branch1 is returned")
  (check-equal?
   (eval-if-exp #false 4 5 (list ADD5-DEF))
   5
   "test is true hence branch2 is returned")
  (check-equal?
   (eval-if-exp (make-cmp '> '(2 3)) 
                (make-arith '- '(2 3)) 
                (make-arith '+ '(2 3)) (list ADD5-DEF))
   5
   "a more complex example"))
; STRATEGY : Function Composition
(define (eval-if-exp test branch1 branch2 lodef)
  (if (eval-expr test lodef)
      (eval-expr branch1 lodef)
      (eval-expr branch2 lodef)))          

; lambda? : Expr -> Boolean
; Returns true if expr is a PDPLang lambda expression.
; EXAMPLE :
(begin-for-test
  (check-pred 
   lambda? 
   (first (eval (list (make-def 'f '(x) 'x) 'f))) "result is lambda")
  (check-equal? (lambda? 2) #false "given a non lambda input"))
; STRATEGY : Data Decomposition on expr : Expr
(define (lambda? expr) 
  (cond
    [(lam? expr)    #true]
    [else #false])) 

; errstr? : Expr -> Boolean
; Returns true if expr is a PDPLang ErrString expression.
; EXAMPLE :
(begin-for-test
  (check-pred 
   errstr? (first (eval (list (make-def 'f '(x) 'x) 'g))) "err: undefined var")
  (check-pred errstr? (first (eval (list 'x))) "err: undefined var")
  (check-equal?
   (errstr? (first (eval (list (make-def 'f '(x) 'x) 'f)))) #false
   "a valid variable"))
; STRATEGY : Data Decomposition on expr : Expr
(define (errstr? expr) 
  (cond
    [(string? expr) #true]
    [else #false])) 

; subst : Result Var Expr -> Expr
; Replaces references to x in e with r.
; Does not replace x with r if x occurs in the body of a lambda
; that shadows x.
; WHERE: r has no unbound variables
; EXAMPLE :
(begin-for-test
  (check-equal? (subst 4 'x 'x) 4 "x matches")
  (check-equal? (subst 4 'y 'x) 'x "y doesnt match")
  (check-equal?
   (subst 4 'x (make-arith ADD '(x 5)))
   (make-arith ADD '(4 5))
   "subst in arith")
  (check-equal?
   (subst 4 'x (make-lam '(y) (make-arith ADD '(x y))))
   (make-lam '(y) (make-arith ADD '(4 y)))
   "subst in lambda")
  (check-equal?
   (subst 4 'x (make-lam '(x) (make-arith ADD '(x 5))))
   (make-lam '(x) (make-arith ADD '(x 5)))
   "dont subst shadowed vars in lambdas")
  (check-equal?
   (subst #false 'x (make-bool AND '(x #true)))
   (make-bool 'and (list false true))
   "subst in bool")
  (check-equal?
   (subst #false 'x "errstring")
   "errstring"
   "given a string as expression"))
; STRATEGY : Data Decomposition on expr : Expr
(define (subst r x expr)
  (cond
    [(number? expr) expr]
    [(boolean? expr)expr]
    [(symbol? expr) (match-and-replace r x expr)]
    [(string? expr) expr]
    [(lam? expr)    (subst-lam r x (lam-params expr) (lam-body expr))] 
    [(arith? expr) (make-arith (arith-op expr) 
                               (subst-list-of-expr r x (arith-args expr)))]
    [(bool? expr)  (make-bool (bool-op expr)
                              (subst-list-of-expr r x (bool-args expr)))]
    [(cmp? expr)   (make-cmp (cmp-op expr)
                             (subst-list-of-expr r x (cmp-args expr)))]
    [(if-exp? expr)(make-if-exp (subst r x (if-exp-test expr)) 
                                (subst r x (if-exp-branch1 expr)) 
                                (subst r x (if-exp-branch2 expr)))]
    [(call? expr)  (make-call (subst r x (call-fn expr)) 
                              (subst-list-of-expr r x (call-args expr)))])) 

; subst-lam : Result Var UniqueListOf<Param> Expr -> Expr
; Returns an Expr which is a (make-lam params body) after replacing x by r 
; in the body if x is not shadowed.
; EXAMPLE : Refer test cases above.
; STRATEGY : Function Composition
(define (subst-lam r x params body)
  (make-lam params
            (if (ormap (λ (y) (eq? x y)) params)
                body
                (subst r x body))))

; subst-list-of-expr : Result Var ListOf<Expr> -> ListOf<Expr>
; Returns a list of expressions with x replaced by r.
; WHERE: r has no unbound variables
; EXAMPLE : Refer test cases above.
; STRATEGY : Data Decomposition on args : ListOf<Expr>
(define (subst-list-of-expr r x loe)
  (cond
    [(empty? loe) '()]
    [else (append (list (subst r x (first loe))) 
                  (subst-list-of-expr r x (rest loe)))]))

; match-and-replace : Result Var Expr -> Expr
; WHERE : Expr is a Var
; Returns r if the expr matches x
; EXAMPLE : Refer test cases above.
; STRATEGY : Function Composition
(define (match-and-replace r x expr)
  (if (eq? expr x)
      r
      expr))

; compile-pgm : Program -> ListOf<Def>
; Returns a list of Defs present in the program.
; EXAMPLE : 
(begin-for-test 
  (check-equal?
   (compile-pgm SAMPLE-PGM)
   (list
    (make-def
     'mk-add
     (list 'n)
     (make-lam (list 'm) (make-arith '+ (list 'n 'm))))
    (make-def
     'add5
     (list 'x)
     (make-call (make-call 'mk-add (list 5)) (list 'x)))
    (make-def
     'add5-or-6
     (list 'y)
     (make-if-exp
      (make-cmp '> (list 'y 0))
      (make-call 'add5 (list 'y))
      (make-call (make-call 'mk-add (list 6)) (list 'y)))))
   "given a valid program"))
; STRATEGY : Data Decomposition on pgm : Program
(define (compile-pgm pgm)
  (cond
    [(empty? pgm) '()]
    [(def? (first pgm)) (append (list (first pgm)) (compile-pgm (rest pgm)))]
    [else (compile-pgm (rest pgm))]))

; simplify-pgm-defs : ListOf<Def> -> ListOf<Def>
; Returns a list that contains all the defs in simplified form.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (simplify-pgm-defs SAMPLE-LODEF)
   (list
    (make-def
     'mk-add
     (list 'n)
     (make-lam (list 'm) (make-arith '+ (list 'n 'm))))
    (make-def 'add5 (list 'x) (make-arith '+ (list 5 'x)))
    (make-def
     'add5-or-6
     (list 'y)
     (make-if-exp
      (make-cmp '> (list 'y 0))
      (make-arith '+ (list 5 'y))
      (make-arith '+ (list 6 'y)))))
   "given a sample valid list of definition to be simplified"))
; STRATEGY : Function Composition
(define (simplify-pgm-defs lodef0)
  (local(; simplify-pgm-defs/a : 
         ;                    ListOf<Def> ListOf<Def> ListOf<Def> -> ListOf<Def>
         ; WHERE :
         ; lodef/a represents the def that are simplified so far, 
         ; and lodef is a subset of lodef0
         ; STRATEGY : Data Decomposition on lodef : ListOf<Def>
         (define (simplify-pgm-defs/a lodef lodef0 lodef/a)
           (cond
             [(empty? lodef) lodef/a]
             [else (simplify-pgm-defs/a 
                    (rest lodef) 
                    lodef0 
                    (append lodef/a 
                            (list (simplify-def (first lodef) lodef0))))])))
    (simplify-pgm-defs/a lodef0 lodef0 '())))

; final-simplified-defs : ListOf<Def> -> ListOf<def>
; Returns list of Def which has no make-call to the other functions, 
; self call will still be there.
; EXAMPLE : refer test cases for example
; STRATEGY : Function Composition
(define (final-simplified-defs lodef)
  (if (any-more-make-calls lodef)
      (final-simplified-defs (simplify-pgm-defs lodef))
      lodef)) 

; simplify-def : Def ListOf<Def> -> Def
; Returns a Def with all the (make-calls ...), except a self call
; replaced by the actual code of the called def.
; EXAMPLE : refer test cases for example
; STRATEGY : Data Decomposition on def : Def
(define (simplify-def def lodef)
  (make-def (def-name def) 
            (def-params def)
            (simplify-def-body (def-body def) lodef)))

; simplify-def-body : Expr ListOf<Def> -> Expr
; Returns an Expr where the (make-call...) has been replaced by the body of the
; called function name and (make-lam...) has been replaced by its body.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (simplify-def-body (make-bool AND '(#true #true)) SAMPLE-LODEF)
   (make-bool 'and (list true true))
   "given a sample boolean operation")
  (check-equal?
   (simplify-def-body "errstring" SAMPLE-LODEF)
   "errstring"
   "given a sample string"))
; STRATEGY : Data Decomposition on expr : Expr
(define (simplify-def-body expr lodef)
  (cond
    [(number? expr) expr]
    [(boolean? expr)expr]
    [(symbol? expr) expr]
    [(string? expr) expr]
    [(lam? expr)    (make-lam (lam-params expr) 
                              (simplify-def-body (lam-body expr) lodef))] 
    [(arith? expr) (make-arith (arith-op expr) 
                               (simplify-list-of-expr (arith-args expr) lodef))]
    [(bool? expr)  (make-bool (bool-op expr)
                              (simplify-list-of-expr (bool-args expr) lodef))]
    [(cmp? expr)   (make-cmp (cmp-op expr)
                             (simplify-list-of-expr (cmp-args expr) lodef))]
    [(if-exp? expr)(make-if-exp 
                    (simplify-def-body (if-exp-test expr) lodef) 
                    (simplify-def-body (if-exp-branch1 expr) lodef) 
                    (simplify-def-body (if-exp-branch2 expr) lodef))]
    [(call? expr)  (simplify-call-expr expr lodef)]))

; simplify-call-expr : Expr ListOf<Def> -> Expr
; Returns an Expr that simplifies the given call statement expr.
; WHERE : expr is a call statement.
; EXAMPLE : Refer test cases for example.
; STRATEGY : Data Decomposition on expr : Expr
(define (simplify-call-expr expr lodef)
  (cond
    [(symbol? (call-fn expr))
     (subst-list (get-params-of-fn (call-fn expr) lodef)
                 (call-args expr)
                 (get-body-of-fn (call-fn expr) lodef))]
    [(lam? (call-fn expr))
     (subst-list (lam-params (call-fn expr))
                 (call-args expr)
                 (lam-body (call-fn expr)))]
    [else 
     (make-call (simplify-def-body (call-fn expr) lodef)
                (simplify-list-of-expr (call-args expr) lodef))])) 

; simplify-list-of-expr : ListOf<Expr> ListOf<Def> -> ListOf<Expr>
; Returns a simplified list of Expr.
; EXAMPLE : Refer test cases for example
; STRATEGY : Data Decomposition on loe : ListOf<Expr>
(define (simplify-list-of-expr loe lodef)
  (cond
    [(empty? loe) '()]
    [else (append (list (simplify-def-body (first loe) lodef)) 
                  (simplify-list-of-expr (rest loe) lodef))]))

; any-more-make-calls : ListOf<Def> -> Boolean
; Retunrs a true if there is any Def in lodef which has a call statement.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (any-more-make-calls SAMPLE-LODEF)
   #false
   "given a list of def that does not have a make-call to other functions"))
; STRATEGY : Function Composition
(define (any-more-make-calls lodef)
  (ormap check-make-call? lodef))

; check-make-call? : Def -> Boolean
; Returns true if def has any call statement.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (check-make-call? (make-def 'min '(x y) (make-arith ADD '(x y))))
   #false
   "given a def with no call to other function")
  (check-equal?
   (check-make-call? (make-def 'min '(x y) (make-call 'add '(2 3))))
   #true
   "given a def with a call to other function"))
; STRATEGY : Data Decomposition on def : Def
(define (check-make-call? def)
  (check-make-call-in-body? (def-name def) (def-body def))) 

; check-make-call-in-body? : Var Expr -> Boolean
; Returns true if expr has any call statement.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (check-make-call-in-body? 'add "string")
   #false
   "given a string")
  (check-equal?
   (check-make-call-in-body? 'add (make-bool AND (list #t #t)))
   #false
   "given a def name that does not exist"))
; STRATEGY : Data Decomposition on expr : Expr
(define (check-make-call-in-body? name expr)
  (cond
    [(number? expr) #false]
    [(boolean? expr)#false]
    [(symbol? expr) #false]
    [(string? expr) #false] 
    [(lam? expr)   (check-make-call-in-body? name (lam-body expr))]  
    [(arith? expr) 
     (ormap (λ (x) (check-make-call-in-body? name x)) (arith-args expr))]
    [(bool? expr)  
     (ormap (λ (x) (check-make-call-in-body? name x)) (bool-args expr))]
    [(cmp? expr)   
     (ormap (λ (x) (check-make-call-in-body? name x)) (cmp-args expr))]
    [(if-exp? expr)(or 
                    (check-make-call-in-body? name (if-exp-test expr)) 
                    (check-make-call-in-body? name (if-exp-branch1 expr)) 
                    (check-make-call-in-body? name (if-exp-branch2 expr)))]
    [(call? expr) (not(eq? name (call-fn expr)))]))   

; get-params-of-fn : Symbol ListOf<Def> -> ListOf<Param>
; WHERE : There exists a Def by the name sym
; returns the list of params of the def by the  name sym.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-params-of-fn 'add5 SAMPLE-LODEF)
   (list 'x)
   "given a valid definition name"))
; STRATEGY : Function Composition
(define (get-params-of-fn sym lodef)
  (def-params (first (filter (λ (x) (eq? sym (def-name x))) lodef))))

; get-body-of-fn : Symbol ListOf<Def> -> Expr
; WHERE : There exists a Def by the name sym.
; returns the body of the def by the  name sym.
(begin-for-test
  (check-equal?
   (get-body-of-fn 'add5 SAMPLE-LODEF)
   (make-arith '+ (list 5 'x))
   "given a valid definition name"))
; STRATEGY : Function Composition
(define (get-body-of-fn sym lodef)
  (def-body (first (filter (λ (x) (eq? sym (def-name x))) lodef))))

; check-if-valid-fn-name : Symbol ListOf<Def> -> Boolean
; returns true if there is a Def name by sym
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (check-if-valid-fn-name 'add5 SAMPLE-LODEF)
   #true
   "given a valid definition name")
  (check-equal?
   (check-if-valid-fn-name 'min SAMPLE-LODEF)
   #false
   "given a valid definition name"))
; STRATEGY : Function Composition
(define (check-if-valid-fn-name sym lodef)
  (ormap (λ (x) (eq? sym (def-name x))) lodef))  

; expr->expr/no-var : Expr -> ExprNoVar
; Replaces Var in e with StaticDist.
; WHERE: there are no unbound variables in e.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (expr->expr/no-var (make-lam '(x) 'x))
   (make-lam/no-var '(0 0))
   "basic lambda")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var
    (make-lam '(x y) 
              (make-lam '(z) 
                        (make-if-exp 
                         (make-bool 'and (list (make-cmp '> '(x y)) #true ))
                         (make-arith '- '(2 3)) 
                         'z))))
   (make-lam/no-var
    (make-lam/no-var
     (make-if-exp
      (make-bool
       'and
       (list (make-cmp '> (list (list 1 0) (list 1 1))) true))
      (make-arith '- (list 2 3))
      (list 0 0))))
   "an if expresion in nested lanbdas")
  (check-equal?
   (expr->expr/no-var "string")
   "string"
   "basic expression"))
; STRATEGY : Function Composition
(define (expr->expr/no-var expr0)
  (local(; expr->expr/no-var/a : Expr ListOf<Param-StaticDist> -> ExprNoVar
         ; WHERE : lst represents the ListOf<Param-StaticDist> collected so far 
         ; and expr is the subset of expr0.
         ; STRATEGY : Data Decomposition on expr : Expr
         (define (expr->expr/no-var/a expr lst)
           (cond
             [(number? expr)  expr]
             [(boolean? expr) expr]
             [(symbol? expr)  (get-corresponding-sd expr lst)]
             [(string? expr)  expr]
             [(lam? expr)    
              (make-lam/no-var 
               (expr->expr/no-var/a 
                (lam-body expr) 
                (append (make-sd-for-lop 
                         (lam-params expr) (lam-body expr)) lst)))] 
             [(arith? expr)  
              (make-arith (arith-op expr)(loe-fn (arith-args expr) lst))]
             [(bool? expr)   
              (make-bool (bool-op expr)(loe-fn (bool-args expr) lst))]
             [(cmp? expr)    
              (make-cmp (cmp-op expr)(loe-fn (cmp-args expr) lst))]
             [(if-exp? expr) 
              (make-if-exp (expr->expr/no-var/a (if-exp-test expr) lst)
                           (expr->expr/no-var/a (if-exp-branch1 expr) lst)
                           (expr->expr/no-var/a (if-exp-branch2 expr) lst))]
             [(call? expr)   
              (make-call (expr->expr/no-var/a (call-fn expr) lst) 
                         (loe-fn (call-args expr) lst))]))
         ; loe-fn : Param ListOf<Expr> Depth -> Depth
         ; Processes the loe.
         ; STRATEGY : Data Decomposition on loe : ListOf<Expr>
         (define (loe-fn loe lst)
           (cond
             [(empty? loe) '()]
             [else 
              (append (list (expr->expr/no-var/a (first loe) lst)) 
                      (loe-fn (rest loe) lst))])))
    (expr->expr/no-var/a expr0 '()))) 


; get-param-depth : Param Expr -> Depth
; Returns the depth of p from the parent lambda.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-param-depth 'p "string")
   0
   "finding depth in an expression that does not have a lambda")
  (check-equal?
   (get-param-depth 'z (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
   2
   "finding depth in an expression that has nested lambda"))
; STRATEGY : Data Decomposition on expr0 : Expr
(define (get-param-depth p expr0)
  (local(
         ; get-param-depth/a : Param Expr Depth -> Depth
         ; WHERE : d represents the epth of p so far, 
         ; and expr is the subset of expr0.
         ; STRATEGY : Data Decomposition on expr : Expr
         (define (get-param-depth/a p expr d)
           (cond
             [(number? expr)  0]
             [(boolean? expr) 0]
             [(symbol? expr)  (if (eq? p expr) d 0)]
             [(string? expr)  0]
             [(lam? expr)    (get-param-depth/a p (lam-body expr)(add1 d))]
             [(arith? expr)  (loe-fn p (arith-args expr) d)]
             [(bool? expr)   (loe-fn p (bool-args expr) d)]
             [(cmp? expr)    (loe-fn p (cmp-args expr)  d)]
             [(if-exp? expr) 
              (max (get-param-depth/a p (if-exp-test expr) d)
                   (get-param-depth/a p (if-exp-branch1 expr) d)
                   (get-param-depth/a p (if-exp-branch2 expr) d))]
             [(call? expr)   (max (get-param-depth/a p (call-fn expr) d) 
                                  (loe-fn p (call-args expr) d))]))
         ; loe-fn : Param ListOf<Expr> Depth -> Depth
         ; Processes the loe.
         ; STRATEGY : Data Decomposition on loe : ListOf<Expr>
;         (define (loe-fn p loe d)
;           (cond
;             [(empty? loe) 0]
;             [else (max (get-param-depth/a p (first loe) d) 
;                        (loe-fn p (rest loe) d))]))
         (define (loe-fn p loe d)
           (get-param-depth/a
            p 
            (argmax (λ (expr) (get-param-depth/a p expr d)) loe) d)))
    (get-param-depth/a p expr0 0)))

; make-sd-for-lop : ListOf<Param> Expr -> ListOf<Param-StaticDist>
; Returns a list of Param-StaticDist from lop and expr.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (make-sd-for-lop '(x y) (make-lam '(z) (make-call 'x '(y z))))
   (list (list 'x 1 0) (list 'y 1 1))
   "given a valid list of param"))
; STRATEGY : Function Composition
(define (make-sd-for-lop lop expr)
  (map (λ (p)
         (append (list p) (list (get-param-depth p expr))
                 (list (index-of-param p lop)))) lop))

; index-of-param : Param ListOf<Param> -> Index 
; WHERE : p will be present in lop.
; Returns a Index of p in lop, in zero based index format.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (index-of-param 'p (list 'x 'y 'p 'q 'r))
   2
   "given a list where 'p is present third to right"))
; STRATEGY : Function Composition
(define (index-of-param p lop)
  (sub1 (length (memf (λ (x) (eq? x p)) (reverse lop))))) 

; get-corresponding-sd : Param ListOf<Param-StaticDist> -> StaticDist
; WHERE : StaticDist Corresponding to p exists in lst.
; Returns the corresponding distance.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-corresponding-sd 'p (list (list 'x 0 1)(list 'y 1 1)(list 'p 1 0)))
   (list 1 0)
   "given a valid variable to be searched in the list"))
; STRATEGY : Function Composition
(define (get-corresponding-sd p lst)
  (rest (first (filter (λ (x) (eq? p (first x))) lst))))

; expr=? : Expr Expr -> Boolean
; Returns true if e1 and e2 are structurally equivalent, up to some
; renaming of variable names.
(begin-for-test
  (check
   expr=?
   (make-lam '(x) 'x)
   (make-lam '(y) 'y)
   "equivalent basic lambdas")
  (check-false
   (expr=?
    (make-lam '(x y) (make-call 'x '(y)))
    (make-lam '(y x) (make-call 'x '(y))))
   "not equivalent")
  (check
   expr=?
   (make-lam '(y) (make-lam '(x) (make-call 'y '(x))))
   (make-lam '(x) (make-lam '(y) (make-call 'x '(y))))
   "equivalent nested-lambdas"))
; STRATEGY : Function Composition
(define (expr=? e1 e2) 
  (expr/no-var=? (expr->expr/no-var e1) (expr->expr/no-var e2)))

; expr/no-var=? : ExprNoVar ExprNoVar -> Boolean
; Returns true if e1 and e2 are structurally equivalent.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (expr/no-var=? 5 5)
   #t
   "The numbers are equal")
  (check-equal?
   (expr/no-var=? #t #t)
   #t
   "The booleansa are the same")
  (check-equal?
   (expr/no-var=? "hello" "hello")
   #t
   "The strings are equal")
  (check-equal?
   (expr/no-var=? SAMPLE-ARITH SAMPLE-ARITH)
   #t
   "The arithmetic expressions are equal")
  (check-equal?
   (expr/no-var=? SAMPLE-BOOL SAMPLE-BOOL)
   #t
   "The boolean expressions are same")
  (check-equal?
   (expr/no-var=? SAMPLE-CMP SAMPLE-CMP)
   #t
   "THe compare expressions are true")
  (check-equal?
   (expr/no-var=? SAMPLE-IF-EXP SAMPLE-IF-EXP)
   #t
   "The if-expr are the same"))
; STRATEGY : Data Decomposition on e1, e2 : ExprNoVar
(define (expr/no-var=? e1 e2)
  (cond
    [(number? e1) (eq? e1 e2)]
    [(boolean? e1) (eq? e1 e2)]
    [(cons? e1) (and (eq? (first e1) (first e2)) (eq? (second e1) (second e2)))]
    [(string? e1) (eq? e1 e2)]
    [(lam/no-var? e1) (expr/no-var=? (lam/no-var-body e1) (lam/no-var-body e2))]
    [(arith? e1) 
     (and (eq? (arith-op e1)(arith-op e2))
          (= (length (arith-args e1)) (length (arith-args e2)))
          (compare-loenv-fn (arith-args e1) (arith-args e2)))]
    [(bool? e1)  
     (and (eq? (bool-op e1)(bool-op e1))
          (= (length (bool-args e1)) (length (bool-args e2)))
          (compare-loenv-fn (bool-args e1) (bool-args e2)))]
    [(cmp? e1)  
     (and (eq? (cmp-op e1)(cmp-op e1))
          (= (length (cmp-args e1)) (length (cmp-args e2)))
          (compare-loenv-fn (cmp-args e1) (cmp-args e2)))]
    [(if-exp? e1)  
     (and (expr/no-var=? (if-exp-test e1) (if-exp-test e2)) 
          (expr/no-var=? (if-exp-branch1 e1) (if-exp-branch1 e2))
          (expr/no-var=? (if-exp-branch2 e1) (if-exp-branch2 e2)))]
    [(call? e1)
     (and (expr/no-var=? (call-fn e1)(call-fn e2))
          (= (length (call-args e1)) (length (call-args e2)))
          (compare-loenv-fn (call-args e1) (call-args e2)))])) 

; compare-loenv-fn : ListOf<ExprNoVar> ListOf<ExprNoVar> -> Boolean
; WHERE : (= (length loe1)(length loe2))
; Returns true if both loe1 and loe2 are same.
; EXAMPLE:
(begin-for-test
  (compare-loenv-fn (list 1 2 3) (list 4 5 6))
  #f
  "The result of the compare expression")
; STRATEGY : Data Decomposition on loe1 loe2 : ListOf<ExprNoVar>
(define (compare-loenv-fn loe1 loe2)
  (cond
    [(empty? loe1) #true]
    [else (and (expr/no-var=? (first loe1) (first loe2)) 
               (compare-loenv-fn (rest loe1) (rest loe2)))])) 

;-------------------------------------------------------------------------------
; advantages of OOP-:
; 1.>The Basic OOP concepts like inheritance, encapsulation abstraction etc. 
; helps in code reuse, maintain and easily extend the code.
; 2.>Large programs are very difficult to write. 
; Object Oriented Programs force designers to go through an extensive 
; planning phase, which makes for better designs with less flaws. 
; In addition, once a program reaches a certain size, 
; Object Oriented Programs are actually easier to program 
; than non-Object Oriented ones.
; 3.>OOP inherently increases the code readability without even 
; giving a informal data definition.

; advantages of function-based programming
; 1.> sometimes the solution of a problem is not dynamic (meaning problem 
; wont change with time and so will the solution) and has a comparitively small
; solution. Using a OOP method makes the solution larger than it really is. 
; This is where a functional-based programming comes in handy.
; 2.> It has less verbose than a OOP style programming.
; 3.>