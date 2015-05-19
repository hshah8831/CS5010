#lang racket
; importing required teachpacks
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require racket/trace)
(define TIME-ON-TASK 25)

 ;providing required functions
(provide mk-Program%)
(provide mk-Expr%)
(provide Program<%>)
(provide Expr<%>)
(provide Result<%>)

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

; An Arith is (make-arith ArithOp 2ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : op represents the operator symbol and the args 
; represents the expressions
(define-struct arith (op args) #:transparent)
; TEMPLATE :
; Arith -> ???
;(define (arith-fn arith)
;  (...(arith-op arith)...(2listofx-fn (arith-args arith))...))
; EXAMPLE :
(define SAMPLE-ARITH (make-arith ADD '(2 3)))

; An Bool is (make-bool BoolOp 2ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : op represents the boolean operator symbol and the args 
; represents the expressions
(define-struct bool (op args) #:transparent)
; TEMPLATE :
; Bool -> ???
;(define (bool-fn bool)
;  (...(bool-op bool)...(2listofx-fn (bool-args bool))...))
; EXAMPLE :
(define SAMPLE-BOOL (make-bool OR '(#t #f)))

; An Comp is (make-cmp CmpOp 2ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : op represents the comparator operator symbol and the args 
; represents the expressions
(define-struct cmp (op args) #:transparent)
; TEMPLATE :
; Comp -> ???
;(define (cmp-fn cmp)
;  (...(cmp-op cmp)...(2listofx-fn (cmp-args cmp))...))
; EXAMPLE :
(define SAMPLE-CMP (make-cmp LESS-THAN '(2 3)))

; An IfExp is (make-if-exp X X X)
; WHERE : X can be Expr or ExprNoVar
; INTERP : test represents the test part of if and the branch1 branch2 
; represents the different branches of IfExp 
(define-struct if-exp (test branch1 branch2) #:transparent)
;TEMPLATE : 
; IfExp -> ???
;(define (if-exp-fn ifexp)
;  (...(expr-fn (if-exp-test expr))...
;      (expr-fn (if-exp-branch1 expr))...
;      (expr-fn (if-exp-branch2 expr))...))
; EXAMPLE :
(define SAMPLE-IF-EXP (make-if-exp (make-cmp LESS-THAN '(2 3)) 2 3))

; An Call is (make-call Expr ListOf<X>)
; WHERE : X can be Expr or ExprNoVar
; INTERP : fn represents the function name that is being called and args 
; represents the aruments passed. 
(define-struct call (fn args) #:transparent)
; TEMPLATE :
; Call -> ???
;(define (call-func call)
;  (...(expr-fn (call-fn expr))...(loe-fn (call-args expr))...))
; EXAMPLE :
(define SAMPLE-CALL (make-call 'f '(2 3)))

; A Var is a Symbol, representing PDPLang variable.

; An ErrString is a String, representing a PDPLang error message.

; A Lambda is a (make-lam UniqueListOf<Param> Expr)
; Represents a lambda expression in PDPLang
(define-struct lam (params body) #:transparent)
; TEMPLATE :
; Lambda -> ???
;(define (lam-fn lam)
;  (...(lop-fn (lam-params lam))...(expr-fn (lam-expr lam))...))
; EXAMPLE :
(define SAMPLE-LAM (make-lam '(a b) (make-arith ADD '(2 3))))

; A Def is a (make-def FnName UniqueListOf<Param> Expr)
; Represents the definition of a function with the specified parameters.
(define-struct def (name params body) #:transparent)
; TEMPLATE :
; Def -> ???
;(define (def-fn def)
;  (...(def-name def)...(loe-fn (def-params))...(expr-fn (def-body def))...))
; EXAMPLE : 
(define SAMPLE-DEF (make-def 'add '(a b) (make-arith ADD '(2 3))))

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


; A FnName is a Var, representing a function name.

;========== Program ==========

; A Program is a:
; - empty
; - (cons Expr Program)
; - (cons Def Program)
; Represents a PDPLang program consisting of function defs and expressions.
; WHERE: no two Defs have the same name

; EXAMPLES:
; - (list 1
;         (make-bool 'and (make-cmp '> 9 0)
;                         (make-cmp '< 0 9)))
;   Will evaluate to '(4 true)
; - (list (make-def 'square '(x) (make-arith '* '(x x)))
;         'square
;         (make-call 'square '(4)))
;   Will evaluate to '((make-lam '(x) (make-arith '* '(x x))) 16)

; TEMPLATE:
; program-fn : Program -> ???
;(define (program-fn p)
;  (cond
;    [(empty? p) ...]
;    [(expr? p) (... (expr-fn (first p)) ... (program-fn (rest p)) ...)]
;    [else (... (def-fn (first p)) ... (program-fn (rest p)) ...)]))

; A StaticDist is a (list Depth Index)
; Represents a variable reference
; where depth is number of additional lambdas between this var ref and the
; lambda for which this variable is a parameter,
; and index is the (0-based) position of this variable in that lambda's
; parameter list.
; TEMPLATE :
; StaticDist -> ???   
;(define (staticdist-fn sd)
;  (...(first sd)...(second sd)...))

; A Param-StaticDist is a (list Param Depth Index)
; Represents the Param and its StaticDist.
; TEMPLATE :
; StaticDist -> ???
;(define (param-staticdist-fn psd)
;  (...(first psd)...(rest psd)))

; A Depth is a Natural
; An Index is a Natural

; A LamNoVar is a (make-lam/no-var ExprNoVar)
(define-struct lam/no-var (body) #:transparent)
; TEMPLATE :
; LamNoVar -> ???
;(define (lamnovar-fn lnv)
;  (...(lam/no-var-body lnv)...))

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

;-----------------Testing Constants--------------------------------------------
(define ARITH-EXPR-PLUS (make-arith '+ (list 123 123)))
(define ARITH-EXPR-SUBT (make-arith '- (list 123 123)))
(define ARITH-EXPR-MULT (make-arith '* (list 10.5 10.5)))
(define ARITH-EXPR-DIV (make-arith '/ (list 125 25)))
(define ARITHMETIC-EXPR-PROGRAM
  (list ARITH-EXPR-PLUS ARITH-EXPR-SUBT ARITH-EXPR-MULT ARITH-EXPR-DIV))
(define CMP-GREATHERTHAN-EXPR (make-cmp '> (list 37 36)))
(define CMP-LESSTHAN-EXPR (make-cmp '< (list 36 37)))
(define CMP-EQUALTO-EXPR (make-cmp '= (list 37 37 37)))
(define COMPARISON-PROGRAM
  (list CMP-GREATHERTHAN-EXPR CMP-LESSTHAN-EXPR CMP-EQUALTO-EXPR))
(define BOOL-AND-EXPR (make-bool 'and (list #t #t #t #t)))
(define BOOL-OR-EXPR (make-bool 'or (list #t #t #f #f)))
(define BOOL-AND-FALSE-EXPR (make-bool 'and (list #t #f)))
(define BOOL-OR-FALSE-EXPR (make-bool 'or (list #f #f)))
(define BOOLEAN-EXPR-PROGRAM
  (list BOOL-AND-EXPR BOOL-OR-EXPR BOOL-AND-FALSE-EXPR BOOL-OR-FALSE-EXPR))
(define NUM1 23)
(define NUM2 12)
(define NUM3 23)
(define BOOL-EXPR-1 (make-bool 'or (list (< NUM1 NUM2) (> NUM1 NUM2))))
(define BOOL-EXPR-2 (make-bool 'or (list (< NUM1 NUM3) (> NUM1 NUM3))))
(define EXPR1 (make-arith '+ (list NUM1 NUM2)))
(define EXPR2 (make-arith '- (list NUM1 NUM3)))
(define IF-EXPR-EVALUATES-EXPR1 (make-if-exp BOOL-EXPR-1 EXPR1 EXPR2))
(define IF-EXPR-EVALUATES-EXPR2 (make-if-exp BOOL-EXPR-2 EXPR1 EXPR2))
(define IF-EXPR-PROGRAM 
  (list IF-EXPR-EVALUATES-EXPR1 IF-EXPR-EVALUATES-EXPR2))
(define SQR-DEF
  (make-def 'Square (list 'num1) (make-arith '* (list 'num1 'num1))))
(define CALL-SQR-DEF (make-call 'Square (list 2)))
(define LAMBDA
  (make-lam (list 'radius) (make-arith '* (list 2 3.142 'radius))))
(define CIRCUMFERENCE-CALL (make-call LAMBDA (list 5)))
(define CIRCUMFERENCE-OF-CIRCLE-PROG (list CIRCUMFERENCE-CALL))
(define CHK-DENOM
  (make-def 'checkDenominator (list 'num1) (make-cmp '= (list 'num1 0))))
(define CHK-DIVIDE-BY-ZERO
  (make-def
   'avoidDivideByZero
   (list 'num1 'num2)
   (make-if-exp
    (make-call 'checkDenominator (list 'num2))
    "Cannot Divide by Zero"
    (make-arith '/ (list 'num1 'num2)))))
(define CHK-DIVIDE-BY-ZERO_1 (make-call 'avoidDivideByZero (list 10 5)))
(define CHK-DIVIDE-BY-ZERO_2 (make-call 'avoidDivideByZero (list 5 0)))
(define PROGRAM-CHK-DIVIDE-BY-ZERO
  (list
   CHK-DENOM
   CHK-DIVIDE-BY-ZERO
   CHK-DIVIDE-BY-ZERO_1
   CHK-DIVIDE-BY-ZERO_2))
(define FINAL-PROGRAM
  (list ARITH-EXPR-PLUS ARITH-EXPR-MULT CALL-SQR-DEF SQR-DEF #t 365))
(define TEST-EXPR-FOR-EXPR-WITH-NO-VAR
  (make-lam (list 'num1 'num2) (make-arith '+ (list 'num1 'num2))))
(define TEST-EXPR-WITH-NO-VAR-RESULT
  (make-lam/no-var (make-arith '+ '((0 0) (0 1)))))
(define LAMBDA-1
  (make-lam (list 'num1 'num2) (make-arith '* (list 'num1 'num2))))
(define LAMBDA-2
  (make-lam (list 'num1 'num2) (make-arith '* (list 'num2 'num1))))
(define EXPR-1
  (make-lam
   '(num1 num3)
   (make-lam
    '(num2 num4)
    (make-arith '* (list 'num1 'num2 (make-arith '* (list 'num3 'num4)))))))
(define EXPR-2
  (make-lam
   '(num3 num1)
   (make-lam
    '(num4 num2)
    (make-arith '* (list 'num3 'num4 (make-arith '* (list 'num1 'num2)))))))
(define SUBTRACT (make-lam (list 'num1 'num2) (make-arith '- '(5 6))))
(define ADD-FN
  (make-def 'addtwonums (list 'num1 'num2) 
            (make-arith '+ (list 'num1 'num2))))
(define WRONG-ARITH-EXPR (make-arith '+ (list #f)))
(define WRONG-BOOL-EXPR (make-bool 'and (list 1 #f)))
(define WRONG-CMP-EXPR (make-cmp 'and (list 1 #f)))
(define WRONG-IF-EXPR 
  (make-if-exp 5 '- (list #f)))
(define WRONG-CALL 
  (make-call 'add '(2 3)))
(define PROGRAM-WITH-ERR-EXPRS-ONLY
  (list WRONG-ARITH-EXPR WRONG-BOOL-EXPR 
        WRONG-CMP-EXPR WRONG-IF-EXPR WRONG-CALL))
(define ADD50-DEF
  (make-def 'add50 '(x) (make-call (make-call 'mk-add '(50)) '(x))))
(define ADD50-OR-6-DEF
  (make-def
   'add50-or-6
   '(y)
   (make-if-exp
    (make-cmp '< '(y 0))
    (make-call 'add50 '(y))
    (make-call (make-call 'mk-add '(6)) '(y)))))

(define Result<%>
  (interface ()
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    app))

(define Expr<%>
  (interface ()
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    eval
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    subst
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr.
    to-expr
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    to-expr/no-var
    ; to-expr/no-var : ListOf<Param-StatisDist> -> ExprNoVar
    ; takes a list that has Param and their corresponding StaticDist and 
    ; Returns a representation of this expression as a non-object ExprNoVar.
    to-expr/no-var/a
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given 
    ; Expr<%>
    expr=?))

(begin-for-test
  (check-equal?
   (send (new Number% [n 5]) expr=? (new Number% [n 5]))
   #t
   "The numbers are equal")
  (check-equal?
   (send (new Boolean% [bool #t]) expr=? (new Boolean% [bool #t]))
   #t
   "The booleansa are the same")
  (check-equal?
   (send (new ErrString% [err "errstring"]) expr=? 
         (new ErrString% [err "errstring"]))
   #t
   "The strings are equal")
  (check-equal?
   (send (mk-Expr% SAMPLE-ARITH) expr=? (mk-Expr% SAMPLE-ARITH))
   #t
   "The arithmetic expressions are equal")
  (check-equal?
   (send (mk-Expr% SAMPLE-BOOL) expr=? (mk-Expr% SAMPLE-BOOL))
   #t
   "The boolean expressions are same")
  (check-equal?
   (send (mk-Expr% SAMPLE-CMP) expr=? (mk-Expr% SAMPLE-CMP))
   #t
   "THe compare expressions are true")
  (check-equal?
   (send (mk-Expr% SAMPLE-IF-EXP) expr=? (mk-Expr% SAMPLE-IF-EXP))
   #t
   "The if-expr are the same"))

(begin-for-test
  (check-equal?
   (send (mk-Expr% (make-lam '(x) 'x)) 
         to-expr/no-var)
   (make-lam/no-var '(0 0))
   "basic lambda")
  (check-equal?
   (send (mk-Expr% (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z))))) 
         to-expr/no-var)
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas")
  (check-equal?
   (send (mk-Expr% 
          (make-lam '(x y) 
              (make-lam '(z) 
                        (make-if-exp 
                         (make-bool 'and (list (make-cmp '> '(x y)) #true ))
                         (make-arith '- '(2 3)) 
                         'z)))) 
         to-expr/no-var)
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
   (send (mk-Expr% "string") 
         to-expr/no-var)
   "string"
   "basic expression"))

(begin-for-test
  (check-equal?
   (send 
    (send (mk-Expr% 'x) 
          subst (new Number% [n 4]) 'x) to-expr)
   4
   "x matches")
  (check-equal?
   (send 
    (send (mk-Expr% 'y) 
          subst (new Number% [n 4]) 'x) to-expr)
   'y
   "does not match")
  (check-equal?
   (send 
    (send (mk-Expr% (make-arith ADD '(x 5))) 
          subst (new Number% [n 4]) 'x) to-expr)
   (make-arith ADD '(4 5))
   "subst in arith")
  (check-equal?
   (send 
    (send (mk-Expr% (make-lam '(y) (make-arith ADD '(x y)))) 
          subst (new Number% [n 4]) 'x) to-expr)
   (make-lam '(y) (make-arith ADD '(4 y)))
   "subst in lambda")
  (check-equal?
   (send 
    (send (mk-Expr% (make-lam '(x) (make-arith ADD '(x 5)))) 
          subst (new Number% [n 4]) 'x) to-expr)
   (make-lam '(x) (make-arith ADD '(x 5)))
   "dont subst shadowed vars in lambdas")
  (check-equal?
   (send 
    (send (mk-Expr% (make-bool AND '(x #true))) 
          subst (new Boolean% [bool #false]) 'x) to-expr)
   (make-bool 'and (list false true))
   "subst in bool")
  (check-equal?
   (send 
    (send (mk-Expr% "errstring") 
          subst (new Boolean% [bool #false]) 'x) to-expr)
   "errstring"
   "given a string as expression")
  (check-equal?
   (send 
    (send (mk-Expr% "errstring") 
          app empty empty) to-expr)
   "errstring"
   "given a string as expression")
  (check-equal?
   (send 
    (send (mk-Expr% 4) 
          app empty empty) to-expr)
   "errstring"
   "given a string as expression")
  (check-equal?
   (send 
    (send (mk-Expr% #false) 
          app empty empty) to-expr)
   "errstring"
   "given a string as expression"))

; A Number% is a
; (new Number% [n Number]) 
; INTERPRETATION : represents a number expression.
(define Number%
  (class* object% (Expr<%> Result<%>)
    (init-field n)
    ; INTERPRETATION :
    ; n represents a number expression.
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval lodef) (new Number% [n n]))
    
    ; subst : Result<%> Var -> Expr<%>
    (define/public (subst res var) (new Number% [n n]))
    
    ; to-expr : -> Expr
    (define/public (to-expr) n)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var) n)
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) n)
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr) 
      (= (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    (define/public (app lor lodef) (new ErrString% [err "errstring"]))
    
    (super-new)))  

; A Boolean% is a
; (new Boolean% [bool Boolean]) 
; INTERPRETATION : represents a boolean expression.
(define Boolean%
  (class* object% (Expr<%> Result<%>)
    (init-field bool)
    ; INTERPRETATION :
    ; bool represents a boolean expression.
    
    ; eval : ListOf<Def> -> Result<%>
   (define/public (eval lodef) (new Boolean% [bool bool]))
    
    ; subst : Result<%> Var -> Boolean%
    (define/public (subst res var) (new Boolean% [bool bool]))
    
    ; to-expr : -> Expr
    (define/public (to-expr) bool)
    
    ; to-expr/no-var : -> ExprNoVar
   (define/public (to-expr/no-var) bool)
    
    ; to-expr/no-var : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) bool)
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr) 
      (boolean=? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    (define/public (app lor lodef)
      (new ErrString% [err "errstring"]))
    
    (super-new))) 

(begin-for-test
  (check-equal?
   (send (new Var% [var 'x]) to-expr/no-var)
   'x
   "a var is returned as it is as no staticdist is found")
  (check-equal?
   (send (new Var% [var 'x]) expr=? (new Var% [var 'x]))
   #true
   "same var given for comprison"))
; A Var% is a
; (new Var% [var Symbol]) 
; INTERPRETATION : represents a variable expression.
(define Var%
  (class* object% (Expr<%>)
    (init-field var)
    ; INTERPRETATION :
    ; var represents a variable expression.
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval lodef) 
      (local ((; ListOf<Def> -> Def
               ; returns the definition of the matched variable.
               ; STRATEGY : Function Composition
               define (get-elements lodef)
                (first (process-var-helper lodef))))
        (if (empty? (process-var-helper lodef))
            (new ErrString% [err "Variable Not Found"])
            (new Lambda% [params (def-params (get-elements lodef))]
                 [body (mk-Expr% (def-body (get-elements lodef)))]))))
   
    ; subst : Result<%> Var -> Expr<%>
    (define/public (subst res x) (if (symbol=? x var) res this))
     
    ; to-expr : -> Expr
    (define/public (to-expr) var)
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var) var)
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) 
      (get-corresponding-sd var lopsd))
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given 
    ; Expr<%>
    (define/public (expr=? expr) 
      (equal? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; process-var-helper: ListOf<Def> : Listof<Expr>
    ; Seachres and Returns the ListofExpr which are all make-def and
    ; whose def name is matching
    ; Strategy : Function Composition
    (define (process-var-helper lodef)
      (filter (λ (def) 
                (symbol=? (def-name def) var))
              lodef))
    
    (super-new)))

(begin-for-test
  (check-equal?
   (send (new ErrString% [err "errstring"]) to-expr/no-var)
   "errstring"
   "a errstring is returned as it is")
  (check-equal?
   (send (new ErrString% [err "errstring"]) to-expr/no-var/a empty)
   "errstring"
   "a errstring is returned as it is"))

; A ErrString% is a
; (new ErrString% err) 
; INTERPRETATION : represents a error string.
(define ErrString%
  (class* object% (Expr<%> Result<%>)
    (init-field err)
    ; INTERPRETATION :
    ; err represents an error string.
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the boolean to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval lodef) 
      (new ErrString% [err err]))
    
    ; subst : Result<%> Var -> ErrString%
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst res x) (new ErrString% [err err]))
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr.
    (define/public (to-expr) err)
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; the first empty is ListOf<Symbol> 
    ; the second empty is ListOf<Depth> 
    ; the third empty is ListOf<Index> 
    (define/public (to-expr/no-var) err)
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var/a lopsd) err)
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given 
    ; Expr<%>
    (define/public (expr=? expr) 
      (string=? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    (define/public (app lor lodef)
      (new ErrString% [err err]))
    
    (super-new)))

; A Lambda% is a
; (new Lambda% [params UniqueListOf<Param>] [body ListOf<Expr<%>>]) 
; INTERPRETATION : represents a lambda expression.
(define Lambda% 
  (class* object% (Expr<%> Result<%>)
    (init-field params body)
    ; INTERPRETATION :
    ; lam represents an error string.
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the boolean to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval lodef) (new Lambda% [params params] 
                                     [body body]))
    
    ; subst : Result<%> Var -> Lambda%
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst res x) 
      (new Lambda% [params params]
           [body (if (ormap (λ (param) (symbol=? x param)) params)
                      body (send body subst res x))]))
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr.
    (define/public (to-expr) 
      (make-lam params (send body to-expr)))
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; the first empty is ListOf<Symbol> 
    ; the second empty is ListOf<Depth> 
    ; the third empty is ListOf<Index> 
    (define/public (to-expr/no-var) 
      (to-expr/no-var/a '())) 
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) 
      (make-lam/no-var 
       (send body to-expr/no-var/a  
             (append (make-sd-for-lop 
                      params (send body to-expr)) lopsd))))
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given 
    ; Expr<%>
    (define/public (expr=? expr) 
      (equal? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    (define/public (app lor lodef)
      (send (subst-list params lor) eval lodef))
    
    ; subst-list : ListOf<Param> ListOf<Result> Expr -> Expr/ErrString
    ; Replaces all the params in the expr with corresponding element in lov
    (define (subst-list lop lov)
      (if (= (length lop) (length lov))
          (replace-var-with-value lop lov body)
          (new ErrString% 
               [err (string-append "expected " (number->string (length lop)) 
                                   " number of arguments, given " 
                                   (number->string(length lov)))])))
    
    ; replace-var-with-value : 
    ;                   ListOf<Param> ListOf<Result> Expr -> Expr/ErrString
    ; Replaces all the params in the expr with corresponding element in lov
    ; in the expression body/a. 
    (define (replace-var-with-value lop lov body/a)
      (cond
        [(empty? lop) body/a]
        [else (replace-var-with-value 
               (rest lop) (rest lov)  
               (send body/a subst (first lov) (first lop)))]))
    
    (super-new)))


(begin-for-test
  (check-equal?
   (map (λ (expr) (send expr to-expr )) 
        (send (mk-Program% 
               (append ARITHMETIC-EXPR-PROGRAM 
                       COMPARISON-PROGRAM BOOLEAN-EXPR-PROGRAM)) eval))
   '(246 0 110.25 5 #t #t #t #t #t #f #f)
   "checks all the basic operations")
  (check-equal?
   (map (λ (expr) (send expr to-expr )) 
        (send (mk-Program% 
               PROGRAM-WITH-ERR-EXPRS-ONLY) eval))
   '("+:expects a number as arguments"
     "and:question result is not true or false"
     "and:expects a real as argument"
     "test does not return boolean"
     "Variable Not Found")
   "checks all the basic operations that give errors"))
; A Arith% is a
; (new Arith% [op ArithOp] [args 2ListOf<Expr<%>>]) 
; INTERPRETATION : represents an arithmetic expression.
(define Arith%
  (class* object% (Expr<%>)
    (init-field op args)
    ; INTERPRETATION :
    ; op represents an arithmetic operator.
    ; args represents the objects that implement the Expr<%> iterface.
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval lodef)
      (local ((define lor (map (λ (x) (send x eval lodef)) args)))
      (if (andmap number? (map (λ (res) (send res to-expr)) lor))  
          (eval-arith lodef)
          (new ErrString% [err (string-append (symbol->string op) 
                         ":expects a number as arguments")])))) 
    
    ; subst : Result<%> Var -> Arith%
    (define/public (subst res x) 
      (new Arith% [op op] 
           [args (map (λ (expr) (send expr subst res x)) args)]))
    
    ; to-expr : -> Expr
    (define/public (to-expr) 
      (make-arith op (map (λ (x) (send x to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var) 
      (make-arith op (map (λ (arg) (send arg to-expr/no-var)) args)))
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) 
      (make-arith op (map (λ (arg) (send arg to-expr/no-var/a lopsd)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr) 
      (equal? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; eval-arith : ListOf<Def> -> Result<%>
    ; WHERE : all the Expr in args evaluate to a Number
    ; Evaluates an arithmatic expression.
    ; STRATEGY : Data Decomposition on op : ArithOp
    (define (eval-arith lodef)
      (local (; ListOf<Expr<%>> -> ListOf<Result>
              ; returns the result after evaluating each exprs.
              ; STRATEGY : Function Composition
              (define (eval-and-get-expr exprs)
                (map (λ (res)(send res to-expr)) 
                                     (map (λ (x) (send x eval lodef)) exprs))))
      (cond
        [(eq? op ADD) 
         (new 
          Number% [n (foldl + 0 (eval-and-get-expr args))])]
        [(eq? op SUB) 
         (new Number% 
              [n (- (send (send (first args) eval lodef) to-expr)
                    (foldl + 0 (eval-and-get-expr (rest args))))])]
        [(eq? op MULTIPLY) 
         (new Number% 
              [n (foldl * 1 (eval-and-get-expr args))])]
        [(eq? op DIVIDE) 
         (new Number% 
              [n (/ (send (send (first args) eval lodef) to-expr)
                    (foldl  * 1 (eval-and-get-expr (rest args))))])])))
    
    (super-new)))

; A Bool% is a
; (new Bool% [op BoolOp] [args 2ListOf<Expr<%>>]) 
; INTERPRETATION : represents a boolean expression.
(define Bool%
  (class* object% (Expr<%>)
    (init-field op args)
    ; INTERPRETATION :
    ; op represents an boolean operator.
    ; args represents the arguments of the boolean expression.
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval lodef)
      (local ((define lor (map (λ (expr) (send expr eval lodef)) args)))
      (if (andmap boolean? (map (λ (res) (send res to-expr)) lor))
          (eval-bool lodef)
          (new ErrString% [err (string-append (symbol->string op) 
                         ":question result is not true or false")]))))
    
    ; subst : Result<%> Var -> Bool%
    (define/public (subst res x) 
      (new Bool% [op op] 
           [args (map (λ (expr) (send expr subst res x)) args)]))
    
    ; to-expr : -> Expr
    (define/public (to-expr) 
      (make-bool op (map (λ (expr) (send expr to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var) 
      (make-bool op (map (λ (arg) (send arg to-expr/no-var)) args)))
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) 
      (make-bool op (map (λ (arg) (send arg to-expr/no-var/a lopsd)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr) 
      (equal? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; eval-bool : ListOf<Def> -> Result<%>
    ; WHERE : all the Expr in args evaluates to a Boolean.
    ; Evaluates a boolean expression.
    ; STRATEGY  : Data Decomposition on op : BoolOp
    (define (eval-bool lodef)
      (local (; ListOf<Expr<%>> -> ListOf<Result>
              ; returns the result after evaluating each exprs.
              ; STRATEGY : Function Composition
              (define (eval-and-get-expr exprs)
                (map (λ (res)(send res to-expr)) 
                                     (map (λ (x) (send x eval lodef)) exprs))))
      (cond
        [(eq? op AND) 
         (new Boolean% 
              [bool (andmap (λ (x) (eq? x #true))(eval-and-get-expr args))])]
        [(eq? op OR)  
         (new Boolean% 
              [bool 
               (ormap  (λ (x) (eq? x #true)) (eval-and-get-expr args))])])))
    
    (super-new)))

; A Cmp% is a
; (new Cmp% [op CmpOp] [args 2ListOf<Expr<%>>]) 
; INTERPRETATION : represents a comparison expression.
(define Cmp%
  (class* object% (Expr<%>)
    (init-field op args)
    ; INTERPRETATION :
    ; op represents an comprison operator.
    ; args represents the arguments of the boolean expression.
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the arithmetic expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval lodef)
      (local ((define lor (map (λ (expr) (send expr eval lodef)) args))) 
      (if (andmap number? (map (λ (res) (send res to-expr)) lor))
          (eval-comp lodef)
          (new ErrString% [err (string-append 
                                (symbol->string op) 
                                ":expects a real as argument")]))))
    
    ; subst : Result<%> Var -> Cmp%
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst res x) 
      (new Cmp% [op op] 
           [args (map (λ (expr) (send expr subst res x)) args)]))
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr.
    (define/public (to-expr) 
      (make-cmp op (map (λ (expr) (send expr to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; the first empty is ListOf<Symbol> 
    ; the second empty is ListOf<Depth> 
    ; the third empty is ListOf<Index> 
    (define/public (to-expr/no-var) 
      (make-cmp op (map (λ (arg) (send arg to-expr/no-var)) args)))
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) 
      (make-cmp op (map (λ (arg) (send arg to-expr/no-var/a lopsd)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given 
    ; Expr<%>
    (define/public (expr=? expr) 
      (equal? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    ; eval-comp : CmpOp 2ListOf<Expr> ListOf<Def> -> Result
    ; WHERE : all the Expr in args evaluates to a Number.
    ; Evaluates a comparison expression.
    ; STRATEGY : Data Decomposition on op : CmpOp
    (define (eval-comp lodef)  
      (local (; ListOf<Expr<%>> -> ListOf<Result>
              ; returns the result after evaluating each exprs.
              ; STRATEGY : Function Composition
              (define (eval-and-get-expr exprs)
                (map (λ (res)(send res to-expr)) 
                     (map (λ (x) (send x eval lodef)) exprs))))
        (cond
          [(eq? op EQUALS)
           (new Boolean% [bool (compare-a-list = (eval-and-get-expr args))])]
          [(eq? op LESS-THAN)
           (new Boolean% [bool (compare-a-list < (eval-and-get-expr args))])]
          [(eq? op GREATER-THAN)
           (new Boolean% [bool (compare-a-list > (eval-and-get-expr args))])])))
    
    ; compare-a-list : [Natural Natural -> Boolean] 2ListOf<Expr<%>> -> Boolean
    ; Returns true if all the Expr in lst evaluate positively to the 
    ; compare test.
    ; STRATEGY : Data Decompositon on lst : 2ListOf<Expr>
    (define (compare-a-list cmpop lst)
      (cond
        [(empty? (rest (rest lst))) (cmpop (first lst) (second lst))]
        [else (and (cmpop (first lst) (second lst)) 
                   (compare-a-list cmpop (rest lst)))]))
    
    (super-new)))

; A If-Exp% is a
; (new If-Cmp% [test Expr<%>] [branch1 Expr<%>] [branch2 Expr<%>]) 
; INTERPRETATION : represents a if expression expression.
(define If-Exp%
  (class* object% (Expr<%>)
    (init-field test branch1 branch2)
    ; INTERPRETATION :
    ; test represents an test of an if expression.
    ; branch1 represents the first branch of the if expression.
    ; branch2 represents the first branch of the if expression.
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval lodef) 
      (if (boolean? (send (send test eval lodef) to-expr))
                    (further-process-if lodef)
                    (new ErrString% [err "test does not return boolean"])))
    
    ; further-process-if : ListOf<Def> -> Result<%>
    ; returns results of the branch that is activated depending on the test.
    (define (further-process-if lodef)
      (if (false? (send (send test eval lodef) to-expr))
          (send branch2 eval lodef) (send branch1 eval lodef)))
    
    ; subst : Result<%> Var -> If-Exp%
    (define/public (subst res x) 
      (new If-Exp% [test (send test subst res x)] 
           [branch1 (send branch1 subst res x)] 
           [branch2 (send branch2 subst res x)]))
    
    ; to-expr : -> Expr
    (define/public (to-expr) 
      (make-if-exp (send test to-expr) 
                   (send branch1 to-expr) (send branch2 to-expr)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var) 
      (make-if-exp (send test to-expr/no-var) (send branch1 to-expr/no-var)
                  (send branch2 to-expr/no-var)))
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) 
      (make-if-exp (send test to-expr/no-var/a lopsd) 
                   (send branch1 to-expr/no-var/a lopsd)
                   (send branch2 to-expr/no-var/a lopsd)))
    
    ; expr=? : Expr<%> -> Boolean
    (define/public (expr=? expr) 
      (equal? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    (super-new)))

(begin-for-test
  (check-equal?
   (map (λ (expr) (send expr to-expr )) 
        (send (mk-Program% 
               (list
                MK-ADD-DEF
                ADD50-DEF
                ADD50-OR-6-DEF
                (make-call 'add50 '(100))
                (make-call 'add50-or-6 '(2))
                (make-call 'add50-or-6 '(-2)))) eval))
   (list 150 8 48)
   "checks all the basic call and substitution operations")
  (check-equal?
   (map (λ (expr) (send expr to-expr )) 
        (send (mk-Program% 
               (list CALL-SQR-DEF SQR-DEF)) eval))
   (list 4)
   "fn call before def")
  (check-equal?
   (map (λ (expr) (send expr to-expr )) 
        (send (mk-Program% 
               CIRCUMFERENCE-OF-CIRCLE-PROG) eval))
   '(31.419999999999998)
   "fn call after def")
  (check-equal?
   (map (λ (expr) (send expr to-expr )) 
        (send (mk-Program% 
               PROGRAM-CHK-DIVIDE-BY-ZERO) eval))
   '(2 "Cannot Divide by Zero")
   "fn call after def")
  (check-equal?
   (map (λ (expr) (send expr to-expr )) 
        (send (mk-Program% 
               FINAL-PROGRAM) eval))
   '(246 110.25 4 #t 365)
   "multiple function calls ")
  (check-equal?
   (send 
    (send (mk-Expr% 
           (make-arith '+ (list 3 'num1))) 
          subst (new Number% [n 4]) 'num1) to-expr)
   (make-arith '+ (list 3 4))
   "basic substitution test1")
  (check-equal?
   (send 
    (send (mk-Expr% 
           (make-arith '+ (list 5 'num))) 
          subst (new Number% [n 4]) 'num1) to-expr)
   (arith '+ '(5 num))
   "substitution unused var")
  (check-equal?
   (send 
    (send (mk-Expr% 
           (make-lam '(x y) (make-arith '* '(y x)))) 
          subst (new Number% [n 10]) 'num1) to-expr)
   (lam '(x y) (arith '* '(y x)))
   "substitution unused var")
  (check-equal?
   (send 
    (send (mk-Expr% 
           (make-lam '(a) (make-call 'y (list (make-lam '(b c) 'y))))) 
          subst (mk-Expr% (make-lam '(x) (make-arith '+ '(x 5)))) 'y) to-expr)
   (lam
    '(a)
    (call
     (lam '(x) (arith '+ '(x 5)))
     (list (lam '(b c) (lam '(x) (arith '+ '(x 5)))))))
   "substitution with nested lambda")
  (check-equal?
   (send (mk-Expr% 
           TEST-EXPR-FOR-EXPR-WITH-NO-VAR) to-expr/no-var)
   TEST-EXPR-WITH-NO-VAR-RESULT
   "static distance: one lam")
  (check-equal?
   (send (mk-Expr% (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z))))) 
         to-expr/no-var)
   (lam/no-var (lam/no-var (call '(1 0) '((1 1) (0 0)))))
   "static distance: nested lambdas")
  (check-equal?
   (send (mk-Expr% 
          (make-lam 
           '(x y) 
           (make-lam '(y x) (make-arith '/ '(x x x y))))) to-expr/no-var)
   (lam/no-var
    (lam/no-var (arith '/ '((0 1) (0 1) (0 1) (0 0)))))
   "static distance: shadowed vars")
  (check-equal?
   (send (mk-Expr% 
          (make-lam '(x) (make-call (make-lam '(x) (make-call 'x '(x))) '(x))))
         to-expr/no-var)
   (lam/no-var
    (call (lam/no-var (call '(0 0) '((0 0)))) '((1 0))))
   "static distance: shadowed vars")
  (check-equal?
   (send (mk-Expr% LAMBDA-1) expr=? (mk-Expr% LAMBDA-2))
   #false
   "two different expressions")
  (check-equal?
   (send (first (send (mk-Program% (list SUBTRACT)) eval)) 
         expr=? (mk-Expr% SUBTRACT))
   #true
   "two structurally similar expressions")
  (check-equal?
   (map 
    (λ (expr) (send expr to-expr )) 
    (send (mk-Program% 
           (list 
            (make-call 
             (make-call 
              (make-lam (quote (x)) (make-call (quote x) (quote (x)))) 
              (list (make-lam (quote (x)) (quote x)))) 
             (list (make-lam (quote (x)) (quote x)))))) eval))
   (list (lam '(x) 'x))
   "evaluate to lambda")
  (check-equal?
   (map 
    (λ (expr) (send expr to-expr )) 
    (send (mk-Program% (list ADD50-DEF (make-call 'add50 '(2 3 4 5)))) eval))
   (list "expected 1 number of arguments, given 4")
   "invalid number of parameters passed in the call")
  (check-equal?
   (send (mk-Expr% 
          (make-call 
           (make-lam (quote (x)) (make-call (quote x) (quote (x)))) 
           (list (make-lam (quote (x)) (quote x))))) 
         expr=?
         (mk-Expr% 
          (make-call 
           (make-lam (quote (x)) (make-call (quote x) (quote (x)))) 
           (list (make-lam (quote (x)) (quote x))))))
   #true
   "equating a call expression"))

; A Call% is a
; (new Call% [fn Expr<%>] [args ListOf<Expr<%>>]) 
; INTERPRETATION : represents a calling expression.
(define Call%
  (class* object% (Expr<%>)
    (init-field fn args)
    ; INTERPRETATION :
    ; fn represents a function call name.
    ; args represents represents the arguments of the function call.
    
    ; eval : ListOf<Def> -> Result<%>
    (define/public (eval lodef)
          (send (send fn eval lodef) app 
                (map (λ (expr) (send expr eval lodef)) args) lodef))

    ; subst : Result<%> Var -> Call%
    (define/public (subst res x) 
      (new Call% [fn (send fn subst res x)]
           [args (map (λ (expr) (send expr subst res x)) args)]))
    
    ; to-expr : -> Expr
    (define/public (to-expr) 
      (make-call (send fn to-expr) (map (λ (arg) (send arg to-expr)) args)))
    
    ; to-expr/no-var : -> ExprNoVar
    (define/public (to-expr/no-var) 
      (make-call (send fn to-expr/no-var) 
                 (map (λ (arg) (send arg to-expr/no-var)) args)))
    
    ; to-expr/no-var/a : ListOf<Param-StatisDist> -> ExprNoVar
    (define/public (to-expr/no-var/a lopsd) 
      (make-call (send fn to-expr/no-var/a lopsd) 
                 (map (λ (arg) (send arg to-expr/no-var/a lopsd)) args)))
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given 
    ; Expr<%>
    (define/public (expr=? expr) 
      (equal? (send this to-expr/no-var) (send expr to-expr/no-var)))
    
    (super-new)))

(define Program<%>
  (interface ()
    ; eval : -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in 
    ; the program.
    eval))

; A Program% is a
; (new Program% [lodef ListOf<Def>] [loexpr ListOf<Expr<%>>]) 
; INTERPRETATION : represents a program.
(define Program%
  (class* object% (Program<%>)
    (init-field lodef loexpr)
    ; lodef represents the list of all the definitions present in the program.
    ; loexpr represnts the list of all the expression  present in the program.
    
    ; eval : -> ListOf<Result<%>>
    (define/public (eval)
      (map (λ (expr) (send expr eval lodef)) loexpr))
    
    (super-new)))

; mk-Program% : Program -> Program<%>
; returns the object represntation of the given program.
; STRATEGY : Data Decomposition on p : Program
(define (mk-Program% pgm)
  (local((define lodef (filter def? pgm))
         (define loexpr (filter-not def? pgm)))
    (new Program% [lodef lodef] [loexpr (map mk-Expr% loexpr)])))

; mk-Expr% : Expr -> Expr<%>
; returns the object represenation of the given expression.
; STRATEGY : Data Decompositiono on expr : Expr
(define (mk-Expr% expr) 
  (cond
    [(number? expr) (new Number% [n expr])]
    [(boolean? expr) (new Boolean% [bool expr])]
    [(symbol? expr) (new Var% [var expr])]
    [(string? expr) (new ErrString% [err expr])]
    [(lam? expr) (new Lambda% [params (lam-params expr)] 
                     [body (mk-Expr% (lam-body expr))])]
    [(arith? expr) 
     (new Arith% [op (arith-op expr)] [args (map mk-Expr% (arith-args expr))])]
    [(bool? expr) 
     (new Bool% [op (bool-op expr)] [args (map mk-Expr% (bool-args expr))])]
    [(cmp? expr) 
     (new Cmp% [op (cmp-op expr)] 
          [args (map mk-Expr% (cmp-args expr))])]
    [(if-exp? expr) 
     (new If-Exp% [test (mk-Expr% (if-exp-test expr))] 
          [branch1 (mk-Expr% (if-exp-branch1 expr))] 
          [branch2 (mk-Expr% (if-exp-branch2 expr))])]
    [(call? expr) 
     (new Call% [fn (mk-Expr% (call-fn expr))] 
          [args (map mk-Expr% (call-args expr))])]))  

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
         (define (loe-fn p loe d)
           (get-param-depth/a
            p 
            (argmax (λ (expr) (get-param-depth/a p expr d)) loe) d)))
    (get-param-depth/a p expr0 0)))

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

;-------------------------------------------------------------------------------
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
; 3.> It gives an oppurtunity of giving an informal data definition.
