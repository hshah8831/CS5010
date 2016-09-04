;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; importing Packages
(require rackunit)
(require "extras.rkt")
(require 2htdp/batch-io)
(require 2htdp/image)
(define TIME-ON-TASK 25); Time taken to finish the task in hours

; CONSTANTS:
(define OPEN-BRACKET "(")
(define CLOSE-BRACKET ")")
(define MULTIPLY "*")
(define ADDITION "+")
(define INFIX-PAD 1)
(define PREFIX-PAD 3)
(define SPACE " ")

;(provide expr->strings)
; An Expr is one of:
; - Integer
;  - (make-add (cons Expr NEListOf<Expr>) Boolean)
;  - (make-mul (cons Expr NEListOf<Expr>) Boolean)
; Represents a numeric expression that is either an integer, or an addition
; or multiplication of two or more expressions.
; The Boolean flag indicates whether the expression uses infix(true)
; or prefix(false) 
; notation.
(define-struct add (exprs infix?))
; Add is a (make-add (cons Expr NEListOf<Expr>) Boolean) which is an 
; addition expression.
(define-struct mul (exprs infix?))
; Mul is a (make-mul (cons Expr NEListOf<Expr>) Boolean) which is an 
; multiplication expression

; TEMPLATE :
;(define (expr-fn ex)
;  (cond
;    [(integer? ex)...]
;    [(add? ex)...(add-fn ex)...]
;    [(mul? ex)...(mul-fn ex)...]))
;
;(define (add-fn addex)
;  (...(loexprs-fn(add-exprs addex))...(add-infix? addex)...))
;
;(define (mul-fn mulex)
;  (...(loexprs-fn(mul-exprs mulex))...(mul-infix? mulex)...))
;
;(define (loexprs-fn lox)
;  (cond
;    [(empty? (rest(rest lox))) ...]
;    [else
;     (... (expr-fn (first lox)) ...
;          (loexprs-fn (rest lox)) ...)]))

; expr->strings : Expr Natural -> ListOf<String>
; Returns a rendering of exp as a sequence of lines,
; where each line is a string not longer than width characters.
; EFFECT: errors if expr cannot fit width even after breaking
(begin-for-test (check-error (expr->strings 
                              (make-add (list (make-mul (list 1 2) true)
                                              (make-add (list 3 4) false))
                                        true) 4) "doesn't fit"))
(begin-for-test (check-equal? 
                 (expr->strings 
                  (make-add (list (make-mul (list 1 2) true)
                                  (make-add (list 3 4) false))
                            true) 100)
                 (list "((1 * 2) + (+ 3 4))")
                 "unstacked"))
(begin-for-test (check-equal? 
                 (expr->strings 
                  (make-add (list (make-mul (list 1 2) true)
                                  (make-add (list 3 4) false))
                            true) 10)
                 (list "( (1 * 2)" " +" " (+ 3 4))")      
                 "stacked with an infix"))

(begin-for-test (check-equal? 
                 (expr->strings 
                  (make-add (list (make-mul (list 1 2) false)
                                  (make-add (list 3 4) false))
                            false) 10)
                 (list "   (* 1 2)" "(   (+ 3 4))")
                 "stacked with all prefix"))

(begin-for-test
  (check-equal?
   (expr->strings (make-add (list 1 2 3) true) 100)
   '("(1 + 2 + 3)")
   "The expression is an addition expression which 
is also an unstacked infix"))

(begin-for-test
  (check-equal?
   (expr->strings (make-mul (list 3 4) true) 100)
   '("(3 * 4)")
   "The expression is a multiplication expression which 
is also an unstacked infix"))

(begin-for-test
  (check-equal?
   (expr->strings (make-add (list 4 5) false) 100)
   '("(+ 4 5)")
   "The expression is an addition expression which
is an unstacked prefix exp"))

(begin-for-test
 (check-equal?
  (expr->strings (make-mul (list 4 7) false) 100)
  '("(* 4 7)")
  "The expression is a mul expression which is an unstacked
 prefix exp"))

(begin-for-test
 (check-equal?
  (expr->strings (make-mul (list 4 7) false) 5)
  (list "   4" "(   7)")
  "The expression is a mul expression which is an unstacked
 prefix exp"))

(begin-for-test 
  (check-equal? 
   (expr->strings (make-add 
                    (list 1 
                          (make-mul (list 2 
                                          (make-add (list 3 4) true))
                                    true))false) 100)
   (list "(+ 1 (2 * (3 + 4)))")
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))

(begin-for-test
  (check-equal?
   (expr->strings (make-add (list (make-mul (list 3 4) true)
                                   (make-mul (list 5 6) true))
                             true) 100)
   (list "((3 * 4) + (5 * 6))")
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))
(begin-for-test
  (check-equal?
   (expr->strings (make-add (list (make-mul (list 3 4 5) true)
                                   (make-mul (list 5 6) true))
                             true) 100)
   '("((3 * 4 * 5) + (5 * 6))")
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))
(begin-for-test
  (check-equal?
   (expr->strings (make-add (list (make-mul (list 3 4 5) true)
                                   (make-mul (list 7 5 6) false))
                             true) 100)
   '("((3 * 4 * 5) + (* 7 5 6))")
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))
(begin-for-test
  (check-equal?
   (expr->strings (make-add (list (make-mul (list 3 4 5) true)
                                   (make-mul (list 7 5 6) false))
                             true) 10)
   '("(  3" "  *" "(  4" "  *" "  5)" " +" " (* 7 5 6))")
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))
(begin-for-test
  (check-equal?
   (expr->strings (make-add (list (make-mul (list 3 4) true)
                                   (make-mul (list 5 6 7) true))
                             true) 10)
   '("( (3 * 4)" " +" "  5" "  *" "(  6" "  *" "  7))")
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))
; STRATEGY : Function Composition
(define (expr->strings expr0 width)
  (local (; expr->strings/a : Expr Natural ListOf<String> -> ListOf<String>
          ; Returns a rendering of exp as a sequence of lines,
          ; where each line is a string not longer than width characters.
          ; WHERE: los is the ListOf<String> eligible to be 
          ; rendered from expr0 so far 
          ; expr is the elements of expr0 whose pretty printing is to be done.
          ; padding is the space indetation required so far
          ; expression
          ; Strategy: Data Decomposition on expr : Expr
          (define (expr->strings/a expr padding los)
            (cond
              [(integer? expr) 
               (concat-integer expr padding los)]
              [(add? expr)
               (concat-addition expr padding los )]
              [(mul? expr)
               (concat-multiplication expr padding los)])) 
          ; concat-integer : Integer Natural ListOf<String> -> ListOf<String>
          ; Returns a ListOf<String> which has the incoming Integer 
          ; string-appended either in the (first los) or appended to los
          ; depending on the length of (first los) compared to width.
          ; STRATEGY : Function Compsoition
          (define (concat-integer int padding los) 
            (if (> (+ (length-of-last-string los) 
                   (string-length (number->string int))) 
                   (- width padding))
                (error int "doesn't fit")  
                (append-in-last (final-integer-string padding int) los)))
          ; concat-addition : Add Natural ListOf<String>  -> ListOf<String>
          ; Returns a ListOf<String> which has the incoming Add converted to 
          ; string & string-appended either in the (first los) or appended to 
          ; los depending on the length of (first los) compared to width.
          ; STRATEGY : Function Composition
          (define (concat-addition add padding los)
            (if (<= (+ (length-of-last-string los) 
                   (string-length (expr-to-string add))) (- width padding))
                    (list (string-append (n-space-padding padding)  
                                         (expr-to-string add)))
                    (add-break-expr add padding los)))
          ; add-break-expr : Add Natural ListOf<String>  -> ListOf<String>
          ; Returns a ListOf<String> which has been broken appropriately at 
          ; proper places.
          ; STARTEGY : Data Decomposition on expr : Add
          (define (add-break-expr expr padding los)
            (if (add-infix? expr)
                        (infix-break (add-exprs expr) 
                                     (+ INFIX-PAD padding) 
                                     los ADDITION)
                        (prefix-break (add-exprs expr)  
                                      (+ PREFIX-PAD padding) 
                                      los)))  
          ; concat-multiplication : Mul Natural ListOf<String>  
          ;                                                -> ListOf<String>
          ; Returns a ListOf<String> which has the incoming Mul converted to    
          ; string & string-appended either in the (first los) or appended to 
          ; los depending on the length of (first los) compared to width.
          ; STRATEGY : Function Composition
          (define (concat-multiplication mul padding los)
              (if (<= (+ (length-of-last-string los) 
                   (string-length (expr-to-string mul))) (- width padding))
                    (list (string-append (n-space-padding padding)  
                                         (expr-to-string  mul)))
                    (mul-break-expr mul padding los)))
          ; add-break-expr : Mul Natural ListOf<String>  -> ListOf<String>
          ; Returns a ListOf<String> which has been broken appropriately at 
          ; proper places.
          ; STARTEGY : Data Decomposition on expr : Mul
          (define (mul-break-expr expr padding los)
            (if (mul-infix? expr)
                        (infix-break (mul-exprs expr) 
                                     (+ INFIX-PAD padding)
                                     los MULTIPLY)
                        (prefix-break (mul-exprs expr)  
                                      (+ PREFIX-PAD padding) 
                                      los)))
          ; prefix-break : NEListOf<Expr> Natural ListOf<String> 
          ;                                               -> ListOf<String>
          ; Returns a ListOf<String> which has been broken appropriately at 
          ; proper places according to the prefix rules.
          ; STRATEGY  : Data Decomposition on loexpr : NEListOf<Expr>
          (define (prefix-break loexpr padding los)
            (cond
              [(empty? (rest loexpr)) 
               (append-in-last ")"
                               (append-in-first 
                                "("(expr->strings/a (first loexpr)  
                                             padding  
                                             los)))]
              [else (append (expr->strings/a (first loexpr)  
                                                   padding 
                                                   los)
                            (prefix-break (rest loexpr) 
                                          padding los))]))   
          ; prefix-break : NEListOf<Expr> Natural ListOf<String> 
          ;                                               -> ListOf<String>
          ; Returns a ListOf<String> which has been broken appropriately at 
          ; proper places according to the infix rules.
          ; STRATEGY  : Data Decomposition on loexpr : NEListOf<Expr>
          (define (infix-break loexpr padding los operator)  
            (cond
              [(empty? (rest (rest loexpr)))
               (append-in-last ")" 
                               (append-in-first "("
                                       (append (expr->strings/a (first loexpr) 
                                                           padding los)
                       (list (string-append 
                              (n-space-padding padding) operator))
                       (expr->strings/a (second loexpr) 
                                        padding los))))]
              [else (append (expr->strings/a (first loexpr) 
                                             padding los)
                            (list (string-append 
                                   (n-space-padding padding) operator))
                            (infix-break (rest loexpr) 
                                         padding los operator))])))     
    (expr->strings/a expr0 0 (list ""))))           

; append-in-last : s ListOf<String> -> ListOf<String>
; returns a list of string with s appended to the right of the last string
; EXAMPLE: 
(begin-for-test
  (check-equal?
   (append-in-last "a" (list "b" "c"))
   (list "b" "ca")
   "The string a is added to the last"))
; STRATEGY : Data Decomposition on los : ListOf<String>
(define (append-in-last s los)
  (cond
    [(empty? (rest los)) 
     (append (list (string-append (first los) s)) (rest los))]
    [else (append (list (first los)) (append-in-last s (rest los)))]))

; append-in-first : s ListOf<String> -> ListOf<String>
; returns a list of string with s appended to the left of the first string
; EXAMPLE: 
(begin-for-test
  (check-equal?
   (append-in-first "a" (list "b" "c"))
   (list "ab" "c")
   "The string a is added to the last"))
; STRATEGY : Data Decomposition on los : ListOf<String>
(define (append-in-first s los)
  (append (list (string-append s (first los))) (rest los)))

; length-of-last-string : NEListOf<String> -> Natural
; returns the length of the last string present in the list
; EXAMPLE:
(begin-for-test
  (check-equal?
   (length-of-last-string (list "abc" "b" "cggc"))
   4
   "The length of the last string in the list"))
(begin-for-test
  (check-equal?
   (length-of-last-string (list "abbgg"))
   5
   "The length of the first of the list"))
; STRATEGY : Data Decomposition on nelos : NEListOf<String>  
(define (length-of-last-string nelos)
  (cond
    [(empty? (rest nelos)) (string-length (first nelos))]
    [else (length-of-last-string (rest nelos))])) 
            
; n-space-padding : Natural -> String
; returns a string with n spaces.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (n-space-padding 5)
   "     "
   "There are 5 spaces in the string"))
;STRATEGY : Function Composition
(define (n-space-padding n)
  (make-string n #\ ))  

; final-integer-string : Natural Natural Natural -> String
; appends the int with proper space padding.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (final-integer-string 5 5)
   "     5"
   "The expression with the spaces"))
;STRATEGY : Function Composition
(define (final-integer-string padding int )
  (string-append (n-space-padding padding) 
                 (number->string int)))

; expr-to-string: Expr -> String
; Takes an Expression and gives a String which
; contains the addition or multiplication expression
; either as an infix or a prefix.
;EXAMPLE:
(begin-for-test
  (check-equal?
   (expr-to-string (make-add (list 1 2 3) true))
   "(1 + 2 + 3)"
   "The expression is an addition expression which 
is also an unstacked infix"))

(begin-for-test
  (check-equal?
   (expr-to-string (make-mul (list 3 4) true))
   "(3 * 4)"
   "The expression is a multiplication expression which 
is also an unstacked infix"))

(begin-for-test
  (check-equal?
   (expr-to-string (make-add (list 4 5) false))
   "(+ 4 5)"
   "The expression is an addition expression which
is an unstacked prefix exp"))

(begin-for-test
 (check-equal?
  (expr-to-string (make-mul (list 4 7) false))
  "(* 4 7)"
  "The expression is a mul expression which is an unstacked
 prefix exp"))

(begin-for-test 
  (check-equal? 
   (expr-to-string (make-add 
                    (list 1 
                          (make-mul (list 2 
                                          (make-add (list 3 4) true))
                                    true))false))
   "(+ 1 (2 * (3 + 4)))"
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))

(begin-for-test
  (check-equal?
   (expr-to-string (make-add (list (make-mul (list 3 4) true)
                                   (make-mul (list 5 6) true))
                             true))
   "((3 * 4) + (5 * 6))"
   "The expression is a nested expression on adds and muls with
varying unstacked infixes and prefixes"))
; STRATEGY: Function Composition
(define (expr-to-string expr0)
  (local (; expr->strings/a : Expr Natural ListOf<String> -> ListOf<String>
          ; Returns a rendering of exp as a sequence of lines,
          ; where each line is a string not longer than width characters.
          ; WHERE: string-so-far is the string representation of expr and 
          ; eventually represent expr0.
          ; Strategy: Data Decomposition on expr : Expr
          (define (expr-to-string/a expr string-so-far)
            (cond
              [(integer? expr) (concat-integer expr string-so-far)]
              [(add? expr)(concat-addition expr string-so-far)]
              [(mul? expr)(concat-multiplication expr string-so-far)])) 
          ; concat-integer : Integer String -> String
          ; Returns a String which has the incoming Integer 
          ; string-appended to string-so-far
          ; STRATEGY : Function Compsoition
          (define (concat-integer int string-so-far)
            (string-append string-so-far (number->string int)))
          ; concat-addition : Add String -> String
          ; Returns a String which has the incoming Add appened to 
          ; string-so-far.
          ; STRATEGY : Function Composition
          (define (concat-addition add string-so-far)
            (string-append  string-so-far (add->string add string-so-far)))
          ; add->string : Add String -> String
          ; Returns a String which has the incoming Add appened to 
          ; string-so-far according to the infix or prefix style of the Add.
          ; STARTEGY : Data Decomposition on add : Add
          (define (add->string add string-so-far)
            (if (add-infix? add) 
                (string-append OPEN-BRACKET
                               (infix-append (add-exprs add) string-so-far "+"))
                (string-append OPEN-BRACKET ADDITION
                               (prefix-append (add-exprs add) string-so-far) 
                               CLOSE-BRACKET)))
          ; concat-multiplication : Mul String -> String
          ; Returns a String which has the incoming Mul appened to 
          ; string-so-far.
          ; STRATEGY : Function Composition
          (define (concat-multiplication mul string-so-far)
              (string-append string-so-far (mul->string mul string-so-far)))
          ; mul->string : Mul String -> String
          ; Returns a String which has the incoming Mul appened to 
          ; string-so-far according to the infix or prefix style of the Add.
          ; STARTEGY : Data Decomposition on mul : Mul
          (define (mul->string mul string-so-far)
            (if (mul-infix? mul)
                (string-append OPEN-BRACKET
                               (infix-append (mul-exprs mul) string-so-far "*"))
                (string-append OPEN-BRACKET MULTIPLY
                               (prefix-append (mul-exprs mul)string-so-far) 
                               CLOSE-BRACKET)))
          ; prefix-append : NEListOf<Expr> String -> String
          ; Returns a String which has the incoming list of expr appened to 
          ; string-so-far according to the prefix style.
          ; STRATEGY  : Data Decomposition on loexpr : NEListOf<Expr>
          (define (prefix-append loexpr string-so-far)
            (cond
              [(empty? (rest loexpr)) 
               (string-append SPACE 
                              (expr-to-string/a (first loexpr) string-so-far))]
              [else
               (string-append SPACE 
                (expr-to-string/a 
                 (first loexpr) string-so-far) 
                (prefix-append (rest loexpr) string-so-far))]))
          ; infix-append : NEListOf<Expr> String -> String
          ; Returns a String which has the incoming list of expr appened to 
          ; string-so-far according to the infix style.
          ; STRATEGY  : Data Decomposition on loexpr : NEListOf<Expr>
          (define (infix-append loexpr string-so-far operator) 
            (cond
              [(empty? (rest (rest loexpr)))
               (string-append (expr-to-string/a (first loexpr) string-so-far)
                              SPACE operator SPACE 
                          (expr-to-string/a 
                           (second loexpr) string-so-far) CLOSE-BRACKET)]
              [else (string-append 
                     (expr-to-string/a (first loexpr) string-so-far) SPACE 
                     operator SPACE
                     (infix-append (rest loexpr) string-so-far operator))])))
    (expr-to-string/a expr0 "")))

; display-strings! : ListOf<String> -> Void
; EFFECT: Prints the given list of strings, one per line.
;(display-strings! (expr->strings expr0 width)) 