###1 Drawing Application Extension  
####1.1 Additional Preliminaries   
Save your solutions for this problem to a file named draw.rkt.  
Run the following expression (you must have required extras.rkt) to check that your file is properly named and is in the proper directory:  
(check-location "12" "draw.rkt")  
Add these additional provides at the top of your file (below the requires), so that we can test your solution:  
(provide INITIAL-WORLD)  
(provide handle-mouse)  
(provide Shape<%>)  
(provide get-world-shapes)  
(provide create-triangle)  
####1.2 Problem Description  
Extend your drawing program from set11 with triangles. If your code is well-organized, this should be a straightforward addition.  
Specifically, add the capability to draw equilateral triangles. When drawing the triangle, the location of the initial "button-down" determines the triangle center. The location of the "button-up" determines one of the corners, with the radius of the triangle being the distance from any corner to its center. During "drag" (before "button-up"), the program should display a triangle silhouette, just like before, anchored at its center, with a corner at the current mouse location (and the other corners should be equidistant to the center), rotating accordingly to mouse "move" events.  
Grabbing one of the triangle corners with the select tool should resize and rotate the triangle with its center anchored.  
UPDATE 2015-04-07: The “control region” is a square containing anything within 5 pixels of the corner point itself, just as it was for the rectangles in set11.  
Grabbing the body of the triangle with its select tool should smooth drag it without changing its orientation.  
UPDATE 2015-04-08: The signature and purpose statement for the required create-triangle function is:  
; create-triangle : posn posn -> Shape<%>  

; Creates a triangle Shape<%> object from a center

; posn and one arbitrary corner.

(define (create-triangle center corner) ...)

UPDATE 2015-05-07: We have released a triangle library (right-click and choose save-as) with the following helper functions.
; Posn functions ——————————————–

 
; subtract-posn : posn posn -> posn

; GIVEN: Two points, considered "positive" and "negative", respectively.

; RETURNS: The difference between those points, componentwise. Considering the

;    result as a vector, the vector points from the negative point to the

;    positive one.

 
; add-posn : posn posn -> posn

; GIVEN: Two points

; RETURNS: The sum of those points, componentwise.

 
; distance : posn posn -> Real

; GIVEN: Two points,

; RETURNS: the scalar distance between them.

 
; Triangle functions ————————————–

 
; in-triangle? : posn posn posn posn -> Boolean

; GIVEN: One "candidate" point and three corner points,

; RETURNS: true iff the candidate is inside the triangle defined

;   by the last three points.

 
; render-equilateral-triangle : posn posn String String Image -> Image

; GIVEN: A center Posn, an arbitrary corner Posn, an opacity String, a color

;    String, and a background Image,

; RETURNS: An Image like the background but with an equilateral triangle placed

;    onto it, such that its center is as given, one of its corners is at the

;    given corner, and its opacity and color as given. (The "center" is the

;    point that's equidistant from the three corners.)

 
; compute-corners : posn posn -> (list posn posn posn)

; Computes the corners of an equilateral triangle from a center and one corner.

; The given corner is also included in the output.

###2 Interpreter, OO Style  
####2.1 Additional Preliminaries
Save your solutions for this problem to a file named eval.rkt.  
Run the following expression (you must have required extras.rkt) to check that your file is properly named and is in the proper directory:  
(check-location "12" "eval.rkt")  
Add these additional provides at the top of your file (below the requires), so that we can test your solution:  
(provide mk-Program%)  
(provide mk-Expr%)  
(provide Program<%>)  
(provide Expr<%>)  
(provide Result<%>)  
####2.2 Problem Description  
The interpreter pattern is a common OO design pattern used to implement (unsurprisingly) a variety of interpreters. Revise your solution from set09 to useinterfaces and classes.  
Your interpreter should be able to handle the same kinds of expressions as in set09. A different class should represent each possible expression. You should also have a class forPrograms. (You may choose whether to convert definitions, ie Defs, to classes or to leave them as define-structs. Be ready to justify your design decision during your code walk.)  
Each class should implement one (or more) of the following interfaces. You may extend the given or add new interfaces if you wish. However, all the requested methods must be implemented.  
Make sure to follow the OOP Style guide.  
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
    ; expr=? : Expr<%> -> Boolean

    ; Returns true if this expression is structurally equivalent to the given Expr<%>

    expr=?))
(define Program<%>

  (interface ()

    ; eval : -> ListOf<Result<%>>

    ; Evaluates expressions in the program to Result<%>s

    ; WHERE: A function may be called before it is defined.

    ; WHERE: The results have the same order as their original expressions in the program.

    eval))  
In addition, define and provide the following functions. If you are familiar with OO patterns, these can be viewed as static factory methods (which are essentially just regular functions).  
; mk-Program% : Program -> Program<%>

(define (mk-Program% p) ...)

; mk-Expr% : Expr -> Expr<%>

(define (mk-Expr% e) ...)

The Program and Expr data definitions are the same as in set09.  
Note: Since we are using full Racket, define-structs must have the #:transparentannotation. In other words, use the following struct definitions for mk-Expr% and other functions.  
(define-struct def (name params body) #:transparent)  

(define-struct arith (op args) #:transparent)

(define-struct bool (op args) #:transparent)

(define-struct cmp (op args) #:transparent)

(define-struct if-exp (test branch1 branch2) #:transparent)

(define-struct call (fn args) #:transparent)

(define-struct lam (params body) #:transparent)

(define-struct lam/no-var (body) #:transparent)


