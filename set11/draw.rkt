#lang racket
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(define TIME-ON-TASK 48)

(provide INITIAL-WORLD)
(provide handle-mouse)
(provide Shape<%>)
(provide get-world-shapes)
(provide create-rectangle)
(provide create-circle) 

;==================== Constants ====================

(define EMPTY-LIST '())
(define TRUE #t)
(define FALSE #f)
(define ZERO 0)
(define INITIAL-RADIUS 0)

(define INITIAL-LEFT 0)
(define INITIAL-TOP 0)
(define INITIAL-RIGHT 0)
(define INITIAL-BOTTOM 0)
(define CONTROL-AREA-LIMIT 5)
(define INITIAL-BDX -1)
(define INITIAL-BDY -1)

; MouseEvent constants
(define BUTTON-DOWN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")

(define OUTLINE "outline")
(define SOLID "solid")
(define BLACK "black")
(define WHITE "white")
(define RED "red")
(define HALF-OPACITY 128)
(define FONT-SIZE 16)
(define WIDTH 489)
(define HEIGHT 326)
(define MT (empty-scene WIDTH HEIGHT))

;==================== Data definition ====================

;========== BoundingBox ==========

; A BoundingBox is a (list Coordinate Coordinate Coordinate Coordinate)
; INTERPRETATION: (list left top right bottom).
; A BoundingBox represents a box whose left x-coordinate is at "left", whose
; top y-coordinate is at "top", whose right x-coordinate is at "right", and
; whose bottom y-coordinate is at "bottom".

; EXAMPLES:
; (list 0 0 100 100)

; TEMPLATE:
; ListOf<X> template

;========== MouseEvent ==========

; button-down? : MouseEvent -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (button-down? "button-down")
   true
   "Button down event"))
; STRATEGY : Function Composition
(define (button-down? me) (mouse=? me BUTTON-DOWN))

; button-up? : MouseEvent -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (button-up? "button-up")
   true
   "Button up event"))
; STRATEGY : Function Composition
(define (button-up? me) (mouse=? me BUTTON-UP))
; drag? : MouseEvent -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (drag? "drag")
   true
   "Drag event"))
; STRATEGY : Function Composition
(define (drag? me) (mouse=? me DRAG))

;========== Func ==========

; A Func is one of:
; - 'ptr
; - 'rec
; - 'cir

; INTERPRETATION: represents the functionality
; - 'ptr represents the Pointer functionality
; - 'rec represents the make rectangle functionality
; - 'cir represents the make circle functionality

; EXAMPLES:
; 'ptr

(define INITIAL-FUNC 'ptr)

; TEMPLATE :
; func-fn : Func -> ???
;(define (func-fn f)
;  (cond
;    [(ptr? f) ...]
;    [(rec? f) ...]
;    [(cir? f) ...]))

; ptr? : Func -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (ptr? INITIAL-FUNC)
   true
   "Pointer"))
; STRATEGY : Data Decomposition on w : World
(define (ptr? f)
  (symbol=? f 'ptr))

; rec? : Func -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (rec? 'rec)
   true
   "Make rectangle"))
; STRATEGY : Data Decomposition on w : World
(define (rec? f)
  (symbol=? f 'rec))

; cir? : Func -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (cir? 'cir)
   true
   "Make circle"))
; STRATEGY : Data Decomposition on w : World
(define (cir? f)
  (symbol=? f 'cir))

;========== State ==========

; A State is one of:
; - 'create
; - 'resize
; - 'move
; - 'idle

; INTERPRETATION: represents the functionality
; - 'create represents the Shape<%> being created
; - 'resize represents the Shape<%> being resized
; - 'move represents the the Shape<%> being moved
; - 'idle represents the the Shape<%> being idled

; EXAMPLES:
; Create

(define INITIAL-STATE 'create)

; TEMPLATE :
; state-fn : State -> ???
;(define (state-fn state)
;  (cond
;    [(create? state) ...]
;    [(resize? state) ...]
;    [(move? state) ...]
;    [(idle? state) ...]))

; create? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (create? 'create)
   true
   "Create state"))
; STRATEGY : Data Decomposition on state : State
(define (create? state)
  (symbol=? state 'create))

; resize? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (resize? 'resize)
   true
   "Resize state"))
; STRATEGY : Data Decomposition on state : State
(define (resize? state)
  (symbol=? state 'resize))

; move? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (move? 'move)
   true
   "Move state"))
; STRATEGY : Data Decomposition on state : State
(define (move? state)
  (symbol=? state 'move))

; idle? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (idle? 'idle)
   true
   "Idle state"))
; STRATEGY : Data Decomposition on state : State
(define (idle? state)
  (symbol=? state 'idle))

;========== Corner ==========

; A Corner is one of:
; - 'top-left
; - 'top-right
; - 'bottom-left
; - 'bottom-right

; INTERPRETATION: represents the functionality
; - 'top-left represents the top left corner of a rectangle 
; - 'top-right represents the top right corner of a rectangle
; - 'bottom-left represents the bottom left corner of a rectangle
; - 'bottom-right represents the bottom right corner of a rectangle

; EXAMPLES:
; 'top-left

(define DEFAULT-CORNER 'top-left)

; TEMPLATE :
; corner-fn : Corner -> ???
;(define (corner-fn corner)
;  (cond
;    [(top-left? corner) ...]
;    [(top-right? corner) ...]
;    [(bottom-left? corner) ...]
;    [(bottom-right? corner) ...]))

; top-left? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (top-left? 'top-left)
   true
   "Top left corner"))
; STRATEGY : Data Decomposition on corner : Corner
(define (top-left? corner)
  (symbol=? corner 'top-left))

; top-right? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (top-right? 'top-right)
   true
   "Top right corner"))
; STRATEGY : Data Decomposition on corner : Corner
(define (top-right? corner)
  (symbol=? corner 'top-right))

; bottom-left? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (bottom-left? 'bottom-left)
   true
   "Bottom left corner"))
; STRATEGY : Data Decomposition on corner : Corner
(define (bottom-left? corner)
  (symbol=? corner 'bottom-left))

; bottom-right? : State -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (bottom-right? 'bottom-right)
   true
   "Bottom right corner"))
; STRATEGY : Data Decomposition on corner : Corner
(define (bottom-right? corner)
  (symbol=? corner 'bottom-right))


;==================== World ====================

; A World is a (define-struct ListOf<Shape<%> Func)
; INTERP:
; A ListOf<Shape<%>> is a list of Shape<%>.
; A Func is one of ptr, rec, cir defined above.
(define-struct world [lst-shape f])

; EXAMPLE
(define INITIAL-WORLD (make-world empty INITIAL-FUNC))

; TEMPLATE:
; world-fn : World -> ???
; (define (world-fn world)
;   (... (los-fn (world-lst-shape world)) ... (world-f world)...)

;==================== Interface ====================

(define Shape<%>
  (interface ()
    ; get-bounds : -> BoundingBox
    ; RETURNS: The BoundingBox for the corresponding shape
    get-bounds
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Shape<%>
    ; RETURNS: a shape with proper reaction the mouseevent.
    handle-mouse
    
    ; render : Image -> Image
    ; RETURNS: an image representation of the shape over given image.
    render))

;========== Circle% ==========

; TEST:
(begin-for-test
  (check-equal?
   (send
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'resize] [bdx 200] [bdy 149])
          handle-mouse 200 149 "button-down")
    get-bounds)
   '(149 149 251 251)
   "Button down snap")
  (check-equal?
   (send
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'move] [bdx 200] [bdy 149])
          handle-mouse 200 201 "button-down")
    get-bounds)
   '(150 150 250 250)
   "Button down inside cir, nothing changes")
  (check-equal?
   (send
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'idle] [bdx 200] [bdy 149])
          handle-mouse 200 300 "button-down")
    get-bounds)
   '(150 150 250 250)
   "Button down outside cir, nothing changes")
  (check-equal?
   (send
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'idle] [bdx 200] [bdy 149])
          handle-mouse 200 201 "button-up")
    get-bounds)
   '(150 150 250 250)
   "Button up inside cir, nothing changes")
  (check-equal?
   (send 
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'resize] [bdx 200] [bdy 149])
          handle-mouse 200 140 "drag")
    get-bounds)
   '(140 140 260 260)
   "Drag - Resize cir")
  (check-equal?
   (send 
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'idle] [bdx 200] [bdy 140])
          handle-mouse 200 140 "drag")
    get-bounds)
   '(150 150 250 250)
   "Drag - Not resize because bdx bdy was outside")
  (check-equal?
   (send 
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'move] [bdx 200] [bdy 200])
          handle-mouse 200 140 "drag")
    get-bounds)
   '(150 90 250 190)
   "Drag - Move")
  (check-equal?
   (send 
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'idle] [bdx 200] [bdy 200])
          handle-mouse 200 140 "enter")
    get-bounds)
   '(150 150 250 250)
   "Other mouse event")
  (check-equal?
   (send 
    (send (new Circle% [x0 200] [y0 200] [r 50]
               [state 'create] [bdx 200] [bdy 200])
          handle-mouse 200 140 "button-up")
    get-bounds)
   '(150 150 250 250)
   "Create a circle"))

; A Circle% is a
; (new Circle% [x0 Natural] [y0 Natural] [r Natural] [state State]
; [bdx Coordinate] [bdy Coordinate] 
; INTERPRETATION : represents a Circle with a X Coordinate, Y Coordinate and
; a r radius.
(define Circle%
  (class* object% (Shape<%>)
    (init-field x0 y0 r state bdx bdy)
    ; INTERPRETATION :
    ; 'x0' is the X Coordinate of center of circle,
    ; 'y0' is the Y Coordinate of center of circle,
    ; 'r' is the radius of circle,
    ; 'state' is a State, representing one of 'create, 'resize, 'move and 'idle
    ; 'bdx' & 'bdy' represents the position of the last mouse event 
    ;  inside or on control area the circle.
    
    ; get-bounds : Natural Posn -> BoundingBox
    ; Returns the BoundingBox for this Circle
    (define/public (get-bounds)
      (list (- x0 r) (- y0 r)
            (+ x0 r) (+ y0 r)))
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Circle%
    ; returns a new circle after updating its characteristics on mouse events.
    ; STRATEGY : Data Decomposition on me : MouseEvent
    (define/public (handle-mouse x y me)
      (cond
        [(button-down? me) (set-selected?-bdx-bdy x y)]
        [(button-up? me) (reset-mode/color/selected?)]
        [(drag? me) (resize/move-drag x y)]
        [else this]))
    
    ;========== resize/move-btn-down ==========
    
    ; resize/move-btn-down : Coordinate Coordinate -> Circle%
    ; returns a circle with selected toggled to true if the shape is 
    ; selected.
    (define (set-selected?-bdx-bdy mx my)
      (if (cir-on?/cir-in? mx my)
          (record-bdposn-toggle-selected? mx my)
          this))
    
    (define (record-bdposn-toggle-selected? mx my)
      (if (cir-on? mx my)
          (new Circle% [x0 x0] [y0 y0] [r (dist x0 mx y0 my)]
               [state 'resize] [bdx mx] [bdy my])
          (new Circle% [x0 x0] [y0 y0] [r r]
               [state 'move] [bdx mx] [bdy my])))
    
    ;========== resize/move-drag ==========
    
    ; resize/move-drag : Coordinate Coordinate -> Circle%
    ; returns a resized or a moved circle if selected else as it is.
    ; STRATEGY : Data Decomposition on state : State
    (define (resize/move-drag mx my)
      (cond
        [(create? state) (create-cir mx my)]
        [(resize? state) (resize-cir mx my)]
        [(move? state) (move-cir mx my)]
        [(idle? state) this]))
    
    ; create-cir : Coordinate Coordinate -> Circle%
    ; Given the mouse position, returns a new Circle%
    (define (create-cir mx my)
      (new Circle%
           [x0 x0] [y0 y0] [r (dist x0 mx y0 my)]           
           [state state] [bdx bdx] [bdy bdy]))
    
    ; resize-cir : Coordinate Coordinate -> Circle%
    ; Given the mouse position, returns a new Circle%
    (define (resize-cir mx my)
      (new Circle%
           [x0 x0] [y0 y0] [r (dist x0 mx y0 my)]           
           [state state] [bdx bdx] [bdy bdy]))
    
    ; move-cir : Coordinate Coordinate -> Circle%
    ; returns a new circle with modified position.
    (define (move-cir mx my)
      (new Circle%
           [x0 (+ x0 (- mx bdx))] [y0 (+ y0 (- my bdy))] [r r]
           [state state] [bdx mx] [bdy my]))
    
    ;========== reset-mode/color/selected? ==========
    
    ; reset-mode/color/selected? : -> Circle%
    ; returns a new circle after resetting mode color and the selected switch.
    (define
      (reset-mode/color/selected?)
      (new Circle% [x0 x0] [y0 y0] [r r]
           [state 'idle] [bdx bdx] [bdy bdy]))
    
    ; cir-on?/cir-in? : Coordinate Coordinate -> Boolean
    ; returns true if(mx,my) are is in or on the circle.
    (define (cir-on?/cir-in? mx my)
      (<= 0 (dist mx x0 my y0) (+ r 2)))
    
    ; cir-on? : Coordinate Coordinate -> Boolean
    ; returns true if (x,y) is on the control area of the circle.
    (define (cir-on? x y)
      (<= (- r 2) (dist x x0 y y0) (+ r 2)))
    
    ; render : -> Image
    ; returns the image of the circle over a the scene.
    ; STRATEGY : Data Decomposition on state : State
    (define/public (render scene)
      (cond
        [(create? state) (place-image (circle r HALF-OPACITY RED) x0 y0 scene)]
        [(resize? state) (place-image (circle r OUTLINE BLACK) x0 y0 scene)]
        [(move? state) (place-image (circle r OUTLINE BLACK) x0 y0 scene)]
        [(idle? state) (place-image (circle r OUTLINE BLACK) x0 y0 scene)]))
    
    (super-new))) 

;========== Rectangle ==========

; TEST:
(begin-for-test
  (check-equal?
   (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
              [state 'idle] [bdx 230] [bdy 230] [corner 'top-left])
         get-bounds)
   '(200 200 250 250)
   "Bounding box test")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'resize] [bdx 230] [bdy 230] [corner 'top-left])
          handle-mouse 202 202 "button-down")
    get-bounds)
   '(202 202 250 250)
   "Button down top left snap")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 220] [right 240] [bottom 250]
               [state 'resize] [bdx 280] [bdy 80] [corner 'top-left])
          handle-mouse 201 250 "button-down")
    get-bounds)
   '(201 220 240 250)
   "Button down snap bottom left of rec in 'ptr")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 220] [right 240] [bottom 250]
               [state 'resize] [bdx 280] [bdy 80] [corner 'top-left])
          handle-mouse 241 220 "button-down")
    get-bounds)
   '(200 220 241 250)
   "Button down snap top right of rec in 'ptr")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 220] [right 240] [bottom 250]
               [state 'resize] [bdx 280] [bdy 80] [corner 'top-left])
          handle-mouse 240 251 "button-down")
    get-bounds)
   '(200 220 240 251)
   "Button down snap bottom right of rec in 'ptr")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'move] [bdx 250] [bdy 250] [corner 'top-left])
          handle-mouse 230 230 "button-down")
    get-bounds)
   '(200 200 250 250)
   "Button down inside rec in 'ptr")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'idle] [bdx 250] [bdy 250] [corner 'top-left])
          handle-mouse 400 400 "button-down")
    get-bounds)
   '(200 200 250 250)
   "Button down outside rec in 'ptr")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'idle] [bdx 230] [bdy 230] [corner 'top-left])
          handle-mouse 202 202 "button-up")
    get-bounds)
   '(200 200 250 250)
   "Button up snap")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'idle] [bdx 250] [bdy 250] [corner 'top-left])
          handle-mouse 230 230 "button-up")
    get-bounds)
   '(200 200 250 250)
   "Button up inside rec in 'ptr")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'idle] [bdx 250] [bdy 250] [corner 'top-left])
          handle-mouse 400 400 "button-up")
    get-bounds)
   '(200 200 250 250)
   "Button up outside rec in 'ptr")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'resize] [bdx 200] [bdy 200] [corner 'top-left])
          handle-mouse 205 205 "drag")
    get-bounds)
   '(205 205 250 250)
   "Drag top left of rec in 'ptr to resize")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'resize] [bdx 200] [bdy 200] [corner 'top-left])
          handle-mouse 206 206 "drag")
    get-bounds)
   '(206 206 250 250)
   "Drag top left of rec in 'ptr to resize")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'resize] [bdx 200] [bdy 250] [corner 'bottom-left])
          handle-mouse 206 251 "drag")
    get-bounds)
   '(206 200 250 251)
   "Drag bottom left of rec in 'ptr to resize")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'resize] [bdx 250] [bdy 200] [corner 'top-right])
          handle-mouse 256 201 "drag")
    get-bounds)
   '(200 201 256 250)
   "Drag top right of rec in 'ptr to resize")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
               [state 'resize] [bdx 250] [bdy 250] [corner 'bottom-right])
          handle-mouse 256 251 "drag")
    get-bounds)
   '(200 200 256 251)
   "Drag bottom right of rec in 'ptr to resize")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 220] [right 240] [bottom 250]
               [state 'move] [bdx 220] [bdy 230] [corner 'top-left])
          handle-mouse 250 250 "drag")
    get-bounds)
   '(230 240 270 270)
   "Drag to move")
  (check-equal?
   (send 
    (send (new Rectangle% [left 240] [top 240] [right 200] [bottom 200]
               [state 'idle] [bdx 200] [bdy 200] [corner 'top-left])
          handle-mouse 220 220 "button-down")
    get-bounds)
   '(200 200 240 240)
   "flipping left and right and top and bottom")
  (check-equal?
   (send 
    (send (new Rectangle% [left 200] [top 220] [right 240] [bottom 250]
               [state 'idle] [bdx 220] [bdy 230] [corner 'top-left])
          handle-mouse 250 250 "enter")
    get-bounds)
   '(200 220 240 250)
   "Other mouse events"))

; A Rectangle is a
; (new Rectangle% [left Coordinate] [top Coordinate] [right Coordinate]
; [bottom Coordinate] [state State] [bdx Coordinate] bdy Coordinate]
; [corner Corner])
; INTERPRETATION: represents a Rectangle with corners at (left, top) 
;                (left, bottom) (right, top) (right, bottom).
(define Rectangle%
  (class* object% (Shape<%>)
    (init-field left top right bottom state bdx bdy corner)
    ; INTERPRETATION :
    ; 'left' is the rectangle's left x-coordinate (e.g. 5),
    ; 'top' is the rectangle's top y-coordinate (e.g. 5),
    ; 'right' is the rectangle's right x-coordinate (e.g. 45),
    ; 'bottom' is the rectangle's bottom y-coordinate (e.g. 45).
    ; 'state' is an itemization data , representing
    ; one of States: 'create, 'resize, 'move and 'idle
    ; 'bdx' & 'bdy' represents the position of the last mouse event 
    ; inside or on control area the circle.
    ; 'corner' is an itemization data , representing
    ; one of Corners 'top-left, 'top-right, 'bottom-left and 'bottom-right.
    
    ; get-bounds : Natural Posn -> BoundingBox
    ; Returns the BoundingBox for this Circle
    (define/public (get-bounds)
      (list (lesser-of left right) (lesser-of top bottom) 
            (greater-of left right) (greater-of top bottom))) 
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Circle%
    ; returns a new rectangle after updating its characteristics on mouse events
    ; STRATEGY : Data Decomposition on me : MouseEvent
    (define/public (handle-mouse x y me)
      (cond
        [(button-down? me) (set-selected?-bdx-bdy x y)]
        [(button-up? me) (reset-mode/color/selected?)]
        [(drag? me) (resize/move-drag x y)]
        [else this]))
    
    ;========== resize/move-btn-down ==========
    
    ; resize/move-btn-down : Coordinate Coordinate -> Rectangle%
    ; returns a rectangle with selected toggled to true if the shape is 
    ; selected.
    (define (set-selected?-bdx-bdy mx my)
      (if (rec-on?/rec-in? mx my)
          (record-bdposn-toggle-selected? mx my)
          this))
    
    ; record-bdposn-toggle-selected? : Coordinate Coordinate -> Rectangle%
    ; returns a rectangle with respective corners activated.
    (define (record-bdposn-toggle-selected? mx my)
      (if (rec-on? mx my)
          (get-corner-selected mx my)
          (new Rectangle% [left left] [top top] [right right] [bottom bottom]
               [state 'move] [bdx mx] [bdy my] [corner corner])))
    
    ; get-corner-selected : Coordinate Coordinate -> Rectangle%
    ; returns a rectangle with respective corners activated.
    (define (get-corner-selected mx my)
      (if (given-corner? left top mx my)
          (new Rectangle% [left mx] [top my] [right right] [bottom bottom]
               [state 'resize] [bdx mx] [bdy my] [corner 'top-left])
          (check-other-corners mx my)))
    
    ; check-other-corners : Coordinate Coordinate -> Rectangle%
    ; returns a rectangle with respective corners activated.
    (define (check-other-corners mx my)
      (if (given-corner? right top mx my)
          (new Rectangle% [left left] [top my] [right mx] [bottom bottom]
               [state 'resize] [bdx mx] [bdy my] [corner 'top-right])
          (check-other-two-corner mx my)))
    
    ; check-other-two-corner : Coordinate Coordinate -> Rectangle%
    ; returns a rectangle with respective corners activated.
    (define (check-other-two-corner mx my)
      (if (given-corner? left bottom mx my)
          (new Rectangle% [left mx] [top top] [right right] [bottom my]
               [state 'resize] [bdx mx] [bdy my] [corner 'bottom-left])
          (new Rectangle% [left left] [top top] [right mx] [bottom my]
               [state 'resize] [bdx mx] [bdy my] [corner 'bottom-right])))
        
    ; Coordinate Coordinate -> Boolean
    ; Returns true if the top left corner control area is clicked.
    ; STRATEGY : Function Composition
    (define (given-corner? xaxis yaxis mx my)
      (and (<= (- xaxis CONTROL-AREA-LIMIT) mx (+ xaxis CONTROL-AREA-LIMIT)) 
           (<= (- yaxis CONTROL-AREA-LIMIT) my (+ yaxis CONTROL-AREA-LIMIT))))
    
    ;========== resize/move-drag ==========
    
    ; resize/move-drag : Coordinate Coordinate -> Rectangle%
    ; returns a resized or a moved rectanlge.
    ; STRATEGY : Data Decomposition on state : State
    (define (resize/move-drag mx my)
      (cond
        [(create? state) (create-rec mx my)]
        [(resize? state) (resize-rec mx my)]
        [(move? state) (move-rec mx my)]
        [(idle? state) this]))
    
    ;========== create-rec ==========
    
    ; create-rec : Coordinate Coordinate -> Rectangle%
    ; Given the mouse position, returns a new Rectangle% that is resized
    ; according to the selected corner.
    (define (create-rec mx my)
      (cond
        [(top-left? corner)
          (new Rectangle% [left mx] [top my] [right right] [bottom bottom]
               [state state] [bdx mx] [bdy my] [corner corner])]
        [(top-right? corner)
         (new Rectangle% [left left] [top my] [right mx] [bottom bottom]
               [state state] [bdx mx] [bdy my] [corner corner])]
        [(bottom-left? corner)
         (new Rectangle% [left mx] [top top] [right right] [bottom my]
               [state state] [bdx mx] [bdy my] [corner corner])]
        [(bottom-right? corner)
         (new Rectangle% [left left] [top top] [right mx] [bottom my]
               [state state] [bdx mx] [bdy my] [corner corner])]))
    
    ;========== resize-rec ==========
    
    ; resize-cir : Coordinate Coordinate -> Rectangle%
    ; Given the mouse position, returns a new Rectangle% that is resized
    ; according to the selected corner.
    (define (resize-rec mx my)
      (cond
        [(top-left? corner)
          (new Rectangle% [left mx] [top my] [right right] [bottom bottom]
               [state state] [bdx mx] [bdy my] [corner corner])]
        [(top-right? corner)
         (new Rectangle% [left left] [top my] [right mx] [bottom bottom]
               [state 'resize] [bdx mx] [bdy my] [corner corner])]
        [(bottom-left? corner)
         (new Rectangle% [left mx] [top top] [right right] [bottom my]
               [state 'resize] [bdx mx] [bdy my] [corner corner])]
        [(bottom-right? corner)
         (new Rectangle% [left left] [top top] [right mx] [bottom my]
               [state 'resize] [bdx mx] [bdy my] [corner corner])]))
    
    ;========== move-rec ==========
    
    ; move-rec : Coordinate Coordinate -: Rectangle%
    ; returns a rectangle with move corners.
    (define (move-rec mx my)
      (new Rectangle% [left (+ left (- mx bdx))] [top (+ top (- my bdy))]
           [right (+ right (- mx bdx))] [bottom (+ bottom (- my bdy))]
           [state state] [bdx mx] [bdy my] [corner corner]))
    
    ;========== reset-mode/color/selected? ==========
    
    ; reset-mode/color/selected? : -> Rectangle%
    ; resets the mode color and the selected switch.
    (define
      (reset-mode/color/selected?)
      (new Rectangle% [left left] [top top] [right right] [bottom bottom]
           [state 'idle] [bdx bdx] [bdy bdy] [corner corner]))
    
    ; rec-on?/rec-in? : Coordinate Coordinate -> Boolean
    ; returns true if (mx,my) is inside the shape or control area of the shape.
    (define (rec-on?/rec-in? mx my)
      (or (rec-on? mx my) (rec-in? mx my)))
    
    ; rec-on? : Coordinate Coordinate -> Boolean
    ; returns true if (x,y) is in the control area of the rectangle.
    (define (rec-on? x y)
      (or (and (<= (- left CONTROL-AREA-LIMIT) x (+ left CONTROL-AREA-LIMIT)) 
               (<= (- top CONTROL-AREA-LIMIT) y (+ top CONTROL-AREA-LIMIT)))
          (and 
           (<= (- left CONTROL-AREA-LIMIT) x (+ left CONTROL-AREA-LIMIT)) 
           (<= (- bottom CONTROL-AREA-LIMIT) y (+ bottom CONTROL-AREA-LIMIT)))
          (and (<= (- right CONTROL-AREA-LIMIT) x (+ right CONTROL-AREA-LIMIT)) 
               (<= (- top CONTROL-AREA-LIMIT) y (+ top CONTROL-AREA-LIMIT)))
          (and
           (<= (- right CONTROL-AREA-LIMIT) x (+ right CONTROL-AREA-LIMIT)) 
           (<= (- bottom CONTROL-AREA-LIMIT) y (+ bottom CONTROL-AREA-LIMIT)))))
    
    ; rec-in? : Coordinate Coordinate -> Boolean
    ; returns true if (x,y) is inside the rectangle.
    (define (rec-in? x y)
      (and (if (< left right) (< left x right) (< right x left))
           (if (< top bottom) (< top y bottom) (< bottom y top))))
    
    ; render : Image -> Image
    ; adds the image of the rectangle over scene.
    ; STRATEGY : Data Decomposition on state : State
    (define/public (render scene)    
      (cond
        [(create? state)
         (place-image 
          (rectangle (abs (- right left)) 
                     (abs (- bottom top)) HALF-OPACITY RED)
          (+ left (/ (- right left) 2)) (+ top (/ (- bottom top) 2)) scene)]
        [(resize? state)
         (place-image 
          (rectangle (abs (- right left)) 
                     (abs (- bottom top)) OUTLINE BLACK)
          (+ left (/ (- right left) 2)) (+ top (/ (- bottom top) 2)) scene)]
        [(move? state)
         (place-image 
          (rectangle (abs (- right left)) 
                     (abs (- bottom top)) OUTLINE BLACK)
          (+ left (/ (- right left) 2)) (+ top (/ (- bottom top) 2)) scene)]
        [(idle? state)
         (place-image 
          (rectangle (abs (- right left)) 
                     (abs (- bottom top)) OUTLINE BLACK)
          (+ left (/ (- right left) 2)) (+ top (/ (- bottom top) 2)) scene)]))
    
    (super-new)))

;==================== big-bang ====================

; run : World -> World
; Connect to the given chat server with user name nam.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (render TEST-CREATE-WORLD-REC)
   (foldr 
    (λ (shape scene) (send shape render scene)) 
    (place-image/align 
     (button-set (world-f TEST-CREATE-WORLD-REC)) 0 0 "left" "top"
     (empty-scene 489 326)) 
    (world-lst-shape TEST-CREATE-WORLD-REC))
   "World with 1 rec")
  (check-equal?
   (render TEST-CREATE-WORLD-CIR)
   (foldr 
    (λ (shape scene) (send shape render scene)) 
    (place-image/align 
     (button-set (world-f TEST-CREATE-WORLD-CIR)) 0 0 "left" "top"
     (empty-scene 489 326)) 
    (world-lst-shape TEST-CREATE-WORLD-CIR))
   "World with 1 cir"))
; STRATEGY: Function Composition
(define (run world)
  (big-bang world
            (on-mouse handle-mouse)
            (to-draw render)))

; handle-mouse : World Coordinate Coordinate MouseEvent -> World
; GIVEN: A World, mouse coordinates, and a MouseEvent
; RETURNS: A new World, like the given one, updated to reflect the action of
;    the mouse event, in the ways specified in the problem set.
; EXAMPLE:
(begin-for-test
    (check-equal?
     (world-f (handle-mouse TEST-WORLD-REC 100 100 "button-down"))
     'ptr
     "Button down event")
    (check-equal?
     (world-f (handle-mouse TEST-WORLD-REC 100 100 "drag"))
     'ptr
     "Drag event")
    (check-equal?
     (world-f (handle-mouse TEST-WORLD-REC 100 100 "button-up"))
     'ptr
     "Button down event")
    (check-equal?
     (handle-mouse TEST-WORLD-REC 100 100 "enter")
     TEST-WORLD-REC
     "Other mouse event")
    (check-equal?
     (world-f (handle-mouse
               (make-world 
                (list 
                 TEST-REC 
                 (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
                      [state 'resize] [bdx 200] [bdy 200] [corner 'top-left]))
                'ptr)
               100 100 "drag"))
     'ptr
     "Selected test case for rectangle")
    (check-equal?
     (world-f (handle-mouse
               (make-world 
                (list 
                 TEST-CIR
                 (new Circle% [x0 200] [y0 200] [r 50]
                      [state 'resize] [bdx -1] [bdy -1]))
                'ptr) 100 100 "drag"))
     'ptr
     "Selected test case for circle"))
; STRATEGY : Data Decomposition on me : MouseEvent
(define (handle-mouse w mx my me)
  (cond
    [(string=? me "button-down") 
     (select-function w mx my me)]
    [(string=? me "drag")
     (resize/move-shapes (world-lst-shape w) (world-f w) mx my me)]
    [(string=? me "button-up")
     (resize/move-shapes (world-lst-shape w) (world-f w) mx my me)]
    [else w]))

; render : World -> Image
; renders the world as an image.
; EXAMPLE : refer "run" test cases.
; STRATEGY : Data Decomposition on w : World
(define (render w)
  (local 
    ((define paint-canvas
       (place-image/align 
        (button-set (world-f w)) 0 0 "left" "top" (empty-scene 489 326))))
    (foldr 
     (λ (shape scene) (send shape render scene)) 
     paint-canvas 
     (world-lst-shape w))))  

; button-set : Func -> Image
; determines the current functionality of the world.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (button-set 'rec)
   (above (button-image "p" "white")
          (button-image "r" "black")
          (button-image "c" "white"))
   "Create rectangle")
  (check-equal?
   (button-set 'cir)
   (above (button-image "p" "white")
          (button-image "r" "white")
          (button-image "c" "black"))
   "Create circle"))
; STRATEGY : Data Decomposition on f : Func
(define (button-set f)
  (cond
    [(ptr? f) (above (button-image "p" "black")
                     (button-image "r" "white")
                     (button-image "c" "white"))]
    [(rec? f) (above (button-image "p" "white")
                     (button-image "r" "black")
                     (button-image "c" "white"))]
    [(cir? f) (above (button-image "p" "white")
                     (button-image "r" "white")
                     (button-image "c" "black"))]))

; button-image : 1String String -> Image
; WHERE : ch is one of "p" "r" "c".
;         color is either "black" "white".
; returns the image of a button.
; EXAMPLE : refer "run" test cases.
; STRATEGY: Function Composition
(define (button-image ch color)
  (if 
   (string=? color BLACK)
   (overlay (text ch FONT-SIZE WHITE) (rectangle 20 20 SOLID BLACK))
   (overlay (text ch FONT-SIZE BLACK) (rectangle 20 20 OUTLINE BLACK)))) 

; get-world-shapes : World -> ListOf<Shape<%>>
; GIVEN: A World,
; RETURNS: All the Shape<%>s which make up that world, i.e. all those that
;    have been created by the user through using the tools.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (map (λ (shape)
          (send shape get-bounds))
        (get-world-shapes TEST-CREATE-WORLD-REC))
   '((200 200 250 250))
   "Get shapes"))
; STRATEGY : Data Decomposition on w : World
(define (get-world-shapes w)
  (world-lst-shape w))

; create-circle : Posn Integer -> Shape<%>
; GIVEN: A center point and a radius
; RETURNS: A new Circle% object (implementing Shape<%>) with its center at
;    the given point and radius as given.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (send (create-circle (make-posn 100 100) 40) get-bounds)
   (list 60 60 140 140)
   "created new circle"))
; STRATEGY : Data Decomposition on p : Posn
(define (create-circle p r)
  ;  (new Circle% [x0 (posn-x p)] [y0 (posn-y p)] [r r] 
  ;       [mode HALF-OPACITY] [color RED] [selected? true] 
  ;       [bdx INITIAL-BDX] [bdy INITIAL-BDY])
  (new Circle% [x0 (posn-x p)] [y0 (posn-y p)] [r r]
       [state 'create] [bdx INITIAL-BDX] [bdy INITIAL-BDY]))

; create-rectangle : BoundingBox -> Shape<%>
; GIVEN: A bounding box,
; RETURNS: A new Rectangle% object (implementing Shape<%>) which is bounded
;    by the given BoundingBox.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (send (create-rectangle (list 200 300 250 350)) get-bounds)
   (list 200 300 250 350)
   "created new rectangle"))
; STRATEGY : Data Decomposition on lob : BoundingBox
(define (create-rectangle lob)
  (new Rectangle% [left (first lob)] [top (second lob)] [right (third lob)]
       [bottom (fourth lob)] [state 'create]
       [bdx INITIAL-BDX] [bdy INITIAL-BDY] [corner DEFAULT-CORNER]))

; dist : Coordinate Coordinate -> NonNegReal
; Computes the distance of point (x,y) to (x2, y2)
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (dist 3 0 4 0) 
   5 
   "the distance from point (3 4) to the origin is 5")
  (check-equal? 
   (dist -5 0 12 0) 
   13 
   "the distance from point (3 4) to the origin is 13"))
; STRATEGY : Function Composition
(define (dist x x2 y y2)
  (sqrt (+ (sqr (- x x2)) (sqr (- y y2)))))

;------------------------------BUTTON-DOWN--------------------------------------

; select-function -> World Coordinate Coordinate MouseEvent -> World
; returns world with either selected functionality or 
; created/moved/resized shapes.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (world-f (select-function TEST-WORLD-REC 1 1 "button-up"))
   'ptr 
   "Pointer"))
; STRATEGY : Function Composition
(define (select-function w mx my me)
  (if (between? mx my 0 20 0 20)
      (select-pointer w)
      (check-rest-func w mx my me)))

; check-rest-func : World Coordinate Coordinate MouseEvent -> World
; returns world with either selected functionality or 
; created/moved/resized shapes.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (world-f (check-rest-func TEST-WORLD-CIR 1 21 "button-up"))
   'rec 
   "Creat rectangle"))
; STRATEGY : Function Composition
(define (check-rest-func w mx my me)
  (if (between? mx my 0 20 20 40)
      (select-rectangle w)
      (check-for-circle w mx my me)))

; check-for-circle : World Coordinate Coordinate MouseEvent -> World
; returns world with either selected functionality or 
; created/moved/resized shapes.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (world-f (check-rest-func TEST-WORLD-CIR 1 41 "button-up"))
   'cir 
   "Creat circle"))
; STRATEGY : Data Decomposition on w : World 
(define (check-for-circle w mx my me)
  (if (between? mx my 0 20 40 60)
      (select-cirle w)
      (create/resize/move-shapes-aft-dwn (world-lst-shape w) 
                                         (world-f w) mx my me)))

; between? : Coordinate Coordinate Coordinate Coordinate Coordinate
; Coordinate -> Boolean
; returns true if (x,y) lies between (x0,y0) and (x1,y1).
; STRATEGY : Function Composition
(define (between? x y x0 x1 y0 y1)
  (and (< x0 x x1) (< y0 y y1)))

; select-pointer : World -> World
; returns world with pointer functionality
; EXAMPLE :refer "select-function" test cases
; STRATEGY : Data Decomposition on w : World
(define (select-pointer w)
  (make-world (world-lst-shape w) 'ptr))

; select-cirle : World -> World
; returns world with circle functionality.
; EXAMPLE :refer "check-for-circle" test cases
; STRATEGY : Data Decomposition on w : World
(define (select-cirle w)
  (make-world (world-lst-shape w) 'cir))

; select-rectangle : World -> World
; returns world with rectangle functionlity.
; EXAMPLE :refer "check-rest-func" test cases
; STRATEGY : Data Decomposition on w : World
(define (select-rectangle w)
  (make-world (world-lst-shape w) 'rec))

; create/resize/move-shapes-aft-dwn : ListOf<Shape<%>> Func Coordinate 
;                                                 Coordinate MouseEvent -> World
; returns world with a either a new shape added or move or resized 
; shapes or the same world as it is.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (world-f
    (create/resize/move-shapes-aft-dwn (list TEST-REC) 'rec 1 1 "button-up"))
   'rec
   "Add rec to the list")
  (check-equal?
   (world-f
    (create/resize/move-shapes-aft-dwn (list TEST-CIR) 'cir 1 1 "button-up"))
   'cir
   "Add rec to the list"))
; STRATEGY : Data Decomposition on f : Func
(define (create/resize/move-shapes-aft-dwn lst-shape f mx my me)
  (cond
    [(ptr? f) (make-world (map 
                           ; Shape -> Shape
                           ; sends shape to handle-mouse method.
                           ; STRATEGY : Function Composition
                           (λ (shape) (send shape handle-mouse mx my me))
                           lst-shape) f) ]
    [(rec? f) (add-rectangle-to-world lst-shape f mx my me)]
    [(cir? f) (add-circle-to-world lst-shape f mx my me)]))

; add-rectangle-to-world : ListOf<Shape<%>> Func Coordinate
;                                                 Coordinate MouseEvent -> World
; returns a world with added rectangle in the worlds list of shapes.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (world-f (add-rectangle-to-world (list TEST-REC) 'ptr 1 1 "button-up"))
   'ptr
   "Add rec to the list"))
; STRATEGY : Function Composition
(define (add-rectangle-to-world lst-shape f x y me)
  (make-world (add-rectangle-to-shapes lst-shape x y) f))

; add-circle-to-world : Func Coordinate Coordinate MouseEvent -> World
; returns a world with added circle in the worlds list of shapes.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (world-f (add-circle-to-world (list TEST-CIR) 'ptr 1 1 "button-up"))
   'ptr
   "Add cir to the list"))
; STRATEGY : Function Composition
(define (add-circle-to-world lst-shape f x y me)
  (make-world 
   (add-circle-to-shapes lst-shape x y) f)) 

; add-rectangle-to-shapes : ListOf<Shape<%>> Coordinate Coordinate ->
;                                                               ListOf<Shape<%>>
; returns a new rectangle created at (x,y) added to lst-shape.
; EXAMPLE : refer "add-circle-to-world" test cases.
; STRATEGY : Function Composition
(define (add-rectangle-to-shapes lst-shape x y)
  (cons (create-rectangle (list x y x y)) lst-shape))

; add-circle-to-shapes : ListOf<Shape<%>> Coordinate Coordinate -> 
;                                                               ListOf<Shape<%>>
; returns a new circle created at (x,y) added to lst-shape.
; EXAMPLE : refer "add-circle-to-world" test cases.
; STRATEGY : Function Composition
(define (add-circle-to-shapes lst-shape x y)
  (cons (create-circle (make-posn x y) INITIAL-RADIUS) lst-shape))

;------------------------------DRAG & BUTTON-UP---------------------------------

; resize/move-shapes : ListOf<Shape<%>> Func Coordinate
;                                                 Coordinate MouseEvent -> World
; returns a world with all the selected shapes resized or moved.
; EXAMPLE : refer handle-mouse test cases.
;STRATEGY : Function Composition
(define (resize/move-shapes lst-shape f mx my me)
  (make-world (map
               ; Shape -> Shape
               ; sends shape to handle-mouse method.
               ; STRATEGY : Function Composition
               (λ (shape) (send shape handle-mouse mx my me))
               lst-shape) f))

; slope-constant : Coordinate Coordinate Coordinate Coordinate -> Natural
; returns the slope constant of the line that runs from (x1,y1) to (x2,y2).
; EXAMPLE
(begin-for-test
  (check-equal?
   (slope-constant 1 2 3 4) 2.0 "two points given"))
; STRATEGY : Function COmposition
(define (slope-constant x1 y1 x2 y2)
  (/ (dist x1 x2 y1 y2) (sqrt (add1 (sqr (slope x1 y1 x2 y2)))))) 

; Slope : Coordinate Coordinate Coordinate Coordinate -> Natural
; returns the slope of the line that runs from (x1,y1) to (x2,y2).
; EXAMPLE :
(begin-for-test
  (check-equal?
   (slope 1 2 3 4) 1 "two points given"))
; STRATEGY : Function Composition
(define (slope x1 y1 x2 y2)
  (/ (- y2 y1) (- x2 x1)))

; lesser-of : Coordinate Coordinate -> Coordinate
; returns lesser of x and y.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (lesser-of 4 5) 4 "given two distinct coordinates")
  (check-equal?
   (lesser-of 4 3) 3 "given two distinct coordinates"))
; STRATEGY : Function Composition
(define (lesser-of x y)
  (if (< x y) x y))

; lesser-of : Coordinate Coordinate -> Coordinate
; returns greater of x and y.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (greater-of 4 5) 5 "given two distinct coordinates")
  (check-equal?
   (greater-of 4 3) 4 "given two distinct coordinates"))
; STRATEGY : Function Composition
(define (greater-of x y)
  (if (> x y) x y))

;------------------------------TEST CONSTANTS---------------------------------

(define TEST-REC
  (new Rectangle% [left 200] [top 200] [right 250] [bottom 250]
       [state 'move] [bdx 200] [bdy 200] [corner 'top-left]))

(define TEST-CIR
  (new Circle% [x0 200] [y0 200] [r 50] [state 'move]
       [bdx -1] [bdy -1] ))

(define TEST-WORLD-REC (make-world (list TEST-REC) 'ptr))
(define TEST-WORLD-CIR (make-world (list TEST-CIR) 'ptr))

(define TEST-CREATE-REC (create-rectangle (list 200 200 250 250)))
(define TEST-CREATE-CIR (create-circle (make-posn 200 200) 50))


(define TEST-CREATE-WORLD-REC (make-world (list TEST-CREATE-REC) 'ptr))
(define TEST-CREATE-WORLD-CIR (make-world (list TEST-CREATE-CIR) 'ptr))
