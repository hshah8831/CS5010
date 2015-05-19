#lang racket
(require racket/trace)
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(define TIME-ON-TASK 25)
(require "extras.rkt")

(provide Unit<%>)
(provide StatefulWorld<%>)
(provide mk-world)
(provide mk-ally)
(provide mk-enemy)
(provide mk-merc)

;==================== Constants ====================

(define EMPTY-LIST '())
(define TRUE #t)
(define FALSE #f)
(define ZERO 0)

;==================== Images ====================

(define ENEMY-RADIUS 12)
(define ENEMY-COLOR "red")
(define ENEMY-MODE "solid")
(define ALLY-SIDE 20)
(define ALLY-COLOR "green")
(define ALLY-MODE "solid")

(define ENEMY-IMG (circle ENEMY-RADIUS ENEMY-MODE ENEMY-COLOR))
(define ALLY-IMG (square ALLY-SIDE ALLY-MODE ALLY-COLOR))
(define PLUS-IMG (overlay (rectangle 20 1 "outline" "black")
                          (rectangle 1 20 "outline" "black")))
(define CROSSAIR (overlay/align "center" "center" PLUS-IMG
                                (circle 5 "outline" "black")
                                (circle 10 "outline" "black")))

(define ENEMY-ELIMINATED (/ 40 5))
(define ENEMY-REACH-BASE (/ -40 5))

(define ALLY-ELIMINATED (/ -20 5))
(define ALLY-REACH-BASE (/ 20 5))

(define MERC-ENEMY-ELIMINATED (/ 60 5))
(define MERC-ALLY-ELIMINATED (/ -60 5))

(define MERC-ENEMY-REACH-BASE (/ -60 5))
(define MERC-ALLY-REACH-BASE (/ 60 5))

(define MERC-SWITCH-SIDE-FREQ 3)
(define ADD-NEW-UNIT-FREQ 4)

(define FONT-SIZE 16)
(define WIDTH 400)
(define CENTER (/ WIDTH 2))
(define HEIGHT 500)

(define INITIAL-UNIT-HEIGHT 0)
(define BASE-INITIAL-HEIGHT 50)
(define GAME-END-HEIGHT-TEN 10)
(define GAME-END-HEIGHT-FIVE-HUNDRED 500)

(define MT (empty-scene WIDTH HEIGHT))
(define GAME-OVER-IMG (overlay/align "center" "center" 
                                (text "Game Over" 20 "red")
                                MT))

(define CLICK-DEAD 'dead)
(define TOUCH-BASE 'touchbase)

;========== MouseEvent ==========

; A MouseEvent is one of
; - "button-down"
; - else

; me-fn : MouseEvent -> ???
; STRATEGY : Data Decomposition on me : MouseEvent
;(define (me-fn me)
;  (cond
;    [(button-down? me) ...]
;    [else ...]))

; MouseEvent constants
(define BUTTON-DOWN "button-down")

; button-down? : MouseEvent -> Boolean
; EXAMPLE :
(begin-for-test
  (check-equal?
   (button-down? "button-down")
   true
   "Button down event"))
; STRATEGY : Function Composition
(define (button-down? me) (mouse=? me BUTTON-DOWN))


;========== Random-Unit ==========

; A Random-Unit is one of
; 0 is Ally%
; 1 is Enemy%
; 2 is Mercenery%

; ru-fn : Random-Unit -> ???
; STRATEGY : Data Decomposition on ru : Random-Unit
;(define (ru-fn ru)
;  (cond
;    [(= 1 ru) ...]
;    [(= 2 ru) ...]
;    [(= 3 ru) ...]))

;========== Death-Type ==========

; A Death-Type of one of
; 'dead
; 'touchbase

; INTERPRETATION : 
; 'dead represents the unit elimination by mouse clicking on it
; 'touchbase represents the unit elimination by touching the base tower

; dt-fn : Death-Type -> ???
; STRATEGY : Data Decomposition on type : Death-Type
;(define (dt-fn type)
;  (cond
;    [(symbol=? type 'dead) ...]
;    [(symbol=? type 'touchbase) ...]))

;==================== Publisher<%> ====================

(define Publisher<%>
  (interface ()
    ; subscribe! : Subscriber<%> -> Void
    ; EFFECT: Adds a new subscriber.
    add-unit!))

;==================== Subscriber<%> ====================

(define Subscriber<%>
  (interface ()
    ; report-base-height! : Natural -> Void
    ; Reports a baseheight.
    ; EFFECT: Reports the base height to all subscribers
    report-base-height!))

;==================== StatefulWorld<%> ====================

; Represents a mutable world object.
(define StatefulWorld<%>
  (interface ()
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    on-tick!
    
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: mutates this world to its next state from the given
    ; mouse parameters.
    on-mouse!
    
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    target-loc
    
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units in this world.
    get-units
    
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given unit to the world
    add-unit!))

;==================== World% ====================

;; A World% is a naive container for StatefulWorld<%>.
;; All it does is delegate to the appropriate handlers of its subcomponents.
(begin-for-test
  (check-equal?
   (send TEST-WORLD on-mouse! 300 300 "button-down")
   (void)
   "Nothing happens when clicks in the middle of
   screen in the beginning")
  (send TEST-WORLD on-tick!)
  (check-equal?   
   (send TEST-WORLD on-mouse! 300 300 "button-down")
   (void)
   "Nothing happens when clicks in the middle of
   screen in the beginning")
  (check-equal?
   (posn-x (send TEST-WORLD target-loc))
   300
   "Target X 300")
  (check-equal?
   (posn-y (send TEST-WORLD target-loc))
   300
   "Target Y 300")
  (check-equal?
   (length (send TEST-WORLD get-units))
   10
   "Total 10 units")
  (send TEST-WORLD on-tick!)
  (send TEST-WORLD on-tick!)
  (send TEST-WORLD on-tick!)
  (send TEST-WORLD on-tick!)
  (check-equal?
   (length (send TEST-WORLD get-units))
   11
   "Total 11 units because it's the fourth tick")
  (check-equal?
   (send TEST-WORLD get-game-over?)
   false
   "Game still going")
  (check-equal?
   (send TEST-WORLD render MT)
   (begin
     (send TEST-WORLD on-mouse! 300 300 "drag")
   (send TEST-WORLD  render MT))
   "the world does not react to any mouse event except bottun down")
  (check-equal?
   (begin
     (send GAME-OVER-WORLD on-tick!)
   (send GAME-OVER-WORLD  get-game-over?))
   #true
   "the game is over"))

(define World%
  (class* object% (StatefulWorld<%> Publisher<%>)
    (init-field  min-v max-v units [ticks 0] [base-height BASE-INITIAL-HEIGHT])

    (inspect false)
    
    (define mx -1)
    (define my -1)
    
    ; Sends the initial base height to all the units
    (for-each (λ (unit) (send unit report-base-height! base-height)) units)
    
    ; on-tick! : -> Void
    ; Computes the next world state. 
    (define/public (on-tick!)
      (begin 
        (if (add-new-unit?)
            (begin
              (unit-on-tick!-publish-base-height!)
              (add-unit! (gen-random-unit min-v max-v)))
            (unit-on-tick!-publish-base-height!))
        (local
          ((define touch-down-units
             (filter (λ (unit)
                       (send unit touch-down?))
                     units)))
          (set-base-height!-unit! touch-down-units TOUCH-BASE)))
      (set! ticks (add1 ticks)))
    
    ; add-new-unit? : -> Boolean
    ; Checks if a new unit needs to be added in this tick
    (define (add-new-unit?)
      (and (zero? (modulo ticks ADD-NEW-UNIT-FREQ))
           (not (zero? ticks))))
    
    ; unit-on-tick!-publish-base-height! -> Void
    ; Get all the units moving and publish the current
    ; base height to all units
    (define (unit-on-tick!-publish-base-height!)
      (for-each (λ (o) (send o on-tick!)) units)
      (for-each 
       (λ (unit) (send unit report-base-height! base-height)) units))
    
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: mutates this world to its next state from the given
    ; mouse parameters.
    (define/public (on-mouse! x y me)
      (set! mx x)
      (set! my y)
      (cond
        [(button-down? me)
         (local
           ((define eliminated-units
              (filter (λ (unit)
                        (send unit eliminated? x y))
                      units)))
           (set-base-height!-unit! eliminated-units CLICK-DEAD))]
        [else (void)]))
    
    ; set-base-height!-unit! : ListOf<Unit> Symbol -> Void
    ; Abstraction method that sets the base-height and units
    ; according to the dead-units and the way they die
    (define (set-base-height!-unit! dead-units modus-death)
      (set! base-height 
                   (+ base-height 
                      (foldl 
                       (λ (unit del-h) 
                         (send unit get-del-height modus-death)) 0
                       dead-units)))
      (set! units (remove* dead-units units)))
    
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    (define/public (target-loc)
      (make-posn mx my))
    
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units in this world.
    (define/public (get-units)
      units)
    
    ; get-game-over? : -> Boolean
    ;Returns the game-over boolean
    (define/public (get-game-over?)
      (or (<= base-height GAME-END-HEIGHT-TEN)
          (>= base-height GAME-END-HEIGHT-FIVE-HUNDRED)))
    
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given unit to the world
    (define/public (add-unit! u)
      (set! units (cons u units)))
    
    ; render : Image -> Image
    ; Draws the world objects, in order, onto the given img.
    (define/public (render init-scene)
      (local
        ((define base-img
           (rectangle WIDTH base-height "solid" "yellow"))
         (define score-img
           (text (number->string (calculate-score)) 10 "blue"))
         (define scene+base-img
           (overlay/align "center" "bottom" score-img base-img init-scene))
         (define scene+base-img+crossair
           (place-image CROSSAIR mx my scene+base-img)))
        (foldr (λ (o scene) (send o render scene))
               scene+base-img+crossair units)))
    
    ; calculate-score : -> Natural
    ; Calculates the score from the base height
    (define (calculate-score)
      (* 5 (- base-height BASE-INITIAL-HEIGHT)))
    
    (super-new)))

;==================== Unit<%> ====================

; Represents a unit in the game.
(define Unit<%>
  (interface ()
    ; get-loc : -> posn
    ; Returns the location of this unit as a posn.
    get-loc
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    get-color
    
    ; on-tick! : -> Void
    ; EFFECT : mutates the y coordinate of the Unit<%>.
    on-tick!
    
    ; eliminated? : Coordinate Coordinate -> Boolean
    ; Returns if the target is dead or not
    eliminated?
    
    ; get-del-height : Death-Type -> Natural
    ; Returns the height change according to the death type
    ; which could be either mouse click dead or touch base dead
    get-del-height
    
    ; touch-down? : -> Boolean
    ; Returns whether the unit touches the base
    touch-down?
    
    ; render : Image -> Image
    ; returns the image representation of the unit over img
    render))

(begin-for-test
  (check-equal?
   (posn-x (send TEST-ALLY get-loc))
   50
   "Initial X 50")
  (check-equal?
   (posn-y (send TEST-ALLY get-loc))
   0
   "Initial Y 0")
  (check-equal?
   (send TEST-ALLY get-color)
   ALLY-COLOR   
   "Ally color")
  (check-equal?
   (send TEST-ALLY render MT)
   (place-image ALLY-IMG 50 0 MT)
   "Test render")
  (check-equal?
   (send TEST-ALLY get-del-height CLICK-DEAD)
   -4
   "Ally eliminated")
  (check-equal?
   (send TEST-ALLY get-del-height TOUCH-BASE)
   4
   "Ally reached base")
  (check-equal?
   (send TEST-ALLY eliminated? 50 300)
   false
   "Not dead"))
(define Ally%
  (class* object% (Unit<%> Subscriber<%>)
    (init-field x y v )
    (inspect false)
    ; x represents the x-coord of the centre of the ally unit
    ; y represents the y-coord of the centre of the ally unit
    ; v represents the velocity of the ally unit
    
    (define base-height 10)
    
    ; report-base-height! : Natural -> Void
    ; Reports a baseheight.
    ; EFFECT: Reports the base height to all subscribers
    (define/public (report-base-height! height)
      (set! base-height height))
    
    ; get-loc : -> posn
    (define/public (get-loc)
      (make-posn x y))
    
    ; get-color : -> Color
    (define/public (get-color)
      ALLY-COLOR)
    
    ; on-tick! : -> Void
    (define/public (on-tick!)
      (set! y (+ y v)))
    
    ; eliminated? : Coordinate Coordinate -> Boolean
    ; Returns if the target is dead or not
    (define/public (eliminated? mx my)
      (and
       (<= (- x (/ ALLY-SIDE 2)) mx (+ x (/ ALLY-SIDE 2)))
       (<= (- y (/ ALLY-SIDE 2)) my (+ y (/ ALLY-SIDE 2)))))
    
    ; touch-down? : -> Boolean
    ; Returns whether the unit touches the base
    (define/public (touch-down?)
      (>= (+ y (/ ALLY-SIDE 2)) (- HEIGHT base-height)))
    
    ; render : Image -> Image
    ; Renders curent unit
    (define/public (render scene)
      (place-image ALLY-IMG x y scene))
    
    ; get-del-height : Death-Type -> Natural
    ; Returns the height change according to the death type
    ; which could be either mouse click dead or touch base dead
    (define/public (get-del-height death-type)
      (cond
        [(symbol=? death-type CLICK-DEAD) ALLY-ELIMINATED]
        [(symbol=? death-type TOUCH-BASE) ALLY-REACH-BASE]))
       
    (super-new)))

(begin-for-test
  (check-equal?
   (posn-x (send TEST-ENEMY get-loc))
   50
   "Initial X 50")
  (check-equal?
   (posn-y (send TEST-ENEMY get-loc))
   0
   "Initial Y 0")
  (check-equal?
   (send TEST-ENEMY get-color)
   ENEMY-COLOR   
   "Enemy color")
  (check-equal?
   (send TEST-ENEMY render MT)
   (place-image ENEMY-IMG 50 0 MT)
   "Test render")
  (check-equal?
   (send TEST-ENEMY get-del-height CLICK-DEAD)
   8
   "Enemy eliminated")
  (check-equal?
   (send TEST-ENEMY get-del-height TOUCH-BASE)
   -8
   "Enemy reached base"))
(define Enemy%
  (class* object% (Unit<%> Subscriber<%>)
    (init-field x y v)
    (inspect false)
    ; x represents the x-coord of the centre of the ally unit
    ; y represents the y-coord of the centre of the ally unit
    ; v represents the velocity of the ally unit
    
    (define base-height 10)
    
    ; report-base-height! : Natural -> Void
    ; Reports a baseheight.
    ; EFFECT: Reports the base height to all subscribers
    (define/public (report-base-height! height)
      (set! base-height height))
    
    ; get-loc : -> posn
    ; Returns the location of this unit as a posn.
    (define/public (get-loc)
      (make-posn x y))
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    (define/public (get-color)
      ENEMY-COLOR)
    
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    (define/public (on-tick!)
      (set! y (+ y v)))
    
    ; eliminated? : Coordinate Coordinate -> Boolean
    ; Returns if the target is dead or not
    (define/public (eliminated? mx my)
      (<= (dist x mx y my) ENEMY-RADIUS)) 
    
    ; touch-down? : -> Boolean
    ; Returns whether the unit touches the base
    (define/public (touch-down?)
      (>= (+ y ENEMY-RADIUS) (- HEIGHT base-height)))
    
    ; render : Image -> Image
    ; Renders curent unit
    (define/public (render scene)
      (place-image ENEMY-IMG x y scene))
    
    ; get-del-height : Death-Type -> Natural
    ; Returns the height change according to the death type
    ; which could be either mouse click dead or touch base dead
    (define/public (get-del-height death-type)
      (cond
        [(symbol=? death-type CLICK-DEAD) ENEMY-ELIMINATED]
        [(symbol=? death-type TOUCH-BASE) ENEMY-REACH-BASE]))
    
    (super-new)))

(begin-for-test
  (check-equal?
   (posn-x (send TEST-MERC get-loc))
   50
   "Initial X 50")
  (check-equal?
   (posn-y (send TEST-MERC get-loc))
   0
   "Initial Y 0")
  
  (check-equal?
   (send TEST-MERC2 eliminated? 100 0)
   true
   "TEST-MERC2 by clicking in proximity")
  (check-equal?
   (send TEST-MERC2 render MT)
   (place-image ALLY-IMG 100 0 MT)
   "Ally image in the 0 ticks")
  (check-equal?
   (send TEST-MERC2 get-del-height TOUCH-BASE)
   12
   "Height change when ally touches base")
  (check-equal?
   (send TEST-MERC2 get-del-height CLICK-DEAD)
   -12
   "Height change when ally gets eliminated")
  
  
  (send TEST-MERC3 on-tick!)
  (send TEST-MERC3 on-tick!)
  (send TEST-MERC3 on-tick!)  
  (check-equal?
   (send TEST-MERC3 eliminated? 200 15)
   true
   "TEST-MERC3 by clicking in proximity")
  (check-equal?
   (send TEST-MERC3 render MT)
   (place-image ENEMY-IMG 200 15 MT)
   "Ally image in the 0 ticks")
  (check-equal?
   (send TEST-MERC3 get-del-height TOUCH-BASE)
   -12
   "Height change when enemy touches base")
  (check-equal?
   (send TEST-MERC3 get-del-height CLICK-DEAD)
   12
   "Height change when enemy gets eliminated")
  
  (check-equal?
   (send TEST-MERC get-color)
   ALLY-COLOR   
   "In the beginning, it should be an ally")
  
  (send TEST-MERC on-tick!)
  (check-equal?
   (send TEST-MERC get-color)
   ALLY-COLOR   
   "After 1 tick, it should still be an ally")
  
  (send TEST-MERC on-tick!)
  (check-equal?
   (send TEST-MERC get-color)
   ALLY-COLOR   
   "After 2 ticks, it should still be an ally")
  
  (send TEST-MERC on-tick!)
  (check-equal?
   (send TEST-MERC get-color)
   ENEMY-COLOR   
   "On 3rd ticks, it should be an enemy")
  
  (send TEST-MERC on-tick!)
  (check-equal?
   (send TEST-MERC get-color)
   ENEMY-COLOR   
   "On 4th ticks, it should still be an enemy")
  
  (send TEST-MERC on-tick!)
  (check-equal?
   (send TEST-MERC get-color)
   ENEMY-COLOR   
   "On 5th ticks, it should still be an enemy")
  
  (send TEST-MERC on-tick!)
  (check-equal?
   (send TEST-MERC get-color)
   ALLY-COLOR   
   "On 6th ticks, it should be an ally"))
(define Mercenary%
  (class* object% (Unit<%> Subscriber<%>)
    (init-field x y v [ticks 0] [enemy? #false])
    (inspect false)
    ; x represents the x-coord of the centre of the ally unit
    ; y represents the y-coord of the centre of the ally unit
    ; v represents the velocity of the ally unit
    
    (define base-height 10)
     
    ; report-base-height! : Natural -> Void
    ; Reports a baseheight.
    ; EFFECT: Reports the base height to all subscribers
    (define/public (report-base-height! height)
      (set! base-height height))
    
    ; get-loc : -> posn
    ; Returns the location of this unit as a posn.
    (define/public (get-loc)
      (make-posn x y))
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    (define/public (get-color)
      (if enemy?
          ENEMY-COLOR
          ALLY-COLOR))
    
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    (define/public (on-tick!)
      (set! y (+ y v))
      (set! ticks (add1 ticks))
      (if (zero? (modulo ticks MERC-SWITCH-SIDE-FREQ))
          (flip-emeny?)
          (void)))
    
    ; flip-enemy? : -> Void
    ; flips the enemy?.
    (define (flip-emeny?)
      (set! enemy? (if enemy? FALSE TRUE)))
    
    ; eliminated? : Coordinate Coordinate -> Boolean
    ; Returns if the target is dead or not
    (define/public (eliminated? mx my)
      (if enemy?
       ;(zero? (modulo ticks MERC-SWITCH-SIDE-FREQ))
          (<= (dist x mx y my) ENEMY-RADIUS)
          (and
           (<= (- x (/ ALLY-SIDE 2)) mx (+ x (/ ALLY-SIDE 2)))
           (<= (- y (/ ALLY-SIDE 2)) my (+ y (/ ALLY-SIDE 2))))))
    
    ; touch-down? : -> Boolean
    ; Returns whether the unit touches the base
    (define/public (touch-down?)
      (if enemy?
       ;(zero? (modulo ticks MERC-SWITCH-SIDE-FREQ))
          (>= (+ y ENEMY-RADIUS) (- HEIGHT base-height))
          (>= (+ y (/ ALLY-SIDE 2)) (- HEIGHT base-height))))
    
    ; render : Image -> Image
    ; Renders curent unit
    (define/public (render scene)
      (if enemy?
          (place-image ENEMY-IMG x y scene)
          (place-image ALLY-IMG x y scene)))
    
    ; get-del-height/impersonation : -> Natural
    ; Returns the height change according to the death type
    ; for the impersonation
    (define (get-del-height/impersonation)
      (if enemy?
          MERC-ENEMY-ELIMINATED
          MERC-ALLY-ELIMINATED))
    
    ; get-del-height : Death-Type -> Natural
    ; Returns the height change according to the death type
    ; which could be either mouse click dead or touch base dead
    (define/public (get-del-height death-type)
      (cond
        [(symbol=? death-type CLICK-DEAD) (get-del-height/impersonation)]
        [(symbol=? death-type TOUCH-BASE)
         (- 0 (get-del-height/impersonation))]))
    
    (super-new)))

; mk-world : Velocity Velocity Natural -> StatefulWorld<%>
; Creates a world with num-units initial Random-Units,
; where units have the specified min and max velocity.
; WHERE: minvel <= maxvel
; STRATEGY : Function Composition
(define (mk-world maxvel minvel num-units)
  (new World% [min-v minvel] [max-v maxvel] 
       [units (unit-factory num-units minvel maxvel)]))

; unit-factory : Natural Velocity Velocity -> ListOf<Unit>
; Randomly generates num-units number of units with a velocity
; between min-v and max-v
(begin-for-test
  (check-equal?
   (unit-factory 0 1 5)
   empty
   "No Random-Unit generated from the factory")
  (check-equal?
   (length (unit-factory 1 1 5))
   1
   "1 Random-Unit generated from the factory"))
; STRATEGY : Function Composition
(define (unit-factory num-units min-v max-v)
  (local
    ((define (unit-factory/a num-units min-v max-v res/a)
       (cond
         [(zero? num-units) res/a]
         [else
          (unit-factory/a (sub1 num-units) min-v max-v
                        (cons (gen-random-unit min-v max-v) res/a))])))
    ; -IN-
    ; unit-factory/a : Natural Velocity Velocity ListOf<Unit<%>> ->
    ;                                                            ListOf<Unit<%>>
    ; Uses an accumulator to generate num-units Random-Units
    ; STRATEGY : Data Decomposition on num-units : Natural
    (unit-factory/a num-units min-v max-v empty)))

; gen-random-unit : Velocity Velocity -> Unit<%>
; Returns a randomly generated Unit<%> object
; STRATEGY : Data Decomposition on rand : Natural
(define (gen-random-unit min-v max-v)
  (local
    ((define init-posn (make-posn (random (add1 WIDTH)) INITIAL-UNIT-HEIGHT))
     (define rand (random 3)))
  (cond
    [(= 0 rand)
     (mk-ally init-posn
              (+ min-v (abs (- (add1 (random (sub1 max-v))) min-v))))]
    [(= 1 rand)
     (mk-enemy init-posn
               (+ min-v (abs (- (add1 (random (sub1 max-v))) min-v))))]
    [(= 2 rand)
     (mk-merc init-posn
              (+ min-v (abs (- (add1 (random (sub1 max-v))) min-v))))])))

; mk-enemy : posn Velocity -> Enemy<%>
; Creates an enemy unit with the given parameters.
; STRATEGY : Function Composition
(define (mk-enemy p v)
  (new Enemy% [x (posn-x p)] [y (posn-y p)] [v v]))

; mk-ally : posn Velocity -> Ally<%>
; Creates an ally unit with the given parameters.
; STRATEGY : Function Composition
(define (mk-ally p v)
  (new Ally% [x (posn-x p)] [y (posn-y p)] [v v]))

; mk-merc : posn Velocity -> Mercenary<%>
; Creates a mercenary unit with the given parameters.
; STRATEGY : Function Composition
(define (mk-merc p v)
  (new Mercenary% [x (posn-x p)] [y (posn-y p)] [v v]))

;==================== dist ====================

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
 

;==================== big-bang ====================

; run : World -> World
; Connect to the given chat server with user name nam.
; EXAMPLE :
; STRATEGY: Function Composition
(define (run world)
  (big-bang world
            (on-tick (λ (w) (send w on-tick!) w))
            (on-mouse (λ (w x y me) (send w on-mouse! x y me) w))
            (to-draw (λ (w) (send w render MT)))
            (stop-when (λ (w) (send w get-game-over?)))))

(define INITIAL-WORLD (mk-world 5 1 2))

;==================== Tests ====================

; TEST-WORLD:
; max-v 5, min-v 1, number of units 10
(define TEST-WORLD (mk-world 5 1 10))
(define TEST-ALLY (mk-ally (make-posn 50 0) 5))
(define TEST-ENEMY (mk-enemy (make-posn 50 0) 5))
(define TEST-MERC (mk-merc (make-posn 50 0) 5))
(define TEST-MERC2 (mk-merc (make-posn 100 0) 5))
(define TEST-MERC3 (mk-merc (make-posn 200 0) 5))
(define GAME-OVER-ENEMY (mk-enemy (make-posn 250 (- 500 25)) 5))
(define GAME-OVER-WORLD (new World% [min-v 1] [max-v 5] 
                             [units (list GAME-OVER-ENEMY)] [base-height 12]))
(define GAME-OVER-ENEMY2 (mk-enemy (make-posn 250 (- 500 25)) 5))
(define GAME-OVER-WORLD2 (new World% [min-v 1] [max-v 5] 
                             [units (list GAME-OVER-ENEMY2)] [base-height 12]))



;==================== Alternate Designs ====================

; 1)
; A global mutable height class that both units and world have access to.
; When a unit dies or crosses the base line, it mutates the height field of
; the height object. The world then refers to the same object while rendering.
; Its a bad design because every thing has access to the height, which violates
; encapsulation of OOP design.

; 2)
; In the original design, we created the num-units of units in the world, where
; we passed this as a parameter in the world. So, in the world, we would have
; random generate ball methods. This is bad design since the world does not have
; to know how to create units. Therefore, we used the factory design pattern
; instead where we have the unit-factory and gen-random-unit functions
; outside the class. It's beneficial because now the original num-units of units
; will just be passed in to the world and every 4 ticks, when a new unit is
; generated, we can just call the gen-random-unit function.

; 3)
; The ticks in the mercenery can be pushed or pulled from the world which 
; already maintains the ticks past so far.