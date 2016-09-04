;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname walls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; importing Packages
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 45); Time taken to finish the task in hours

; providing the required functions
(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide mouse-handler)
(provide end?)
(provide get-balls)
(provide mk-ball)
(provide replace-balls)
(provide get-ball-x)
(provide get-ball-y)
(provide score)
(provide level)

; Physical Constants:
(define WIDTH 400); empty scene width in pixel
(define HEIGHT 400); empty scene height in pixel
(define CANVAS-AREA (* WIDTH HEIGHT)) ; are of canvas in pixel^2

; Rounding Constant
(define EPS 1) 
 
; Ball Constant
(define DIAMETER 40) ; pixels
(define RADIUS (/ DIAMETER 2)) ; pixels
(define BALL (circle RADIUS "solid" "blue")); worm segment
(define FONT-SIZE 16); Size of font in pixel
(define FONT-COLOR "Black"); Font color

; Graphical Constant:
(define MT (empty-scene WIDTH HEIGHT)); Empty Scene
(define BALL-TOUCHING-FLOOR (- HEIGHT RADIUS)) ; pixels
(define BALL_TOUCHING-CIELING RADIUS) ; pixels
(define BALL-TOUCHING-LEFT-WALL RADIUS) ; pixels
(define BALL-TOUCHING-RIGHT-WALL (- WIDTH RADIUS)) ; pixels

; Random Function Limits
(define LIMIT 400) ;pixels
(define VEL-LIMIT 10) ; pixels
(define ONE 1) ; pixel

; World Constants : 
(define PRESENT #true) ; World-awf
(define NOT-PRESENT #false) ; World-awf
(define ZERO 0) ; Initial-Score
(define GOAL-LIMIT 90) ; limit to goal in %

; Wall Constants : 
(define MINOR-AXIS 4) ; pixels
(define INITIAL-MAJOR 2) ; pixels
(define WALL-H (rectangle 4 400 "solid" "brown"))
(define WALL-V (rectangle 400 4 "solid" "brown"))

; Orientation Constants : 
(define HORIZONTAL #true) ; Wall-or
(define VERTICAL #false) ; Wall-or
(define ACTIVE-WALL-VERTICAL-HELP (text "Click to create VERTICAL wall" 
                               FONT-SIZE FONT-COLOR))
(define ACTIVE-WALL-HORIZONTAL-HELP (text "Click to create HORIZONTAL wall" 
                               FONT-SIZE FONT-COLOR))

; A HorizontalVelocity is a Real
; WHERE : HorizontalVelocity vx is one of
; - [1,10] ; pixels/tick 
; - [-1,-10] ; pixels/tick
; INTERP : positive value represents that the direction of movement is
; towards the right,i.e. increasing direction along x-axis, while negative 
; direction repressents towards the left boundary, i.e. decreasing direction 
; along x-axis.

; A VerticalVelocity is a Real
; WHERE : VerticalVelocity vy is one of
; - [1,10] ; pixels/tick 
; - [-1,-10] ;  pixels/tick
; INTERP : positive value represents that the direction of movement is
; towards the floor,i.e. increasing direction along y-axis, while negative 
; direction repressents towards the cieling boundary, i.e. decreasing direction
; along y-axis.

; A FloorBoundary is a Coordinate
; WHERE : FloorBoundary fb is in 
; (BALL-TOUCHING-CIELING, BALL-TOUCHING-FLOOR] pixels
; INTERP : this represents the floor boundary of a ball, trying to pass this 
; boundary would make the ball reverse its direction, i.e towards cieling

; A CielingBoundary is a Coordinate
; WHERE : CielingBoundary ub is in 
; [BALL-TOUCHING-CIELING, BALL-TOUCHING-FLOOR) pixels
; INTERP : this represents the cieling boundary of a ball,trying to pass this 
; boundary would make the ball reverse its direction, i.e towards floor.

; A RightBoundary is a Coordinate
; WHERE : RightBoundary rb can be in 
; (BALL-TOUCHING-LEFT-WALL, BALL-TOUCHING-RIGHT-WALL]
; INTERP : this represents the right boundary of a ball, trying to pass this
; boundary would make the ball reverse its direction, i.e towards left

; A LeftBoundary is a Coordinate
; WHERE : LeftBoundary lb can be in 
; [BALL-TOUCHING-LEFT-WALL, BALL-TOUCHING-RIGHT-WALL)
; INTERP : this represents the left boundary of a ball, trying to pass this
; boundary would make the ball reverse its direction, i.e towards right

; A Ball is a (make-ball Coordinate 
;                        Cordinate 
;                        HorizontalVelocity 
;                        VerticalVelocity 
;                        FloorBoundary 
;                        CielingBoundary 
;                        RightBoundary 
;                        LeftBoundary)
; INTERP : (make-ball x y vx vy fb ub rb lb)
; x represents the x Coordinate of the centre of the Ball
; y represents the y Coordinate of the centre of the Ball
; vx represents the HorizontalVelocity of the Ball
; vy represents the VerticalVelocity of the Ball
; fb represents the FloorBoundary of the Ball,
; against which the ball would bounce.
; ub represents the CielingBoundary of the Ball,
; against which the ball would bounce.
; rb represents the RightBoundary of the Ball,
; against which the ball would bounce.
; lb represents the LowerBoundary of the Ball, 
; against which the ball would bounce.

(define-struct ball[x y vx vy fb ub rb lb])

;TEMPLATE : 
(define (ball-fn b)
  (...(ball-x b)...(ball-y b)...(ball-vx b)...(ball-vy b)
      ...(ball-fb b)...(ball-ub b)...(ball-rb b)...(ball-lb b)...))

(define INITIAL-BALL (make-ball 50 50 3 3 
                                BALL-TOUCHING-FLOOR 
                                BALL_TOUCHING-CIELING 
                                BALL-TOUCHING-RIGHT-WALL 
                                BALL-TOUCHING-LEFT-WALL ))

(define SAMPLE-BALL (make-ball 100 50 3 7 
                                BALL-TOUCHING-FLOOR 
                                BALL_TOUCHING-CIELING 
                                BALL-TOUCHING-RIGHT-WALL 
                                BALL-TOUCHING-LEFT-WALL ))

; An Orientation is a Boolean
; WHERE : Orientation can be on of
; - HORIZONTAL (#true)
; - VERTICAL (#false)
; INTERP : Orientation represents whether a Wall is VERTICAL or HORIZONTAL 
; on the canvas. 

; A HigherBoundary is a Coordinate
; WHERE : HigherBoundary is in (0, 400]
; INTERP : HigherBoundary represents the highest Coordinate of the 
; major-axis of a Wall.

; A LowerBoundary is a Coordinate
; WHERE : LowerBoundary is in [0, 400)
; INTERP : LowerBoundary represents the lowest Coordinate of the 
; major-axis of a Wall.

; A Wall is a (make-wall Posn Boolean LowerBoundary HigherBoundary)
; INTERP : (make-wall p or lb hb)
; p represents the Posn of the centre of the wall
; or represents the orientation of the wall, it can be 
; - HORIZONTAL (#t)
; - VERTICAL (#f)
; lb represents the lower boundary of the active wall, i.e. if the orientation
; of the ActiveWall is HORIZONTAL the lb will give the left boundary till which
; the ActiveWall will grow.
; hb represents the higher boundary of the active wall, i.e. if the orientation
; of the ActiveWall is HORIZONTAL the hb will give the right boundary tillwhich
; the ActiveWall will grow.

(define-struct wall [p or lb hb])

;TEMPLATE : 
(define (wall-fn w)
  (...(wall-p w)...(wall-or w)...(wall-lb w)...(wall-hb w)...))

(define SAMPLE-WALL (make-wall (make-posn 50 50) #true 0 400))

; A Major is a PosReal
; WHERE : Major m is in (0,400]
; INTERP : Major represents the length of the major-axis of an ActiveWall.

; A ActiveWall is a (make-activewall Posn Major Boolean 
;                                    LowerBoundary HigherBoundary)
; INTERP : (make-activewall p major or lb hb)
; p represents the position of the ActiveWall along which it will grow, either 
; Horizontal or Vertical.
; major represents the length of the ActiveWall along the major axis.   
; or represents the orientation of the active wall, it can be 
; - HORIZONTAL (#t)
; - VERTICAL (#f)
; lb represents the lower boundary of the active wall, i.e. if the orientation
; of the ActiveWall is HORIZONTAL the lb will give the left boundary till which
; the ActiveWall will grow.
; hb represents the higher boundary of the active wall, i.e. if the orientation
; of the ActiveWall is HORIZONTAL the hb will give the right boundary tillwhich
; the ActiveWall will grow.

(define-struct activewall [p major or lb hb])

;TEMPLATE : 
(define (activewall-fn w)
  (...(activewall-p w)...(activewall-major w)...(activewall-or w)...
      (activewall-lb w)...(activewall-hb w)...))

(define INITIAL-ACTIVEWALL (make-activewall 
                            (make-posn 0 0) INITIAL-MAJOR HORIZONTAL 0 0))

; A AWFlag is a Boolean
; WHERE : awf is one of
; -PRESENT (#true)
; -NOT-PRESENT (#false)
; INTERP : is set to true if there exists a wall that is active in the world
; else set to false.

; A Score is a PosReal
; WHERE : Score can be [0,100]
; INTERP : It represents the percentage of the area that has been acquired by 
; the non active walls, ie the area in the canvas that no Ball can enter.

; A World is a (make-world NEListOf<Ball> ListOf<Wall> ActiveWall AWFlag Score)
; INTERP : (make-world nelob low aw awf score)
; nelob represents the Non Empty list of Ball i.e. all the balls that are 
; present in the World at the moment.
; low represents the list of all the user made walls that were in some point 
; of time were active.
; aw represents the wall that is currently active in the world.
; awf is a flag that tells that if there is a wall that is active in the 
; current world state.
; score is the percentage of area that has been acquired by the Walls, ie area 
; that is unreachable by the Ball.

(define-struct world [nelob low aw awf score])

;TEMPLATE : 
(define (world-fn w)
  (...(world-nelob w)...(world-low w)...(world-aw w)...
      (world-awf w)...(world-awf w)...))

(define INITIAL-WORLD 
  (make-world (list INITIAL-BALL) empty 
              INITIAL-ACTIVEWALL NOT-PRESENT ZERO))
 
; run : World -> World
; runs a simulation of the balls and walls game.Takes in a World and produces 
; a new World based on the KeyEvent, MouseEvent or Tick.
; STRATEGY :  Function Composition
(define (run w)
  (big-bang w
            (on-tick next-world)
            (on-key key-handler)
            (on-mouse mouse-handler)
            (on-draw render)
            (stop-when end? render-last)))

; next-world : World -> World
; consumes a World and produces a next state of the World after a tick has 
; occured depending on current state of the World
(begin-for-test (check-equal? (next-world 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))
                              (make-world
                               (list
                                (make-ball 238 372 3 7 380 45 380 20)
                                (make-ball 188 188 3 3 380 45 380 20))
                               (list
                                (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall
                                (make-posn 253 288) 178 true 0 400) true 45)
                              "Inital world status given with score yet to 
                                 reach the goal as input"))
;(begin-for-test (chec-equal? (next-world 
;                               (make-world
;                                (list
;                                 (make-ball 235 365 3 7 380 45 380 20)
;                                 (make-ball 185 185 3 3 380 45 380 20))
;                               (list (make-wall (make-posn 201 25) true 0 400))
;                                (make-activewall (make-posn 253 288) 
;                                                 162 true 0 400) true 70))))
;                              (make-world
;                               (list (make-ball 220 300 2 2 380 20 380 20) 
;                                     (make-ball 60 380 1 3 380 20 380 20) 
;                                     (make-ball 260 100 3 6 380 20 380 20))
;                               empty
;                               (make-activewall (make-posn 0 0) 2 true 0 0)
;                               false
;                               0)
;                              "Inital world status given with score yet to 
;                                 reach the goal as input"))
(begin-for-test (world? (next-world 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 70))))
; STRATEGY : Data Decomposition on w : World
(define (next-world w)
  (if (>= (world-score w) (get-goal (world-nelob w)))
      (reset-world w)
      (normal-next-world w)))

; reset-world : World -> World
; resets the current World by adding a Ball to the ListOf<Ball> and randomizing
; the positions and the velocity of all the Ball, removes all the Walls present
; in ListOf<Wall> and resets the score to zero.
;(begin-for-test (check-equal? (reset-world 
;                               (make-world
;                                (list
;                                 (make-ball 235 365 3 7 380 45 380 20)
;                                 (make-ball 185 185 3 3 380 45 380 20))
;                               (list (make-wall (make-posn 201 25) true 0 400))
;                                (make-activewall (make-posn 253 288) 
;                                                 162 true 0 400) true 70))
;                              (make-world
;                               (list (make-ball 220 300 2 2 380 20 380 20) 
;                                     (make-ball 60 380 1 3 380 20 380 20) 
;                                     (make-ball 260 100 3 6 380 20 380 20))
;                               empty
;                               (make-activewall (make-posn 0 0) 2 true 0 0)
;                               false
;                               0)
;                              "Inital world status given with score yet to 
;                                 reach the goal as input"))

; STRATEGY : Data Decomposition on World. 
(define (reset-world w)
  (make-world (add-new-ball (world-nelob w)) empty 
              INITIAL-ACTIVEWALL NOT-PRESENT ZERO))

; add-new-ball : NEListOf<Ball> -> NEListOf<Ball>
; returns a list with one Ball added and randomizing all the positions and 
; velocities of the consisting Balls.
;(begin-for-test (check-equal? (add-new-ball 
;                               (list
;                                (make-ball 235 365 3 7 380 45 380 20)
;                                (make-ball 185 185 3 3 380 45 380 20)))
;                              (list (make-ball 140 60 8 2 380 20 380 20) 
;                                    (make-ball 100 300 1 6 380 20 380 20) 
;                                    (make-ball 380 180 3 7 380 20 380 20))
;                              "input with two Ball in the list given"))
; STRATEGY : Function Composition 
(define (add-new-ball nelob)
  (map randomize-ball (cons INITIAL-BALL nelob)))

; randomize-ball Ball -> Ball
; returns a Ball with all the parameters reset and all the Coordinates and 
; Velocity randomly calculated.
;(begin-for-test (check-equal? (randomize-ball 
;                                (make-ball 235 365 3 7 380 45 380 20))
;                                (make-ball 380 180 3 7 380 20 380 20)
;                              "input Ball given and returns all the 
;                                parameters reset"))
; STRATEGY : Function Composition  
(define (randomize-ball b)
  (make-ball
   (random-function LIMIT DIAMETER RADIUS)
   (random-function LIMIT DIAMETER RADIUS)
   (random-function VEL-LIMIT ONE ONE)
   (random-function VEL-LIMIT ONE ONE)
   BALL-TOUCHING-FLOOR 
   BALL_TOUCHING-CIELING 
   BALL-TOUCHING-RIGHT-WALL 
   BALL-TOUCHING-LEFT-WALL ))
                      
; random-function : Real Real Real -> Real
; generates a random number within given range and at given interval
;(begin-for-test (check-equal? (random-function 400 40 20)
;                                20
;                              "input range and interval given as 400 & 20"))
; STRATEGY : Function Composition 
(define (random-function width interval offset)
   (+ offset (* interval (random (quotient width interval)))))


; normal-next-world World -> World
; consumes a World and produces a next World at the fall of a tick, either with
; an ActiveWall or not.
(begin-for-test (check-equal? (normal-next-world 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))
                              (make-world
                               (list (make-ball 238 372 3 7 380 45 380 20) 
                                     (make-ball 188 188 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 253 288) 
                                                178 true 0 400)
                               true
                               45)
                              "Inital world status given with ActiveWall 
                                present"))
(begin-for-test (check-equal? (normal-next-world 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) false 45))
                              (make-world
                               (list (make-ball 238 372 3 7 380 45 380 20) 
                                     (make-ball 188 188 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 253 288) 
                                                162 true 0 400) true 6)
                              "Inital world status given with ActiveWall 
                                not present"))
; STRATEGY : Data Decomposition on w : World
(define (normal-next-world w)
  (if (world-awf w)
      (make-world (next-balls w) 
                  (next-low w) 
                  (next-activewall (world-aw w))
                  (update-active-wall-status (world-aw w))
                  (world-score w))
      (make-world (next-balls w) 
                  (world-low w) 
                  (world-aw w)  
                  (update-active-wall-status (world-aw w))
                  (update-score w))))

; update-active-wall-status : ActiveWall -> ActiveWallFlag
; consumes an ActiveWall and returns if the ActiveWall is not longer active
(begin-for-test (check-equal? (update-active-wall-status 
                               (make-activewall (make-posn 253 288) 
                                                 162 true 0 400))
                              PRESENT
                              "the input ActiveWall has been completed"))
(begin-for-test (check-equal? (update-active-wall-status 
                               (make-activewall (make-posn 253 288) 
                                                 162 true 0 150))
                              NOT-PRESENT
                              "the input ActiveWall has been completed"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (update-active-wall-status aw)
  (if (active-wall-completed? aw) NOT-PRESENT PRESENT))

; next-low : World -> ListOf<Wall> 
; returns the updated list of walls if the current ActiveWall completes
(begin-for-test (check-equal? (next-low 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) true 40))
                              (list (make-wall (make-posn 201 25) true 0 400))
                              "the input World with not completed ActiveWall 
                                                      has been completed"))
(begin-for-test (check-equal? (next-low 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 150) true 40))
                              (list (make-wall (make-posn 81 288) true 0 150) 
                                    (make-wall (make-posn 201 25) true 0 400))
                              "the input World with completed ActiveWall 
                                                      has been completed"))
; STRATEGY : Data Decompoition on w : World
(define (next-low w)
  (if (active-wall-completed? (world-aw w)) 
      (add-active-wall (world-aw w) (world-low w)) (world-low w)))

; add-active-wall : ActiveWall ListOf<Wall> -> ListOf<Wall>  
; adds ActiveWall to the ListOf<Wall>
(begin-for-test (check-equal? (add-active-wall 
                           (make-activewall (make-posn 253 288) 162 true 0 150)
                               (list (make-wall (make-posn 201 25) true 0 400)))
                               (list (make-wall (make-posn 81 288) true 0 150)
                                     (make-wall (make-posn 201 25) true 0 400))
                              "the input ActiveWall is added to ListOf<Wall>"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (add-active-wall aw low)
  (append 
   (list (make-wall (make-new-posn (activewall-p aw) 
                                   (activewall-or aw) 
                                   (activewall-major aw)
                                   (activewall-lb aw)) 
                    (activewall-or aw)
                    (activewall-lb aw) 
                    (activewall-hb aw))) low))

; make-new-posn : Posn Orientation Major LowerBoundary -> Posn
; returns updated Posn of the completed activewall
(begin-for-test (check-equal? (make-new-posn (make-posn 253 288) true 162 0) 
                               (make-posn 81 288)
                              "the horizontal ActiveWall given as input"))
(begin-for-test (check-equal? (make-new-posn (make-posn 253 288) false 162 0) 
                               (make-posn 253 81)
                              "the vertical ActiveWall given as input"))
; STRATEGY : Data Decomposition on p : Posn
(define (make-new-posn p ornt major lb)
  (if ornt (make-posn (+ lb (/ major 2)) (posn-y p))
         (make-posn (posn-x p) (+ lb (/ major 2)))))

; active-wall-completed? : ActiveWall -> AWFlag
; returns true if the activewall-major is greater than the difference of 
; LowerBoundary and HigherBoundary
(begin-for-test (check-equal? (active-wall-completed? 
                               (make-activewall 
                                (make-posn 253 288) 162 true 0 150)) 
                               PRESENT
                              "an ActiveWall that has not been 
                               completed is given as output"))
(begin-for-test (check-equal? (active-wall-completed? 
                               (make-activewall 
                                (make-posn 253 288) 162 true 0 400)) 
                               NOT-PRESENT
                              "an ActiveWall that has been 
                               completed is given as output"))
; STRATEGY : Data Decomposition
(define (active-wall-completed? aw)
  (>= (activewall-major aw) (- (activewall-hb aw) (activewall-lb aw))))

; next-activewall : ActiveWall -> ActiveWall
; returns a new state of ActiveWall that is a 16 pixels larger at major-axis 
; than then the current one.
(begin-for-test (check-equal? (next-activewall 
                               (make-activewall 
                                (make-posn 253 288) 162 true 0 150)) 
                               (make-activewall (make-posn 253 288) 
                                                150 true 0 150)
                              "an ActiveWall that has not been 
                               completed is given as output"))
(begin-for-test (check-equal? (next-activewall 
                               (make-activewall 
                                (make-posn 253 288) 162 true 0 400)) 
                               (make-activewall (make-posn 253 288) 
                                                178 true 0 400)
                              "an ActiveWall that has been 
                               completed is given as output"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (next-activewall aw)
  (if (active-wall-completed? aw)
      (make-activewall (activewall-p aw) 
                   (- (activewall-hb aw) (activewall-lb aw)) 
                   (activewall-or aw)
                   (activewall-lb aw)
                   (activewall-hb aw))
      (make-activewall (activewall-p aw) 
                   (+ 16 (activewall-major aw)) 
                   (activewall-or aw)
                   (activewall-lb aw)
                   (activewall-hb aw))))

; next-balls : World -> NEListOfBall
; consumes a World  and produces a new NEListOf<Ball>  which has all  
; the Ball with new positions according to the HorizontalVelocity 
; & VerticalVelocity.
(begin-for-test (check-equal? (next-balls 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) true 40))
                              (list (make-ball 238 372 3 7 380 45 380 20) 
                                    (make-ball 188 188 3 3 380 45 380 20))
                              "the input World with not completed ActiveWall 
                                                      has been completed"))
(begin-for-test (check-equal? (next-balls 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 150) true 40))
                              (list (make-ball 235 365 3 7 380 308 380 20) 
                                    (make-ball 185 185 3 3 268 45 380 20))
                              "the input World with completed ActiveWall 
                                                      has been completed"))
; STRATEGY : Data Decomposition on w : World
(define (next-balls w)
  (if (and (active-wall-completed? (world-aw w)) (world-awf w))
      (reset-boundary-in-list (world-nelob w) (world-aw w))
      (map new-position (world-nelob w))))


; reset-boundary-in-list : NEListOf<Ball> ActiveWall -> NEListOf<Ball>
; resets the boundaries of each Ball in NEListOf<Ball> in order to accomodate 
; the new Wall that was an ActiveWall a tick before.
(begin-for-test (check-equal? (reset-boundary-in-list 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               (make-activewall (make-posn 253 288) 162 
                                                 true 0 150))
                              (list (make-ball 235 365 3 7 380 308 380 20) 
                                    (make-ball 185 185 3 3 268 45 380 20))
                              "the input NEListOf<Ball> with completed 
                                         ActiveWall has been completed"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (reset-boundary-in-list nelob aw)
  (reset-boundary nelob aw))

; get-activewall-x : Posn -> Coordinate
; returns the x-Coordinate of the ActiveWall given
(begin-for-test (check-equal? (get-activewall-x 
                               (make-posn 253 288))
                              253
                              "the input posn is passed"))
; STRATEGY : Data Decomposition on p : Posn
(define (get-activewall-x p)
  (posn-x p))

; get-activewall-y : Posn -> Coordinate
; returns the y-Coordinate of the ActiveWall given
(begin-for-test (check-equal? (get-activewall-y 
                               (make-posn 253 288))
                              288
                              "the input posn is passed"))
; STRATEGY : Data Decomposition on p : Posn
(define (get-activewall-y p)
  (posn-y p))

; new-position : Ball -> Ball
; consumes a Ball and produces the new position of the Ball on the canvas
; depending on the current velocity and position of the Ball.
(begin-for-test (check-equal? (new-position 
                               (make-ball 185 185 3 3 380 45 380 20))
                              (make-ball 188 188 3 3 380 45 380 20)
                              "the input Ball is given whose posiiton is being 
                                        changed"))
; STRATEGY : Data Decomposition on b : Ball
(define (new-position b)
  (make-ball (new-x-position b) (new-y-position b) 
             (new-vx-velocity b) (new-vy-velocity b) 
                 (ball-fb b) (ball-ub b) (ball-rb b) (ball-lb b)))

; new-x-position : Ball -> Coordinate
; consumes a Ball and returns the new x-Coordinate of the Ball,
; depending on the HorizontalVelocity and Boundaries of the Ball.
(begin-for-test (check-equal? (new-x-position 
                               (make-ball 185 185 3 3 380 45 380 20))
                              188 
                              "the input Ball is given whose x-Coordinate is
                                        being changed"))
; STRATEGY : Data Decomposition in b : Ball
(define (new-x-position b)
  (check-lower-boundary (ball-x b) (ball-vx b) (ball-lb b) (ball-rb b)))

; new-y-position : Ball -> Coordinate
; consumes a Ball and returns the new y-Coordinate of the Ball,
; depending on the VerticalVelocity and Boundaries of the Ball.
(begin-for-test (check-equal? (new-y-position 
                               (make-ball 185 185 3 3 380 45 380 20))
                              188 
                              "the input Ball is given whose y-Coordinate is  
                                        being changed"))
; STRATEGY : Data Decomposition in b : Ball
(define (new-y-position b)
  (check-lower-boundary (ball-y b) (ball-vy b) (ball-ub b)(ball-fb b)))

; check-lower-boundary : Coordinate X Coordinate Coordinate -> Coordinate
; consumes a Coordinate and Velocity and LowerBoundary and HigherBoundary 
; returns the new Coordinate after checking if the ball is going past the 
; lower or higher boundary the Ball Cordinate is set to the boundary.
(begin-for-test (check-equal? (check-lower-boundary 
                               185 3 45 186)
                              186 
                              "the input values are given such that the 
                            resultant Coordinate crosses the higher boundary"))
(begin-for-test (check-equal? (check-lower-boundary 
                               46 -3 45 186)
                              45 
                              "the input values are given such that the 
                            resultant Coordinate crosses the lower boundary"))
; STRATEGY : Function Composition
(define (check-lower-boundary z vz lb hb)
  (local (; Coordinate X Coordinate -> Coordinate
          ; checks if the resultant Coordinate after adding velocity is 
          ; greater than higher boundary of the wall
          (define (check-higher-boundary z vz hb)
           (if (> (+ z vz) hb) hb (+ z vz))))
    ;-IN-
   (if (< (+ z vz) lb) lb
      (check-higher-boundary z vz hb))))
                                                                       
; new-vx-velocity : Ball -> HorizontalVelocity
; consumes a Ball and reverses the sign of the current 
; HorizontalVelocity if Ball bounces off Left or Right Boundaries 
; else keeps it same.
(begin-for-test (check-equal? (new-vx-velocity 
                               (make-ball 185 185 3 3 380 45 380 20))
                              3 
                              "the input Ball such that there no change in 
                               direction hence no sign change"))
(begin-for-test (check-equal? (new-vx-velocity 
                               (make-ball 185 185 3 3 186 45 380 20))
                              3 
                              "the input Ball such that there no change in 
                               direction hence no sign change"))
(begin-for-test (check-equal? (new-vx-velocity 
                               (make-ball 185 185 3 3 186 45 186 20))
                              -3 
                              "the input Ball such that there is a change in 
                               direction hence in sign as well"))
; STRATEGY : Data Decomposition on b :Ball
(define (new-vx-velocity b)
  (sign-change(ball-x b) (ball-vx b) (ball-lb b) (ball-rb b)))

; new-vy-velocity : Ball -> VerticalVelocity
; consumes a Ball and reverses the sign of the current 
; VerticalVelocity if Ball bounces off LowerBoundarys or UpperBoundary else 
; keeps it same.
(begin-for-test (check-equal? (new-vy-velocity 
                               (make-ball 185 185 3 3 380 45 380 20))
                              3 
                              "the input Ball such that there no change in 
                               direction hence no sign change of velocity"))
(begin-for-test (check-equal? (new-vy-velocity 
                               (make-ball 185 185 3 3 186 45 186 20))
                              -3 
                              "the input Ball such that there is a change in 
                               direction hence in sign as well of velocity"))
; STRATEGY : Data Decomposition on b :Ball
(define (new-vy-velocity b)
  (sign-change(ball-y b) (ball-vy b) (ball-ub b) (ball-fb b)))

; sign-change : Coordinate X Coordinate Coordinate -> Y
; changes the sign of the velocity if the Ball has to bounce.
(begin-for-test (check-equal? (sign-change 185 3 45 380)
                              3 
                              "the input value is given such that the result 
                               has no effect"))
; STRATEGY : Function Composition 
(define (sign-change z zv lb hb)
  (if (or 
       (< (+ z zv) lb)
       (> (+ z zv) hb))
      (* -1 zv)
      zv))

; key-handler : World -> World
; takes a World and returns new World with changes according to the KeyEvent;
; here it switches the click controller from making a horizontally orieanted 
; wall to vertical and vice versa on pressing a space.
(begin-for-test (check-equal? (key-handler 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) true 40) " ")
                              (make-world
                               (list (make-ball 235 365 3 7 380 45 380 20) 
                                     (make-ball 185 185 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 253 288) 
                                                162 true 0 400) true 40)
                              "the input World with space bar pressed"))
(begin-for-test (check-equal? (key-handler 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) true 40) "e")
                              (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) true 40)
                              "the input World with key other than
                                                space bar pressed"))
; STRATEGY : Data Decomposition on ke : KeyEvent
(define (key-handler w ke)
  (cond
    [(string=? " " ke) (space-bar-handler w)]
    [else w]))

; space-bar-handler : World -> World
; consumes a World and returns a World that has switched active wall 
; orientation
(begin-for-test (check-equal? (space-bar-handler 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) true 40))
                              (make-world
                               (list (make-ball 235 365 3 7 380 45 380 20) 
                                     (make-ball 185 185 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 253 288) 
                                                162 true 0 400) true 40)
                             "the input World such that ActiveWall is PRESENT"))
(begin-for-test (check-equal? (space-bar-handler 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40))
                              (make-world
                               (list (make-ball 235 365 3 7 380 45 380 20) 
                                     (make-ball 185 185 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 253 288) 
                                                162 false 0 400) false 40)
                             "the input World such that ActiveWall 
                                                         is NOT-PRESENT"))
; STRATEGY : Data Decomposition on w :World
(define (space-bar-handler w)
  (if (not (world-awf w))
   (make-world (world-nelob w) (world-low w) 
              (switch-key-control(world-aw w)) (world-awf w) (world-score w))
   w))
  
; switch-key-control : ActiveWall -> ActiveWall
; switches the click control from making horizontal active wall to 
; vertical active wall and vice versa.
(begin-for-test (check-equal? (switch-key-control 
                               (make-activewall (make-posn 253 288) 162 
                                                 true 0 400))
                              (make-activewall (make-posn 253 288) 162 
                                                 false 0 400)
                             "the input World such that ActiveWall 
                                                         is PRESENT"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (switch-key-control aw )
  (make-activewall (activewall-p aw) (activewall-major aw) 
                   (not (activewall-or aw)) (activewall-lb aw)  
                   (activewall-hb aw)))

; mouse-handler : World Coordinate Coordinate MouseEvent -> World
; consumes a World and makes a new World depending on the MouseEvent occured.
(begin-for-test (check-equal? (mouse-handler 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40) 
                               50 50 "button-down")
                              (make-world
                               (list (make-ball 235 365 3 7 380 45 380 20) 
                                     (make-ball 185 185 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 50 50) 2 true 0 400)
                               true
                               40)
                             "the input World such that ActiveWall is 
                              NOT-PRESENT and button-down occurs at (50,50)"))
(begin-for-test (check-equal? (mouse-handler 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40) 
                               50 50 "button-up")
                              (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40)
                             "the input World such that ActiveWall is 
                              NOT-PRESENT and button-up occurs at (50,50)"))
; STRATEGY : Data Decomposition on w : World
(define (mouse-handler w x y me)
  (cond
    [(string=? "button-down" me) (instantiate-active-wall w x y)]
    [else w])) 

; instantiate-active-wall : World Coordinate Coordinate -> World
; it takes a World and mouse click Coordinates and produces a next state of 
; World which has an active wall in it.
(begin-for-test (check-equal? (instantiate-active-wall 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40) 
                               50 50 )
                              (make-world
                               (list (make-ball 235 365 3 7 380 45 380 20) 
                                     (make-ball 185 185 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 50 50) 2 true 0 400)
                               true
                               40)
                             "the input World such that the button-down occurs 
                                  at (50,50) which is inside active area"))
(begin-for-test (check-equal? (instantiate-active-wall 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40) 
                               1 1 )
                              (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40)
                             "the input World such that the button-down occurs 
                                  at (1,1) which is outside active area"))
; STRATEGY : Data Decomposition on w : World
(define (instantiate-active-wall w x y)
  (if (and (click-in-active-area? (world-nelob w) x y ) (not (world-awf w)))
   (initiate-world-with-activewall w x y) w))

; initiate-world-with-activewall : World Coordinate Coordinate -> World
; returns a World which has ActiveWall PRESENT in it.
(begin-for-test (check-equal? (initiate-world-with-activewall 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40) 
                               50 50 )
                              (make-world
                               (list (make-ball 235 365 3 7 380 45 380 20) 
                                     (make-ball 185 185 3 3 380 45 380 20))
                               (list (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall (make-posn 50 50) 2 true 0 400)
                               true
                               40)
                             "the input World such that the button-down occurs 
                                  at (50,50) which is inside active area and 
                                     this will instantiate the ActiveWall"))
; STRATEGY : Data Decomposition on w : World
(define (initiate-world-with-activewall w x y)
  (make-world (world-nelob w) (world-low w)
   (make-new-activewall (world-aw w) x y (world-nelob w)) 
   (not(world-awf w)) (world-score w)))

; reset-boundary : NEListOf<Ball>  ActiveWall -> NEListOf<Ball>
; resets the boundary of each Ball in the NEListOf<Ball> to the new one  
; depending on the position of the ActiveWall origin.
(begin-for-test (check-equal? (reset-boundary 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               (make-activewall (make-posn 50 50) 2 true 0 400))
                              (list (make-ball 235 365 3 7 380 70 380 20) 
                                    (make-ball 185 185 3 3 380 70 380 20))
                          "the input ActiveWall is such that it is Horizontal"))
(begin-for-test (check-equal? (reset-boundary 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                              (make-activewall (make-posn 50 50) 2 false 0 400))
                              (list (make-ball 235 365 3 7 380 45 380 70) 
                                    (make-ball 185 185 3 3 380 45 380 70))
                           "the input ActiveWall is such that it is Vertical"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (reset-boundary nelob aw)
  (if (activewall-or aw)
      (reset-x-boundary reset-ver-boundary-for-ball posn-y 
                          nelob (activewall-p aw)) 
      (reset-x-boundary reset-hor-boundary-for-ball posn-x 
                          nelob (activewall-p aw))))

; reset-x-boundary :  [Ball Coordinate -> Ball]
;                     [Posn -> Coordinate ]
;                     NEListOf<Ball> Posn -> NEListOfBall
; resets the vertical boundaries of all the Ball in the list.
(begin-for-test (check-equal? (reset-x-boundary
                               reset-ver-boundary-for-ball
                               posn-y
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               (make-posn 50 50))
                              (list
                               (make-ball 235 365 3 7 380 70 380 20)
                               (make-ball 185 185 3 3 380 70 380 20))
                          "the input ActiveWall is such that it is horizontal 
                            and activewall origin is inside ball section"))
(begin-for-test (check-equal? (reset-x-boundary
                               reset-hor-boundary-for-ball
                               posn-x
                               (list
                                 (make-ball 235 365 3 7 100 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               (make-posn 350 350))
                              (list
                               (make-ball 235 365 3 7 100 45 380 20)
                               (make-ball 185 185 3 3 380 45 330 20))
                          "the input ActiveWall is such that it is horizontaland
                             activewall origin is outside ball section"))
; STRATEGY : Function Composition
(define (reset-x-boundary f1 f2 nelob p)
  (local (; Ball -> Ball
          ; returns a Ball with its CielingBoundary & FloorBoundary 
          ; reset accordingly.
          ; STRATEGY : Data Decomposition on b : Ball
          (define (compare-click-boundaries b)
           (if(and (check-click-coord-inside (ball-lb b) (ball-rb b) (posn-x p))
                  (check-click-coord-inside (ball-ub b) (ball-fb b) (posn-y p)))
               (f1 b (f2 p)) b)))
          (map compare-click-boundaries nelob)))


; reset-hor-boundary-for-ball : Ball Coordinate -> Ball
; returns a Ball with updated horizontal boundaries depending on the 
; position of the click.
(begin-for-test (check-equal? (reset-hor-boundary-for-ball 
                               (make-ball 185 185 3 3 380 45 380 20) 50)
                              (make-ball 185 185 3 3 380 45 380 70)
                          "the input Ball has its horizontal boundaries reset"))
; STRATEGY : Data Decomposition on b : Ball
(define (reset-hor-boundary-for-ball b x)
  (make-ball (ball-x b)
             (ball-y b)
             (ball-vx b)
             (ball-vy b)
             (ball-fb b)
             (ball-ub b)
             (new-boundary < - (ball-rb b) x (ball-x b))
             (new-boundary > + (ball-lb b) x (ball-x b))))

; reset-ver-boundary-for-ball : Ball Coordinate -> Ball
; returns a Ball with updated vertical boundaries depending on the 
; position of the Coordinates.
(begin-for-test (check-equal? (reset-ver-boundary-for-ball 
                               (make-ball 185 185 3 3 380 45 380 20) 50)
                              (make-ball 185 185 3 3 380 70 380 20)
                          "the input Ball has its vertical boundaries reset"))
; STRATEGY : Data Decomposition on b : Ball
(define (reset-ver-boundary-for-ball b y)
  (make-ball (ball-x b)
             (ball-y b)
             (ball-vx b)
             (ball-vy b)
             (new-boundary < - (ball-fb b) y (ball-y b))
             (new-boundary > + (ball-ub b) y (ball-y b))
             (ball-rb b)
             (ball-lb b)))

; new-boundary : [Coordinate Coordinate -> Boolean] 
;                [Coordinate Coordinate -> Coordinate]
;                X Coordinate Coordinate                        -> X
; returns the new boundary of the ball 
(begin-for-test (check-equal? (new-boundary 
                               < - 380 350 300)
                              330
                          "the input values are given such that the z is 
                           between ball-coordinate and boundary"))
; STRATEGY : Fucntion Composition
(define (new-boundary f1 f2 bn z bc)
  (if (and (f1 bc z)(f1 z bn))
      (f2 z RADIUS) bn))

; make-new-activewall :  ActiveWall Coordinate Coordinate NEListOf<Ball> 
;                                                                -> ActiveWall
; returns an ActiveWall that has initiated and carry on growing till the end 
; meet the boundaries.
(begin-for-test (check-equal? (make-new-activewall 
                               (make-activewall (make-posn 50 50) 2 true 0 400)
                               50 50
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              (make-activewall (make-posn 50 50) 2 true 0 400)
                          "the input ActiveWall and all the balls given 
                           as input"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (make-new-activewall aw x y nelob)
  (make-activewall (make-posn x y) INITIAL-MAJOR (activewall-or aw)
                   (- (find-activewall-lb nelob x y aw) RADIUS)
                   (+ (find-activewall-hb nelob x y aw) RADIUS)))

; find-activewall-lb : NEListOf<Ball> Coordinate Coordinate ActiveWall 
;                                                         -> Coordinate
; returns the LowerBoundary of the ActiveWall
(begin-for-test (check-equal? (find-activewall-lb 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50 
                               (make-activewall (make-posn 50 50) 2 true 0 400))
                              20
                          "gets the LowerBoundary of the ActiveWall from the 
                          LeftBoudanry of the Ball"))
(begin-for-test (check-equal? (find-activewall-lb 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50 
                              (make-activewall (make-posn 50 50) 2 false 0 400))
                              45
                          "gets the LowerBoundary of the ActiveWall from the 
                               CielingBoundary of the Ball"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (find-activewall-lb nelob x y aw)
  (if (activewall-or aw)
      (find-horizontal-lb-boundary nelob x y)
      (find-vertical-lb-boundary nelob x y)))

; find-horizontal-boundary : NEListOf<Ball> Coordinate Coordinate -> Coordinate
; returns the LeftBoundary of the Ball which is in the same 
; section as that of the MouseEvent.
(begin-for-test (check-equal? (find-horizontal-lb-boundary 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50 )
                              20
                          "gets the LeftBoundary of the Ball"))

; STRATEGY :  Data Decomposition on Ball
(define (find-horizontal-lb-boundary nelob x y)
  (find-activewall-boundary ball-lb nelob x y))

; find-vertical-boundary : NEListOf<Ball> Coordinate Coordinate -> Coordinate
; returns the CielingBoundary of the Ball which is in the same 
; section as that of the MouseEvent
(begin-for-test (check-equal? (find-vertical-lb-boundary 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50 )
                              45
                          "gets the CielingBoundary of the Ball"))
; STRATEGY :  Data Decomposition on Ball
(define (find-vertical-lb-boundary nelob x y)
  (find-activewall-boundary ball-ub nelob x y))

; find-activewall-hb : NEListOf<Ball> Coordinate Coordinate ActiveWall 
;                                                         -> Coordinate
; returns the HigherBoundary of the ActiveWall
(begin-for-test (check-equal? (find-activewall-hb 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50 
                               (make-activewall (make-posn 50 50) 2 true 0 400))
                              380
                          "gets the HigherBoundary of the ActiveWall from the 
                          RightBoudanry of the Ball"))
(begin-for-test (check-equal? (find-activewall-hb 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50 
                              (make-activewall (make-posn 50 50) 2 false 0 400))
                              380
                          "gets the HigherBoundary of the ActiveWall from the 
                               FloorBoundary of the Ball"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (find-activewall-hb nelob x y aw)
  (if (activewall-or aw)
      (find-horizontal-hb-boundary nelob x y)
      (find-vertical-hb-boundary nelob x y)))

; find-horizontal-boundary : NEListOf<Ball> Coordinate Coordinate -> Coordinate
; returns the lower horizontal boundary of the Ball which is in the same 
; section as that of the MouseEvent.
(begin-for-test (check-equal? (find-horizontal-hb-boundary 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50)
                              380
                          "gets the RightBoudanry of the Ball"))
; STRATEGY :  Data Decomposition on Ball
(define (find-horizontal-hb-boundary nelob x y)
  (find-activewall-boundary ball-rb nelob x y))

; find-vertical-boundary : NEListOf<Ball> Coordinate Coordinate -> Coordinate
; returns the lower horizontal boundary of the Ball which is in the same 
; section as that of the MouseEvent.
(begin-for-test (check-equal? (find-vertical-hb-boundary 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               50 50)
                              380
                          "given a list of balls and a activewall origin"))
; STRATEGY :  Data Decomposition on Ball
(define (find-vertical-hb-boundary nelob x y)
  (find-activewall-boundary ball-fb nelob x y))

; find-activewall-boundary : [Ball -> Coordinate] NEListOf<Ball> 
;                            Coordinate Coordinate -> X
; returns the boundaries of the Ball from the NEListOf<Ball> that are in the 
; active area where the ActiveWall is originating.
(begin-for-test (check-equal? (find-activewall-boundary ball-fb 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 350 45 380 20))
                               50 375)
                              380
                          "gets the FloorBoundary of the Ball"))
; STRATEGY : Double Decomposition on b : Ball
;                                nelob : NEListOf<Ball> 
(define (find-activewall-boundary f nelob x y)
  ( f (first (get-list-of-ball nelob x y))))

; get-list-of-ball : NEListOf<Ball> Coordinate Coordinate -> NEListOfBall
; returns a NEListOfBall, which contains all the Balls that are in the same 
; section of that of the MouseEvent from the input NEListOf<Ball>
(begin-for-test (check-equal? (get-list-of-ball
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 350 45 380 20)) 375 375)
                              (list (make-ball 235 365 3 7 380 45 380 20))
                          "given a list of ball and active wall origin with one 
                              ball inside the region while other not"))
; STRATEGY : Function Composition 
(define (get-list-of-ball nelob x y)
  (local(; Ball Coordinate Coordinate -> Boolean
         ; returns #true if the Coordinates are inside a Balls section
         ; STRATEGY : Data Decomposition on b : Ball
         (define (compare-click-boundaries b)
           (and (check-click-coord-inside (ball-lb b) (ball-rb b) x) 
                (check-click-coord-inside (ball-ub b) (ball-fb b) y))))
         (filter compare-click-boundaries nelob)))

; click-in-active-area? : World Coordinate Coordinate -> Boolean
; returns true if the MouseEvent happened inside an active area, i.e.,
; atleast one Ball is present inside the area.
(begin-for-test (check-equal? (get-list-of-ball
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 350 45 380 20)) 375 375)
                              (list (make-ball 235 365 3 7 380 45 380 20))
                          "given a list of ball and active wall origin with one 
                              ball inside the region while other not"))
(begin-for-test (check-equal? (get-list-of-ball
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 350 45 380 20)) 5 5)
                              empty
                          "given a list of ball and and click is outside the 
                           active region of any Ball"))
; STRATEGY : Data Decomposition on nelob : NEListOfBall
(define (click-in-active-area? nelob x y)
  (local (; compare-click-boundaries : Ball Coordinate Coordinate -> Boolean
          ; returns true if the Coordinates are inside the Balls boundaries
          ; STRATEGY : Data Decomposition on b : Ball
          (define (compare-click-boundaries b)
            (and (check-click-coord-inside (ball-lb b) (ball-rb b) x) 
                 (check-click-coord-inside (ball-ub b) (ball-fb b) y))))
          (ormap compare-click-boundaries nelob)))

; check-click-coord-inside : X Y Coordinate -> Boolean
; returns true if the Coordinate of the click is inside the lower 
; (LeftBoundary/CielingBoundary) and higher (RightBoundary/FloorBoundary) 
; boundaries of the Ball.
(begin-for-test (check-equal? (check-click-coord-inside
                               20 380 300) 
                              #true
                          "given a input values that is between the lower and 
                           higher boundary"))
(begin-for-test (check-equal? (check-click-coord-inside
                               20 280 300) 
                              #false
                          "given a input values that is between the lower and 
                           higher boundary"))
; STRATEGY : Function Composition
(define (check-click-coord-inside lb hb z)
  (local (; Coordinate X -> Boolean
          ; returns true if the input coordinate is inside the higher boundary
          (define (check-higher-boundary z hb)
           (< z (+ hb RADIUS))))
    ;-IN-
   (if (> z (- lb RADIUS)) (check-higher-boundary z hb) #false )))

; render : World -> Image
; represents the status if the World in an Image.
(begin-for-test (check-equal? (render 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) false 40))
                              (overlay/align "middle" "top" 
                                             (level-score-goal-image 
                                              (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 40)
                                             (image-of-ball-and-wall 
                                              (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               empty (make-activewall (make-posn 253 288) 162 
                                                 true 0 400)))
                             "the input World with two Ball and no Wall and 
                                 no ActiveWall present"))
(begin-for-test (check-equal? (render 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400) true 40))
                              (overlay/align "middle" "top" 
                                             (level-score-goal-image 
                                              (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 40)
                                             (image-of-active-wall 
                                              (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               empty (make-activewall (make-posn 253 288) 162 
                                                 true 0 400)))
                             "the input World with two Ball and no Wall and 
                                 no ActiveWall present"))
; STRATEGY : Data Decomposition on w : World
(define (render w)
  (if (world-awf w)
          (overlay/align "middle" "top" 
                         (level-score-goal-image 
                          (world-nelob w) (world-score w))
                         (image-of-active-wall (world-nelob w) 
                                               (world-low w) (world-aw w)))
          (overlay/align "middle" "top" 
                         (level-score-goal-image 
                          (world-nelob w) (world-score w))
                         (image-of-ball-and-wall (world-nelob w) 
                                                 (world-low w) (world-aw w)))))
  
; level-score-goal-image : NEListOf<Ball> Score : Image
; returns an image representation of the Score , level and goal.
(begin-for-test (check-equal? (level-score-goal-image 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 40)
                              (underlay/offset 
                               (get-level-image 
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                                               300 0
                                               (above (get-score-image 40)
                                                      (get-goal-image 
                                                       (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))))
                             "the input list with two Ball and score of
                              40 is given"))
; STRATEGY : Function Composition 
(define (level-score-goal-image nelob s)
  (underlay/offset (get-level-image nelob)
                  300 0
                  (above (get-score-image s)
                         (get-goal-image nelob))))

; get-level-image : NEListOf<Ball> -> Image
; returns the Image of the level of the game by calculating number of Ball 
; in the list and making an imge of it.
(begin-for-test (check-equal? (get-level-image 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              (text "LEVEL:2" FONT-SIZE FONT-COLOR)
                             "the input list with two Ball is given"))
; STRATEGY : Function Composition  
(define (get-level-image nelob)
  (text (get-level-string nelob) FONT-SIZE FONT-COLOR))

; get-level-string : NEListOf<Ball> -> String
; returns a string having the level number, this will used in display
(begin-for-test (check-equal? (get-level-string 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              "LEVEL:2"
                             "the input list with two Ball is given"))
; STRATEGY : Function Composition
(define (get-level-string nelob)
  (string-append "LEVEL:" (number->string (length nelob))))

; get-score-image : Score -> Image
; returns an Image with Score details in it
(begin-for-test (check-equal? (get-score-image 40)
                              (text "SCORE:40%" FONT-SIZE FONT-COLOR)
                             "the input score of 40 is given"))
; STATEGY : Function Composition 
(define (get-score-image s)
  (text (get-score-text s) FONT-SIZE FONT-COLOR))

; get-score-image : Score -> String
; returns an String with Score details in it
(begin-for-test (check-equal? (get-score-text 40)
                              "SCORE:40%" 
                             "the input score of 40 is given"))
; STRATEGY : Function Composition 
(define (get-score-text s)
  (string-append "SCORE:" (number->string s) "%"))

; get-goal-image : NEListOf<Ball> -> Image
; returns on Image of the goal details of the current level
(begin-for-test (check-equal? (get-goal-image 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              (text "GOAL:60%" FONT-SIZE FONT-COLOR)
                             "the input list with two Ball is given"))
; STRATEGY : Function Composition 
(define (get-goal-image nelob)
  (text (get-goal-text nelob) FONT-SIZE FONT-COLOR))

; get-goal-text : NEListOf<Ball> -> String
; returns String representation of the goal of the current level
(begin-for-test (check-equal? (get-goal-text (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              "GOAL:60%" 
                             "the input list with two balls is given"))
; STRATEGY : Function Composition 
(define (get-goal-text nelob)
  (string-append "GOAL:" 
                 (number->string (get-goal nelob)) "%"))

; get-goal : NEListOf<Ball> -> Number
; returns the goal of the current level
(begin-for-test (check-equal? (get-goal (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              60 
                             "the input list with two ball is given"))
(begin-for-test (check-equal? (get-goal (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              90 
                             "the input list with two ball is given"))
; STRATEGY : Function Composition 
(define (get-goal nelob)
  (if (< (+ 50 (* 5 (length nelob))) GOAL-LIMIT)
  (+ 50 (* 5 (length nelob))) GOAL-LIMIT))
                  
; image-of-active-wall : NEListOf<Ball> ListOf<Wall> ActiveWall -> Image
; creates an Image of the ActiveWall on the game
(begin-for-test (check-equal? (image-of-active-wall 
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400))
                              (overlay/align 
                               "left" "bottom" 
                               ACTIVE-WALL-HORIZONTAL-HELP  
                               (place-horizontal-active-wall (make-posn 253 288)
                                    162 (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 
                                    empty 
                                    (make-activewall (make-posn 253 288) 162 
                                                 true 0 400)))
                             "the input World with two Ball and a 
                                     activewall present"))
(begin-for-test (check-equal? (image-of-active-wall 
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                              (overlay/align 
                               "left" "bottom" 
                               ACTIVE-WALL-VERTICAL-HELP  
                               (place-vertical-active-wall (make-posn 253 288) 
                                    162 (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 
                                    empty 
                                    (make-activewall (make-posn 253 288) 162 
                                                 false 0 400)))
                             "the input World with two Ball and a 
                                     activewall present"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (image-of-active-wall nelob low aw)
  (if (activewall-or aw)
      (overlay/align "left" "bottom" ACTIVE-WALL-HORIZONTAL-HELP  
                     (place-horizontal-active-wall (activewall-p aw) 
                                    (activewall-major aw) nelob low aw))
      (overlay/align "left" "bottom" ACTIVE-WALL-VERTICAL-HELP 
                     (place-vertical-active-wall (activewall-p aw) 
                                  (activewall-major aw) nelob low aw)))) 

; place-horizontal-active-wall : Posn Major NEListOf<Ball> 
;                                ListOf<Wall> ActiveWall   -> Image
; creates the Image of horizontal ActiveWall on the Image of the game
(begin-for-test (check-equal? (place-horizontal-active-wall 
                                (make-posn 253 288)
                               162
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 253 288) 162 
                                                 true 0 400))
                              (place-image 
                               (rectangle 162 MINOR-AXIS "solid" "brown")
                               253 288 
                               (image-of-ball-and-wall 
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) empty 
                                   (make-activewall (make-posn 253 288) 162 
                                                 true 0 400)))
                             "the input World with two Ball and a 
                                     horizontal activewall present"))
; STRATEGY : Data Decompsoition on p : Posn
(define (place-horizontal-active-wall p major nelob low aw)
  (place-image (rectangle major MINOR-AXIS "solid" "brown")
                   (posn-x p) (posn-y p) 
                   (image-of-ball-and-wall nelob low aw)))

; place-vertical-active-wall : Posn Major NEListOf<Ball> 
;                                ListOf<Wall> ActiveWall   -> Image
; creates the Image of vertical ActiveWall on the Image of the game
(begin-for-test (check-equal? (place-vertical-active-wall 
                                (make-posn 253 288)
                               162
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                              (place-image 
                               (rectangle MINOR-AXIS 162 "solid" "brown")
                               253 288 
                               (image-of-ball-and-wall 
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) empty 
                                   (make-activewall (make-posn 253 288) 162 
                                                 false 0 400)))
                             "the input World with two Ball and a 
                                     vertical activewall present"))
; STRATEGY : Data Decompsoition on p : Posn 
(define (place-vertical-active-wall p major nelob low aw)
      (place-image (rectangle MINOR-AXIS major "solid" "brown")
                   (posn-x p) (posn-y p) 
                   (image-of-ball-and-wall nelob low aw))) 

; image-of-ball-and-wall : NEListOf<Ball> ListOf<Wall> ActiveWall
; renders the image of the Ball and the Wall present in the incoming lists
(begin-for-test (check-equal? (image-of-ball-and-wall 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                              (image-of-balls 
                               (list-of-ball-posn 
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                               (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                             "the input World with two Ball and a 
                                     no wall in it"))
(begin-for-test (check-equal? (image-of-ball-and-wall 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 200 200)  
                                                 false 0 400))
                                (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                              (image-of-walls 
                               (list (make-wall (make-posn 200 200)  
                                                 false 0 400)) 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 
                               (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                             "the input World with two Ball and a 
                                     one wall in it"))
; STRATEGY : Function Composition 
(define (image-of-ball-and-wall nelob low aw)
  (if (empty? low) (image-of-balls (list-of-ball-posn nelob) aw) 
               (image-of-walls low nelob aw)))

; image-of-walls : ListOf<Wall> NEListOf<Ball> ActiveWall -> Image
; renders an image of Wall present in the list along with Balls
(begin-for-test (check-equal? (image-of-walls 
                               (list (make-wall (make-posn 200 200)  
                                                 false 0 400)) 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 
                               (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                              (place-image 
                               (rectangle MINOR-AXIS 400 "solid" "brown") 
                               200 200 
                               (image-of-balls 
                                (list-of-ball-posn 
                                 (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))) 
                                (make-activewall (make-posn 253 288) 162 
                                                 false 0 400)))
                             "the input World with two Ball and a vertical
                                     wall in it"))
(begin-for-test (check-equal? (image-of-walls 
                               (list (make-wall (make-posn 200 200)  
                                                 true 0 400)) 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)) 
                               (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                              (place-image 
                               (rectangle 400 MINOR-AXIS "solid" "brown") 
                               200 200 
                               (image-of-balls 
                                (list-of-ball-posn 
                                 (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))) 
                                (make-activewall (make-posn 253 288) 162 
                                                 false 0 400)))
                             "the input World with two Ball and a horizontal
                                     wall in it"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (image-of-walls low nelob aw)
    (local (; Wall Image -> Image 
            ; adds a WALL at p to scene
            ; STRATEGY : Data Decompostion on w : Wall
          (define (add-one-wall w scene)
            (if (wall-or w)
            (place-horizontal-wall (wall-p w) (- (wall-hb w) (wall-lb w)) scene)
           (place-vertical-wall (wall-p w) (- (wall-hb w) (wall-lb w)) scene))))
    (foldr add-one-wall 
           (image-of-balls (list-of-ball-posn nelob) aw) low)))

; place-horizontal-wall : Posn Major Image -> Image
; adds a horizontal Wall on the input Image
(begin-for-test (check-equal? (place-horizontal-wall 
                               (make-posn 200 200)
                               400
                               MT)
                               (place-image 
                                (rectangle 400 MINOR-AXIS "solid" "brown") 
                                200 200 MT)
                             "the input with Wall origin at (200,200) "))
; STRATEGY : Data Decompostion on p : Posn
(define (place-horizontal-wall p major scene)
  (place-image (rectangle major MINOR-AXIS "solid" "brown") 
               (posn-x p) (posn-y p) scene))

; place-vertical-wall : Posn Major Image -> Image
; adds a vertical Wall on the input Image
(begin-for-test (check-equal? (place-vertical-wall 
                               (make-posn 200 200)
                               400
                               MT)
                               (place-image 
                                (rectangle MINOR-AXIS 400 "solid" "brown") 
                                200 200 MT)
                             "the input with Wall origin at (200,200) "))
; STRATEGY : Data Decompostion on p : Posn
(define (place-vertical-wall p major scene)
  (place-image (rectangle MINOR-AXIS major "solid" "brown") 
               (posn-x p) (posn-y p) scene))

; list-of-ball-posn : NEListOf<Ball> -> ListOf<Posn>
; consumes a NEListOf<Ball> and extracts the x and y Coordinate of each Ball 
; and makes a ListOf<Posn> of it.
(begin-for-test (check-equal? (list-of-ball-posn 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                               (list
                                 (make-posn 235 365)
                                 (make-posn 185 185))
                             "a list with two balls provided "))
; STRATEGY : Function Composition
(define (list-of-ball-posn nelob)
    (map make-ball-posn nelob))

; make-ball-posn : Ball -> Posn
; returns the Posn of a Ball
(begin-for-test (check-equal? (make-ball-posn 
                               (make-ball 185 185 3 3 380 45 380 20))
                               (make-posn 185 185)
                             "a list with two balls provided "))
; STRATEGY : Data Decomposition on b : Ball
(define (make-ball-posn b)
  (make-posn (ball-x b) (ball-y b)))

; image-of-balls : ListOf<Posn> ActiveWall -> Image
; returns an image of balls and text on canvas.
(begin-for-test (check-equal? (image-of-balls 
                               (list
                                 (make-posn 235 365)
                                 (make-posn 185 185))
                               (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                               (place-image BALL 235 365
                                            (place-image
                                             BALL 185 185
                                             (overlay/align 
                                              "left" "bottom" 
                                              ACTIVE-WALL-VERTICAL-HELP MT)))
                             "a list with two ball positions provided "))
; STRATEGY : Function Composition 
(define (image-of-balls lop aw)
(local (; Posn Image -> Image 
        ; adds a BALL at p to scene
        ; STRATEGY : Data Decomposition on p : Posn
        (define (add-one-ball p scene)
          (place-image BALL (posn-x p) (posn-y p) scene))) 
    (foldr add-one-ball (show-text-image aw) lop)))

; show-text-image : ActiveWall : Image
; creates an Image with ActiveWall help text on it.
(begin-for-test (check-equal? (show-text-image 
                               (make-activewall (make-posn 253 288) 162 
                                                 false 0 400))
                               (overlay/align "left" "bottom" 
                                              ACTIVE-WALL-VERTICAL-HELP MT)
                             "a image of a vertical wall"))
(begin-for-test (check-equal? (show-text-image 
                               (make-activewall (make-posn 253 288) 162 
                                                 true 0 400))
                               (overlay/align "left" "bottom" 
                                              ACTIVE-WALL-HORIZONTAL-HELP MT)
                             "a image of a vertical wall"))
; STRATEGY :  Data Decomposition on aw : ActiveWall
(define(show-text-image aw) 
  (if (activewall-or aw)
      (overlay/align "left" "bottom" ACTIVE-WALL-HORIZONTAL-HELP MT)
      (overlay/align "left" "bottom" ACTIVE-WALL-VERTICAL-HELP MT)))

; end? : World -> Boolean
; returns true if the game has reached an end
(begin-for-test (check-equal? (end? 
                                (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 185 185) 162 
                                                 true 0 400) true 40))
                              #true
                             "the origin of the ActiveWall is 
                              on one of the Ball"))
(begin-for-test (check-equal? (end? 
                                (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                empty
                                (make-activewall (make-posn 40 40) 162 
                                                 true 0 400) false 40))
                              #false
                             "the origin of the ActiveWall is does not overlap 
                              with any of the Ball"))
; STRATEGY : Data Decomposition on w : World
(define (end? w)
  (if (world-awf w)
   (any-ball-overlap-activewall? (world-nelob w) (world-aw w)) #false))

; any-ball-overlap-activewall? : NEListOf<Ball> ActiveWall -> Boolean
; returns true if any ball in the active area overlaps with the ActiveWall
(begin-for-test (check-equal? (any-ball-overlap-activewall? 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (make-activewall (make-posn 185 185) 162 
                                                 true 0 400))
                              #true
                             "the origin of the ActiveWall is 
                              on one of the Ball"))
(begin-for-test (check-equal? (any-ball-overlap-activewall? 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                               (make-activewall (make-posn 40 40) 162 
                                                 false 0 400))
                              #false
                             "the origin of the ActiveWall is does not overlap 
                              with any of the Ball"))
; STRATEGY : Data Decomposition on aw : ActiveWall
(define (any-ball-overlap-activewall? nelob aw)
  (local (; Ball -> Boolean
          ; returns true if a Ball touches an active wall
          ; STRATEGY : Data Decomposition on aw :ActiveWall
          (define (check-ball-activewall-overlap? b)
            (if (activewall-or aw) (check-horizontal-overlap? b aw) 
             (check-vertical-overlap? b aw))))
    (ormap check-ball-activewall-overlap? 
           (get-list-of-ball nelob (get-activewall-x (activewall-p aw)) 
                             (get-activewall-y (activewall-p aw))))))

; Ball -> Boolean
; returns true if a Ball touches a active wall that is vertical
(begin-for-test (check-equal? (check-vertical-overlap? 
                               (make-ball 185 185 3 3 380 45 380 20)
                               (make-activewall (make-posn 185 185) 162 
                                                 true 0 400))
                              #true
                             "the origin of the ActiveWall overlaps 
                              with the given Ball"))
; STRATEGY : Data Decomposition on b : Ball
;            Data Decomposition on aw : ActiveWall
(define (check-vertical-overlap? b aw)
  (and (between? (ball-x b) 
                 (+ RADIUS (get-activewall-x (activewall-p aw))) 
                 (- (get-activewall-x (activewall-p aw)) RADIUS)) 
       (between? (ball-y b) 
                 (+ (get-activewall-y (activewall-p aw)) 
                    (/ (activewall-major aw) 2)) 
                 (- (get-activewall-y (activewall-p aw)) 
                    (/ (activewall-major aw) 2)))))

; Ball -> Boolean
; returns true if a Ball touches an active wall that is Horizontal
(begin-for-test (check-equal? (check-horizontal-overlap? 
                               (make-ball 185 185 3 3 380 45 380 20)
                               (make-activewall (make-posn 185 185) 162 
                                                 true 0 400))
                              #true
                             "the origin of the ActiveWall overlaps 
                              with the given Ball"))
; STRATEGY : Data Decomposition on b : Ball
;            Data Decomposition on aw : ActiveWall
(define (check-horizontal-overlap? b aw)
  (and (between? (ball-y b) 
                 (+ RADIUS (get-activewall-y (activewall-p aw))) 
                 (- (get-activewall-y (activewall-p aw)) RADIUS))
       (between? (ball-x b) 
                 (+ (get-activewall-x (activewall-p aw)) 
                    (/ (activewall-major aw) 2)) 
                 (- (get-activewall-x (activewall-p aw)) 
                    (/ (activewall-major aw) 2)))))

; between? : Coordinate Coordinate Coordinate -> Boolean
; returns true if the first coordinate is between the other two
(begin-for-test (check-equal? (between? 
                               1 2 3)
                              #false
                             "the value does not lie between the given"))
(begin-for-test (check-equal? (between? 
                               2 1 3)
                              #true
                             "the value does lie between the given"))
(begin-for-test (check-equal? (between? 
                               2 3 1)
                              #true
                             "the value does lie between the given"))
; STRATEGY : Function Composition 
(define (between? x y z)
  (or (and (> x z)(< x y))(and (< x z) (> x y))))

; render-last : World -> Image
; renders the game end image over the snapshot of last scene on the game
(begin-for-test (check-equal? (render-last 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))
                              (overlay/align 
                               "middle" "middle" (text "GAME OVER" 20 "black") 
                               (render (next-world (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))))
                             "the value of given such that it has a active 
                               wall and a normal wall and two balls"))
; STRATEGY : Data Decomposition on w : World
(define (render-last w)
  (overlay/align "middle" "middle" (text "GAME OVER" 20 "black") 
                 (render (next-world w))))


; get-balls : World -> ListOf<Ball>
; Returns all the balls in the given world.
(begin-for-test (check-equal? (get-balls 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))
                              (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                             "the value of given such that it has a active 
                               wall and a normal wall and two balls"))
; STRATEGY : Data Decomposition on w : World
(define (get-balls w)
  (world-nelob w))
 
; mk-ball : Coordinate Coordinate Real Real -> Ball
; Returns a Ball with center at (x,y), with the given velocities.
; A positive x velocity is in the x-increasing direction and vice versa.
; The y velocity is similar.
(begin-for-test (check-equal? (mk-ball 235 365 3 7)
                                 (make-ball 235 365 3 7 380 20 380 20)
                             "the position of the new ball is (235,365) with 
                               VerticalVelocity of 7 and 
                                                     HorizontalVelocity of 3"))
; STRATEGY : Function Composition
(define (mk-ball x y vx vy)
  (make-ball x y vx vy BALL-TOUCHING-FLOOR BALL_TOUCHING-CIELING 
             BALL-TOUCHING-RIGHT-WALL  BALL-TOUCHING-LEFT-WALL ))
 
; replace-balls : World ListOf<Ball> -> World
; Replaces the Balls in the given World with the given Balls.
(begin-for-test (check-equal? (replace-balls 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45)
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 100 100 -3 -7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              (make-world
                               (list
                                (make-ball 235 365 3 7 380 45 380 20)
                                (make-ball 100 100 -3 -7 380 45 380 20)
                                (make-ball 185 185 3 3 380 45 380 20))
                               (list
                                (make-wall (make-posn 201 25) true 0 400))
                               (make-activewall
                                (make-posn 253 288)
                                162 true 0 400) true 45)
                             "the World with two Ball replaced by World 
                                   with three Ball"))
; STRATEGY : Function Composition
(define (replace-balls w nelob)
  (make-world nelob (world-low w) (world-aw w) (world-awf w) (world-score w))) 
 
; get-ball-x : Ball -> Coordinate
; Returns the x position of the Ball's center.
(begin-for-test (check-equal? (get-ball-x (make-ball 235 365 3 7 380 20 380 20))
                              235
                             "the Ball at position (235,365) is given"))
; STRATEGY : Data Decompostion on b : Ball
(define (get-ball-x b)
  (ball-x b))
 
; get-ball-y : Ball -> Coordiate
; Returns the y position of the Ball's center.
(begin-for-test (check-equal? (get-ball-y (make-ball 235 365 3 7 380 20 380 20))
                              365
                             "the Ball at position (235,365) is given"))
; STRATEGY : Data Decomposition on b : Ball
(define (get-ball-y b)
  (ball-y b))

; score : World -> Natural
; Returns the current score.
(begin-for-test (check-equal? (score 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))
                              45
                             "the World score of 45 is given as input"))
; STRATEGY : Data Decomposition on w : World
(define (score w)
  (world-score w))
 
; level : World -> Natural
; Returns the current level.
(begin-for-test (check-equal? (level 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))
                              2
                             "the World with two Balls is given as input"))
; STRATEGY : Data Decomposition on w : World
(define (level w)
  (length (world-nelob w)))

; update-score : World -> Score
; updates the score of the current World of the game.
(begin-for-test (check-equal? (update-score 
                               (make-world
                                (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20))
                                (list (make-wall (make-posn 201 25) true 0 400))
                                (make-activewall (make-posn 253 288) 
                                                 162 true 0 400) true 45))
                              6
                             "the World with two Balls and a wall and one 
                                  activewall is given as input"))
; STRATEGY : Data Decomposition on w : World
(define (update-score w)
 (round/eps(* 100 (/(- CANVAS-AREA (area-of-active-section (world-nelob w))) 
          CANVAS-AREA))))

; round/eps : Real -> Real
; rounds the input to an integer
(begin-for-test (check-equal? (round/eps 
                               3.5)
                              4
                             "the input value is given in floating point"))
; STATEGY : Function Composition 
(define (round/eps x)
  (* (inexact->exact (round (/ x EPS))) EPS))

; area-of-active-section : NEListOf<Ball> -> Real
; returns the area of the section where the Ball s are allowed to bounce in.
(begin-for-test (check-equal? (area-of-active-section 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              150000
                             "the list of two Balls is given as input"))
; STRATEGY : Function Composition
(define (area-of-active-section nelob)
  (foldl + 0 (list-of-area (remove-duplicates nelob))))

; list-of-area : NEListOf<Ball> -> ListOf<Real>
; returns a list of active area of each of the Ball in the list
(begin-for-test (check-equal? (list-of-area 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)))
                              (list 150000 150000)
                             "the list of two Balls is given as input"))
; STRATEGY : Function Composition
(define (list-of-area nelob)
  (map area-of-ball-section nelob))

; remove-duplicates : NEListOf<Ball> -> NEListOf<Ball>
; returns a list of Ball which does not have any Ball that has same 
; LeftBoundary, RightBoundary, CielingBoundary and FloorBoundary
(begin-for-test (check-equal? (remove-duplicates 
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 65)))
                              (list
                               (make-ball 185 185 3 3 380 45 380 20)
                               (make-ball 235 365 3 7 380 45 380 65))
                             "the list of three Balls is given as input
                              , two have same boundaries"))
; STRATEGY : Function Composition
(define (remove-duplicates nelob)
      (foldr (lambda (a b) 
               (cond
                 [(not (same-boundaries? a b)) (cons a b)]
                 [else b])) empty nelob))

; same-boundaries? : Ball NEListOf<Ball> -> Boolean
; returns true if the Ball given as input has all the boudaries same as any 
; of the Bal in the given list else returns false
(begin-for-test (check-equal? (same-boundaries? 
                               (make-ball 235 365 3 7 380 45 380 20)
                               (list
                                 (make-ball 235 365 3 7 380 45 380 20)
                                 (make-ball 185 185 3 3 380 45 380 20)
                                 (make-ball 235 365 3 7 380 45 380 65)))
                              #true
                             "the Ball given as input has all the boudaries  
                               same as any of the Ball in the given list"))
; STRATEGY : Data Decomposition on nelob : NEListOf<Ball>
(define (same-boundaries? b nelob)
  (cond
    [(empty? nelob) #false]
    [else (or (compare-ball-boundary b (first nelob)) 
              (same-boundaries? b (rest nelob)))]))

; compare-ball-boundary : Ball Ball -> Boolean
; returns true if all the Boundaries of the both the input ball are same
(begin-for-test (check-equal? (compare-ball-boundary 
                               (make-ball 235 365 3 7 380 45 380 20)
                               (make-ball 185 185 3 3 380 45 380 25))
                              #false
                             "the Balls given as input has some of the boudaries
                               not same"))
(begin-for-test (check-equal? (compare-ball-boundary 
                               (make-ball 235 365 3 7 380 45 380 20)
                               (make-ball 235 365 3 7 380 45 380 20))
                              #true
                             "the Balls given as input has all the boudaries  
                               same"))
; STRATEGY : Double Decomposition on b1, b2 : Ball 
(define (compare-ball-boundary b1 b2)
  (and (= (ball-ub b1) (ball-ub b2))
       (= (ball-fb b1) (ball-fb b2))
       (= (ball-lb b1) (ball-lb b2))
       (= (ball-rb b1) (ball-rb b2))))

; add-areas : Ball Ball -> Real
; returns the summation of the areas of the given Ball.
(begin-for-test (check-equal? (add-areas 
                               (make-ball 235 365 3 7 380 45 380 20)
                               (make-ball 235 365 3 7 380 45 380 20))
                              300000
                             "the two Balls given as input "))
; STRATEGY : Function Composition 
(define (add-areas b1 b2)
  (+ (area-of-ball-section b1)
     (area-of-ball-section b2)))

; area-of-ball-section : Ball -> Real
; returns the area of the section in which the given Ball is allowed to 
; bounce in.
(begin-for-test (check-equal? (area-of-ball-section 
                               (make-ball 235 365 3 7 380 45 380 20))
                              150000
                             "the Ball given as input to get the area of 
                               section it is allowed to bounce in"))
; STRATEGY : Data Decomposition on b : Ball
(define (area-of-ball-section b)
  (* (+ 40 (- (ball-fb b) (ball-ub b))) (+ 40 (- (ball-rb b) (ball-lb b)))))

    
;------------------------------END OF CODE -------------------------------------

;Alternate Definition

;1.> Taking endpoints (Posn) of the ActiveWall and Wall instead of origin of 
;the them (make-activewall Posn Posn)
; INTERP : Posn p1, Posn p2 represents the endpoints of the Wall/ActiveWall 

;Pros : 
; The resetting of Origin of the ActveWall after every time the ActiveWall
; completes, which has to be done in structure 
;(make-activewall Posn Major Boolean LowerBoundary HigherBoundary), can be 
; avoided in this type of structuring. 
; This would also inherently contain the 
; information of the orientation of the ActiveWall. If the x Coordinates of 
; the Posn s are same that means the ActiveWall is vertical and if y Coordinate
; of the Posn s are same that mean the ActiveWall is Horizontal.

;Cons: 
; It would be difficult/impossible to carry the position of the mouse click 
; around the program.

;2.> Keeping Direction (Left, Right, Up, Down) in the Ball's structure  
; instead of +/- velocity to indicate directions.

;Pros
; The usage of directions will make the code much more readible, and easy to 
; track what happens when the Ball's direction is what.

;Cons
; This would clutter the structure of the Ball as we have to anyways use 
; velocity.

;3.> Taking Ball Coordinates in Posn

;Pros
; Would be easy to convert Ball Posn s to an individual list of Posn when 
; required to compare to wall/boundary.

;Cons
; would get difficult in avoiding double decomposition when a single Ball's
; position is to be compared.