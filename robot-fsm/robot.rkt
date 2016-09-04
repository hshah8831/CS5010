;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Importing Packages
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 5 ); Time taken to finish the task in hours

;;Providing the functions to other packages
(provide initial-robot)
(provide robot-left)
(provide robot-right)
(provide robot-x)
(provide robot-y)
(provide robot-forward)

;;Canvas Constants
(define WIDTH 200) ; pixels
(define HEIGHT 400) ; pixels
(define EMPTY-SCENE (empty-scene WIDTH HEIGHT)) ; Image
 
;;Ball Constants
(define ROBOT-RADIUS 15) ; pixels
(define ROBOT-IMG (circle ROBOT-RADIUS "solid" "red")) ; Image
(define BALL-TOUCHING-CIELING ROBOT-RADIUS) ; pixels
(define BALL-TOUCHING-FLOOR (- HEIGHT ROBOT-RADIUS)) ; pixels
(define BALL-TOUCHING-LEFT-WALL ROBOT-RADIUS) ; pixels
(define BALL-TOUCHING-RIGHT-WALL (- WIDTH ROBOT-RADIUS)) ; pixels

;;A Direction is one of these strings
;;- "up"
;;- "down"
;;- "right"
;;- "left"
;;INERP: A Direction represents the direction in 
;;which the robot is facing, UP being towards cieling
;;DOWN being towards floor, RIGHT meaning towards right wall
;;and LEFT meaning towards left wall.

;;Direction Constants
(define UP "up")
(define DOWN "down")
(define RIGHT "right")
(define LEFT "left")
  
;;------------------------------------------------------------------------------
;;up? : string -> boolean
;;checks if the input direction is "up" or not  
;;GIVEN: string ed
;;RETURN: boolean 
;;EXAMPLE: up? "down" -> #true
;;       : up? "up"   -> #false
;;STRATEGY: Function Composition
(define (up? dir)
  (string=? dir UP))

(begin-for-test (check-equal? (up? UP) #true 
                "checks if the input direction is up or not ")
                (check-equal? (up? DOWN) #false 
                "checks if the input direction is up or not "))

;;------------------------------------------------------------------------------
;;down? : string -> boolean
;;checks if the input direction is "down" or not  
;;GIVEN: string ed
;;RETURN: boolean 
;;EXAMPLE: down? "down" -> #true
;;       : down? "up" -> #false
;;STRATEGY: Function Composition
(define (down? dir)
  (string=? dir DOWN))

(begin-for-test (check-equal? (down? DOWN) #true 
                "checks if the input direction is down or not")
                (check-equal? (down? UP) #false 
                "checks if the input direction is down or not"))

;;------------------------------------------------------------------------------
;;left? : string -> boolean
;;checks if the input direction is "left" or not  
;;GIVEN: string ed
;;RETURN: boolean 
;;EXAMPLE: left? "left" -> #true
;;       : left? "up" -> #false
;;STRATEGY: Function Composition
(define (left? dir)
  (string=? dir LEFT))

(begin-for-test (check-equal? (left? LEFT) #true 
                "checks if the input direction is left or not")
                (check-equal? (left? UP) #false 
                "checks if the input direction is left or not"))

;;------------------------------------------------------------------------------
;;right? : string -> boolean
;;checks if the input direction is "right" or not  
;;GIVEN: string ed
;;RETURN: boolean 
;;EXAMPLE: right? "right" -> #true
;;       : right? "up" -> #false
;;STRATEGY: Function Composition
(define (right? dir)
  (string=? dir RIGHT))

(begin-for-test (check-equal? (right? RIGHT) #true 
                "checks if the input direction is right or not")
                (check-equal? (right? UP) #false 
                "checks if the input direction is right or not"))
  
;; TEMPLATE:
;; direction-fn: direcction -> ???
;;(define (direction-fn dir)
;;  (cond
;;    [(up? dir) ...]
;;    [(down? dir) ...]
;;    [(right? dir) ...]
;;    [(left? dir) ...]))

;;robot : (make-robot coordinate coordinate direction)
;;INTERP: (make-editor x y dir) means 
;;x gives the distance of the point from origin on x-axis
;;y gives the distance of the point from origin on y-axis
;;dir gives the direction at the robot is facing
(define-struct robot [ x y dir ])

;;------------------------------------------------------------------------------
;;initial-robot : Coordinate Coordinate -> Robot
;;Returns a Robot located at (x,y), facing up.  
;;GIVEN: coordinate x, coordinate y
;;RETURN: robot r
;;EXAMPLE: initial-robot 2 5 -> ( make-robot 2 5 "up" )
;;STRATEGY: Data Decomposition
(define (initial-robot x y)
  (make-robot x y "up" ))

(begin-for-test (check-equal? (initial-robot 50 50) 
                              (make-robot 50 50 "up")  
                "Returns a Robot located at (x,y), facing up"))
 

;;------------------------------------------------------------------------------
;;robot-left : Robot -> Robot
;;Returns a Robot like r, but turned either 90 degrees left.  
;;GIVEN: robot r0
;;RETURN: robot r1
;;EXAMPLE: robot-left (make-robot 2 5  "up") -> (make-robot x y "left" ) 
;;         robot-left (make-robot 2 5  "down") -> (make-robot x y "right" )
;;         robot-left (make-robot 2 5  "left") -> (make-robot x y "down" )
;;         robot-left (make-robot 2 5  "right") -> (make-robot x y "up" )  
;;STRATEGY: Data Decomposition
(define (robot-left r)
  (cond
    [(up? (robot-dir r)) (make-robot (robot-x r) (robot-y r) LEFT )]
    [(down? (robot-dir r)) (make-robot (robot-x r) (robot-y r) RIGHT )]
    [(right? (robot-dir r)) (make-robot (robot-x r) (robot-y r) UP )]
    [(left? (robot-dir r)) (make-robot (robot-x r) (robot-y r) DOWN )]))

(begin-for-test (check-equal? (robot-left (make-robot 50 50 "up" )) 
                              (make-robot 50 50 "left")  
                "Returns a Robot like r, but turned either 90 degrees left")
                (check-equal? (robot-left (make-robot 50 50 "down" )) 
                              (make-robot 50 50 "right")  
                "Returns a Robot like r, but turned either 90 degrees left")
                (check-equal? (robot-left (make-robot 50 50 "right" )) 
                              (make-robot 50 50 "up")  
                "Returns a Robot like r, but turned either 90 degrees left")
                (check-equal? (robot-left (make-robot 50 50 "left" )) 
                              (make-robot 50 50 "down")  
                "Returns a Robot like r, but turned either 90 degrees left"))

;;------------------------------------------------------------------------------
;;robot-right : Robot -> Robot
;;Returns a Robot like r, but turned either 90 degrees right.  
;;GIVEN: robot r0
;;RETURN: robot r1
;;EXAMPLE: robot-right (make-robot 2 5  "up") -> (make-robot x y "right" ) 
;;         robot-right (make-robot 2 5  "down") -> (make-robot x y "left" )
;;         robot-right (make-robot 2 5  "right") -> (make-robot x y "down" )
;;         robot-right (make-robot 2 5  "left") -> (make-robot x y "up" ) 
;;STRATEGY: Data Decomposition
(define (robot-right r)
  (cond
    [(up? (robot-dir r)) (make-robot (robot-x r) (robot-y r) RIGHT)]
    [(down? (robot-dir r)) (make-robot (robot-x r) (robot-y r) LEFT)]
    [(right? (robot-dir r)) (make-robot (robot-x r) (robot-y r) DOWN)]
    [(left? (robot-dir r)) (make-robot (robot-x r) (robot-y r) UP)]))

(begin-for-test (check-equal? (robot-right (make-robot 50 50 "up" )) 
                              (make-robot 50 50 "right")  
                "Returns a Robot like r, but turned either 90 degrees right")
                (check-equal? (robot-right (make-robot 50 50 "down" )) 
                              (make-robot 50 50 "left")  
                "Returns a Robot like r, but turned either 90 degrees right")
                (check-equal? (robot-right (make-robot 50 50 "right" )) 
                              (make-robot 50 50 "down")  
                "Returns a Robot like r, but turned either 90 degrees right")
                (check-equal? (robot-right (make-robot 50 50 "left" )) 
                              (make-robot 50 50 "up")  
                "Returns a Robot like r, but turned either 90 degrees right"))
 
 
;;------------------------------------------------------------------------------
;;robot-forward : Robot NonNegReal -> Robot
;;Returns a Robot like r, but moved forward by d pixels.  
;;If the robot is inside the room and moving would put any part of the
;;robot outside the room, the robot should stop at the wall that it's facing.  
;;GIVEN: robot r0, NonNegReal i
;;RETURN: robot r1
;;EXAMPLE: robot-forward (make-robot 50 50 "up") 5 -> 
;;                                                (make-robot 50 45 "up") 
;;       robot-forward (make-robot 50 50 "down") 5 -> 
;;                                                (make-robot 50 55 "down") 
;;       robot-forward (make-robot 50 50 "right") 5 -> 
;;                                                (make-robot 55 50 "right") 
;;       robot-forward (make-robot 50 50 "left") 5  -> 
;;                                                (make-robot 45 50 "left")
;;       robot-forward (make-robot 15 15 "up") 5  -> 
;;                                                (make-robot 15 15 "up")
;;       robot-forward (make-robot 50 182 "down") 5 -> 
;;                                                (make-robot 50 185 "down")
;;       robot-forward (make-robot 15 15 "left") 5 -> 
;;                                                (make-robot 15 15 "left")
;;       robot-forward (make-robot 382 50 "right") 5 -> 
;;                                                (make-robot 385 50 "right")
;;STRATEGY: Data Decomposition
(define (robot-forward r d)
  (cond
    [(up? (robot-dir r)) (make-robot (robot-x r) (normalize r d) UP)]
    [(down? (robot-dir r)) (make-robot (robot-x r) (normalize r d) DOWN)]
    [(right? (robot-dir r)) (make-robot (normalize r d) (robot-y r) RIGHT)]
    [(left? (robot-dir r)) (make-robot (normalize r d) (robot-y r) LEFT)]))

(begin-for-test (check-equal? (robot-forward (make-robot 50 50 "up" ) 5 ) 
                              (make-robot 50 45 "up")  
                "Returns a Robot like r, but moved forward by d pixels")
                (check-equal? (robot-forward (make-robot 50 50 "down" ) 5 ) 
                              (make-robot 50 55 "down")  
                "Returns a Robot like r, but moved forward by d pixels")
                (check-equal? (robot-forward (make-robot 50 50 "right" ) 5 ) 
                              (make-robot 55 50 "right")  
                "Returns a Robot like r, but moved forward by d pixels")
                (check-equal? (robot-forward (make-robot 50 50 "left" ) 5 ) 
                              (make-robot 45 50 "left")  
                "Returns a Robot like r, but moved forward by d pixels")
                (check-equal? (robot-forward (make-robot 15 15 "up" ) 5 ) 
                              (make-robot 15 15 "up") 
                "Returns a Robot like r, but moved forward by d pixels")
                (check-equal? (robot-forward (make-robot 50 382 "down" ) 5 ) 
                              (make-robot 50 385 "down")  
                "Returns a Robot like r, but moved forward by d pixels")
                (check-equal? (robot-forward (make-robot 15 15 "left" ) 5 ) 
                              (make-robot 15 15 "left")  
                "Returns a Robot like r, but moved forward by d pixels")
                (check-equal? (robot-forward (make-robot 182 50 "right" ) 5 ) 
                              (make-robot 185 50 "right")  
                "Returns a Robot like r, but moved forward by d pixels"))

;;------------------------------------------------------------------------------
;;normalize : Robot NonNegReal -> NonNegReal
;;Returns the new distance if the given distance would take any part of the 
;;robot or the whole robot outside the canvas.
;;GIVEN: robot r, NonNegReal d0
;;RETURN: real d1
;;EXAMPLE: normalize (make-robot 50 50 "up") 5   -> 45 
;;       normalize (make-robot 50 50 "down") 5   -> 55 
;;       normalize (make-robot 50 50 "right") 5  -> 55 
;;       normalize (make-robot 50 50 "left") 5   -> 45
;;       normalize (make-robot 15 15 "up") 5     -> 15
;;       normalize (make-robot 50 382 "down") 5  -> 385
;;       normalize (make-robot 15 15 "left") 5   -> 15
;;       normalize (make-robot 182 50 "right") 5 -> 185  
;;STRATEGY: Data Decomposition
(define (normalize r d)
  (cond
    [(and (< (- (robot-y r) d) ROBOT-RADIUS) (up? (robot-dir r))) 
     BALL-TOUCHING-CIELING ]
    [(and (>= (- (robot-y r) d) ROBOT-RADIUS) (up? (robot-dir r))) 
     (- (robot-y r) d)]
    [(and (> (+ (robot-y r) d) (- HEIGHT ROBOT-RADIUS)) (down? (robot-dir r))) 
     BALL-TOUCHING-FLOOR ]
    [(and (<= (+ (robot-y r) d) (- HEIGHT ROBOT-RADIUS)) (down? (robot-dir r))) 
     (+ (robot-y r) d) ]
    [(and (< (- (robot-x r) d) ROBOT-RADIUS) (left? (robot-dir r))) 
     BALL-TOUCHING-LEFT-WALL]
    [(and (>= (- (robot-x r) d) ROBOT-RADIUS) (left? (robot-dir r))) 
     (- (robot-x r) d)]
    [(and (> (+ (robot-x r) d) (- WIDTH ROBOT-RADIUS)) (right? (robot-dir r))) 
     BALL-TOUCHING-RIGHT-WALL]
    [(and (<= (+ (robot-x r) d) (- WIDTH ROBOT-RADIUS)) (right? (robot-dir r))) 
     (+ (robot-x r) d)]))

(begin-for-test (check-equal? (normalize (make-robot 50 50 "up" ) 5 ) 
                              45  
                "calculation final coordinate ")
                (check-equal? (normalize (make-robot 50 50 "down" ) 5 ) 
                              55  
                "calculation final coordinate")
                (check-equal? (normalize (make-robot 50 50 "right" ) 5 ) 
                              55  
                "calculation final coordinate")
                (check-equal? (normalize (make-robot 50 50 "left" ) 5 ) 
                              45  
                "calculation final coordinate")
                (check-equal? (normalize (make-robot 15 15 "up" ) 5 ) 
                              15  
                "calculation final coordinate")
                (check-equal? (normalize (make-robot 50 382 "down" ) 5 ) 
                              385  
                "calculation final coordinate")
                (check-equal? (normalize (make-robot 15 15 "left" ) 5 ) 
                              15  
                "calculation final coordinate")
                (check-equal? (normalize (make-robot 182 50 "right" ) 5 ) 
                              185  
                "calculation final coordinate")) 

