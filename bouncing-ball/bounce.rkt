;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bounce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Importing Packages
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 20 ); Time taken to finish the task in hours

;;Providing the functions to other packages
(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide mouse-handler)
(provide world-ball)
(provide world-paused?)
(provide ticks-since-click)
(provide score)
(provide ball-x)
(provide ball-y)

;;Canvas Constants
(define WIDTH 300) ; pixels
(define HEIGHT 400) ; pixels
(define EMPTY-SCENE (empty-scene WIDTH HEIGHT)) ; Image
(define FONT-SIZE 16); pixels 

;;Ball Constants
(define BALL-RADIUS 20) ; pixels
(define BALL-IMAGE (circle BALL-RADIUS "solid" "black")) ; Image
(define BALL-START-X 150) ; pixels
(define BALL-START-Y 20) ; pixels
(define FLOOR (- HEIGHT BALL-RADIUS)) ; pixels
(define LEFT-WALL BALL-RADIUS) ; pixels
(define RIGHT-WALL (- WIDTH BALL-RADIUS)) ; pixels
(define HORIZONTAL-DISPLACEMENT 3) ; pixels
(define GRAVITY 1) ; pixels/tick^2
(define ZERO 0) ; pixels/tick
(define BOUNCE-CO-EFFICIENT 0.9)
(define EXPLOSIVE-THRUST -10) ; pixels/tick
(define INITIAL-HD "right") ; Initial HorizontalDirection
(define INITIAL-VD "down") ; initial VerticalDirection 
(define INITIAL-VELOCITY 0) ; pixels.tick 
(define INITIAL-TICK-SINCE-CLICK 0) ; initial ticks since click
(define INITIAL-PAUSE-FLAG #false) ; initial PauseFlag 
(define INITIAL-SCORE 0) ; initial score

;;Rounding Constant
(define EP 0.001)

;;A VerticalDirection is string and is one of
;;- "up"
;;- "down"
;;INTERP: A VerticalDirection represents the horizontal direction in 
;;which the ball is moving, UP meaning towards the cieling of the window
;;and DOWN meaning towards the floor of the window.
;;VerticalDirection Constants
(define UP "up")
(define DOWN "down")

;;TEMPLATE:
;;VerticalDirection-fn : VerticalDirection -> ???
;;(define (VerticalDirection-fn w)
;;  (cond
;;    [(up? w) ...]
;;    [(down? w) ...]))

;;A HorizontalDirection is string and is one of
;;- "right"
;;- "left"
;;INTERP: A HorizontalDirection represents the horizontal direction in 
;;which the ball is moving, RIGHT meaning towards right wall
;;and LEFT meaning towards left wall.
;;HorizontalDirection Constants
(define RIGHT "right")
(define LEFT "left")

;;TEMPLATE:
;;HorizontalDirection-fn : HorizontalDirection -> ???
;;(define (HorizontalDirection-fn w)
;;  (cond
;;    [(left?  w) ...]
;;    [(right? w) ...]))
  
;;------------------------------------------------------------------------------
;;up? : Ball -> boolean
;;Returns #true if the VerticalDirection of the incoming ball is "up"  
;;else #false.
;;GIVEN: ball w
;;RETURN: boolean 
;;EXAMPLE: up? (make-ball 150 20 "left" "up" 0 0 #false 0)     -> #true
;;       : up? (make-ball 150 20 "left" "down" 0 0 #false 0)   -> #false
;;STRATEGY: Data Decomposition on w: Ball
(define (up? w)
  (string=? (ball-y-dir w) UP))

(begin-for-test (check-equal? (up? (make-ball 150 20 "left" "up" 0 0 #false 0))
                               #true 
                "ball's VerticalDirection is up "))
(begin-for-test (check-equal? (up? 
                               (make-ball 150 20 "left" "down" 0 0 #false 0)) 
                              #false 
                "ball's VerticalDirection is down"))

;;------------------------------------------------------------------------------
;;down? : Ball -> boolean
;;Returns #true if the VerticalDirection of the incoming ball is "down"  
;;else #false.
;;GIVEN: ball w
;;RETURN: boolean 
;;EXAMPLE: down? (make-ball 150 20 "left" "down" 0 0 #false 0)   -> #true
;;       : down? (make-ball 150 20 "left" "up" 0 0 #false 0)     -> #false
;;STRATEGY: Data Decomposition on w: Ball
(define (down? w)
  (string=? (ball-y-dir w) DOWN))

(begin-for-test (check-equal? (down? 
                               (make-ball 150 20 "left" "down" 0 0 #false 0)) 
                              #true 
                "ball's VerticalDirection is down"))
(begin-for-test (check-equal? (down? 
                               (make-ball 150 20 "left" "up" 0 0 #false 0)) 
                              #false 
                "ball's VerticalDirection is up"))

;;------------------------------------------------------------------------------
;;left? : Ball -> boolean
;;Returns #true if the HorizontalDirection of the incoming ball is "left"  
;;else #false. 
;;GIVEN: ball w
;;RETURN: boolean 
;;EXAMPLE: left? (make-ball 150 20 "left" "up" 0 0 #false 0) -> #true
;;       : left? (make-ball 150 20 "right" "up" 0 0 #false 0) -> #false
;;STRATEGY: Data Decomposition on w: Ball
(define (left? w)
  (string=? (ball-x-dir w) LEFT))

(begin-for-test (check-equal? (left? 
                               (make-ball 150 20 "left" "up" 0 0 #false 0)) 
                              #true 
                "ball's HorizontalDirection is left"))
(begin-for-test (check-equal? (left? 
                               (make-ball 150 20 "right" "up" 0 0 #false 0)) 
                              #false 
                "ball's HorizontalDirection is right"))

;;------------------------------------------------------------------------------
;;right? : Ball -> boolean
;;Returns #true if the HorizontalDirection of the incoming ball is "right"  
;;GIVEN: ball ed
;;RETURN: boolean 
;;EXAMPLE: right? (make-ball 150 20 "right" "up" 0 0 #false 0) -> #true
;;       : right? (make-ball 150 20 "left" "up" 0 0 #false 0) -> #false
;;STRATEGY: Data Decomposition on w: Ball
(define (right? w)
  (string=? (ball-x-dir w) RIGHT))

(begin-for-test (check-equal? (right? 
                               (make-ball 150 20 "right" "up" 0 0 #false 0)) 
                              #true 
                "ball's HorizontalDirection is right"))
(begin-for-test (check-equal? (right? 
                               (make-ball 150 20 "left" "up" 0 0 #false 0)) 
                              #false 
                "ball's HorizontalDirection is left"))
  
;;A CurrentVelocity is Real
;;INTERP: A CurrentVelocity represents the vertical velocity of the 
;;ball at a particular tick. The negative CurrentVelocity represents the 
;;upward velocity of the ball, while the positive CurrentVelocity represents  
;;the downward velocity of the all.

;;A Time is NonNegInt
;;WHERE: Time t can be in [0,10)
;;INTERP: Time represents the time, in tick, that has elapsed since the 
;;start of the explosion, that is, a button down event has occured inside the 
;;ball. Time 0 means that there is no explosion.

;;PauseFlag is Boolean and is one of
;;- PAUSED (#true)
;;- UNPAUSED (#false)
;;INTERP: A PauseFlag is a switch to identify whether the ball is paused or 
;;not. PAUSED means that the ball is paused while UNPAUSED means the ball is
;;not paused and is will move according to the defined laws.
;;PauseFlag constants
(define PAUSED #true)
(define UNPAUSED #false)

;;TEMPLATE:
;;PauseFlag-fn : ball -> ???
;;(define (PauseFlag-fn w)
;;  (cond
;;    [(pause? (ball-p w)) ...]
;;   [(unpause? (ball-p w)) ...]))

;;------------------------------------------------------------------------------
;;pause? : ball -> boolean
;;Returns #true if the PauseFlag of the ball is #true  
;;GIVEN: ball w
;;RETURN: boolean 
;;EXAMPLE: pause? (make-ball 150 20 "right" "down" 0 0 #true 0)  -> #true
;;       : pause? (make-ball 150 20 "right" "down" 0 0 #false 0) -> #false
;;STRATEGY: Data Decomposition on w: Ball
(define (pause? w)
  (equal? PAUSED (ball-p w)))

(begin-for-test (check-equal? (pause? 
                             (make-ball 150 20 "right" "down" 0 0 #true 0))
                              #true 
                "ball's PauseFlag is set to true"))
(begin-for-test (check-equal? (pause? 
                            (make-ball 150 20 "right" "down" 0 0 #false 0)) 
                              #false 
                "ball's PauseFlag is set to false"))

;;------------------------------------------------------------------------------
;;unpause? : ball -> boolean
;;Returns #false if the PauseFlag of the ball is #false  
;;GIVEN: ball w
;;RETURN: boolean 
;;EXAMPLE: pause? (make-ball 150 20 "right" "down" 0 0 UNPAUSE ZERO)  -> #true
;;       : pause? (make-ball 150 20 "right" "down" 0 0 PAUSE ZERO)-> #false
;;STRATEGY: Data Decomposition on w: Ball
(define (unpause? w)
  (equal? UNPAUSED (ball-p w)))

(begin-for-test (check-equal? (unpause? 
                            (make-ball 150 20 "right" "down" 0 0 #false 0))
                              #true 
                "ball's PauseFlag is set to false"))
(begin-for-test (check-equal? (unpause? 
                             (make-ball 150 20 "right" "down" 0 0 #true 0)) 
                              #false 
                "ball's PauseFlag is set to true"))

;;Score is a NonNegInt
;;INTERP: Score represents the number of times a mouse click event has happened 
;;inside the ball, i.e. explosion has occured, since the last time ball touched
;;the floor of the window.

;;ball : (make-ball coordinate 
;;                  coordinate 
;;                  HorizontalDirection 
;;                  VerticalDirection 
;;                  Velocity 
;;                  Time
;;                  PauseFlag
;;                  Score)
;;INTERP: (make-ball x y x-dir y-dir t p) means 
;;x gives the distance of the point from origin on x-axis.
;;y gives the distance of the point from origin on y-axis.
;;x-dir represents the direction in which the ball is moving horizontally.
;;y-dir represents the direction in which the ball is moving vertically.
;;t represents the time, in ticks, that has gone past the since the start of
;;the explosion. It's value can be [0,10).
;;p is a boolean
;;sc keEP a count of the 
(define-struct ball [ x y x-dir y-dir v t p sc ])

(define INITIAL-WORLD (make-ball BALL-START-X 
                                 BALL-START-Y 
                                 INITIAL-HD 
                                 INITIAL-VD 
                                 INITIAL-VELOCITY 
                                 INITIAL-TICK-SINCE-CLICK 
                                 INITIAL-PAUSE-FLAG 
                                 INITIAL-SCORE))

; TEMPLATE:
; ball-fn : ball -> ???
(define (ball-fn w)
  (... (ball-x w) 
       ... (ball-y w) 
       ... (ball-x-dir w) 
       ... (ball-y-dir w) 
       ... (ball-v w) 
       ... (ball-t w)
       ... (ball-p w)
       ... (ball-sc w)))

;;A world is a ball
;;INTERP: The world in this problem is considered to be the ball as a whole
;;,meaning , the ball is focal point for every changes in the world.Hence
;;the world is same as ball.

;;------------------------------------------------------------------------------
;;main : ball -> Image
;;The function applies big-bang to simulate the bounce game
;;GIVEN: ball
;;RETURNS: Image
;;EXAMPLES: 
;;STRATEGY: Function Composition
(define (main w)
  (big-bang w
            [to-draw  render        ]
            [on-tick  next-world    ]
            [on-key   key-handler   ]
            [on-mouse mouse-handler ]))

;;------------------------------------------------------------------------------
;;render : ball -> image
;;This function renders an image with details furnished by the incoming 
;;data from ball  
;;GIVEN: ball w
;;RETURN: image 
;;EXAMPLE: ( make-ball 150 20 "right" "down" 0 0 #false) -> 
;;         (place-image (circle 20 "solid" "black") 
;;                      150
;;                      20 
;;                      (empty-scene WIDTH HEIGHT))
;;STRATEGY: Data Decomposition on w: Ball
(define (render w)
  (cond
    [(pause? w)                               (paused-image w)   ]
    [(and (<= (ball-t w) 9) (> (ball-t w) 0)) (explosion-image w)] 
    [else                                     (normal-image w)   ]))

;;------------------------------------------------------------------------------
;;paused-image : ball -> image
;;This function renders an image with the game events paused. 
;;GIVEN: ball w
;;RETURN: image 
;;EXAMPLE: paused-image ( make-ball 150 20 "right" "down" 0 0 #true 0) -> 
;;         (place-image (overlay/align "left" "top" (text "*PAUSED*" 10 "black") 
;;         (place-image (circle 20 "solid" "black") 150 20 
;;                                                      (empty-scene 300 400)))
;;STRATEGY: Data Decomposition on w: Ball
(define (paused-image w)
  (overlay/align "right" "top" (score-image w)
                 (overlay/align "left" "top" (text "*PAUSED*" FONT-SIZE "black") 
  (normal-image w))))


(begin-for-test (check-equal? (paused-image 
                             (make-ball 150 20 "right" "down" 0 0 #true 2)) 
                              (overlay/align "right" "top" 
                                             (text 
                        (string-append "*count:" (number->string 2)) 16 "black")
                              (overlay/align "left" "top" 
                                             (text "*PAUSED*" FONT-SIZE "black") 
                                             (overlay/align "right" "top" 
                                             (text 
                        (string-append "*count:" (number->string 2)) 16 "black") 
                                             (place-image 
                                              (circle 20 "solid" "black") 
                                              150 
                                              20 
                                              (empty-scene 300 400))))) 
                "ball's image paused on the canvas"))

;;------------------------------------------------------------------------------
;;normal-image : ball -> image
;;This function renders an image with details furnished by the incoming 
;;data from ball when the game is unpaused and no explosions are going.  
;;GIVEN: ball w
;;RETURN: image 
;;EXAMPLE: normal-image ( make-ball 150 20 "right" "down" 0 0 #false 0) -> 
;;         (overlay/align "right" "top" (score-image w) 
;;         (place-image (circle 20 "solid" "black") 150 20 
;;                                                  (empty-scene 300 400)))
;;STRATEGY: Data Decomposition on w: Ball
(define (normal-image w)
  (overlay/align "right" "top" (score-image w) 
           (place-image BALL-IMAGE (ball-x w) (ball-y w) EMPTY-SCENE)))

(begin-for-test (check-equal? (normal-image 
                             (make-ball 150 20 "right" "down" 0 0 #false 2)) 
                              (overlay/align "right" "top" 
                                             (text 
                                    (string-append "*count:" (number->string 2)) 
                                               16 "black") 
                                             (place-image 
                                              (circle 20 "solid" "black") 
                                              150 
                                              20 
                                              (empty-scene 300 400)))
                "ball's image as in normal scenario"))

;;------------------------------------------------------------------------------
;;explosion-image : ball -> image
;;This function renders an image with radial start inside the ball to show an 
;;explosion inside the ball.  
;;GIVEN: ball w
;;RETURN: image 
;;EXAMPLE: explosion-image ( make-ball 150 20 "right" "down" 0 2 #false 2) -> 
;;          (place-image (overlay/align "middle" "middle" 
;;          (radial-star 12 (* 2 2) (* 4 2) "solid" "yellow")
;;          (circle 20 "solid" "black")) 150 20 (empty-scene 300 400)))
;;STRATEGY: Data Decomposition on w: Ball
(define (explosion-image w)
  (overlay/align "right" "top" (score-image w)
  (place-image (overlay/align "middle" "middle" (radial-star-image w)
                      BALL-IMAGE) (ball-x w) (ball-y w) EMPTY-SCENE)))

(begin-for-test (check-equal? (explosion-image 
                             (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                              (overlay/align "right" "top" 
                                             (text 
                        (string-append "*count:" (number->string 2)) 16 "black")
                              (place-image (overlay/align "middle" 
                                             "middle" 
                                             (radial-star 12 
                                                          (* 2 9) 
                                                          (* 4 9) 
                                                          "solid" 
                                                          "yellow")
                                             (circle 20 "solid" "black")) 
                              150 
                              20 
                              (empty-scene 300 400)))
                         "ball's image when explosion occured inside the ball"))

;;------------------------------------------------------------------------------
;;radial-star-image : ball -> image
;;This function renders a radial star with the radius details depending on the 
;;time elapsed since the start of explosion, recieved in the world ball.
;;GIVEN: ball w
;;RETURN: image 
;;EXAMPLE: radial-star-image ( make-ball 150 20 "right" "down" 0 2 #false 2) -> 
;;         (radial-star 12 (* 2 2) (* 4 2) "solid" "yellow"))
;;STRATEGY: Data Decomposition on w: Ball
(define (radial-star-image w) 
  (radial-star 12 (* 2 (ball-t w)) (* 4 (ball-t w)) "solid" "yellow"))

(begin-for-test (check-equal? (radial-star-image 
                            (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                            (radial-star 12 (* 2 9) (* 4 9) "solid" "yellow")
                          "radial start inside the ball when explosion occurs"))

;;------------------------------------------------------------------------------
;;score-image : ball -> image
;;This function creates an image of score using the incoming world ball 
;;GIVEN: ball w
;;RETURN: image 
;;EXAMPLE: score-image ( make-ball 150 20 "right" "down" 0 0 #false 2) -> 
;;         (text (string-append "count:" (number->string 2)) 10 "black"))
;;STRATEGY: Data Decomposition on w: Ball
(define (score-image w) 
  (text 
   (string-append "*count:" (number->string (ball-sc w))) FONT-SIZE "black"))

(begin-for-test (check-equal? (score-image 
                            (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                            (text 
                             (string-append "*count:" (number->string 2)) 
                             16 
                             "black")
                          "radial start inside the ball when explosion occurs"))

;;------------------------------------------------------------------------------
;;next-world : ball -> ball
;;This function takes in a ball and creates a new ball depending on the 
;;directions of the ball after each tick.It returns same state if PauseFlag is
;;true.
;;GIVEN: ball w0, current state of the ball
;;RETURN: ball w1, next state of the ball 
;;EXAMPLE: next-world (make-ball 150 20 "right"  "down" 0 0 #false) -> 
;;         (make-ball 156 20.5 "right" "down" 1 2 #false)
;;STRATEGY: Function Composition
(define (next-world w)
  (cond
    [(pause? w) w]
    [(direction-up-and-left? w)       (direction-up-and-left-handler w)] 
    [(direction-up-and-right? w)     (direction-up-and-right-handler w)]
    [(direction-down-and-left? w)   (direction-down-and-left-handler w)]
    [(direction-down-and-right? w) (direction-down-and-right-handler w)]))

(begin-for-test (check-equal? (next-world 
                            (make-ball 150 20 "right" "down" 0 9 #true 2)) 
                            (make-ball 150 20 "right" "down" 0 9 #true 2)
                          "ball's PauseFlag is set to #true"))

(begin-for-test (check-equal? (next-world 
                            (make-ball 150 20 "left" "up" 0 9 #false 2)) 
                            (direction-up-and-left-handler 
                              (make-ball 150 20 "left" "up" 0 9 #false 2))
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is up"))

(begin-for-test (check-equal? (next-world 
                            (make-ball 150 20 "left" "down" 0 9 #false 2)) 
                            (direction-down-and-left-handler 
                              (make-ball 150 20 "left" "down" 0 9 #false 2))
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (next-world 
                            (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                            (direction-down-and-right-handler 
                              (make-ball 150 20 "right" "down" 0 9 #false 2))
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (next-world 
                            (make-ball 150 20 "right" "up" 0 9 #false 2)) 
                            (direction-up-and-right-handler 
                              (make-ball 150 20 "right" "up" 0 9 #false 2))
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is up"))

;;------------------------------------------------------------------------------
;;direction-up-and-right? : ball -> boolean
;;This function returns #true if the x-direction of the ball is "right" 
;;and y-direction of the world is "up".  
;;GIVEN: ball w, current state
;;RETURN: boolean 
;;EXAMPLE: direction-up-and-right? (make-ball 150 20 "right"  "up" 0 0 #false)   
;;                                               ->                      #true
;;       : direction-up-and-right? (make-ball 150 20 "right"  "down" 0 0 #false) 
;;                                               ->                      #false
;;       : direction-up-and-right? (make-ball 150 20 "left"  "down" 0 0 #false )  
;;                                               ->                      #false
;;       : direction-up-and-right? (make-ball 150 20 "left"  "up" 0 0 #false)     
;;                                               ->                      #false
;;STRATEGY: Function Composition			
(define (direction-up-and-right? w)
	(and (up? w) (right? w)))

(begin-for-test (check-equal? (direction-up-and-right? 
                            (make-ball 150 20 "left" "up" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is up"))

(begin-for-test (check-equal? (direction-up-and-right? 
                            (make-ball 150 20 "left" "down" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-up-and-right? 
                            (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-up-and-right? 
                            (make-ball 150 20 "right" "up" 0 9 #false 2)) 
                            #true
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is up"))
	
;;------------------------------------------------------------------------------
;;direction-down-and-right? : ball -> boolean
;;This function returns #true if the x-direction of the ball is "right" 
;;and y-direction of the world is "down".  
;;GIVEN: ball w, current state
;;RETURN: boolean 
;;EXAMPLE: direction-down-and-right? (make-ball 150 20 "right"  "up" 0 0 #false)  
;;                                         ->                            #false
;;       direction-down-and-right? (make-ball 150 20 "right"  "down" 0 0 #false)
;;                                         ->                            #true
;;       direction-down-and-right? (make-ball 150 20 "left"  "down" 0 0 #false) 
;;                                         ->                            #false
;;       direction-down-and-right? (make-ball 150 20 "left"  "up" 0 0 #false)   
;;                                         ->                            #false
;;STRATEGY: Function Composition
(define (direction-down-and-right? w)
	(and (down? w) (right? w)))

(begin-for-test (check-equal? (direction-down-and-right? 
                            (make-ball 150 20 "left" "up" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is up"))

(begin-for-test (check-equal? (direction-down-and-right? 
                            (make-ball 150 20 "left" "down" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-down-and-right? 
                            (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                            #true
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-down-and-right? 
                            (make-ball 150 20 "right" "up" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is up"))
	
;;------------------------------------------------------------------------------
;;direction-up-and-left? : ball -> boolean
;;This function returns #true if the x-direction of the ball is "left" 
;;and y-direction of the world is "up".  
;;GIVEN: ball w, current state
;;RETURN: boolean 
;;EXAMPLE: direction-up-and-left? (make-ball 150 20 "right"  "up" 0 0 #false)    
;;                                           ->                          #false
;;       : direction-up-and-left? (make-ball 150 20 "right"  "down" 0 0 #false)  
;;                                           ->                          #false
;;       : direction-up-and-left? (make-ball 150 20 "left"  "down" 0 0 #false)   
;;                                           ->                          #false
;;       : direction-up-and-left? (make-ball 150 20 "left"  "up" 0 0 #false)    
;;                                           ->                          #true
;;STRATEGY: Function Composition
(define (direction-up-and-left? w)
	(and (up? w) (left? w)))

(begin-for-test (check-equal? (direction-up-and-left? 
                            (make-ball 150 20 "left" "up" 0 9 #false 2)) 
                            #true
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is up"))

(begin-for-test (check-equal? (direction-up-and-left? 
                            (make-ball 150 20 "left" "down" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-up-and-left? 
                            (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-up-and-left? 
                            (make-ball 150 20 "right" "up" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is up"))
;;------------------------------------------------------------------------------
;;direction-down-and-left? : ball -> boolean
;;This function returns #true if the x-direction of the ball is "left" 
;;and y-direction of the world is "down".  
;;GIVEN: ball w, current state
;;RETURN: boolean 
;;EXAMPLE: direction-down-and-left? (make-ball 150 20 "right"  "up" 0 0 #false)    
;;                                            ->                         #false
;;       direction-down-and-left? (make-ball 150 20 "right"  "down" 0 0 #false)  
;;                                            ->                         #false
;;       direction-down-and-left? (make-ball 150 20 "left"  "down" 0 0 #false)   
;;                                            ->                         #true
;;       direction-down-and-left? (make-ball 150 20 "left"  "up" 0 0 #false)     
;;                                            ->                         #false
;;STRATEGY: Function Composition
(define (direction-down-and-left? w)
	(and (down? w) (left? w)))

(begin-for-test (check-equal? (direction-down-and-left? 
                            (make-ball 150 20 "left" "up" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is up"))

(begin-for-test (check-equal? (direction-down-and-left? 
                            (make-ball 150 20 "left" "down" 0 9 #false 2)) 
                            #true
                          "ball's HorizontalDirection is left and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-down-and-left? 
                            (make-ball 150 20 "right" "down" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is down"))

(begin-for-test (check-equal? (direction-down-and-left? 
                            (make-ball 150 20 "right" "up" 0 9 #false 2)) 
                            #false
                          "ball's HorizontalDirection is right and 
                                VerticalDirection is up"))
	
;;------------------------------------------------------------------------------
;;going-past-left-wall? : ball -> boolean
;;Returns #true if the ball is going past the left wall of the 
;;canvas else returns #false.  
;;GIVEN: ball w, current state
;;RETURN: boolean 
;;EXAMPLE: going-past-left-wall? (make-ball 10 20 "left"  "up" 0 0 #false)
;;                                                                    -> #true
;;       : going-past-left-wall? (make-ball 150 20 "left"  "down" 0 0 #false)
;;                                                                    -> #false
;;STRATEGY: Data Decomposition on w: Ball 
(define (going-past-left-wall? w)
	(>= LEFT-WALL (- (ball-x w) HORIZONTAL-DISPLACEMENT)))

(begin-for-test (check-equal? (going-past-left-wall? 
                            (make-ball 10 20 "left"  "up" 0 0 #false 0)) 
                            #true
                          "ball's x coordinate is such that the ball is  
                             partially outside the left wall"))

(begin-for-test (check-equal? (going-past-left-wall? 
                            (make-ball 150 20 "left"  "down" 0 0 #false 0)) 
                            #false
                          "ball's x coordinate is such that the ball is  
                             completely inside the left wall"))
	
;;------------------------------------------------------------------------------
;;going-past-right-wall? : ball -> boolean
;;Returns #true if the ball is going past the right wall of the 
;;canvas else returns #false.  
;;GIVEN: ball w, current state
;;RETURN: boolean 
;;EXAMPLE: going-past-right-wall? (make-ball 290 20 "right"  "up" 0 0 #false 0)   
;;                                           ->                          #true
;;       :going-past-right-wall? (make-ball 150 20 "right"  "down" 0 0 #false 0) 
;;                                           ->                          #false
;;STRATEGY: Data Decomposition on w: Ball
(define (going-past-right-wall? w)
	(<= RIGHT-WALL (+ (ball-x w) HORIZONTAL-DISPLACEMENT)))

(begin-for-test (check-equal? (going-past-right-wall? 
                            (make-ball 290 20 "right"  "up" 0 0 #false 0)) 
                            #true
                          "ball's x coordinate is such that the ball  is  
                             partially outside the right wall"))

(begin-for-test (check-equal? (going-past-right-wall? 
                            (make-ball 150 20 "left"  "down" 0 0 #false 0)) 
                            #false
                          "ball's x coordinate is such that the ball is  
                             completely inside the right wall"))
	
;;------------------------------------------------------------------------------
;;CurrentVelocity-zero? : ball -> boolean
;;Returns #true if the velocity of the ball is equal to zero else returns #false
;;GIVEN: ball w, current state
;;RETURN: boolean 
;;EXAMPLE: currentvelocity-zero? (make-ball 150 20 "right"  "up" 0 0 #false 0)
;;                                           ->                           #true
;;       currentvelocity-zero? (make-ball 150 20 "right"  "down" 10 10 #false 0) 
;;                                           ->                           #false
;;STRATEGY: Data Decomposition on w: Ball
(define (currentvelocity-zero? w)
	(<= (ball-v w) ZERO))

(begin-for-test (check-equal? (currentvelocity-zero? 
                            (make-ball 150 20 "right"  "up" 0 0 #false 0)) 
                            #true
                          "ball's CurrentVelocity is zero"))

(begin-for-test (check-equal? (currentvelocity-zero? 
                            (make-ball 150 20 "right"  "down" 10 0 #false 0)) 
                            #false
                          "ball's CurrentVelocity is nonZero"))
	
;;------------------------------------------------------------------------------
;;going-past-floor? : ball -> boolean
;;Returns #true if any part of the ball is outside or touching the floor of 
;;the canvas otherwise it returns #false.
;;GIVEN: ball w, current sate
;;RETURN: boolean 
;;EXAMPLE: going-past-floor? (make-ball 150 390 "right"  "up" 0 0 #false 0) 
;;                                      ->                                #true
;;       : going-past-floor? (make-ball 150 20 "right"  "down" 10 10 #false 0) 
;;                                      ->                               #false
;;STRATEGY: Data Decomposition on w: Ball
(define (going-past-floor? w)
	(< FLOOR (+ (ball-y w) (y-distance w))))

(begin-for-test (check-equal? (going-past-floor? 
                            (make-ball 150 390 "right"  "up" 0 0 #false 0)) 
                            #true
                          "ball's y coordinate is such that the ball is  
                             partially outside the floor"))

(begin-for-test (check-equal? (going-past-floor? 
                            (make-ball 150 20 "right"  "down" 10 10 #false 0)) 
                            #false
                          "ball's y coordinate is such that the ball is  
                             completely inside the floor"))
	
;;------------------------------------------------------------------------------
;;going-past-left-wall-handler : ball -> ball
;;This function will switch the HorizontalDirection of the ball from "left" 
;;to "right" and reset the x-coordinate of the ball to the LEFT-WALL. 
;;GIVEN: ball w, current ball state
;;RETURN: ball w1, next ball state with HorizontalDireection and x-coordinate
;;reset. 
;;EXAMPLE: going-past-left-wall-handler 
;;          (make-ball 10 20 "left"  "up" 0 0 #false 0) 
;;    ->    (make-ball 20 20 "right"  "up" 0 0 #false 0)
;;STRATEGY: Data Decomposition on w: Ball
(define (going-past-left-wall-handler w)
	(make-ball LEFT-WALL 
                   (ball-y w) 
                   RIGHT 
                   (ball-y-dir w) 
                   (ball-v w) 
                   (next-tick-value w)
                   UNPAUSED
                   (ball-sc w)))

(begin-for-test (check-equal? (going-past-left-wall-handler 
                            (make-ball 10 20 "left"  "up" 0 0 #false 0)) 
                            (make-ball 20 20 "right"  "up" 0 0 #false 0)
                          "ball's x coordinate is such that the ball is  
                             partially outside the left wall"))

;;------------------------------------------------------------------------------
;;going-past-right-wall-handler : ball -> ball
;;If any part of the ball is touching or going past the right wall this 
;;function will switch the x-direction of the ball from "right" to "left" and 
;;reset the x-coordinate of the ball to the RIGHT-WALL. 
;;GIVEN: ball w, current ball state
;;RETURN: ball w1, next ball state 
;;EXAMPLE: going-past-left-wall-handler
;;                            (make-ball 290 20 "right"  "up" 0 0 #false 0)
;;  ->                        (make-ball 280 20 "left"  "up" 0 0 #false 0)
;;STRATEGY: Data Decomposition on w: Ball
(define (going-past-right-wall-handler w)
	(make-ball RIGHT-WALL 
                   (ball-y w) 
                   LEFT 
                   (ball-y-dir w) 
                   (ball-v w) 
                   (next-tick-value w)
                   UNPAUSED
                   (ball-sc w)))

(begin-for-test (check-equal? (going-past-right-wall-handler 
                            (make-ball 290 20 "right"  "up" 0 0 #false 0)) 
                            (make-ball 280 20 "left"  "up" 0 0 #false 0)
                          "ball's x coordinate is such that the ball  is  
                             partially outside the right wall"))

;;------------------------------------------------------------------------------
;;currentvelocity-zero-handler : ball -> ball
;;If the velocity of the ball becomes zero this function will switch 
;;the y-direction of the ball from "up" to "down".
;;And it will do the Horizontal displacement as usual.
;;GIVEN: ball w, current ball state
;;RETURN: ball w1, next world state 
;;EXAMPLE: currentvelocity-zero-handler
;;        (make-ball 150 20 "right"  "up" 0 0 #false 0)
;; ->     (make-ball 153 20 "right"  "down" 0 0 #false 0)
;;       : currentvelocity-zero-handler
;;        (make-ball 150 20 "left"  "up" 0 0 #false 0)
;; ->     (make-ball 147 20 "left"  "down" 0 0 #false 0)
;;STRATEGY: Data Decomposition on w: Ball

(define (currentvelocity-zero-handler w)
	( cond 
           [(left? w) 
		(make-ball (- (ball-x w) HORIZONTAL-DISPLACEMENT)   
                           (ball-y w) 
                           (ball-x-dir w) 
                           DOWN 
                           (ball-v w) 
                           (next-tick-value w)
                           UNPAUSED
                           (ball-sc w))]
           [(right? w) 
		(make-ball (+ (ball-x w) HORIZONTAL-DISPLACEMENT)   
                           (ball-y w) 
                           (ball-x-dir w) 
                           DOWN 
                           (ball-v w) 
                           (next-tick-value w)
                           UNPAUSED
                           (ball-sc w))]))

(begin-for-test (check-equal? (currentvelocity-zero-handler 
                            (make-ball 150 20 "right"  "up" 0 0 #false 0)) 
                            (make-ball 153 20 "right"  "down" 0 0 #false 0)
                          "ball's CurrentVelocity is zero and 
                           HorizontalDirection is right"))

(begin-for-test (check-equal? (currentvelocity-zero-handler 
                            (make-ball 150 20 "left"  "up" 0 0 #false 0)) 
                            (make-ball 147 20 "left"  "down" 0 0 #false 0)
                          "ball's CurrentVelocity is zero and 
                           HorizontalDirection is left"))

;;------------------------------------------------------------------------------
;;going-past-floor-handler : ball -> ball
;;If any part of the ball is touching or going past the floor of the canvas  
;;this function will switch the y-direction of the ball from "down" to "up"
;;and set the new velocity to the 90% of the ball's velocity at touchdown. 
;;GIVEN: ball w, current state of the world ball
;;RETURN: ball w1, next state of the world ball 
;;EXAMPLE: going-past-floor-handler
;;         (make-ball 150 390 "right"  "down" 10 0 #false 0)
;;  ->     (make-ball 150 380 "right"  "up" (exact->inexact 8.524799999999999)
;;                              0 #false 0)
;;STRATEGY: Function Composition
(define (going-past-floor-handler w)
	(make-ball (ball-x w ) 
                   FLOOR 
                   (ball-x-dir w) 
                   UP 
                   (* BOUNCE-CO-EFFICIENT (velocity-when-touchdown w)) 
                   (next-tick-value w)
                   UNPAUSED
                   ZERO))

(begin-for-test (check-equal? (going-past-floor-handler 
                            (make-ball 150 390 "right"  "down" 10 0 #false 0)) 
                            (make-ball 150 380 "right"  "up" 
                             (exact->inexact 8.524799999999999)
                              0 #false 0)
                          "ball's y coordinate is such that the ball is  
                             partially outside the right wall"))

;;------------------------------------------------------------------------------
;;velocity-when-touchdown : ball -> Real
;;This function will calculate the velocity of the ball at touchdown. 
;;GIVEN: ball w, current ball state
;;RETURN: Real v , velocity at touchdown 
;;EXAMPLE: velocity-when-touchdown
;;                 (make-ball 150 380 "right"  "down" 10 0 #false 0) -> 10.0 
;;STRATEGY: Data Decomposition on w: Ball
(define (velocity-when-touchdown w)
  (round/eps (+ (ball-v w) (* GRAVITY (time-to-touchdown w)))))

(begin-for-test (check-= (velocity-when-touchdown 
                            (make-ball 150 380 "right"  "down" 10 0 #false 0)) 
                            10.0
                            EP
                         "ball's velocity while it is flush against the floor"))

;;------------------------------------------------------------------------------
;;time-to-touchdown : ball -> NonNegReal
;;This function finds the time the ball takes to touch the floor from the   
;;current y-coordinate and velocity. 
;;GIVEN: ball w, current state
;;RETURN: NonNegReal, time the ball takes to touch the floor 
;;EXAMPLE: time-to-touchdown(make-ball 150 370 "left"  "up" 10 0 #false 0) 
;;  ->     0.477
;;STRATEGY: Data Decomposition on b: ball
(define (time-to-touchdown w)
  (round/eps (/ (+  (* -1(ball-v w))  (sqrt (discriminant w))) (* 2 GRAVITY))))

(begin-for-test (check-= (time-to-touchdown  
                            (make-ball 150 370 "left"  "up" 10 0 #false 0)) 
                            0.477
                            EP
                          "ball's is about to touch the floor"))
    
;;------------------------------------------------------------------------------
;;discriminant : ball -> Real
;;This function calculates the discriminant for the quadratic equiation to   
;;find the time to touchdown. 
;;GIVEN: ball w, current state
;;RETURN: Real d, discriminant for the kinematic quadratic equation 
;;EXAMPLE: discriminant(make-ball 150 370 "left"  "up" 10 0 #false 0) 
;;  ->     120.0
;;STRATEGY: Data Decomposition on b: ball
(define (discriminant w)
 (round/eps (- (sqr (ball-v w)) (* 4 0.5 GRAVITY (* -1 (- FLOOR (ball-y w)))))))

(begin-for-test (check-= (discriminant  
                            (make-ball 150 370 "left"  "up" 10 0 #false 0)) 
                            120.0
                            EP
                          "discriminant to find the time to touchdown"))

;;------------------------------------------------------------------------------
;;direction-up-and-left-handler : ball -> ball
;;This function checks if the ball is passing through the left wall or the   
;;the vertical velocity becomes zero and call appropriate handlers else the ball
;;is moved accordingly.
;;GIVEN: ball w, Current ball state.
;;RETURN: ball w1, next ball state with respective changes. 
;;EXAMPLE:direction-up-and-left-handler
;;                            (make-ball 10 20 "left"  "up" 0 0 #false 0) 
;;                     ->     (make-ball 20 20 "right"  "up" 0 0 #false 0)
;;        direction-up-and-left-handler
;;                            (make-ball 150 20 "left"  "up" 0 0 #false 0) 
;;                     ->     (make-ball 147 20 "left"  "down" 0 0 #false 0)
;;        direction-up-and-left-handler
;;                            (make-ball 200 200 "left"  "up" 2 0 #false 0) 
;;                     ->     (make-ball 197 (exact->inexact 198.5) "left"  "up"
;;                                       (exact->inexact 1.0) 0 #false 0)
;;STRATEGY: Data Decomposition on w: ball
(define (direction-up-and-left-handler w)
	(cond 
          [ (going-past-left-wall? w) (going-past-left-wall-handler w)]
          [ (currentvelocity-zero? w) (currentvelocity-zero-handler w)]
          [ else (make-ball (- (ball-x w) HORIZONTAL-DISPLACEMENT) 
                            (- (ball-y w) (y-distance w)) 
                            LEFT 
                            (ball-y-dir w) 
                            (new-velocity w) 
                            (next-tick-value w)
                            UNPAUSED
                            (ball-sc w))]))

(begin-for-test (check-equal? (direction-up-and-left-handler 
                            (make-ball 10 20 "left"  "up" 0 0 #false 0)) 
                            (make-ball 20 20 "right"  "up" 0 0 #false 0)
                          "ball's x coordinate is such that the ball is  
                             partially outside the left wall"))

(begin-for-test (check-equal? (direction-up-and-left-handler 
                            (make-ball 150 20 "left"  "up" 0 0 #false 0)) 
                            (make-ball 147 20 "left"  "down" 0 0 #false 0)
                          "ball's CurrentVelocity is zero"))

(begin-for-test (check-equal? (direction-up-and-left-handler 
                            (make-ball 200 200 "left"  "up" 2 0 #false 0)) 
                            (make-ball 197 (exact->inexact 198.5) "left"  "up"
                                       (exact->inexact 1.0) 0 #false 0)
                          "ball's HorizontalDirection is left and 
                            VerticalDirection  is up and the ball is 
                             well inside the boundaries"))

;;------------------------------------------------------------------------------
;;direction-up-and-right-handler : ball -> ball
;;This function checks if the ball is passing through the right wall or the   
;;the vertical velocity becomes zero and call appropriate handlers else the ball
;;is moved accordingly.
;;GIVEN: ball w0
;;RETURN: ball w1 
;;EXAMPLE:direction-up-and-right-handler
;;             (make-ball 200 200 "right"  "up" 2 0 #false 0) ->
;;                         (make-ball 203 (exact->inexact 198.5) 
;;                                           "right"  "up"                 
;;                                        (exact->inexact 1.0) 0 #false 0)
;;         direction-up-and-right-handler
;;             (make-ball 290 20 "right"  "up" 0 0 #false 0) ->
;;                         (make-ball 280 20 "left"  "up" 0 0 #false 0)
;;         direction-up-and-right-handler
;;             (make-ball 150 20 "right"  "up" 0 0 #false 0) ->
;;                         (make-ball 153 20 "right"  "down" 0 0 #false 0)
;;STRATEGY: Data Decomposition on w: ball
(define (direction-up-and-right-handler w)
	(cond  
          [ (going-past-right-wall? w) (going-past-right-wall-handler w)]
          [ (currentvelocity-zero? w) (currentvelocity-zero-handler w)]
          [ else (make-ball (+ (ball-x w) HORIZONTAL-DISPLACEMENT) 
                            (- (ball-y w) (y-distance w)) 
                            RIGHT 
                            (ball-y-dir w) 
                            (new-velocity w) 
                            (next-tick-value w)
                            UNPAUSED
                            (ball-sc w))]))

(begin-for-test (check-equal? (direction-up-and-right-handler 
                            (make-ball 290 20 "right"  "up" 0 0 #false 0)) 
                            (make-ball 280 20 "left"  "up" 0 0 #false 0)
                          "ball's x coordinate is such that the ball  is  
                             partially outside the right wall"))

(begin-for-test (check-equal? (direction-up-and-right-handler 
                            (make-ball 150 20 "right"  "up" 0 0 #false 0)) 
                            (make-ball 153 20 "right"  "down" 0 0 #false 0)
                          "ball's CurrentVelocity is zero"))

(begin-for-test (check-equal? (direction-up-and-right-handler 
                            (make-ball 200 200 "right"  "up" 2 0 #false 0)) 
                            (make-ball 203 (exact->inexact 198.5) "right"  "up"
                                       (exact->inexact 1.0) 0 #false 0)
                          "ball's HorizontalDirection is right and 
                            VerticalDirection  is up and the ball is 
                             well inside the boundaries"))
	
;;------------------------------------------------------------------------------
;;direction-down-and-left-handler : ball -> ball
;;This function checks if the ball is passing through the left wall or the   
;;is passing through the floor and call appropriate handlers else ball is moved
;;accordingly.
;;GIVEN: ball w0
;;RETURN: ball w1 
;;EXAMPLE:direction-down-and-left-handler(make-ball 280 20 "left" "up" 0 9 
;;                                                                       #false)
;;  ->     (make-ball 280 20 "left" "down" 0 0 #false)
;;STRATEGY: Data Decomposition on w: ball
(define (direction-down-and-left-handler w)
	(cond
          [ (going-past-left-wall? w) (going-past-left-wall-handler w)] 
          [ (going-past-floor? w) (going-past-floor-handler w)]
          [ else (make-ball (- (ball-x w) HORIZONTAL-DISPLACEMENT) 
                           (+ (ball-y w) (y-distance w))
                           (ball-x-dir w) 
                           (ball-y-dir w) 
                           (new-velocity w) 
                           (next-tick-value w)
                           UNPAUSED
                           (ball-sc w))]))

(begin-for-test (check-equal? (direction-down-and-left-handler 
                            (make-ball 10 20 "left"  "down" 0 0 #false 0)) 
                            (make-ball 20 20 "right"  "down" 0 0 #false 0)
                          "ball's x coordinate is such that the ball is  
                             partially outside the left wall"))

(begin-for-test (check-equal? (direction-down-and-left-handler 
                            (make-ball 150 390 "left"  "down" 10 0 #false 0)) 
                            (make-ball 150 380 "left"  "up" 
                            (exact->inexact 8.524799999999999) 0 #false 0)
                          "ball's y coordinate is such that the ball is  
                             partially outside the floor "))

(begin-for-test (check-equal? (direction-down-and-left-handler 
                            (make-ball 200 200 "left"  "down" 10 0 #false 0)) 
                            (make-ball 197 (exact->inexact 211.5) "left"  "down" 
                             (exact->inexact 11.0)
                              0 #false 0)
                          "ball's HorizontalDirection is left and 
                            VerticalDirection  is down and the ball is 
                             well inside the boundaries"))

;;------------------------------------------------------------------------------
;;direction-down-and-right-handler : ball -> ball
;;This function checks if the ball is passing through the right wall or the   
;;is passing through the floor and call appropriate handlers else ball is moved
;;accordingly. 
;;GIVEN: ball w0
;;RETURN: ball w1 
;;EXAMPLE: direction-down-and-right-handler
;;         (make-ball 290 20 "right"  "up" 0 0 #false 0) ->  
;;         (make-ball 280 20 "left"  "up" 0 0 #false 0)
;;         direction-down-and-right-handler
;;         (make-ball 150 390 "right"  "down" 10 0 #false 0) ->  
;;         (make-ball 150 380 "right"  "up" 
;;                            (exact->inexact 8.524799999999999) 0 #false 0)
;;          direction-down-and-right-handler
;;          (make-ball 200 200 "right" "down" 10 0 #false 0)
;;           (make-ball 203 (exact->inexact 211.5) "right" "down" 
;;                             (exact->inexact 11.0)
;;                              0 #false 0)
;;STRATEGY: Data Decomposition
(define (direction-down-and-right-handler w)
	(cond
          [ (going-past-right-wall? w) (going-past-right-wall-handler w)] 
          [ (going-past-floor? w) (going-past-floor-handler w)]
          [ else (make-ball (+ (ball-x w) HORIZONTAL-DISPLACEMENT) 
                            (+ (ball-y w) (y-distance w)) 
                            (ball-x-dir w) 
                            (ball-y-dir w) 
                            (new-velocity w) 
                            (next-tick-value w)
                            UNPAUSED
                            (ball-sc w))]))

(begin-for-test (check-equal? (direction-down-and-right-handler 
                            (make-ball 290 20 "right"  "up" 0 0 #false 0)) 
                            (make-ball 280 20 "left"  "up" 0 0 #false 0)
                          "ball's x coordinate is such that the ball  is  
                             partially outside the right wall"))

(begin-for-test (check-equal? (direction-down-and-right-handler 
                            (make-ball 150 390 "right"  "down" 10 0 #false 0)) 
                            (make-ball 150 380 "right"  "up" 
                            (exact->inexact 8.524799999999999) 0 #false 0)
                          "ball's y coordinate is such that the ball is  
                             partially outside the floor of the canvas"))

(begin-for-test (check-equal? (direction-down-and-right-handler 
                            (make-ball 200 200 "right" "down" 10 0 #false 0)) 
                            (make-ball 203 (exact->inexact 211.5) "right" "down" 
                             (exact->inexact 11.0)
                              0 #false 0)
                          "ball's HorizontalDirection is right and 
                            VerticalDirection  is down and the ball is 
                             well inside the boundaries"))

;;------------------------------------------------------------------------------
;;y-distance : Ball -> Real
;;This function calculates the vertical displacement of the ball using the 
;;new velocity.  
;;GIVEN: ball w, current state
;;RETURN: Real y, calculated displacement 
;;EXAMPLE: y-distance (make-ball 200 200 "right" "down" 10 0 #false 0) -> 11.5
;;       : y-distance "up" -> #false
;;STRATEGY: Data Decomposition
(define (y-distance w)
    (round/eps (+(* (new-velocity w) 1) (* 0.5 GRAVITY ))))

(begin-for-test (check-= (y-distance 
                            (make-ball 200 200 "right" "down" 10 0 #false 0)) 
                            11.5
                            EP
                          "ball's HorizontalDirection is right and 
                            VerticalDirection  is down and the ball is 
                             well inside the boundaries"))

;;------------------------------------------------------------------------------
;;new-velocity : Ball -> Real
;;Calculates the new velocity from the current velocity of the ball and rounds
;;it to nearest three decimals.  
;;GIVEN: ball w, current ball state
;;RETURN: real  
;;EXAMPLE: new-velocity (make-ball 200 200 "right" "up" 10 0 #false 0) -> 9.0
;;       : new-velocity (make-ball 200 200 "right" "down" 10 0 #false 0) ->11.0
;;STRATEGY: Data Decomposition on w: ball
(define (new-velocity w)
  (cond
      [(up? w) (round/eps (+ (ball-v w) (* -1 GRAVITY )))]
      [(down? w) (round/eps (+ (ball-v w) GRAVITY ))]))

(begin-for-test (check-= (new-velocity 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)) 
                            9.0
                            EP
                            "ball's VerticalDirection  is up and the ball is 
                             well inside the boundaries"))

(begin-for-test (check-= (new-velocity 
                            (make-ball 200 200 "right" "down" 10 0 #false 0)) 
                            11.0
                            EP 
                          "ball's VerticalDirection  is down and the ball is 
                             well inside the boundaries"))


;;------------------------------------------------------------------------------
;;key-handler : ball KeyEvent -> ball
;;checks the PauseFlag and the KeyEvent to run appropriate handlers.  
;;GIVEN: ball w, KeyEvent ke
;;RETURN: Ball w1, next ball state 
;;EXAMPLE: key-handler (make-ball 200 200 "right" "up" 10 0 #false 0) "p" -> 
;;                     (make-ball 200 200 "right" "up" 10 0 #true 0)
;;       : key-handler (make-ball 200 200 "right" "down" 10 0 #false 0) "e" -> 
;;                      (make-ball 200 200 "right" "down" 10 0 #false 0)
;;       : key-handler (make-ball 200 200 "right" "down" 10 0 #true 0) "p" -> 
;;                      (make-ball 200 200 "right" "down" 10 0 #false 0)
;;STRATEGY: Data Decomposition on w: ball. 
(define (key-handler w ke)
  (cond 
    [(and (p-pressed? ke) (unpause? w)) (pause-pressed-handler w)]
    [(and (p-pressed? ke) (pause? w)) (unpause-pressed-handler w)]
    [else w]))

(begin-for-test (check-equal? (key-handler 
                            (make-ball 200 200 "right" "up" 10 0 #false 0) "p") 
                            (make-ball 200 200 "right" "up" 10 0 #true 0)
                           "ball's PauseFlag is set to false and p is pressed"))

(begin-for-test (check-equal? (key-handler 
                           (make-ball 200 200 "right" "down" 10 0 #false 0) "e") 
                            (make-ball 200 200 "right" "down" 10 0 #false 0)
                           "ball's PauseFlag is set to false and e is pressed"))

(begin-for-test (check-equal? (key-handler 
                           (make-ball 200 200 "right" "down" 10 0 #true 0) "p") 
                            (make-ball 200 200 "right" "down" 10 0 #false 0)
                           "ball's PauseFlag is set to true and p is pressed"))

;;------------------------------------------------------------------------------
;;p-pressed? : KeyEvent -> boolean
;;This function returns #true if the key "p" is pressed, it returns 
;;false for every other key presses.
;;GIVEN: KeyEvent ke, key pressed in the key board
;;RETURN: boolean 
;;EXAMPLE: p-pressed? "p"   -> #true 
;;       : p-pressed? "e"   -> #false 
;;STRATEGY: Data Decomposition on ke: Keyevent
(define (p-pressed? ke)
	(string=? "p" ke))

(begin-for-test (check-equal? (p-pressed? "p") 
                            #true
                           "KeyEvent p occurs"))

(begin-for-test (check-equal? (p-pressed? "e") 
                            #false
                           "Keyevent other than p occurs"))

;;------------------------------------------------------------------------------
;;pause-pressed-handler : ball -> ball
;;This function sets the PauseFlag to #true for the incoming ball. 
;;GIVEN: ball w, current ball state
;;RETURN: ball w1, next ball state 
;;EXAMPLE: pause-pressed-handler (make-ball 200 200 "right" "up" 10 0 #false 0)
;;                         -> (make-ball 200 200 "right" "up" 10 0 #true 0) 
;;STRATEGY: Data Decomposition on w: ball
(define (pause-pressed-handler w)
	(make-ball (ball-x w) 
                   (ball-y w) 
                   (ball-x-dir w) 
                   (ball-y-dir w) 
                   (ball-v w) 
                   (ball-t w) 
                   PAUSED
                   (ball-sc w)))

(begin-for-test (check-equal? (pause-pressed-handler 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)) 
                            (make-ball 200 200 "right" "up" 10 0 #true 0)
                           "incoming ball's PauseFlag is set to #false"))

;;------------------------------------------------------------------------------
;;unpause-pressed-handler : ball -> ball
;;This function sets the PauseFlag to #false for the incoming ball. 
;;GIVEN: ball w
;;RETURN: ball w 
;;EXAMPLE: unpause-pressed-handler "p"   -> #true 
;;       : p-pressed? "e"   -> #false 
;;STRATEGY: Data Decomposition
(define (unpause-pressed-handler w)
	(make-ball (ball-x w) 
                   (ball-y w) 
                   (ball-x-dir w) 
                   (ball-y-dir w) 
                   (ball-v w) 
                   (ball-t w) 
                   UNPAUSED
                   (ball-sc w)))

(begin-for-test (check-equal? (unpause-pressed-handler 
                            (make-ball 200 200 "right" "up" 10 0 #true 0)) 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                           "ball's PauseFlag is set to #false"))

;;------------------------------------------------------------------------------
;;world-paused? : ball -> ball
;;This function sets the PauseFlag to #false for the incoming ball. 
;;GIVEN: ball w
;;RETURN: ball w 
;;EXAMPLE: world-paused? "p"   -> #true 
;;       : world-paused? "e"   -> #false 
;;STRATEGY: Data Decomposition
(define (world-paused? w)
  (ball-p w))

(begin-for-test (check-equal? (world-paused? 
                            (make-ball 200 200 "right" "up" 10 0 #true 0)) 
                            #true
                           "incoming ball's PauseFlag is set to #true"))

(begin-for-test (check-equal? (world-paused? 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)) 
                            #false
                           "incoming ball's PauseFlag is set to #false"))

;;------------------------------------------------------------------------------
;;round/eps : Real -> Real
;;Rounds x to within EP precision
;;GIVEN: Real x
;;RETURN: Real y 
;;EXAMPLE: round/eps 1.59998767   -> #i1.6 
;;STRATEGY: Data Decomposition
(define (round/eps x)
  (exact->inexact (* (inexact->exact (round (/ x EP))) EP)))
  
(begin-for-test (check-= (round/eps 1.59998767) #i1.6 0.001
                           "incoming exact number with floating decimal"))

;;------------------------------------------------------------------------------
;;mouse-handler : ball coOrdinate coOrdinate MouseEvent -> ball
;;This function reacts to the mouse events captured during the simulation 
;;GIVEN: ball w, current state 
;;       coOrdinate x , x-coordinate of the MouseEvent
;;       coOrdinate y , y-coordinate of the MouseEvent
;;       MouseEvent me , MouseEvent occured
;;RETURN: ball w1, next state depending on the mouseevent. 
;;EXAMPLE: mouse-handler (make-ball 200 200 "right" "up" 10 0 #false 0)
;;                           200 200 "button-down")   -> 
;;                       (make-ball 200 200 "right" "up" 0 1 #false 1) 
;;       : mouse-handler (make-ball 200 200 "right" "up" 10 0 #false 0)
;;                            200 200 "button-up")   -> 
;;                       (make-ball 200 200 "right" "up" 10 0 #false 0) 
;;STRATEGY: Data Decomposition on me MouseEvent
(define (mouse-handler w x y me)
  (cond
    [(string=? me "button-down") (mouse-button-down-handler w x y)]
    [else w]))

(begin-for-test (check-equal? (mouse-handler 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            200 200 "button-down") 
                            (make-ball 200 200 "right" "up" 0 1 #false 1)
                           "a MouseEvent button-down occurs "))

(begin-for-test (check-equal? (mouse-handler 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            200 200 "button-up") 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                           "a MouseEvent other than button-down occurs"))

;;------------------------------------------------------------------------------
;;mouse-button-down-handler : ball -> ball
;;This function returns the same ball state if the paused, starts explosion
;;if mouse click is inside the ball otherwise returns the same bal state.
;;GIVEN: ball w, current state
;;RETURN: ball w1, next state depending on the mouseevent. 
;;EXAMPLE: mouse-handler (make-ball 200 200 "right" "up" 10 0 #true 0) 200 200)
;;                    -> (make-ball 200 200 "right" "up" 10 0 #true 0) 
;;       : mouse-handler (make-ball 200 200 "right" "up" 10 0 #false 0) 150 20)
;;                    -> (make-ball 200 200 "right" "up" 10 0 #false 0)
;;       : mouse-handler (make-ball 200 200 "right" "up" 10 0 #false 0) 200 200)
;;                    -> (make-ball 200 200 "right" "up" 0 1 false 1)       
;;STRATEGY: Data Decomposition on w: ball
(define (mouse-button-down-handler w x y)
  (cond
    [(pause? w) w]
    [(mouse-click-inside-ball? w x y) (start-explosion w)]
    [else w]))

(begin-for-test (check-equal? (mouse-button-down-handler 
                            (make-ball 200 200 "right" "up" 10 0 #true 0)
                            200 200) 
                            (make-ball 200 200 "right" "up" 10 0 #true 0)
                           "a MouseEvent button-down occurs but the PauseFlag
                              is sest to #true"))

(begin-for-test (check-equal? (mouse-button-down-handler 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            150 20 ) 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                           "a MouseEvent button-down occurs but outside 
                             the ball area"))

(begin-for-test (check-equal? (mouse-button-down-handler 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            200 200 ) 
                            (make-ball 200 200 "right" "up" 0 1 false 1)
                           "a MouseEvent button-down occurs and inside 
                             the ball area"))

;;------------------------------------------------------------------------------
;;mouse-click-inside-ball? : ball -> boolean
;;This function checks if the mouse button down occurs inside the ball area;
;;if yes returns #true else returns #false
;;GIVEN: ball w, current state
;;RETURN: boolean b 
;;EXAMPLE: mouse-click-inside-ball? 
;;                        (make-ball 200 200 "right" "up" 10 0 #true 0) 200 200)
;;                    -> #true 
;;       : mouse-click-inside-ball? 
;;                        (make-ball 200 200 "right" "up" 10 0 #false 0) 150 20)
;;                    -> #false
;;       : mouse-click-inside-ball? 
;;                       (make-ball 200 200 "right" "up" 10 0 #false 0) 200 150)
;;                    -> #false       
;;STRATEGY: Data Decomposition on w: ball
(define (mouse-click-inside-ball? w x y)
  (if (and (< x (+ (ball-x w) BALL-RADIUS) )
           (> x (- (ball-x w) BALL-RADIUS) ) 
           (< y (+ (ball-y w) BALL-RADIUS) )
           (> y (- (ball-y w) BALL-RADIUS) )) #true #false))

(begin-for-test (check-equal? (mouse-click-inside-ball? 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            200 200) 
                            #true
                           "a MouseEvent occurs inside the ball"))

(begin-for-test (check-equal? (mouse-click-inside-ball? 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            150 200) 
                            #false
                           "a MouseEvent's x-coordinate is outside the ball"))

(begin-for-test (check-equal? (mouse-click-inside-ball? 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            200 150) 
                            #false
                           "a MouseEvent's y-coordinate is outside the ball"))

(begin-for-test (check-equal? (mouse-click-inside-ball? 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)
                            250 250) 
                            #false
                           "a MouseEvent's x-coordinate and y-coordinate are
                             outside the ball"))

;;------------------------------------------------------------------------------
;;start-explosion : ball -> ball
;;This function starts the explosion sequence by changing VerticalDirection to
;;up and changes the velocity to explosion velocity and increases the score by 
;;1.
;;GIVEN: ball w, current state
;;RETURN: boolean b 
;;EXAMPLE: start-explosion 
;;                        (make-ball 200 200 "right" "up" 10 0 #false 0)
;;                    ->  (make-ball 200 200 "right" "up" 0 1 false 1)
;;       : start-explosion 
;;                        (make-ball 200 200 "right" "down" 10 0 #false 0)
;;                    ->  (make-ball 200 200 "right" "up" -10 1 false 1)
;;STRATEGY: Data Decomposition on w: ball
(define (start-explosion w)
  (make-ball (ball-x w)
              (ball-y w)
              (ball-x-dir w)
              UP
              (explosion-velocity w)
              1
              (ball-p w)
              (add1 (ball-sc w))))

(begin-for-test (check-equal? (start-explosion 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)) 
                            (make-ball 200 200 "right" "up" 0 1 false 1)
                           "the ball is moving up when explosion occurs"))

(begin-for-test (check-equal? (start-explosion 
                            (make-ball 200 200 "right" "down" 10 0 #false 0)) 
                            (make-ball 200 200 "right" "up" -10 1 false 1)
                           "the ball is moving down when explosion occurs"))

;;------------------------------------------------------------------------------
;;explosion-velocity : ball -> Real
;;This function calculates the new velocity after the explosion has started. 
;;GIVEN: ball w, current state
;;RETURN: boolean b 
;;EXAMPLE: start-explosion 
;;                        (make-ball 200 200 "right" "up" 10 0 #false 0)
;;                    ->  (make-ball 200 200 "right" "up" 0 1 false 1)
;;       : start-explosion 
;;                        (make-ball 200 200 "right" "down" 10 0 #false 0)
;;                    ->  (make-ball 200 200 "right" "up" -10 1 false 1)
;;STRATEGY: Data Decomposition on w: ball
(define (explosion-velocity w)
  (cond
    [(up? w) (+ (ball-v w) EXPLOSIVE-THRUST)]
    [(down? w) EXPLOSIVE-THRUST]))

(begin-for-test (check-equal? (explosion-velocity 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)) 
                            0 
                           "the ball is moving up when explosion occurs"))

(begin-for-test (check-equal? (explosion-velocity 
                            (make-ball 200 200 "right" "down" 10 0 #false 0)) 
                            -10 
                           "the ball is moving down when explosion occurs"))

;;------------------------------------------------------------------------------
;;next-tick-value : World -> [0 10)
;;Adds 1 to the current tick value tills the tick reaches 9 and then resets to
;;0. 0 means no explosion. 
;;GIVEN: ball w, current state
;;RETURN: [0, 10), depending on the current value of the tick  
;;EXAMPLE: next-tick-value 
;;                        (make-ball 200 200 "right" "up" 10 0 #false 0)
;;                    ->  0
;;       : next-tick-value 
;;                        (make-ball 200 200 "right" "up" 10 2 #false 2)
;;                    ->  3
;;STRATEGY: Data Decomposition on w: ball
(define (next-tick-value w)
(if (and (<= (ball-t w) 9) (> (ball-t w) 0)) (+ (ball-t w) 1) 0 ))

(begin-for-test (check-equal? (next-tick-value 
                            (make-ball 200 200 "right" "up" 10 0 #false 0)) 
                            0 
                           "current value of time cince click is zero"))

(begin-for-test (check-equal? (next-tick-value 
                            (make-ball 200 200 "right" "up" 10 2 #false 2)) 
                            3 
                           "current value of time since click is 2"))

;;------------------------------------------------------------------------------
;;ticks-since-click : World -> [0 10)
;;Returns the current value of ticks that have passed since the start of the 
;;explosion. 0 means no explosion.
;;GIVEN: ball w, current state
;;RETURN: [0, 10), 
;;EXAMPLE: ticks-since-click 
;;                        (make-ball 200 200 "right" "up" 10 0 #false 0)
;;                    ->  0
;;       : ticks-since-click 
;;                        (make-ball 200 200 "right" "up" 10 2 #false 2)
;;                    ->  2
;;STRATEGY: Data Decomposition on w: ball
(define (ticks-since-click w)
  (ball-t w))

(begin-for-test (check-equal? (ticks-since-click 
                            (make-ball 200 200 "right" "up" 10 2 #false 2)) 
                            2 
                           "current value of time since click is 2"))

;;------------------------------------------------------------------------------
;;score : World -> NonNegInt
;;Returns the current score.
;;GIVEN: ball w, current state
;;RETURN: score, current score of the current ball state 
;;EXAMPLE: score (make-ball 200 200 "right" "up" 10 0 #false 0) ->  0
;;       : score (make-ball 200 200 "right" "up" 10 2 #false 2) ->  2
;;STRATEGY: Data Decomposition on w: ball
(define (score w)
  (ball-sc w))

(begin-for-test (check-equal? (score 
                               (make-ball 200 200 "right" "up" 10 2 #false 2)) 
                            2 
                           "the non zero score taken from the ball structure"))

(begin-for-test (check-equal? (score 
                            (make-ball 200 200 "right" "up" 10 2 #false 0)) 
                            0 
                           "the zero score taken from the ball structure"))

;;------------------------------------------------------------------------------
;;world-ball : World -> Ball
;;Returns a representation of the ball.
;;GIVEN: ball w, current state
;;RETURN: ball w, current state 
;;EXAMPLE: world-ball (make-ball 200 200 "right" "up" 10 0 #false 0) ->  
;;                    (make-ball 200 200 "right" "up" 10 0 #false 0) 
;;       : world-ball (make-ball 200 200 "right" "up" 10 2 #false 2) -> 
;;                    (make-ball 200 200 "right" "up" 10 2 #false 2)
;;STRATEGY: Data Decomposition on w: ball
(define (world-ball w)
  w)

(begin-for-test (check-equal? (world-ball 
                               (make-ball 200 200 "right" "up" 10 2 #false 2)) 
                            (make-ball 200 200 "right" "up" 10 2 #false 2) 
                           "A ball structure is passed"))

;;Alternate Definition:
;;1.>

;;A HorizontalDirection is string and is one of
;;- "right"
;;- "left"
;;INTERP: A HorizontalDirection represents the horizontal direction in 
;;which the ball is moving, RIGHT meaning towards right wall
;;and LEFT meaning towards left wall.

;;A VerticalDirection is string and is one of
;;- "up"
;;- "down"
;;INTERP: A VerticalDirection represents the horizontal direction in 
;;which the ball is moving, UP meaning towards the cieling of the window
;;and DOWN meaning towards the floor of the window.

;;A CurrentVelocity is Real
;;INTERP: A CurrentVelocity represents the vertical velocity of the 
;;ball at a particular tick. The negative CurrentVelocity represents the 
;;upward velocity of the ball, while the positive CurrentVelocity represents  
;;the downward velocity of the all.

;;A Time is NonNegInt
;;WHERE: Time t can be in [0,10)
;;INTERP: Time represents the time, in tick, that has elapsed since the 
;;start of the explosion, that is, a button down event has occured inside the 
;;ball. Time 0 means that there is no explosion.

;;PauseFlag is Boolean and is one of
;;- PAUSED (#true)
;;- UNPAUSED (#false)
;;INTERP: A PauseFlag is a switch to identify whether the ball is paused or 
;;not. PAUSED means that the ball is paused while UNPAUSED means the ball is
;;not paused and is will move according to the defined laws.

;;Score is a NonNegInt
;;INTERP: Score represents the number of times a mouse click event has happened 
;;inside the ball, i.e. explosion has occured, since the last time ball touched
;;the floor of the window.

;;(make-game (PauseFlag Time score))
;;(make-direction (HorizontalDirection VerticalDirection))
;;(make-posn (coordinate coordinate)) 
;;(make-ball (posn direction CurrentVelocity))
;;(make-world (ball game))

;;Pros: The nested structures quantifies the whole world into small coherent 
;;structures. Makes it very easy to understand data structure vise.
;;Cons: Too many levels of nested structure make the code very clumsy and 
;;lengthy. The data decomposition and final composition will get very confusing.
;;Extra effort will have to be spent to make sure the data of individual 
;;consistent with that of the others. Accessing the individual attributes will
;;be nested as well.

;;2.>
;;The other way is to quantify the movement of the ball into horizontal 
;;and vertical directions.
;;(make-game (PauseFlag Time score))
;;(make-verticalmove (coordinate VerticalDirection Velocity))
;;(make-horizontalmove (coordinate HorizontalDirection)) 
;;(make-ball (HorizontalMove VerticalMove))
;;(make-world (ball game))

;;Pros: Again this paradigm divides the problem at hand considerably, and each
;;minute attribute's role is easily understadable.
;;Cons: Too much of nesting can make tracking of control can get really tricky 
;;and the code can go out of hand easily.

;;3.>
;;(make-game (PauseFlag Time score))
;;(make-ball (coordinate 
;;            coordinate HorizontalDirection VerticalDirection Velocity))
;;(make-world (ball game))

;;Pros: This approach also has a nested strcuture but not a deep one. It is
;;both easily understandable and easy to code with less depth. This is the 
;;optimized solution to the problem.
