;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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
(provide end?)
(provide world-worm)
(provide create-worm)
(provide worm-length)
(provide worm-head-x)
(provide worm-head-y)
(provide replace-food)
(provide replace-worm)
(provide posns-overlap?)

;;Physical Constants:
(define WIDTH 300); The empty scene width in pixel
(define HEIGHT 300); The empty scene height in pixel
 
;;Worm Constant
(define DIAMETER 10) ; pixels
(define RADIUS (/ DIAMETER 2)) ; pixels
(define WORM-SEGMENT (circle RADIUS "solid" "red")); worm segment
(define FONT-SIZE 16); Size of font in pixel
(define FONT-COLOR "Black"); Font color

;;Graphical Constant:
(define MT (empty-scene WIDTH HEIGHT)); Empty Scene
(define FLOOR (- HEIGHT RADIUS)) ; pixels
(define CIELING RADIUS) ; pixels
(define LEFT-WALL RADIUS) ; pixels
(define RIGHT-WALL (- WIDTH RADIUS)) ; pixels
(define DISPLACEMENT DIAMETER) ; pixels

;;World Constants : 
(define CRASHED #true) ; World-crash
(define NOT-CRASHED #false) ; World-crash
(define FOOD (circle RADIUS "solid" "green")); worm segment

;;A Direction is a string and one of
;;- UP
;;- DOWN
;;- RIGHT
;;- LEFT
;;INTERP: Direction represents the direction in which the worm segment
;;is moving.
;;Direction Constants
(define UP "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")

;;TEMPLATE:
;;direction-fn : Worm -> ???
;;(define (Direction-fn w)
;;  (cond
;;    [(up? w) ...]
;;    [(down? w) ...]
;;    [(right? w) ...]
;;    [(left? w) ...]))

;;up? : WormSegment -> boolean
;;Returns #true if the y-Dir of the incoming ball is "up"  
;;else #false.
(begin-for-test (check-equal? (up? (make-wormsegment 150 20 "up" ))
                               #true 
                "wormsegments' y-Dir is up "))
(begin-for-test (check-equal? (up? 
                               (make-wormsegment 150 20 "left" )) 
                              #false 
                "wormsegments' y-Dir is down"))
;;STRATEGY: Data Decomposition on w: WormSegment
(define (up? w)
  (string=? (wormsegment-dir w) UP))

;;down? : WormSegment -> boolean
;;Returns #true if the y-Dir of the incoming ball is "down"  
;;else #false.
(begin-for-test (check-equal? (down? 
                               (make-wormsegment 150 20 "down" )) 
                              #true 
                "wormsegments' y-Dir is down"))
(begin-for-test (check-equal? (down? 
                               (make-wormsegment 150 20 "left" )) 
                              #false 
                "wormsegments' y-Dir is up"))
;;STRATEGY: Data Decomposition on w: WormSegment
(define (down? w)
  (string=? (wormsegment-dir w) DOWN))

;;left? : WormSegment -> boolean
;;Returns #true if the x-Dir of the incoming ball is "left"  
;;else #false. 
(begin-for-test (check-equal? (left? 
                               (make-wormsegment 150 20 "left" )) 
                              #true 
                "wormsegments' x-Dir is left"))
(begin-for-test (check-equal? (left? 
                               (make-wormsegment 150 20 "right" )) 
                              #false 
                "wormsegments' x-Dir is right"))
;;STRATEGY: Data Decomposition on w: WormSegment
(define (left? w)
  (string=? (wormsegment-dir w) LEFT))

;;right? : WormSegment -> boolean
;;Returns #true if the x-Dir of the incoming ball is "right"  
(begin-for-test (check-equal? (right? 
                               (make-wormsegment 150 20 "right")) 
                              #true 
                "wormsegments' x-Dir is right"))
(begin-for-test (check-equal? (right? 
                               (make-wormsegment 150 20 "left" )) 
                              #false 
                "wormsegments' x-Dir is left"))
;;STRATEGY: Data Decomposition on w: WormSegment
(define (right? w)
  (string=? (wormsegment-dir w) RIGHT))

;;A WormSegment is (make-wormsegment Coordinate Coordinate Direction)
;;INTERP : (make-wormsegment x y dir) mean
;;x gives the distance of the point from origin on x-axis
;;y gives the distance of the point from origin on y-axis
;;dir represents the direction in which the wormsegment is moving

(define-struct wormsegment [x y dir])

;;A Worm is one of
;;- (cons WormSegment '())
;;- (cons WormSegment Worm)
;;INTERP: A Worm is collection of WormSegment s
;;TEMPLATE:
;;(define (Worm-fn lows)
;;  (cond
;;    [(empty? (rest lows))...]
;;    [else (...(first lows)...(second lows)...
;;          (worm-fn (rest lows))...)]))

;;A World is (make-world Worm Boolean Posn)
;;INTERP: (make-world lows crash food)
;;lows represents the Worm.
;;crash represents whether the Worm has crashed into any of the boundaries or 
;;into itself not.
;;food represents the position of the current food.
;;if crash is true that means the game has crashed else the game continues

(define-struct world [lows crash food])

(define INITIAL-WORMSEGMENT (make-wormsegment  5 15 "down"))
(define SECOND-WORMSEGMENT (make-wormsegment  5 5 "down"))
(define INITIAL-FOOD (make-posn  50 50))
;(define INITIAL-LOWS (cons SECOND-WORMSEGMENT (cons INITIAL-WORMSEGMENT '())))
(define INITIAL-LOWS (cons INITIAL-WORMSEGMENT '()))
(define INITIAL-WORLD (make-world INITIAL-LOWS #false INITIAL-FOOD))

;;run : World PosReal -> World
;;simulates the Worm game
;;STRATEGY : Function Composition
(define (run w tick-rate)
  (big-bang w
            (on-tick next-world tick-rate)
            (on-key key-handler)
            (on-draw render)
            (stop-when end? render-last)
            ))

;;next-world : World -> World
;;returns the next state of the world after a tick
(begin-for-test (check-equal? (next-world (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50)))
                                (make-world
                                 (list (make-wormsegment 100 90 "up"))
                                 false
                                 (make-posn 50 50))
                              "world after a tick and continues execution"))
(begin-for-test (check-equal? (next-world (make-world 
                                             (list
                               (make-wormsegment 265 100 "right")
                               (make-wormsegment 275 100 "right")
                               (make-wormsegment 285 100 "right")
                               (make-wormsegment 295 100 "right")) 
                                             NOT-CRASHED (make-posn 50 50)))
                                (make-world
                                 (list
                                  (make-wormsegment 265 100 "right")
                                  (make-wormsegment 275 100 "right")
                                  (make-wormsegment 285 100 "right")
                                  (make-wormsegment 295 100 "right"))
                                 true
                                 (make-posn 50 50))
                              "world after a tick and continues execution"))
;;STRATEGY : Data Decomposition on w : world
(define (next-world w)
  (if (or (head-crashed? (world-lows w)) (worm-crashed-itself? (world-lows w)))
      (make-world (world-lows w) CRASHED (world-food w)) 
      (normal-next-world w)))

;;worm-crashed-itself? : Worm -> Boolean
;;retruns true if the head of the worm crashes on itself
(begin-for-test (check-equal?  (worm-crashed-itself? (list
                               (make-wormsegment 100 30 "right")
                               (make-wormsegment 100 20 "right")
                               (make-wormsegment 100 10 "right")
                               (make-wormsegment 100 28 "up"))) 
                                #true
                               "worm crashed on itself"))
(begin-for-test (check-equal?  (worm-crashed-itself? (list
                               (make-wormsegment 100 30 "right"))) 
                                #false
                               "worm crashed on itself"))
;;STATEGY : Data Decomposition on w: World
(define (worm-crashed-itself? lows)
  (cond
    [(empty? (rest lows)) NOT-CRASHED] 
    [else (posns-overlap? (get-posn-of-head lows) 
                          (make-worm-listofposn-excluding-head lows))]))

;;make-worm-listofposn-excluding-head Worm -> ListOfPosn
;;makes a list of positions of all the worm segments present in the Worm
;;excluding the head.
(begin-for-test (check-equal?  (make-worm-listofposn-excluding-head (list
                               (make-wormsegment 100 30 "right")
                               (make-wormsegment 100 20 "right")
                               (make-wormsegment 100 10 "right")
                               (make-wormsegment 100 5 "up"))) 
                                (list
                                 (make-posn 100 10)
                                 (make-posn 100 20)
                                 (make-posn 100 30))
                               "list of posn from corresponding to all the 
                                 wormsegment"))
;;STRATEGY : Data Decomposition on Worm
(define (make-worm-listofposn-excluding-head lows)
  (cond
    [(empty? (rest lows)) '()]
    [else (append (make-worm-listofposn-excluding-head (rest lows))
                  (list(make-posn (wormsegment-x (first lows)) 
                             (wormsegment-y (first lows)))))])) 

;;get-posn-of-head : Worm -> Posn
;;returns the Posn of the head of the incoming worm
(begin-for-test (check-equal?  (get-posn-of-head (list
                               (make-wormsegment 100 30 "right")
                               (make-wormsegment 100 20 "right")
                               (make-wormsegment 100 10 "right")
                               (make-wormsegment 100 5 "up"))) 
                                (make-posn 100 5)
                               "position of the head of worm"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (get-posn-of-head lows)
  (make-posn (worm-head-x lows) (worm-head-y lows)))

;;normal-next-world : World -> World
;;returns new state of world after checking whether the worm
;;has eaten the food.
;(begin-for-test (check-equal?  (normal-next-world (make-world 
;                                             (list
;                               (make-wormsegment 100 30 "right")
;                               (make-wormsegment 100 20 "right")
;                               (make-wormsegment 100 10 "right")
;                               (make-wormsegment 100 5 "up")) 
;                                             CRASHED (make-posn 100 5)))
;                               (make-world
;                                (list
;                                 (make-wormsegment 90 30 "right")
;                                 (make-wormsegment 100 30 "right")
;                                 (make-wormsegment 100 20 "right")
;                                 (make-wormsegment 100 10 "right")
;                                 (make-wormsegment 100 5 "up"))
;                                false
;                                (make-posn 125 225))
;                               "world with new food is given"))
;;STRATEGY : Data Decomposition on w : World
(define (normal-next-world w)
  (if (world-food-eaten? w)
      (make-world (add-segment-to-worm (world-lows w)) NOT-CRASHED 
                  (new-food w))
      (make-world (next-world-lows (world-lows w)) NOT-CRASHED (world-food w))))

;;world-food-eaten? : World -> Boolean
;;returns true if the worm has eaten the food present on the screen.
(begin-for-test (check-equal?  (world-food-eaten? (make-world 
                                             (list
                               (make-wormsegment 100 30 "right")
                               (make-wormsegment 100 20 "right")
                               (make-wormsegment 100 10 "right")
                               (make-wormsegment 100 5 "up")) 
                                             CRASHED (make-posn 100 5)))
                               #true
                               "food is eaten by the worm as 
                                overlapping occured"))
;;STRATEGY : Data Decomposition on w : World
(define (world-food-eaten? w)
  ( posns-overlap? (world-food w) 
                   (make-worm-listofposn (world-lows w))))

;;new-food : World -> Food
;;returns new position of food as the previous has been eaten by worm,
;;takes World as the position of worm segments is required to avoid the new f
;;food to.
;;STRATEGY : Data Decomposition of fd : Food
(define (new-food w)
  (food-check (world-food w) 
              (make-worm-listofposn (world-lows w))))

;;add-segment-to-worm : Worm -> Worm
;;adds a WromSegment to the list of WormSegments
(begin-for-test (check-equal?  (add-segment-to-worm (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")))
                               (list
                                (make-wormsegment 100 110 "up")
                                (make-wormsegment 100 100 "up")
                                (make-wormsegment 105 100 "up")
                                (make-wormsegment 110 100 "up")
                                (make-wormsegment 115 100 "up"))
                               "the new worm segment is added to 
                                the existing worm "))
(begin-for-test (check-equal?  (add-segment-to-worm (list
                               (make-wormsegment 100 100 "up")))
                               (list
                                (make-wormsegment 100 110 "up")
                                (make-wormsegment 100 100 "up"))
                               "the new worm segment is added to 
                                the existing worm "))
;;STRATEGY : Data Decomposition on lows : Worm
(define (add-segment-to-worm lows)
  (cond
    [(empty? (rest lows)) (append (list (new-segment (first  lows))) lows)]
    [else (append (list (new-segment (first  lows))) lows)]))

;;new-segment : WormSegment -> WormSegment
;;returns a WormSegment that is exactly behind the incoming 
;;WormSegment
(begin-for-test (check-equal?  (new-segment 
                                (make-wormsegment 115 100 "up"))
                               (make-wormsegment 115 110 "up")
                               "the new worm segment is move to up"))
(begin-for-test (check-equal?  (new-segment 
                                (make-wormsegment 115 100 "down"))
                               (make-wormsegment 115 90 "down")
                               "the new worm segment is move to down"))
(begin-for-test (check-equal?  (new-segment 
                                (make-wormsegment 115 100 "right"))
                               (make-wormsegment 105 100 "right")
                               "the new worm segment is move to right"))
(begin-for-test (check-equal?  (new-segment 
                                (make-wormsegment 115 100 "left"))
                               (make-wormsegment 125 100 "left")
                               "the new worm segment is move to left"))
;;Data Decomposition on wrm : WormSegment
(define (new-segment wrm)
  (cond
    [(up? wrm) (new-segment-up-handler wrm)]
    [(down? wrm) (new-segment-down-handler wrm) ]
    [(right? wrm) (new-segment-right-handler wrm) ]
    [(left? wrm) (new-segment-left-handler wrm) ]))

;;new-segment-up-handler : WormSegment -> WormSegment
;;returns a WormSegment that is exactly below the input segment
(begin-for-test (check-equal?  (new-segment-up-handler 
                                (make-wormsegment 115 100 "up"))
                               (make-wormsegment 115 110 "up")
                               "the new worm segment is move to up"))
;;STRATEGY : Data Decomposition on wrm : WormSegment 
(define (new-segment-up-handler wrm)
  (make-wormsegment (wormsegment-x wrm) (+(wormsegment-y wrm) DIAMETER) UP))

;;new-segment-down-handler : WormSegment -> WormSegment
;;returns a WormSegment that is exactly above the input segment
(begin-for-test (check-equal?  (new-segment-down-handler 
                                (make-wormsegment 115 100 "down"))
                               (make-wormsegment 115 90 "down")
                               "the new worm segment is move to down"))
;;STRATEGY : Data Decomposition on wrm : WormSegment
(define (new-segment-down-handler wrm)
  (make-wormsegment (wormsegment-x wrm) (-(wormsegment-y wrm )DIAMETER) DOWN))

;;new-segment-down-handler : WormSegment -> WormSegment
;;returns a WormSegment that is exactly to the left of the input segment
(begin-for-test (check-equal?  (new-segment-right-handler 
                                (make-wormsegment 115 100 "right"))
                               (make-wormsegment 105 100 "right")
                               "the new worm segment is move to right"))
;;STRATEGY : Data Decomposition on wrm : WormSegment
(define (new-segment-right-handler wrm)
  (make-wormsegment (- (wormsegment-x wrm) DIAMETER) 
                    (wormsegment-y wrm) RIGHT))

;;new-segment-down-handler : WormSegment -> WormSegment
;;returns a WormSegment that is exactly to the right of the input segment
(begin-for-test (check-equal?  (new-segment-left-handler 
                                (make-wormsegment 115 100 "left"))
                               (make-wormsegment 125 100 "left")
                               "the new worm segment is move to left"))
;;STRATEGY : Data Decomposition on wrm : WormSegment
(define (new-segment-left-handler wrm)
  (make-wormsegment (+ (wormsegment-x wrm) DIAMETER)
                                   (wormsegment-y wrm) LEFT))

;;head-crashed? : Worm -> Boolean
;;returns true if the head of the worm has crashed into wall
;;or itself
(begin-for-test (check-equal?  (head-crashed? 
                                (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")))
                               #false
                               "the Worm has not crashed into any boundary"))
(begin-for-test (check-equal?  (head-crashed? 
                                (list
                               (make-wormsegment 265 100 "right")
                               (make-wormsegment 275 100 "right")
                               (make-wormsegment 285 100 "right")
                               (make-wormsegment 295 100 "right")))
                               #true
                               "the Worm has not crashed into any boundary"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (head-crashed? lows)
  (cond
    [(empty? (rest lows)) (crash-test-on-head (first lows))]
    [else (head-crashed? (rest lows))]))

;;crash-test-on-head : WormSegment -> Boolean
;;returns true is the wormsegment has crashed to the any of the walls
;;or itself.
(begin-for-test (check-equal?  (crash-test-on-head 
                                (make-wormsegment 100 5 "up"))
                               #true
                               "the wormsegment is moved up"))
(begin-for-test (check-equal?  (crash-test-on-head 
                                (make-wormsegment 100 295 "down"))
                               true
                               "the wormsegment is moved past floor"))
(begin-for-test (check-equal?  (crash-test-on-head 
                                (make-wormsegment 295 100 "right"))
                               #true
                               "the wormsegment is moving past right wall"))
(begin-for-test (check-equal?  (crash-test-on-head 
                                (make-wormsegment 5 100 "left")) 
                               #true
                               "the wormsegment is moving past left wall"))
(begin-for-test (check-equal?  (crash-test-on-head 
                                (make-wormsegment 50 100 "left")) 
                               #false
                               "the wormsegment is has not crashed"))
;;STRATEGY : Double Decomposition on (wormsegment-dir wrm) : Direction
;;                                   wrm                   : WormSegment 
(define (crash-test-on-head wrm) 
  (cond
    [(up? wrm) (going-past-cieling? (wormsegment-y wrm))] 
    [(down? wrm) (going-past-floor? (wormsegment-y wrm))]
    [(right? wrm) (going-past-right-wall? (wormsegment-x wrm))]
    [(left? wrm) (going-past-left-wall? (wormsegment-x wrm))]))

;;next-world-lows : Worm -> Worm
;;returns the updated Worm with a step forward
(begin-for-test (check-equal?  (next-world-lows
                                (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")))
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 115 90 "up"))
                               "the wormsegment is moved one step up"))
(begin-for-test (check-equal?  (next-world-lows
                                (list(make-wormsegment 100 100 "up")))
                                (list (make-wormsegment 100 90 "up"))
                               "the wormsegment is moved one step up"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (next-world-lows lows)
  (cond
    [(empty? (rest lows)) (cons (head-move-forward (first lows)) '()) ]
    [else (cons (second lows) 
                (next-world-lows (rest lows)))]))

;;head-move-forward : WormSegment -> WormSegment
;;moves the head forward by DIAMETER of the wormsegment
(begin-for-test (check-equal?  (head-move-forward
                                (make-wormsegment 100 100 "up"))
                                (make-wormsegment 100 90 "up")
                               "the wormsegment is moved up"))
(begin-for-test (check-equal?  (head-move-forward
                                (make-wormsegment 100 6 "up"))
                               (make-wormsegment 100 5 "up")
                               "the wormsegment is moved up"))
(begin-for-test (check-equal?  (head-move-forward 
                                (make-wormsegment 100 100 "down"))
                               (make-wormsegment 100 110 "down")
                               "the wormsegment is moved down"))
(begin-for-test (check-equal?  (head-move-forward 
                                (make-wormsegment 100 293 "down"))
                               (make-wormsegment 100 295 "down")
                               "the wormsegment is moved past floor"))
(begin-for-test (check-equal?  (head-move-forward 
                                (make-wormsegment 100 100 "right"))
                               (make-wormsegment 110 100 "right")
                               "the wormsegment is moved right"))
(begin-for-test (check-equal?  (head-move-forward 
                                (make-wormsegment 294 100 "right"))
                               (make-wormsegment 295 100 "right")
                               "the wormsegment is moving past right wall"))
(begin-for-test (check-equal?  (head-move-forward 
                                (make-wormsegment 100 100 "left")) 
                               (make-wormsegment 90 100 "left")
                               "the wormsegment is moved left"))
(begin-for-test (check-equal?  (head-move-forward 
                                (make-wormsegment 5 100 "left")) 
                               (make-wormsegment 5 100 "left")
                               "the wormsegment is moving past left wall"))
;;STRATEGY : Data Decomposition on (wormsegment-dir wrm) : Direction
(define (head-move-forward wrm)
  (cond
    [(up? wrm) (move-head-up wrm)]
    [(down? wrm) (move-head-down wrm)]
    [(right? wrm) (move-head-right wrm)]
    [(left? wrm) (move-head-left wrm)]))

;;move-head-up : WormSegment -> WormSegment 
;;moves the head in upwards direction
(begin-for-test (check-equal?  (move-head-up (make-wormsegment 100 100 "up"))
                               (make-wormsegment 100 90 "up")
                               "the wormsegment is moved up"))
(begin-for-test (check-equal?  (move-head-up (make-wormsegment 100 6 "up"))
                               (make-wormsegment 100 5 "up")
                               "the wormsegment is moved up"))
;;STRATEGY : Data Decomposition on w : Worm
(define (move-head-up wrm)
  (if (going-past-cieling? (- (wormsegment-y wrm) DIAMETER)) 
      (make-wormsegment (wormsegment-x wrm) 
                        CIELING 
                        UP)
   (make-wormsegment (wormsegment-x wrm) 
                     (- (wormsegment-y wrm) DIAMETER) 
                     UP)))

;;move-head-down : WormSegment -> WormSegment
;;moves the head in downwards direction
(begin-for-test (check-equal?  (move-head-down (make-wormsegment 100 100 "up"))
                               (make-wormsegment 100 110 "down")
                               "the wormsegment is moved down"))
(begin-for-test (check-equal?  (move-head-down (make-wormsegment 100 293 "up"))
                               (make-wormsegment 100 295 "down")
                               "the wormsegment is moved past floor"))
;;STRATEGY : Data Decomposition on w : Worm
(define (move-head-down wrm)
  (if (going-past-floor? (+ (wormsegment-y wrm) DIAMETER)) 
      (make-wormsegment (wormsegment-x wrm) 
                        FLOOR 
                        DOWN)
   (make-wormsegment (wormsegment-x wrm) 
                     (+ (wormsegment-y wrm) DIAMETER) 
                     DOWN)))

;;move-head-right : WormSegment -> WormSegment
;;moves the head in rightwards direction.
(begin-for-test (check-equal?  (move-head-right (make-wormsegment 100 100 "up"))
                               (make-wormsegment 110 100 "right")
                               "the wormsegment is moved right"))
(begin-for-test (check-equal?  (move-head-right (make-wormsegment 294 100 "up"))
                               (make-wormsegment 295 100 "right")
                               "the wormsegment is moving past right wall"))
;;STRATEGY : Data Decomposition on w : Worm
(define (move-head-right wrm)
  (if (going-past-right-wall? (+ (wormsegment-x wrm) DIAMETER)) 
      (make-wormsegment RIGHT-WALL 
                        (wormsegment-y wrm) 
                        RIGHT)
   (make-wormsegment (+ (wormsegment-x wrm) DIAMETER) 
                     (wormsegment-y wrm) 
                     RIGHT)))

;;move-head-left : WormSegment -> WormSegment
;;moves the head in leftwards direction
(begin-for-test (check-equal?  (move-head-left (make-wormsegment 100 100 "up")) 
                               (make-wormsegment 90 100 "left")
                               "the wormsegment is moved left"))
(begin-for-test (check-equal?  (move-head-left (make-wormsegment 5 100 "up")) 
                               (make-wormsegment 5 100 "left")
                               "the wormsegment is moving past left wall"))
;;STRATEGY : Data Decomposition on w : Worm
(define (move-head-left wrm)
  (if (going-past-left-wall? (- (wormsegment-x wrm) DIAMETER)) 
      (make-wormsegment LEFT-WALL 
                        (wormsegment-y wrm) 
                        LEFT)
      (make-wormsegment (- (wormsegment-x wrm) DIAMETER) 
                     (wormsegment-y wrm) 
                     LEFT)))

;;going-past-cieling? Coordinate -> Boolean
;;returns true if the Coordinate is going past cieling
;;of the empty canvas.
(begin-for-test (check-equal?  (going-past-cieling? 50) 
                               #false
                               "the y-coordinate of segment is 
                                inside cieling"))
(begin-for-test (check-equal?  (going-past-cieling? 2) 
                               #true
                               "the y-coordinate of segment is 
                                outside cieling"))
;;STRATEGY : Function Composition
(define (going-past-cieling? y)
  (>= CIELING y))

;;going-past-floor? Coordinate -> Boolean
;;returns true if the Coordinate is going past floor
;;of the empty canvas.
(begin-for-test (check-equal?  (going-past-floor? 50) 
                               #false
                               "the y-coordinate of segment is 
                                inside floor"))
(begin-for-test (check-equal?  (going-past-floor? 297) 
                               #true
                               "the y-coordinate of segment is 
                                outside floor"))
;;STRATEGY : Function Composition
(define (going-past-floor? y)
  (<= FLOOR y))

;;going-past-right-wall? Coordinate -> Boolean
;;returns true if the Coordinate is going past right wall
;;of the empty canvas.
(begin-for-test (check-equal?  (going-past-right-wall? 50) 
                               #false
                               "the x-coordinate of segment is 
                                inside right wall"))
(begin-for-test (check-equal?  (going-past-right-wall? 297) 
                               #true
                               "the x-coordinate of segment is 
                                outside right wall"))
;;STRATEGY : Function Composition
(define (going-past-right-wall? x)
  (<= RIGHT-WALL x))

;;going-past-left-wall? Coordinate -> Boolean
;;returns true if the Coordinate is going past left wall
;;of the empty canvas.
(begin-for-test (check-equal?  (going-past-left-wall? 50) 
                               #false
                               "the x-coordinate of segment is 
                                inside left wall"))
(begin-for-test (check-equal?  (going-past-left-wall? 2) 
                               #true
                               "the x-coordinate of segment is 
                                outside left wall"))
;;STRATEGY : Function Composition
(define (going-past-left-wall? x)
  (>= LEFT-WALL x))

;;render : World -> Image
;;renders an image of the world in a separate window
(begin-for-test (check-equal?  (render (make-world (list
                                              (make-wormsegment 100 100 "up"))
                                                NOT-CRASHED (make-posn 50 50))) 
                               (place-image FOOD 50 50 
                               (place-image WORM-SEGMENT  100  100 MT))
                               "a worm with a single segment"))
;;STRATEGY : Data Decomposition on w : World
(define (render w)
  (render-food-and-worm (world-food w) (world-lows w)))
  
;;render-food-and-worm : World -> Image
;;renders an image of the world with food and worm
(begin-for-test (check-equal?  (render-food-and-worm (make-posn 50 50)(list
                                              (make-wormsegment 100 100 "up"))) 
                               (place-image FOOD 50 50 
                               (place-image WORM-SEGMENT  100  100 MT))
                               "a worm with a single segment"))
;;STRATEGY : Data Decomposition on fd : Food
(define (render-food-and-worm fd lows)
  (place-image FOOD (posn-x fd) (posn-y fd)
              (render-worm lows)))

;;render-worm : Worm -> Image
;;renders an image of the worm on a white canvas
(begin-for-test (check-equal?  (render-worm (list
                                              (make-wormsegment 100 100 "up"))) 
                               (place-image WORM-SEGMENT  100  100 MT)
                               "a worm with a single segment"))
(begin-for-test (check-equal?  (render-worm (list
                                              (make-wormsegment 100 100 "up")
                                              (make-wormsegment 90 100 "up"))) 
                               (place-image WORM-SEGMENT 90 100
                                (place-image WORM-SEGMENT  100  100 MT))
                               "a worm with two segments"))
;;STRATEGY : Double Decomposition on lows : Worm 
;;                                  (first lows) : WormSegment 
(define (render-worm lows)
  (cond
    [(empty? (rest lows)) (render-worm-segment (first lows))]
    [else 
     (place-image WORM-SEGMENT (wormsegment-x (first lows)) 
                  (wormsegment-y (first lows))  
                  (render-worm (rest lows)))]))

;;render-worm-segment : WormSegment -> Image
;;renders an image of a worm segment on a canvas
(begin-for-test (check-equal?  (render-worm-segment
                               (make-wormsegment 115 100 "up")) 
                                 (place-image WORM-SEGMENT  115  100 MT)
                               "a worm rendered on a canvas"))
;;STRATEGY : Data Decomposition on WormSegment
(define (render-worm-segment wrm)
  (place-image WORM-SEGMENT (wormsegment-x wrm) (wormsegment-y wrm) MT))

;;end? : World -> Boolean
;;returns true if the incoming crash flag is set to true
(begin-for-test (check-equal?  (end? (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                           NOT-CRASHED (make-posn 50 50)))
                               false
                               "a world with worm that has crashed"))
;;STRATEGY :Data Decomposition on w : World
(define (end? w)
  (world-crash w))

;;render-last : World -> Image
;;renders an image from the world, this is trigerred by the stop-when
(begin-for-test (check-equal?  (render-last (make-world 
                                             (list
                               (make-wormsegment 100 30 "right")
                               (make-wormsegment 100 20 "right")
                               (make-wormsegment 100 10 "right")
                               (make-wormsegment 100 5 "up")) 
                                             CRASHED (make-posn 50 50)))
                               (overlay/align "left" "bottom" 
              (text "worm crashed,score: 3" FONT-SIZE FONT-COLOR) MT)
                               "rendering the last scene after 
                                crash with score"))
;;STRATEGY : Data Decomposition on w : World
(define (render-last w)
  (overlay/align "left" "bottom" 
              (text (string-end-score (world-lows w)) FONT-SIZE FONT-COLOR) MT))

;;string-end-score : Worm -> String
;;produces an string of text with length of the input list in it
(begin-for-test (check-equal? (string-end-score 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")))
                               "worm crashed,score: 3"
                               "string with an end statement and score"))
;;STRATEGY : Data Decomposition on w : World
(define (string-end-score lows)
  (string-append "worm crashed,score: " 
                 (number->string (sub1 (worm-length lows)))))
  

;;key-handler : World KeyEvent -> World
;;reacts to the inputs given on the keyboard, turns the worm to the 
;;direction of the arrow keys pressed, all the other keys are ignored
(begin-for-test (check-equal? (key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                           NOT-CRASHED (make-posn 50 50)) "up")
                               (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 115 90 "up"))
                                false
                                (make-posn 50 50))
                               "a world with a worm that 
                               will move to up"))
(begin-for-test (check-equal? (key-handler (make-world 
                                             (list
                               (make-wormsegment 100 30 "right")
                               (make-wormsegment 100 20 "right")
                               (make-wormsegment 100 10 "right")
                               (make-wormsegment 100 5 "up")) 
                                             CRASHED (make-posn 50 50)) "up")
                                (make-world
                                 (list
                                  (make-wormsegment 100 30 "right")
                                  (make-wormsegment 100 20 "right")
                                  (make-wormsegment 100 10 "right")
                                  (make-wormsegment 100 5 "up"))
                                 true
                                 (make-posn 50 50))
                              "a world with a worm is given with a KeyEvent up
                              will move the worm up and crash"))
(begin-for-test (check-equal? (key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50))
                                           "down")
                               (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 115 110 "down"))
                                false
                                (make-posn 50 50))
                               "a world with a worm that 
                               will move to down after a down key is pressed"))
(begin-for-test (check-equal? (key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50))
                                                "left")
                               (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 105 100 "left"))
                                false
                                (make-posn 50 50))
                               "a world with a worm that 
                               will move to left after a left key is presssed"))
(begin-for-test (check-equal? (key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50))
                                                 "right")
                                (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 125 100 "right"))
                                false
                                (make-posn 50 50))
                              "a world with a right key pressed
                               which moves the worm to right"))
(begin-for-test (check-equal? (key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50))
                                                 "s")
                                (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50))
                              "a world with a key s pressed which is ignored"))
;;STRATEGY : Data Decomposition on ke : KeyEvent
(define (key-handler w ke)
  (cond
    [(string=? "up" ke) (up-key-handler w)]
    [(string=? "down" ke) (down-key-handler w)]
    [(string=? "left" ke) (left-key-handler w)]
    [(string=? "right" ke) (right-key-handler w)]
    [else w]))

;;up-key-handler : World -> World
;;reacts to the up key pressed, if the worm is already going up
;;this key is ignored otherwise the worm directed upwards.
(begin-for-test (check-equal? (up-key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50)))
                               (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 115 90 "up"))
                                false
                                (make-posn 50 50))
                               "a world with a worm that 
                               will move to up"))
(begin-for-test (check-equal? (up-key-handler (make-world 
                                             (list
                               (make-wormsegment 100 30 "right")
                               (make-wormsegment 100 20 "right")
                               (make-wormsegment 100 10 "right")
                               (make-wormsegment 100 5 "up")) 
                                             CRASHED (make-posn 50 50)))
                                (make-world
                                 (list
                                  (make-wormsegment 100 30 "right")
                                  (make-wormsegment 100 20 "right")
                                  (make-wormsegment 100 10 "right")
                                  (make-wormsegment 100 5 "up"))
                                 true
                                 (make-posn 50 50))
                              "a world with a worm is given that 
                               will move to up and crash"))
;;STRATEGY : Data Decomposition on w : World
(define (up-key-handler w)
   (if (head-crashed? (world-lows w))
      (make-world (world-lows w) CRASHED (world-food w))
      (make-world (change-direction-to-up (world-lows w)) 
                  NOT-CRASHED (world-food w))))

;;down-key-handler : World -> World
;;reacts to the up key pressed, if the worm is already going down
;;this key is ignored otherwise the worm directed downwards.
(begin-for-test (check-equal? (down-key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50)))
                               (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 115 110 "down"))
                                false
                                (make-posn 50 50))
                               "a world with a worm that 
                               will move to down"))
(begin-for-test (check-equal? (down-key-handler (make-world 
                                             (list
                               (make-wormsegment 100 265 "right")
                               (make-wormsegment 100 275 "right")
                               (make-wormsegment 100 285 "right")
                               (make-wormsegment 100 295 "down")) 
                                             CRASHED (make-posn 50 50)))
                                (make-world
                                 (list
                                  (make-wormsegment 100 265 "right")
                                  (make-wormsegment 100 275 "right")
                                  (make-wormsegment 100 285 "right")
                                  (make-wormsegment 100 295 "down"))
                                 true
                                 (make-posn 50 50))
                              "a world with a worm is given that 
                               will move to down and crash"))
;;STRATEGY : Data Decomposition on w : World
(define (down-key-handler w)
     (if (head-crashed? (world-lows w))
      (make-world (world-lows w) CRASHED (world-food w))
      (make-world (change-direction-to-down (world-lows w)) 
                  NOT-CRASHED (world-food w))))

;;left-key-handler : World -> World
;;reacts to the up key pressed, if the worm is already going left
;;this key is ignored otherwise the worm directed leftwards.
(begin-for-test (check-equal? (left-key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50)))
                               (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 105 100 "left"))
                                false
                                (make-posn 50 50))
                               "a world with a worm that 
                               will move to left"))
(begin-for-test (check-equal? (left-key-handler (make-world 
                                             (list
                               (make-wormsegment 35 100 "left")
                               (make-wormsegment 25 100 "left")
                               (make-wormsegment 15 100 "left")
                               (make-wormsegment 5 100 "left")) 
                                             CRASHED (make-posn 50 50)))
                                (make-world
                                 (list
                                  (make-wormsegment 35 100 "left")
                                  (make-wormsegment 25 100 "left")
                                  (make-wormsegment 15 100 "left")
                                  (make-wormsegment 5 100 "left"))
                                 true
                                 (make-posn 50 50))
                              "a world with a worm is given that 
                               will move to left and crash"))
;;STRATEGY : Data Decomposition on w : World
(define (left-key-handler w)
       (if (head-crashed? (world-lows w))
      (make-world (world-lows w) CRASHED (world-food w))
      (make-world (change-direction-to-left (world-lows w)) 
                  NOT-CRASHED (world-food w))))
  
;;right-key-handler : World -> World
;;reacts to the up key pressed, if the worm is already going right
;;this key is ignored otherwise the worm directed rightwards.
(begin-for-test (check-equal? (right-key-handler (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50)))
                                (make-world
                                (list
                                 (make-wormsegment 105 100 "up")
                                 (make-wormsegment 110 100 "up")
                                 (make-wormsegment 115 100 "up")
                                 (make-wormsegment 125 100 "right"))
                                false
                                (make-posn 50 50))
                              "a world with a worm is given with 
                               worm that replaces it"))
(begin-for-test (check-equal? (right-key-handler (make-world 
                                             (list
                               (make-wormsegment 265 100 "right")
                               (make-wormsegment 275 100 "right")
                               (make-wormsegment 285 100 "right")
                               (make-wormsegment 295 100 "right")) 
                                             CRASHED (make-posn 50 50)))
                                (make-world
                                 (list
                                  (make-wormsegment 265 100 "right")
                                  (make-wormsegment 275 100 "right")
                                  (make-wormsegment 285 100 "right")
                                  (make-wormsegment 295 100 "right"))
                                 true
                                 (make-posn 50 50))
                              "a world with a worm is given with 
                               worm that replaces it"))
;;STRATEGY : Data Decomposition on w : World
(define (right-key-handler w)
  (if (head-crashed? (world-lows w))
      (make-world (world-lows w) CRASHED (world-food w))
      (make-world (change-direction-to-right (world-lows w)) 
                  NOT-CRASHED (world-food w))))

;;change-direction-to-up : Worm -> Worm
;;changes the direction of the worm to up
(begin-for-test (check-equal? (change-direction-to-up 
                               (list(make-wormsegment 50 30 "down")
                                   (make-wormsegment 50 40 "down")
                                   (make-wormsegment 50 50 "down")))
                              (list
                               (make-wormsegment 50 40 "down")
                               (make-wormsegment 50 50 "down")
                               (make-wormsegment 50 40 "up"))
                              "a worm is passed so the it moves up"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (change-direction-to-up lows)
  (cond
    [(empty? (rest lows)) (cons (move-head-up (first lows)) '()) ]
    [else (cons (first (rest lows)) 
                (change-direction-to-up (rest lows)))]))

;;change-direction-to-down : Worm -> Worm
;;changes the direction of the worm to down
(begin-for-test (check-equal? (change-direction-to-down 
                               (list(make-wormsegment 50 30 "up")
                                   (make-wormsegment 50 40 "up")
                                   (make-wormsegment 50 50 "up")))
                              (list
                               (make-wormsegment 50 40 "up")
                               (make-wormsegment 50 50 "up")
                               (make-wormsegment 50 60 "down"))
                              "a worm is passed so the it moves down"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (change-direction-to-down lows)
  (cond
    [(empty? (rest lows)) (cons (move-head-down (first lows)) '()) ]
    [else (cons (first (rest lows)) 
                (change-direction-to-down (rest lows)))]))

;;change-direction-to-right : Worm -> Worm
;;changes the direction of the worm to right
(begin-for-test (check-equal? (change-direction-to-right 
                               (list(make-wormsegment 50 30 "down")
                                   (make-wormsegment 50 40 "down")
                                   (make-wormsegment 50 50 "down")))
                              (list
                               (make-wormsegment 50 40 "down")
                               (make-wormsegment 50 50 "down")
                               (make-wormsegment 60 50 "right"))
                             "a worm is passed so the it moves right"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (change-direction-to-right lows)
  (cond
    [(empty? (rest lows)) (cons (move-head-right (first lows)) '()) ]
    [else (cons (first (rest lows)) 
                (change-direction-to-right (rest lows)))]))

;;change-direction-to-left : Worm -> Worm
;;changes the direction of the worm to left
(begin-for-test (check-equal? (change-direction-to-left 
                               (list(make-wormsegment 50 30 "down")
                                   (make-wormsegment 50 40 "down")
                                   (make-wormsegment 50 50 "down")))
                              (list
                               (make-wormsegment 50 40 "down")
                               (make-wormsegment 50 50 "down")
                               (make-wormsegment 40 50 "left"))
                              "a worm is passed so the it moves left"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (change-direction-to-left lows)
  (cond
    [(empty? (rest lows)) (cons (move-head-left (first lows)) '()) ]
    [else (cons (first (rest lows)) 
                (change-direction-to-left (rest lows)))]))

;;world-worm : World -> Worm
;;Returns a representation of the Worm in the game.
(begin-for-test (check-equal? (world-worm 
                               (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50)))
                              (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up"))
                              "A world is given in order to get the worm
                                it contains"))
;;STATEGY : Data Decomposition on w : World
(define (world-worm w)
  (world-lows w))

;;create-worm : ListOfPosn -> Worm
;;Creates a worm from the given Posns, using the first Posn in the list
;;as the worm's head, and the rest of the list, in that order, 
;;as the worm's body.
;;The resulting Worm may have other attributes of any value.
;;WHERE: the list of posns are contiguous and form a valid worm
(begin-for-test (check-equal? (create-worm 
                               (cons (make-posn 50 50) 
                                     (cons (make-posn 50 40) 
                                           (cons (make-posn 50 30) '()))))
                              (list(make-wormsegment 50 30 "down")
                                   (make-wormsegment 50 40 "down")
                                   (make-wormsegment 50 50 "down"))
                              "list of posns given such that they form 
                               a contigous worm"))
(define (create-worm loposn)
  (cond
    [(empty? loposn) '()]
    [else (append (create-worm (rest loposn))
           (list(make-wormsegment (posn-x (first loposn)) 
                             (posn-y (first loposn)) DOWN)))]))

;;worm-length : Worm -> PosInt
;;Returns the number of segments in the given worm.
(begin-for-test (check-equal? (worm-length (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up"))) 4
                              "a Worm with 4 segment is passed as input"))
;;STATEGY : Function Composition
(define (worm-length lows)
  (length lows))

;;worm-head-x : Worm -> Coordinate
;;worm-head-y : Worm -> Coordinate
;;Returns the x or y position of the center of the worm's lead segment.
(begin-for-test (check-equal? (worm-head-x (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up"))) 115
                              "a valid Worm with hea at (115, 100) to get 
                                 x-CoOrdinate of head"))
(begin-for-test (check-equal? (worm-head-y (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up"))) 100
                              "a valid Worm with hea at (115, 100) to get 
                                 y-CoOrdinate of head"))
;;STRATEGY : Data Decomposition of lows : Worm
(define (worm-head-x lows)
    (cond
    [(empty? (rest lows)) (get-x (first lows))]
    [else (worm-head-x (rest lows))]))

(define (worm-head-y lows)
    (cond
    [(empty? (rest lows)) (get-y (first lows))]
    [else (worm-head-y (rest lows))]))

;;get-x : WormSegment -> Coordinate
;;extracts the x Coordinate of the incoming WormSegment
(begin-for-test (check-equal? (get-x (make-wormsegment 100 100 "up")) 100
                              "a WormSegment located at (100,100)"))
;;STRATEGY : Data Decomposition of wrm : WormSegment
(define (get-x wrm)
  (wormsegment-x wrm))

;;get-y : WormSegment -> Coordinate
;;extracts the y Coordinate of the incoming WormSegment
(begin-for-test (check-equal? (get-y (make-wormsegment 100 100 "up")) 100
                              "a WormSegment located at (100,100)"))
;;STRATEGY : Data Decomposition of wrm : WormSegment
(define (get-y wrm)
  (wormsegment-y wrm))

;;replace-worm : World Worm -> World
;;Replaces *only the positions* of the Worm in World w with the positions
;;of the given worm. Any other Worm properties in the resulting World 
;;should be the same as in the input World.
;;WHERE: The Worm does not overlap with the food.
(begin-for-test (check-equal? (replace-worm (make-world 
                                             (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")) 
                                             NOT-CRASHED (make-posn 50 50))
                                                (list
                               (make-wormsegment 100 100 "down")
                               (make-wormsegment 105 100 "down"))) 
                              (make-world
                               (list
                                (make-wormsegment 100 100 "down")
                                (make-wormsegment 105 105 "up"))
                               false
                               (make-posn 50 50))
                              "a world with a worm is given with 
                               worm that replaces it"))
;;STRATEGY : Data Decomposition on w : World
(define (replace-worm w worm)
  (make-world (replace-head-dir (world-lows w) worm) 
              (world-crash w) (world-food w)))

;;replace-posns : Worm Worm -> Worm
;;returns the new list of segments with replaced direction of the head of the 
;;incoming worm.
(begin-for-test (check-equal? (replace-head-dir (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up"))
                                                (list
                               (make-wormsegment 100 100 "down")
                               (make-wormsegment 105 100 "down"))) 
                              (list
                               (make-wormsegment 100 100 "down")
                               (make-wormsegment 105 105 "up"))
                              "a valid wormsegment with up as 
                               direction is passed"))
;;STRATEGY : Double Decomposition on lows0 lows1 : Worm
(define (replace-head-dir lows0 lows1)
  (cond
    [(empty? (rest lows1)) (list (make-wormsegment 
                                 (wormsegment-x (first lows1))
                                 (wormsegment-x (first lows1))
                                 (get-head-dir lows0)))]
    [else (append (list(first lows1)) (replace-head-dir lows0 (rest lows1)))]))

;;get-head-dir : Worm Worm -> Direction
;;returns the direction of the head of the input Worm.
(begin-for-test (check-equal? (get-head-dir (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up"))) "up"
                              "a valid wormsegment with up as 
                               direction is passed"))
;;STRATEGY : Data Decomposition on lows0 lows1 : Worm
(define (get-head-dir lows)
  (cond
    [(empty? (rest lows)) (get-dir (first lows))]
    [else (get-head-dir (rest lows))]))

;;get-dir : WormSegment -> Direction
;;returns the direction of the head of the incoming Worm.
(begin-for-test (check-equal? (get-dir (make-wormsegment 115 100 "up")) "up"
                              "a valid wormsegment with up as 
                               direction is passed"))
;;STRATEGY : Data Decomposition on wrm : WormSegment
(define (get-dir wrm)
  (wormsegment-dir wrm))

;;make-worm-listofposn Worm -> ListOfPosn
;;makes a list of positions of all the worm segments present in the Worm
(begin-for-test (check-equal? (make-worm-listofposn 
                               (list
                               (make-wormsegment 100 100 "up")
                               (make-wormsegment 105 100 "up")
                               (make-wormsegment 110 100 "up")
                               (make-wormsegment 115 100 "up")))
                                              (list
                                               (make-posn 115 100)
                                               (make-posn 110 100)
                                               (make-posn 105 100)
                                               (make-posn 100 100))
                              "a valid worm is passed  to get the ListOfPosn"))
;;STRATEGY : Data Decomposition on lows : Worm
(define (make-worm-listofposn lows)
  (cond
    [(empty? lows) '()]
    [else (append (make-worm-listofposn (rest lows))
                  (list(make-posn (wormsegment-x (first lows)) 
                             (wormsegment-y (first lows)))))]))

;;posns-overlap? : Posn ListOfPosn -> Boolean
;;Returns true if p overlaps with any elements of ps.
;;Two posns touching at only their outer edges are not overlapping.
(begin-for-test (check-equal? (posns-overlap? (make-posn 50 50) 
                                              (list
                                               (make-posn 100 100)
                                               (make-posn 105 100)
                                               (make-posn 110 100)
                                               (make-posn 115 100)))
                              #false
                              "checks if input list of posn 
                                                 overlaps with (50,50)"))
(begin-for-test (check-equal? (posns-overlap? (make-posn 50 50) 
                                              (list
                                               (make-posn 55 100)
                                               (make-posn 60 100)
                                               (make-posn 65 100)
                                               (make-posn 70 100)))
                              #false
                              "checks if input list of posn 
                                                 overlaps with (50,50)"))
;;STRATEGY : Data Decomposition on ps : ListOfPosn
(define (posns-overlap? p ps) 
  (cond
    [(empty? (rest ps)) (check-overlap? p (first ps)) ]
    [else (or (check-overlap? p (first ps)) (posns-overlap? p (rest ps)))]))

;;check-overlap? : Posn Posn -> Boolean
;;Returns true if distance between two points is less than the diameter of the 
;;FOOD/WORMSEGMENT.
(begin-for-test (check-equal? (check-overlap? (make-posn 50 50) 
                                              (make-posn 100 100))
                              #false
                              "checks if (100,100) overlaps with (50,50)"))
(begin-for-test (check-equal? (check-overlap? (make-posn 50 50) 
                                              (make-posn 55 55))
                              #true
                              "checks if (100,100) overlaps with (50,50)"))
;;STRATEGY : Function Composition
(define (check-overlap? p0 p1)
  (< (distance-between-two-posns p0 p1) DIAMETER))

;;check-overlap? : Posn Posn -> Real
;;Returns the distance between two Posns
(begin-for-test (check-equal? (distance-between-two-posns (make-posn 50 50) 
                                              (make-posn 100 100))
                              (exact->inexact #i70.71067811865476)
                            "finds the distance between (100,100) and (50,50)"))
(begin-for-test (check-equal? (distance-between-two-posns (make-posn 50 50) 
                                              (make-posn 55 55))
                              (exact->inexact #i7.0710678118654755)
                            "finds the distance between (100,100) and (50,50)"))
;;STRATEGY : Double Decomposition on p0,p1 : Posn
(define (distance-between-two-posns p0 p1)
  (sqrt(+(sqr(- (posn-x p0) (posn-x p1))) (sqr(- (posn-y p0) (posn-y p1))))))

;;random-posn : PosReal PosReal PosReal PosReal -> Posn
;;Returns a random posn within a width x height canvas.
;;WHERE: the returned posn satisfies ???
(define (random-posn width height interval offset)
  (make-posn
   (+ offset (* interval (random (quotient width interval))))
   (+ offset (* interval (random (quotient height interval))))))

;;random-food : ListOfPosn -> Food
;;generates a randon position for food that is inside the boundaries.
;;STRATEGY : Function Composition
(define (random-food not-allowed)
  (food-check
   (random-posn WIDTH HEIGHT DIAMETER RADIUS)
   not-allowed))

;;food-check : Food ListOfPosn -> Posn
;;returns a Posn if the candidate food is not overlapping
;;with the Worm ListofPosns and inside the boundaries.
;;STRATEGY : generative recursion
(define (food-check candidate-food not-allowed)
  (if (posns-overlap? candidate-food not-allowed)
      (random-food not-allowed)
      candidate-food))

;;replace-food : World Posn -> World
;;Inserts a piece of food into the world at the given Coordinates,
;;replacing the existing food.
;;WHERE: The food does not overlap with any of the worm's segments.
(begin-for-test (check-equal? (replace-food INITIAL-WORLD (make-posn 100 100))
                              (make-world
                               (list (make-wormsegment 5 15 "down"))
                               false
                               (make-posn 100 100))
                              "Food in INITIAL-WORLD is replaced by 50,50 "))
;;STRATEGY : Data Decomposition on w : World
(define (replace-food w posn)
  (make-world (world-lows w) (world-crash w) posn))



;;Alternate Data Definitions
;;1.>
;;(make-worm [ListOfPosn dir])
;;A Food is Posn
;;(make-world [worm crash food])

;;Pros: The conversion to get the ListOfPosn of each of the segment can be
;;avoided using this approach. Direction of each segment is not required.

;;Cons: The readability of the code decreases.