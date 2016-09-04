####2.2 Problem Description

UPDATE 2015-02-08: We realize the textbook specifies an arrange-main function instead of arrangement-main. However, follow the requested provides from this problem set (i.e., call your function arrangement-main and provide it).

Do Chapter 13.1, exercises 177-179.
As usual, use rackunit and begin-for-test instead of check-expect for testing.

###3 Feeding Worms

####3.1 Additional Preliminaries

Save your solutions for this problem to a file named worms.rkt.  
Run the following expression (you must have required extras.rkt) to check that your file is properly named and is in the proper directory:  
(check-location "04" "worms.rkt")  

Add these additional provides at the top of your file (below the requires), so that we can test your solution:  
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

####3.2 Problem Description

Do Chapter 13.2, exercises 180-184.  
Define the following run function (instead of the worm-main function mentioned in the problem). You may find that a lower tick rate leads to nicer game play.  
(define (run w tick-rate)  
  (big-bang w  
            (on-tick next-world tick-rate)  
            (on-key key-handler) 
            (on-draw render)  
            (stop-when end? render-last)))  
The worm should begin flush against the upper-left corner of a 300 x 300 canvas, moving down. Use a worm-segment diameter of 10 pixels.  
You should define a World data definition and an INITIAL-WORLD constant, as in previous assignments.
Some clarifications:  
There can only be one food in the game at any time.

The worm eats the food if there is any overlap. A worm and food touching at only their edges are not considered overlapping.

Instead of the food-create and food-check-create mentioned in the textbook, use these functions:  
; random-posn : ???  
; Returns a random posn within a width x height canvas.  
; WHERE: the returned posn satisfies ???  
(define (random-posn width height interval offset)  
  (make-posn  
   (+ offset (* interval (random (quotient width interval))))  
   (+ offset (* interval (random (quotient height interval))))))  
; random-food : ListOfPosn -> Food  
; ???  
(define (random-food not-allowed)  
  (food-check  
   (random-posn CANVAS-WIDTH CANVAS-HEIGHT FOOD-DIAMETER FOOD-RADIUS)  
   not-allowed))  
; food-check : Food ListOfPosn -> Posn  
; ???  
; Strategy: generative recursion  
(define (food-check candidate-food not-allowed)  
  (if (posns-overlap? candidate-food not-allowed)  
      (random-food not-allowed)  
      candidate-food))  
Additionally, implement and provide the following functions.  
UPDATE 2015-02-07: Clarified purpose statement of create-worm and replace-worm.

; world-worm : World -> Worm  
; Returns a representation of the Worm in the game.  
 
; create-worm : ListOfPosn -> Worm  
; Creates a worm from the given Posns, using the first Posn in the list  
; as the worm's head, and the rest of the list, in that order,   
; as the worm's body.  
; The resulting Worm may have other attributes of any value.  
; WHERE: the list of posns are contiguous and form a valid worm  
 
; worm-length : Worm -> PosInt
; Returns the number of segments in the given worm.
 
; worm-head-x : Worm -> Coordinate  
; worm-head-y : Worm -> Coordinate  
; Returns the x or y position of the center of the worm's lead segment.  
 
; replace-worm : World Worm -> World  
; Replaces *only the positions* of the Worm in World w with the positions  
; of the given worm. Any other Worm properties in the resulting World   
; should be the same as in the input World.  
; WHERE: The Worm does not overlap with the food.
(define (replace-worm w worm) ...)  
 
; replace-food : World Posn -> World  
; Inserts a piece of food into the world at the given Coordinates,  
; replacing the existing food.  
; WHERE: The food does not overlap with any of the worm's segments.  
 
; posns-overlap? : Posn ListOfPosn -> Boolean  
; Returns true if p overlaps with any elements of ps.  
; Two posns touching at only their outer edges are not overlapping.  
(define (posns-overlap? p ps) ...)  
As usual, use rackunit and begin-for-test instead of check-expect for testing.  