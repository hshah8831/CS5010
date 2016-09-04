###1 Robot Movement

Write a program that simulates a robot moving around a room, with the following requirements:
UPDATE 2015-01-22: This is not a big-bang program. Notice that you are not required to write any render or key-handler functions.

  Represent the robot with a circle that has a radius of 15 pixels.

The room is 200 pixels wide and 400 pixels long.

Use a graphics-style (x,y) coordinate system (like the Universe library) where (0,0) is the top-left, (200,400) is the bottom-right, and a robot moving "up" would decrease it’s y position.

A robot can start anywhere:  

- completely in the room,

- completely outside the room,

- or partially inside/outside the room.

Once the robot is completely inside the room, it cannot leave (not even partially).

A robot who’s edge is flush against the wall is considered completely inside the room.

If moving an inside-the-room robot would put the robot outside or partially outside the room, it should stop right at the wall.

You may assume a built-in Coordinate data definition.

Implement the simulation by designing the data definitions required by the functions below, and then writing the functions.   
; initial-robot : Coordinate Coordinate -> Robot  
; Returns a Robot located at (x,y), facing up.   
(define (initial-robot x y) ...)
 
; robot-left : Robot -> Robot  
; robot-right : Robot -> Robot  
; Returns a Robot like r, but turned either 90 degrees left or right.  
(define (robot-left r) ...)  
(define (robot-right r) ...)  
 
; robot-x : Robot -> Coordinate  
; robot-y : Robot -> Coordinate  
; Returns the x or y component of the Robot's location.  
(define (robot-x r) ...)  
(define (robot-y r) ...)  
 
; robot-forward : Robot NonNegReal -> Robot  
; Returns a Robot like r, but moved forward by d pixels.  
; If the robot is inside the room and moving would put any part of the  
; robot outside the room, the robot should stop at the wall that it's facing.  
(define (robot-forward r d) ...)  

###2 A Finite State Machine (FSM)

####2.1 Additional Preliminaries

Save your solutions for this problem to a file named fsm.rkt.  
Run the following expression (you must have required extras.rkt) to check that your file is properly named and is in the proper directory:  
(check-location "02" "fsm.rkt")

Add these additional provides at the top of your file (below the requires), so that we can test your solution:  
(provide INITIAL-WORLD)  
(provide next-state)  
(provide render)  
(provide stop?)  
(provide accept-state?)  
(provide error-state?)  

####2.2 Problem Description

UPDATE 2015-01-23: Added INITIAL-WORLD to provide list.

Do exercise 100 from chapter 6.2
You must include the following in your solution file:
a data definition named World representing the states of the FSM (you may always define other data definitions if you want to);

a constant named INITIAL-WORLD;

the following run function and the functions it needs. The body of run (and only run) may be excluded from the 100% test coverage requirement.

(define (run w)  
  (big-bang w   
            (on-key next-state)  
            (to-draw render)  
            (stop-when stop?)))  
Make sure no big-bang simulation starts when executing the program in DrRacket. However, calling (run INITIAL-WORLD) should start your simulation.;

and the following additional functions:    
; accept-state? : World -> Boolean  
; Returns true if World w is an accepting state.  
(define (accept-state? w) ...)  
; error-state? : World -> Boolean  
; Returns true if World w is an error state.  
(define (error-state? w) ...)
  
###3 One-line Editor  

####3.1 Additional Preliminaries

Save your solutions for this problem to a file named editor.rkt.
Run the following expression (you must have required extras.rkt) to check that your file is properly named and is in the proper directory:
(check-location "02" "editor.rkt")

Add these additional provides at the top of your file (below the requires), so that we can test your solution:  
(provide render)  
(provide edit)  
(provide string->editor)  
(provide editor-pre)  
(provide editor-post)  
(provide editor-pos)  

####3.2 Problem Description

Imagine you are writing the software for a label maker by doing exercises 72-75 from chapter 5.6.
You may use the pre and post data definitions from the chapter, or the alternative in exercise 76, or some other data design of your choosing. Regardless of your choice, be prepared to explain the pros and cons of your data design during your code walk. A portion of your presentation grade this week is dependent on your explanation.  
In addition to the functions named in the problem description, you must also define the following functions:  
; string->editor : String -> Editor  
; Returns an Editor containing text str and cursor at position 0.  
(define (string->editor str) ...)  
; editor-pre : Editor -> String  
; Returns the text in editor e before the cursor.  
(define (editor-pre e) ...)  
; editor-post : Editor -> String  
; Returns the text in editor e after the cursor.  
(define (editor-post e) ...)  
; editor-pos : Editor -> Natural  
; Returns the position of the cursor in editor e.  
(define (editor-pos e) ...)  