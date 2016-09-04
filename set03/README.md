###1 Bouncing Ball

####1.1 Additional Preliminaries

Save your solutions for this problem to a file named bounce.rkt.  
Run the following expression (you must have required extras.rkt) to check that your file is properly named and is in the proper directory:  
(check-location "03" "bounce.rkt")

Add these additional provides at the top of your file (below the requires), so that we can test your solution:  
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

####1.2 Problem Description

Use the 2htdp/universe library and big-bang to implement the following game. You’ll also need the 2htdp/image library.  
Your solution should include the following function:  
; next-world : World -> World 
; Computes the next World state from the given World state.  
You should also provide an INITIAL-WORLD constant that satisfies the specifications of the problem.  
Make sure to define and use global constants where appropriate. Strive for a single point of control.   Someone should be able to modify any of the parameters described in the problem by modifying just one place in your program.  
Note: Any gameplay images are intended to demonstrate approximate behavior and may differ from the exact required parameters.  

#####1.2.1 The Ball

The game involves a bouncing ball that:
starts at the middle-top of a 300 x 400 scene, where the top of the ball is flush with the top of the canvas;  

is a solid, black circle with a radius of 20 pixels;

moves horizontally according to the following rules:
The ball has an initial horizontal velocity of 3 pixels/tick, moving right.  

The ball maintains a constant horizontal velocity of 3 pixels/tick with the following exception:  

If a tick would put the ball flush against the left or right edge of the canvas, or if a tick would put any part of the ball past the left or right edge of the canvas, then instead the ball moves just enough so that it is flush against the left or right edge of the canvas, respectively, at the end of the tick, moving the in opposite direction at 3 pixels/tick;  

moves vertically according to the following rules:
The ball has no initial vertical velocity.  

The ball experiences a "gravity" acceleration of 1 pixel/tick2 downwards; in other words, the vertical velocity increases by 1 pixel/tick, in the downward direction, on most ticks.  

The ball’s vertical position changes according to the following equation on most ticks: y(t) = y0 + vy0 t + 0.5ay t2, where:  
y(t) is the new vertical position after the tick,  

y0 is the current y coordinate,  

vy0 is the current vertical velocity,  

t is the ellapsed time,  

and ay is the vertical acceleration.  

UPDATE 2015-01-31: Updated some subscripts for clarity and consistency. No problem set requirements have changed.  

The ball’s vertical velocity changes according to the following equation on most ticks: **vy(t) = vy0 + ay t**, where:  
**vy(t)** is the new vertical velocity after the tick,  

**vy0** is the current vertical velocity,  
 
**t** is the ellapsed time,  

and **ay** is the vertical acceleration.  

These vertical movement equations still apply even if the ball is near, at, or past the top of the canvas, so the ball may temporarily move off the top of the canvas.

Analogous to the horizontal direction, the ball’s vertical movement behaves differently when close to the bottom of the canvas (i.e., the "ground"):  

If a tick would put the ball flush against the ground, or if a tick would put any part of the ball past the ground, then instead the ball moves just enough so that it is flush against (i.e., "hits") the ground at the end of the tick.  

OLD : Then, solve the y(t) quadratic equation above to compute how much "time" it takes for the ball to move this distance to the ground.  

UPDATE 2015-01-31: (This is just a rewording of the above sentence. Nothing about the problem set has changed.) Then, replace y(t) in the equation above with the ball’s y position at the ground, and replace the other known variables with their values as well. This leaves t unknown. Solve for t using the quadratic formula.  

In other words, the two solutions to a quadratic equation **ax2 + bx + c = 0** are:  



(where b2-4ac is known as the "discriminant"). Assume that time cannot move backwards.

Then, use this computed time value and the vy(t) equation above to calculate the ball’s vertical velocity at the moment it hits the ground.

When the ball hits the ground, the direction of its velocity flips to opposite direction at the end of the tick.

When the ball hits the ground, the magnitude of its velocity becomes 90% of what it was at the moment the ball hit the ground. In other words, the ball has a bounce coefficient of 0.9.



Your solution should include the following functions:  
; world-ball : World -> Ball  
; Returns a representation of the ball.  
 
; ball-x : Ball -> Coordinate  
; ball-y : Ball -> Coordinate  
; Returns the x or y position of the given Ball.  

#####1.2.2 Exact Numbers, Inexact Numbers, and Rounding

The Beginning Student language performs exact (rather than floating point) arithmetic. However, doing so severely degrades the performance of the game. Thus, after a tick, round any numbers in your World representation via the following function:  
(define ε 0.001)  
; round/ε : Real -> Real  
; Rounds x to within ε precision  
(define (round/ε x)  
  (exact->inexact (* (inexact->exact (round (/ x ε))) ε)))  

You should also use the check-= RackUnit function where appropriate. When comparing two inexact numbers, use the same ε as above. When comparing a result computed from rounded inputs to an unrounded number, the rounded number should be within 0.1% of expected.  
Here are some examples that may help you check if you are on the right track.  
; Assume any constants used in these tests are as described in the problem.  
 
; new-y : (your signature may vary)  
; Computes a new y position based on current y position, current y velocity,  
; acceleration, and time ellapsed.  
; Strategy: function composition  
(define (new-y curr-y curr-vel a t)  
  (+ curr-y (* curr-vel t) (* 0.5 a t t)))  
 
; addition functions to help test ——————–  
 
; world-ball-y : World -> Coordinate  
; Return's the y position of w's Ball.  
; Strategy: double decomposition(!) on w : World, and Ball  
; (Probably) acceptable since the function is for testing   
; and is less likely to be read by others.  
(define (world-ball-y w)  
  (ball-y (world-ball w)))  
 
(begin-for-test  
  (check-= (world-ball-y  
            (next-world INITIAL-WORLD))  
           (new-y BALL-START-Y BALL-Y-VEL-START G 1)  
           ε  
           "one tick from start")  
  (check-= (world-ball-y  
            (next-world  
             (next-world INITIAL-WORLD)))  
           (new-y BALL-START-Y BALL-Y-VEL-START G 2)  
           ε  
           "two ticks from start"))  
 
; next-world-n : Natural World -> World  
; Computes the next world n times, starting from w  
; This function doesnt fit into any of our current strategies due to its  
; recursive nature. (You won't have to write any other recursive  
; functions this week.)  
(define (next-world-n n w)  
  (if (zero? n)  
      w  
      (next-world-n (sub1 n) (next-world w))))  
 
(begin-for-test  
  (check-= (world-ball-y  
            (next-world-n 26 INITIAL-WORLD))  
           (new-y BALL-START-Y BALL-Y-VEL-START G 26)  
           ε  
           "26 ticks from start")  
  (check-= (world-ball-y  
            (next-world-n 27 INITIAL-WORLD))  
           BALL-GROUND-Y  
           ε  
           "27 ticks from start: ball on ground"))  

The following additional test passes if the bounce coefficient is set to 1 (it’s easy to adjust parameters in your program right?).  
(begin-for-test  
  (check-= (world-ball-y  
            (next-world-n 27 (next-world-n 27   INITIAL-WORLD)))  
           BALL-START-Y  
           (* ε BALL-START-Y)  
           "27*2 ticks from start: ball back to start"))  

#####1.2.3 Input

The simulation should handle the following keyboard inputs:
When "p" is pressed, the game should pause. When the game is paused, the text *PAUSED* should appear at the top-left of the canvas. Pressing "p" again resumes the game. During the paused state, the ball properties should not change and all inputs (except "p") should be ignored.

Your solution should include the following function:  
; world-paused? : World -> Boolean  
; Indicates whether the game is paused.  
; key-handler : World KeyEvent -> World  
; Computes the next world after a key press.  


The simulation should handle the following mouse inputs:  
when the mouse is clicked (i.e., on "button-down"  event) inside the ball, an "explosion" should occur"; a mouse positioned on the ball’s edge is not considered "inside" the ball.

Your solution should include the following function:  
; mouse-handler : World Integer Integer MouseEvent -> World  
; Computes the next world after a mouse event.
An explosion:    
increases the ball’s velocity by 10 pixels/tick in the upward direction, if the ball currently has an upward velocity;  
UPDATE 2015-01-29: this change in velocity only happens once, at the start of the explosion;

changes the ball’s velocity to 10 pixels/tick in the upward direction, if the ball currently has a downward velocity;  
UPDATE 2015-01-29: this change in velocity only happens once, at the start of the explosion;

typically ends 10 ticks after it starts (i.e., on the 10th tick, there should be no indication of an explosion);  

ends early if:  
the ball hits the ground, in which case any ongoing explosion should stop immediately, or

another explosion starts, in which case the previous explosion should end;

is displayed as a solid, yellow, 12-point radial-star centered on top of the ball, with inner radius of twice the number of ticks since the start of the explosion, and outer radius of four times the number of ticks since the start of the explosion.

Your solution should include the following function:  
; ticks-since-click : World -> [0 10)  
; Returns the number of ticks since the last explosion, if there's  
; currently one. 0 means no explosion.  


Here are some examples that may be helpful (of course, avoid magic numbers).  
(begin-for-test  
  (check-true (zero? (ticks-since-click INITIAL-WORLD))  
              "no initial explosion")  
  (check-true (zero?  
               (ticks-since-click  
                (mouse-handler  
                 INITIAL-WORLD BALL-START-X BALL-START-Y "button-down")))  
              "explosion just started")  
  (check-equal? (ticks-since-click  
                 (next-world  
                  (mouse-handler  
                   INITIAL-WORLD BALL-START-X BALL-START-Y "button-down")))
                1  
                "1 tick after explosion")  
  (check-equal? (ticks-since-click  
                 (next-world-n 9  
                  (mouse-handler  
                   INITIAL-WORLD BALL-START-X BALL-START-Y "button-down")))  
                9  
                "9 ticks after explosion")  
  (check-true (zero?  
               (ticks-since-click  
                (next-world-n 10  
                 (mouse-handler  
                  INITIAL-WORLD BALL-START-X BALL-START-Y "button-down"))))  
              "10 ticks after explosion: explosion ended"))  

#####1.2.4 Scoring

The game should additionally:
display a score at the top-right of the canvas; the score is a count of the number of times the ball was clicked since the last time the ball hit the ground;

reset the score to 0 each time the ball hits the ground.

Your solution should include the following function:  
; score : World -> NonNegInt  
; Returns the current score.  
