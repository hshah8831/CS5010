;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;To import packages
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 5 ); Time taken to finish the task in hours

;;To export functions
(provide INITIAL-WORLD)
(provide next-state)
(provide render)
(provide stop?)
(provide accept-state?)
(provide error-state?)

;;Physical Constants:
(define WIDTH 100); The empty scene width
(define HEIGHT 100); The empty scene height
 
;;Graphical Constant:
;;(define MT (empty-scene WIDTH HEIGHT)); Empty Scene

;;A State is the color and can be one of
;;-AA - represents the AA state or the initial state of FSM
;;-BC - represents the BC state or intermediate state of FSM
;;-DD - represents the DD state or the final state of FSM
;;-ER - represents the ER state or the error state of the FSM
;;INTERP: The State represents the different states of an FSM 
;;which are denoted by corresponding colors.

;;TEMPLATE:
;;state-fn : state -> ???
;;(define (state-fn st)
;; (cond
;;   [(AA? st ) ...]
;;   [(BC? st ) ...]
;;   [(DD? st ) ...]
;;   [(ER? st ) ...]))

;;State constants:
(define AA "white"); 
(define BC "yellow")
(define DD "green")
(define ER "red")
(define INITIAL-WORLD AA)

;;------------------------------------------------------------------------------
;;AA? : State -> Boolean
;;The function checks if the current state is AA or not 
;;GIVEN: State c 
;;RETURNS: Boolean
;;EXAMPLE: AA? "white" -> #true
;;         AA? "yellow" -> #false 
;;STRATEGY: Function Composition
(define (AA? c) (string=? c AA))

;;Test Case 1 : If the current state is AA  
(begin-for-test (check-equal? (AA? AA) 
                              #true  
                "The check is successfull "))

;;Test Case 2 : If the current state is BC  
(begin-for-test (check-equal? (AA? BC) 
                              #false  
                "The check is successfull "))

;;------------------------------------------------------------------------------
;;BC? : State -> Boolean
;;The function checks if the current state is BC or not 
;;GIVEN: State s 
;;RETURNS: Boolean
;;EXAMPLE: (BC? "yellow") -> #true
;;         (BC? "white") -> #false  
;;STRATEGY: Function Composition
(define (BC? w) (string=? w BC))

;;Test Case 1 : If the current state is BC  
(begin-for-test (check-equal? (BC? BC) #true ) "The check is successfull")

;;Test Case 2 : If the current state is AA  
(begin-for-test (check-equal? (BC? AA) #false ) "The check is successfull")

;;------------------------------------------------------------------------------
;;DD? : State -> Boolean
;;The function checks if the current state is DD or not 
;;GIVEN: State c 
;;RETURNS: Boolean
;;EXAMPLE: DD? "green" -> #true
;;         DD? "yellow" -> #false 
;;STRATEGY: Function Composition
(define (DD? w) (string=? w DD))

;;Test Case 1 : If the current state is DD  
(begin-for-test (check-equal? (DD? DD) #true ) "The check is successfull")

;;Test Case 2 : If the current state is AA  
(begin-for-test (check-equal? (DD? AA) #false ) "The check is successfull")

;;------------------------------------------------------------------------------
;;ER? : State -> Boolean
;;The function checks if the given state is ER or not 
;;GIVEN: State c 
;;RETURNS: Boolean
;;EXAMPLE: ER? "red" -> #true
;;         ER? "yellow" -> #false 
;;STRATEGY: Function Composition
(define (ER? w) (string=? w ER))

;;Test Case 1 : If the current state is DD  
(begin-for-test (check-equal? (DD? DD) #true ) "The check is successfull")

;;Test Case 2 : If the current state is AA  
(begin-for-test (check-equal? (DD? AA) #false ) "The check is successfull")

;;------------------------------------------------------------------------------
;;run : State -> Image
;;The simulates the FSM as required using a big-bang call 
;;GIVEN: State w 
;;RETURNS: Image
;;EXAMPLE: (main AA) -> 
;;         (main ER) ->  
;;STRATEGY: Function Composition
(define (run w)
  (big-bang w
            (on-key next-state)
            (to-draw render)
            (stop-when stop?)))

;;------------------------------------------------------------------------------
;;run : State -> State
;;The generates the next state of the world depending on the keyevent occured 
;;GIVEN: State w0 
;;RETURNS: State w1
;;EXAMPLE: (next-state AA "a") -> BC
;;         (next-state BC "b") -> BC
;;         (next-state BC "c") -> BC
;;         (next-state BC "d") -> DD
;;         (next-state AA "b") -> ER
;;STRATEGY: Function Composition
(define (next-state w ke)
  (cond
    [(and (string=? ke "a") (AA? w)) BC]
    [(and (string=? ke "b") (BC? w)) BC]
    [(and (string=? ke "c") (BC? w)) BC]
    [(and (string=? ke "d") (BC? w)) DD]
    [else ER ]))

;;Test Case 1 : If the current state is AA  
(begin-for-test (check-equal? (next-state AA "a") "yellow")
                "The check for next-state")

;;Test Case 2 : If the current state is BC  
(begin-for-test (check-equal? (next-state BC "b") "yellow") 
                "The check for next-state")

;;Test Case 3 : If the current state is BC  
(begin-for-test (check-equal? (next-state BC "c") "yellow") 
                "The check for next-state")

;;Test Case 4 : If the current state is BC  
(begin-for-test (check-equal? (next-state BC "d") "green") 
                "The check for next-state")

;;Test Case 5 : If the current state is AA  
(begin-for-test (check-equal? (next-state AA "b") "red") 
                "The check for next-state")

;;Test Case 6 : If the current state is BC  
(begin-for-test (check-equal? (next-state BC "a") "red") 
                "The check for next-state")

;;------------------------------------------------------------------------------
;;render : State -> Image
;;The function generates the next state of the world depending 
;;on the keyevent occured 
;;GIVEN: State w0 
;;RETURNS: State w1
;;EXAMPLE: (render AA ) -> (rectangle 100 100 "solid" "white")
;;         (render BC ) -> (rectangle 100 100 "solid" "yellow")
;;         (render DD ) -> (rectangle 100 100 "solid" "green")
;;         (render ER ) -> (rectangle 100 100 "solid" "red")
;;STRATEGY: Function Composition
(define (render w)
  (rectangle 100 100 "solid" w))

;;Test Case 1 : If the current state is BC  
(begin-for-test (check-equal? (render BC ) 
                              (rectangle 100 100 "solid" "yellow")) 
                "The check is successful")

;;------------------------------------------------------------------------------
;;stop? : State -> Boolean
;;The function checks whether the current state is final or an error state 
;;which in turn will be used to determine the ending of the program. 
;;GIVEN: State w0 
;;RETURNS: Boolean
;;EXAMPLE: (stop? AA ) -> #false
;;         (stop? BC ) -> #false
;;         (stop? DD ) -> #true
;;         (stop? ER ) -> #true
;;STRATEGY: Function Composition
(define (stop? w)
  (or (DD? w) (ER? w)))

;;Test Case 1 : If the current state is BC  
(begin-for-test (check-equal? (stop? BC ) 
                              #false) 
                "To check whne not to stop")

;;Test Case 2 : If the current state is DD  
(begin-for-test (check-equal? (stop? DD ) 
                              #true) 
                "The check when to stop")

;;Test Case 3 : If the current state is ER  
(begin-for-test (check-equal? (stop? ER ) 
                              #true) 
                "To check when to stop")
;;------------------------------------------------------------------------------
;;accept-state? : World -> Boolean
;;The function returns true if World w is an accepting state 
;;GIVEN: State w 
;;RETURNS: Boolean
;;EXAMPLE: (accept-state? AA "a") -> BC
;;         (accept-state? BC "b") -> BC
;;         (accept-state? BC "c") -> BC
;;         (accept-state? BC "d") -> DD
;;         (accept-state? AA "b") -> ER
;;STRATEGY: Function Composition
(define (accept-state? w)
  (if (DD? w) #true #false ))

;;Test Case 1 : If the current state is BC  
(begin-for-test (check-equal? (accept-state? BC ) 
                              #false) 
                "To check accept state")

;;Test Case 2 : If the current state is DD  
(begin-for-test (check-equal? (accept-state? DD ) 
                              #true) 
                "To check accept state")

  
;;------------------------------------------------------------------------------
; error-state? : World -> Boolean
;;The function returns true if World w is an error state. 
;;GIVEN: State w0 
;;RETURNS: State w1
;;EXAMPLE: (error-state? AA "a") -> BC
;;         (error-state? BC "b") -> BC
;;         (error-state? BC "c") -> BC
;;         (error-state? BC "d") -> DD
;;         (error-state? AA "b") -> ER
;;STRATEGY: Function Composition
(define (error-state? w)
  (if (ER? w) #true #false ))

;;Test Case 1 : If the current state is BC  
(begin-for-test (check-equal? (error-state? BC ) 
                              #false) 
                "To check error state")

;;Test Case 2 : If the current state is DD  
(begin-for-test (check-equal? (error-state? ER ) 
                              #true) 
                "To check error state")

;;(run INITIAL-WORLD)