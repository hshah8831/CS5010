;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-unavail) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")
(require rackunit)
(require "sched-general.rkt")
(define TIME-ON-TASK 45) ; hours

(provide schedule/unavail-ok?)
(provide schedule/unavail)

;==================== schedule/unavail-ok? ====================
;
; schedule/unavail-ok? : StudentUnavails CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (schedule/unavail-ok?
    (list (make-student 'jen (list '4:00pmWed)))                        
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'jen) 3)))
   FALSE
   "jen not acceptable time")
  (check-equal?
   (schedule/unavail-ok?
    (list (make-student 'jen (list '2:00pmWed)))                        
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'june 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'jen) 3)))
   FALSE
   "over capacity")
  (check-equal?
   (schedule/unavail-ok?
    (list 
     (make-student 'jen (list '2:00pmTues '4:00pmTues '6:00pmTues '2:00pmWed))
     (make-student 'steve (list '4:00pmTues '6:00pmTues '4:00pmWed)))
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'jen) 3)))
   FALSE
   "steve scheduled multiples times"))
; STRATEGY : Data Decomposition on students : StudentUnavails
(define (schedule/unavail-ok? students cws)
  (schedule/general-ok? students cws unavail))

;========================= schedule/unavail ===================================

; schedule/unavail : StudentUnavails CodeWalks -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; Strategy: Generative Recursion
; TERMINATION ARGUMENT: explain either
; The function terminates because the limit of codewalks permutations
; EXAMPLE :
(begin-for-test
  (check-equal?
   (schedule/unavail
    (list
     (make-student 'steve (list '2:00pmTues))
     (make-student 'brian (list '2:00pmTues))
     (make-student 'linda (list '2:00pmTues '4:00pmTues)))
    SAMPLE-CWS)
   (list
    (make-codewalk '2:00pmTues empty 3)
    (make-codewalk '4:00pmTues (list 'brian 'steve) 3)
    (make-codewalk '6:00pmTues (list 'linda) 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/unavail SAMPLE-SUSTWO SAMPLE-CWS)
   FALSE
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/unavail
    (list
     (make-student 'steve (list '2:00pmTues))
     (make-student 'brian (list '2:00pmTues))
     (make-student 'linda (list '2:00pmTues '4:00pmTues))
     (make-student 'dan (list '2:00pmTues))
     (make-student 'jai (list '2:00pmTues '6:00pmTues '2:00pmWed '4:00pmWed)))
    SAMPLE-CWS)
   (list
    (make-codewalk '2:00pmTues empty 3)
    (make-codewalk '4:00pmTues (list 'jai 'brian 'steve) 3)
    (make-codewalk '6:00pmTues (list 'dan 'linda) 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/unavail
    (list (make-student 'steve (list TUE600 WED200 WED400))
          (make-student 'brian (list TUE600 WED200 WED400))
          (make-student 'oliver (list TUE600 WED200 WED400))
          (make-student 'dan (list TUE600 WED200 WED400))
          (make-student 'kitty (list TUE600 WED200 WED400))
          (make-student 'tim (list TUE600 WED200 WED400))
          (make-student 'amy (list TUE200 WED200 WED400))
          (make-student 'mimi (list TUE200 WED200 WED400))
          (make-student 'jai (list TUE200 WED200 WED400)))
    SAMPLE-CWS)
   (list
    (make-codewalk '2:00pmTues (list 'oliver 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'tim 'kitty 'dan) 3)
    (make-codewalk '6:00pmTues (list 'jai 'mimi 'amy) 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/unavail
    (list (make-student 'steve (list TUE600 WED200 WED400))
          (make-student 'brian (list TUE600 WED200 WED400))
          (make-student 'oliver (list TUE600 WED200 WED400))
          (make-student 'dan (list TUE600 WED200 WED400))
          (make-student 'kitty (list TUE600 WED200 WED400))
          (make-student 'tim (list TUE600 WED200 WED400))
          (make-student 'amy (list TUE200 WED200 WED400))
          (make-student 'mimi (list TUE200 WED200 WED400))
          (make-student 'jai (list TUE200 TUE600 WED200 WED400)))
    SAMPLE-CWS)
   false
   "CodeWalk schedule failed"))
; STRATEGY : Function Composition
(define (schedule/unavail students cws)
  (schedule/general students cws unavail))