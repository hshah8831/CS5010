;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-avail) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")
(require "sched-general.rkt")
(require rackunit)
(define TIME-ON-TASK 45) ; hours

(provide schedule/avail-ok?)
(provide schedule/avail)
(provide avg-choice)

;==================== schedule/avail-ok? ====================

; schedule/avail-ok? : StudentAvails CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (schedule/avail-ok? 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)) SAMPLE-CWS)
   FALSE
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/avail-ok? 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)
          (make-student CCS-DAN SAMPLE-PREFONE)
          (make-student CCS-TIM SAMPLE-PREFEMPTY)) SAMPLE-CWS)
   FALSE
   "CodeWalk schedule failed"))
; STRATEGY : Data Decomposition on students : StudentAvails
(define (schedule/avail-ok? students cws)
  (schedule/general-ok? students cws avail))

;==================== schedule/avail ====================

; schedule/avail : StudentAvails CodeWalks -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (schedule/avail 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)) SAMPLE-CWS )
   (list
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
    (make-codewalk '4:00pmTues empty 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/avail 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)
          (make-student CCS-AMY SAMPLE-PREFFULL)) SAMPLE-CWS )
   (list
    (make-codewalk '2:00pmTues (list 'amy 'linda 'steve) 3)
    (make-codewalk '4:00pmTues empty 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succesful")
  (check-equal?
   (schedule/avail 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)
          (make-student CCS-DAN SAMPLE-PREFONE)
          (make-student CCS-JAI SAMPLE-PREFONE)) SAMPLE-CWS )
   FALSE
   "CodeWalk schedule failed")
  (check-equal?
   (schedule/avail 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)
          (make-student CCS-DAN SAMPLE-PREFONE)
          (make-student CCS-TIM SAMPLE-PREFEMPTY)) SAMPLE-CWS )
   FALSE
   "CodeWalk schedule failed")
  (check-equal?
   (schedule/avail 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)
          (make-student CCS-DAN SAMPLE-PREFTWO)) SAMPLE-CWS )
   (list
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'dan) 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk scheduled succesful"))
; STRATEGY : Data Decomposition on students : StudentAvails
(define (schedule/avail students cws)
  (schedule/general students cws avail))

;==================== avg-choice ====================

; avg-choice : StudentAvails CodeWalks -> PosReal
; WHERE: (schedule/avail-ok? students cws) = #t
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (avg-choice 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)
          (make-student CCS-DAN SAMPLE-PREFTWO)) 
    (list
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'dan) 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3)))
   1.25
   "CodeWalk scheduled given for rank calculation")
  (avg-choice 
    empty 
    (list
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'dan) 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3)))
  0 "empty student list given" )
; STRATEGY : Function Composition
(define (avg-choice students cws)
  (if (zero? (length students))
      ZERO
      (/ (calc-total-rank students cws) (length students))))

;========== calc-total-rank ==========

; calc-total-rank : StudentAvails CodeWalks -> PosReal
; Returns the sum of every students rank.
; EXAMPLE :
(begin-for-test
  (calc-total-rank 
    (list (make-student CCS-STEVE SAMPLE-PREFONE)
          (make-student CCS-BRIAN SAMPLE-PREFONE)
          (make-student CCS-LINDA SAMPLE-PREFTWO)
          (make-student CCS-DAN SAMPLE-PREFTWO)) 
    (list
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'dan) 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3)))
    5
    "all given first pref except dan")
; STRATEGY : Data Decomposition in students : StudentAvail 
(define (calc-total-rank students cws)
  (foldr
   ; Student Natural -> Natural
   ; gets the rank for a student and adds it to accumulator.
   ; STRATEGY : Function Composition
   (λ (student n) (+ n (get-rank-for-student student cws))) ZERO students))

;========== get-rank-for-student ==========

; get-rank-for-student : StudentAvail CodeWalks -> Natural
; Returns the rank of the student based on the preferrence given to 
; him in the schedule.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (get-rank-for-student 
    (make-student CCS-DAN SAMPLE-PREFTWO)
    (list
     (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
     (make-codewalk '4:00pmTues (list 'dan) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed empty 3)
     (make-codewalk '4:00pmWed empty 3)))
   2
   "Dan given second preference"))
; STRATEGY : Data Decomposition on student : StudentAvail
(define (get-rank-for-student student cws)
  (get-rank-of-time (get-time-codewalk (student-id student) cws) 
                                (student-prefs student)))
; Time ListOf<Time> -> Natural
; WHERE : t is present in lot.
; Returns the rank of the t in lot.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-rank-of-time '2:00pmTues SAMPLE-PREFTWO)
   1
   "input is first in the pref")
  (check-equal?
   (get-rank-of-time '4:00pmTues SAMPLE-PREFTWO)
   2
   "input is second in the pref"))
; STRATEGY : Data Decomposition on lot : ListOf<Time>
(define (get-rank-of-time t lot)
  (length (memf (λ (x) (symbol=? x t)) (reverse lot))))

;========== get-time-codewalk ==========

; get-time-codewalk : StudentID CodeWalks -> Time
; Returns time of the codewalk sid is allocated.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (get-time-codewalk 'dan
    (list
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'dan) 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3)))
   '4:00pmTues
   "given dan who allocated in the second codewalk")
  (check-equal?
   (get-time-codewalk 'jai
    (list
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'dan) 3)
    (make-codewalk '6:00pmTues empty 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3)))
   empty
   "given jai who is not allocated in the codewalk"))
; STRATEGY : Data Decomposition on cws : CodeWalks
(define (get-time-codewalk sid cws)
  (cond
    [(empty? cws) empty]
    [else 
     (if (student-allocated? sid (first cws))
           (codewalk-time (first cws))
           (get-time-codewalk sid (rest cws)))]))

;========== student-allocated? ==========

; student-allocated? : StudentID CodeWalk -> Boolean
; Returns true if sid allocated this cw.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (student-allocated? 
    'steve 
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3))
   TRUE
   "steve present in the codewalk")
  (check-equal?
   (student-allocated? 
    'dan 
    (make-codewalk '2:00pmTues (list 'linda 'brian 'steve) 3))
   FALSE
   "dan not present in the codewalk"))
; STRATEGY : Data Decomposition cw : CodeWalk
(define (student-allocated? sid cw)
  (compare-sid-los? sid (codewalk-students cw)))

;========== compare-sid-los? ==========

; compare-sid-los? StudentID ListOf<StudentID> -> Boolean
; Returns true if the sid matches with any of the student in los.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (compare-sid-los? 'steve '(tim brian steve))
   TRUE
   "steve present in the list")
  (check-equal?
   (compare-sid-los? 'steve '(tim brian dan))
   FALSE
   "steve not present in the list"))
; STRATEGY : Funnction Composition
(define (compare-sid-los? sid los)
  (ormap (λ (x) (symbol=? sid x)) los))