;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-general) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require "extras.rkt")
(require rackunit)

(provide (all-defined-out))

;==================== Constants ====================

(define EMPTY-LIST '())
(define TRUE #t)
(define FALSE #f)
(define ZERO 0)

;==================== Data definition ====================

;========== Time ==========

; A Time is a Symbol
; Represents a day of week and time.

; EXAMPLES:
; - '2:00pmTues 
; - '4:00pmTues

(define TUE200 '2:00pmTues)
(define TUE400 '4:00pmTues)
(define TUE600 '6:00pmTues)
(define WED200 '2:00pmWed)
(define WED400 '4:00pmWed)

;========== Preferences ==========

; A Preferences is a ListOf<Time>
; Represents a list of code walk times.

; EXAMPLES:
; (list TUE200)
; (list TUE200 TUE400)

; TEMPLATE:
; ListOf<X> template

(define SAMPLE-PREFONE (list TUE200))
(define SAMPLE-PREFTWO (list TUE200 TUE400))
(define SAMPLE-PREFFULL 
  (list TUE200 TUE400 TUE600 WED200 WED400))
(define SAMPLE-PREFEMPTY '())
(define SAMPLE-PREFFIVE 
  (list TUE200 TUE600 WED200 WED400))

;========== CodeWalk ==========

; A CodeWalk is a (make-codewalk Time ListOf<StudentID> PosInt)
; Represents a codewalk with time, assigned students, and a max capacity.
(define-struct codewalk (time students max))

; TEMPLATE:
; cw-fn : CodeWalk -> ???
(define (cw-fn c)
  (... (codewalk-time c) ...
       (students-fn (codewalk-students c)) ... (codewalk-max c) ...))

; EXAMPLES:
; - (make-codewalk TUE200 empty 1)
; - (make-codewalk TUE400 empty 1)

(define CW-TUE200 (make-codewalk TUE200 empty 3))
(define CW-TUE400 (make-codewalk TUE400 empty 3))
(define CW-TUE600 (make-codewalk TUE600 empty 3))
(define CW-WED200 (make-codewalk WED200 empty 3))
(define CW-WED400 (make-codewalk WED400 empty 3))

;========== CodeWalks ==========

; A CodeWalks is a ListOf<CodeWalk>
; Represents a codewalk schedule.

; EXAMPLES:
; (list (make-codewalk TUE200 empty 1) (make-codewalk TUE400 empty 1))

; TEMPLATE:
; ListOf<X> template

(define SAMPLE-CWS 
  (list CW-TUE200 CW-TUE400 CW-TUE600 CW-WED200 CW-WED400))

;========== StudentID ==========

; A StudentID is a Symbol
; Represents a student (or pair) via their ccs ID(s).

; EXAMPLES:
; - 'steve
; - 'linda

(define CCS-STEVE 'steve)
(define CCS-LINDA 'linda)
(define CCS-AMY 'amy)
(define CCS-BRIAN 'brian)
(define CCS-DAN 'dan)
(define CCS-JAI 'jai)
(define CCS-TIM 'tim)

;========== Max ==========

; a Max is a PosInt
; Represents the max enrollment number of a codewalk at a certain time

; EXAMPLES:
; - 1
; - 2

(define MAX-THREE 3)

;========== Student ==========

; A Student is one of:
; - StudentUnavail
; - StudentAvail

(define-struct student (id prefs))
; prefs represents the student preference.
; id represents the student id.

; EXAMPLES:
; - (make-student 'steve (list TUE200))
; - (make-student 'amy (list TUE200 TUE400))

; TEMPLATE : 
; student-fn : Student -> ???
(define (student-fn student)
  (... (student-id student)...(lot-fn (student-prefs student))...))

;========== Students ==========

; A Students is one of:
; - StudentUnavails
; - StudentAvails
; WHERE: there are no duplicate StudentIDs

; EXAMPLES:
; (list (make-student 'steve (list TUE200))
;       (make-student 'linda (list TUE200 TUE400)))

;========== StudentUnavail ==========

; A StudentUnavail is a (make-student StudentID Preferences)

; Represents a student and their unavailable times.

; TEMPLATE:
; su-fn : StudentUvail -> ???
(define (su-fn s)
  (... (student-id s) ... (lot-fn (student-prefs s)) ...))

; EXAMPLES:
; - (make-student 'steve (list TUE200))
; - (make-student 'amy (list TUE200 TUE400))

(define SAMPLE-SU (make-student CCS-STEVE SAMPLE-PREFTWO))

;========== StudentUnavails ==========

; A StudentUnavails is a ListOf<StudentUnavail>
; Represents a list of StudentUnavail

; EXAMPLES:
; (list (make-student 'steve (list TUE200))
;       (make-student 'linda (list TUE200 TUE400)))

; TEMPLATE:
; sus-fn : StudentUnavails -> ???
(define (sus-fn s)
  (cond 
    [(empty? s) ...]
    [else (... (su-fn (first s)) ... (sus-fn (rest s)) ...)]))

(define SAMPLE-SUSONE (list (make-student CCS-STEVE SAMPLE-PREFONE)
                            (make-student CCS-BRIAN SAMPLE-PREFONE)
                            (make-student CCS-LINDA SAMPLE-PREFTWO)))
; Amy does not prefer any slots
(define SAMPLE-SUSTWO (list (make-student CCS-STEVE SAMPLE-PREFONE)
                            (make-student CCS-LINDA SAMPLE-PREFTWO)
                            (make-student CCS-AMY SAMPLE-PREFFULL)))

(define SAMPLE-SUSTHREE (list (make-student CCS-STEVE SAMPLE-PREFONE)
                              (make-student CCS-BRIAN SAMPLE-PREFONE)
                              (make-student CCS-LINDA SAMPLE-PREFTWO)
                              (make-student CCS-DAN SAMPLE-PREFONE)
                              (make-student CCS-JAI SAMPLE-PREFFIVE)))

(define SAMPLE-SUSFOUR (list (make-student CCS-STEVE SAMPLE-PREFONE)
                             (make-student CCS-BRIAN SAMPLE-PREFONE)
                             (make-student CCS-LINDA SAMPLE-PREFTWO)
                             (make-student CCS-DAN SAMPLE-PREFONE)
                             (make-student CCS-TIM SAMPLE-PREFEMPTY)
                             (make-student CCS-JAI SAMPLE-PREFFIVE)))

;========== StudentAvail ==========

; A StudentAvail is a (make-student StudentID Preferences)
; Represents a student and their available times, most-preferred first.
; An unlisted time means the student is unavailable.

; EXAMPLES:
; - (make-student 'steve (list TUE200))
; - (make-student 'amy (list TUE200 TUE400))

; TEMPLATE:
; sa-fn : StudentAvail -> ???
(define (sa-fn s)
  (... (student-id s) ... (lot-fn (student-prefs s)) ...))
      
;========== StudentAvails ==========

; A StudentAvails is a ListOf<StudentAvail>

; EXAMPLES:
; (list (make-student 'steve (list TUE200))
;       (make-student 'linda (list TUE200 TUE400)))

; TEMPLATE:
; Same as ListOf<X>.

;==================== schedule/general-ok? ====================

; schedule/general-ok? : Students CodeWalks -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (schedule/general-ok?
    (list (make-student 'jen (list '4:00pmWed)))                        
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'jen) 3)) unavail)
   FALSE
   "jen not acceptable time")
  (check-equal?
   (schedule/general-ok?
    (list (make-student 'jen (list '2:00pmWed)))                        
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'june 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'jen) 3)) unavail)
   FALSE
   "over capacity")
  (check-equal?
   (schedule/general-ok?
    (list 
     (make-student 'jen (list '2:00pmTues '4:00pmTues '6:00pmTues '2:00pmWed))
     (make-student 'steve (list '4:00pmTues '6:00pmTues '4:00pmWed)))
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'jen) 3)) unavail)
   FALSE
   "steve scheduled multiples times"))
; STRATEGY : Function Composition
(define (schedule/general-ok? students cws f)
  (and (codewalk-within-capacity? cws)
       (stud-scheduled-once/accept-slot? students cws f))) 

;========== stud-scheduled-once/accept-slot? ==========

; stud-scheduled-once/accept-slot? ListOf<Student> CodeWalks -> Boolean
; Check if all students are scheduled once and
; are scheduled at the acceptable spots
; EXAMPLE :
(begin-for-test
  (check-equal?
   (stud-scheduled-once/accept-slot? 
    (list (make-student 'brian (list TUE600 WED200 WED400))) 
    (list
     (make-codewalk '2:00pmTues '(brian steve dan) 3)
     (make-codewalk '4:00pmTues '(brian phil james) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed empty 3)
     (make-codewalk '4:00pmWed empty 3)) unavail)
   FALSE
   "brian has been allocatd twice")
  (check-equal?
   (stud-scheduled-once/accept-slot? 
    (list (make-student 'brian (list TUE600 WED200 WED400))) 
    (list
     (make-codewalk '2:00pmTues '(brian steve dan) 3)
     (make-codewalk '4:00pmTues '(phil james) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed empty 3)
     (make-codewalk '4:00pmWed empty 3)) unavail)
   TRUE
   "brian has been allocatd once"))
; STRATEGY : Function Composition
(define (stud-scheduled-once/accept-slot? students cws f)
  (andmap
   ; Student -> Boolean
   ; returns true if the student is accepted exactly once.
   ; STRATEGY : Function Composition
   (λ (student)
     (check-every-student-once/accept? student cws f))
   students))

;========== check-every-student-once/accept? ==========

; check-every-student-once/accept? : StudentUnavial CodeWalks -> Boolean
; Returns true if the student has been allocated to single codewalk in cws.
; EXAMPLE : refer test cases above.
; STRATEGY : Function Composition 
(define (check-every-student-once/accept? student cws f)
  (and (student-acceptable-time? student cws f)
       (student-scheduled-once? student cws)))

;========== student-acceptable-time? ==========

; student-acceptable-time? : Student CodeWalks -> Boolean
; Returns if the student is scheduled in an acceptable time slot
; in the CodeWalk
; EXAMPLE :
(begin-for-test
  (check-equal?
   (student-acceptable-time? 
    (make-student 'tim (list '2:00pmTues '4:00pmTues '6:00pmTues '4:00pmWed))
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'amy 'dan 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed empty 3)) unavail)
   TRUE
   "Conditions satisfy")
  (check-equal?
   (student-acceptable-time? 
    (make-student
     'tim (list '2:00pmTues '4:00pmTues '2:00pmWed '6:00pmTues '4:00pmWed))
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'amy 'dan 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed empty 3)) unavail)
   FALSE
   "Conditions not satisfy")
  (check-equal?
   (student-acceptable-time? 
    (make-student 'jen (list '4:00pmWed))
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'amy 'dan 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'jen) 3)) unavail)
   FALSE
   "Conditions not satisfy"))
; STRATEGY : Data Decomposition on student : Student
(define (student-acceptable-time? Student cws f)
  (local ((define sid (student-id Student))
          (define preferred-times (student-prefs Student))
          (define student-codewalk-time (get-student-codewalk-time sid cws)))
    (f (member? student-codewalk-time preferred-times))))

; get-student-codewalk-time : CodeWalks -> Maybe<Time>
; Returns the student's Time from the first CodeWalks
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-student-codewalk-time
    'tim
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'amy 'dan 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed empty 3)))
   '2:00pmWed
   "Time of the StudentID")
  (check-equal?
   (get-student-codewalk-time
    'tim
    empty)
   false
   "Empty CodeWalks"))
; STRATEGY : Data Deocomposition on codewalks : CodeWalks
(define (get-student-codewalk-time studentid cws)
  (local (; allocated codewalk
          (define codewalks (get-student-codewalks studentid cws)))
    (if (empty? codewalks)
        FALSE
        (codewalk-time (first codewalks)))))

; get-student-codewalks : StudentID CodeWalks -> CodeWalks
; Returns all the CodeWalks that has the StudentID from the given CodeWalks.
; EXAMPLE : refer test case above.
; STRATEGY : Function Composition
(define (get-student-codewalks studentid cws)
  (filter
   (; CodeWalk -> StudentID
    ; Checks if studentid is inside the CodeWalk
    ; STRATEGY : Data Decomposition on x : CodeWalk
    λ (x)
     (member? studentid (codewalk-students x)))
   cws))

;========== student-scheduled-once? ==========

; student-scheduled-once? : Student CodeWalks -> Boolean
; Returns if the student is scheduled exactly once in the CodeWalk
; EXAMPLE :
(begin-for-test
  (check-equal?
   (student-scheduled-once?
    (make-student 'tim (list TUE400 TUE600 WED200 WED400))
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'amy 'dan 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed empty 3)))
   true
   "once")
  (check-equal?
   (student-scheduled-once?
    (make-student 'tim (list TUE400 TUE600 WED200 WED400))
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'amy 'dan 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed (list 'tim) 3)))
   false
   "more than once"))
; STRATEGY : Data Decomposition on student : Student
(define (student-scheduled-once? student cws)
  (local (; StudentID of student
          (define sid (student-id student))
          ; list if students allocated to cws
          (define lst-sid (get-list-studentid cws)))
    (and (member? sid lst-sid)
         (exactly-once? sid lst-sid))))

; exactly-once? : StudentID ListOf<StudentID> Natural -> Boolean
; Returns if the StudentID appears more than once in list of StudentID
; EXAMPLE :
(begin-for-test
  (check-equal?
   (exactly-once? 'tim (list 'tim 'hanna 'dan 'jun 'lily))
   true
   "once")
  (check-equal?
   (exactly-once? 'tim (list 'tim 'hanna 'dan 'jun 'tim 'lily))
   false
   "more than once")
  (check-equal?
   (exactly-once? 'tim (list 'tim 'hanna 'dan 'jun 'lily 'tim))
   false
   "more than once"))
; STRATEGY : Function Composition
(define (exactly-once? sid lst-sid)
  (local(
         ; StudentID -> StudentID
         ; number of occurence of sid in lst-sid
         ; STRATEGY : Function Composition
         (define no-of-occurences 
           (length (filter (λ (x) (symbol=? sid x)) lst-sid))))
    (= 1 no-of-occurences )))

; get-list-studentid : CodeWalks -> ListOf<StudentID>
; Returns the list of StudentID from CodeWalks
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-list-studentid
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed empty 3)))
   (list 'steve 'amy 'dan 'steven 'tammy 'daniel 'apple 'orange 'tim)
   "Get list of StudentID")
  (check-equal?
   (get-list-studentid
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'steve 'amy 'dan 'apple 'orange 'tim) 3)
     (make-codewalk '4:00pmWed empty 3)))
   (list 'steve 'amy 'dan 'steven 'tammy 'daniel 'steve 'amy 'dan 'apple
         'orange 'tim)
   "Get list of StudentID"))
; STRATEGY : Data Decompositino on cws : CodeWalks
(define (get-list-studentid cws)
  (cond
    [(empty? cws) EMPTY-LIST]
    [else (append (codewalk-students (first cws))
                  (get-list-studentid (rest cws)))]))

;========== codewalk-within-capacity? ==========

; codewalk-within-capacity? : CodeWalks -> Boolean
; Returns true if all the CodeWalks are within capacity
; EXAMPLE :
(begin-for-test
  (check-equal?
   (codewalk-within-capacity?
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed empty 3)
     (make-codewalk '4:00pmWed empty 3)))
   TRUE
   "CodeWalk within capacity")
  (check-equal?
   (codewalk-within-capacity?
    (list
     (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
     (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
     (make-codewalk '6:00pmTues empty 3)
     (make-codewalk '2:00pmWed (list 'apple 'orange 'tim 'han) 3)
     (make-codewalk '4:00pmWed empty 3)))
   FALSE
   "CodeWalk over capacity"))
; STRATEGY : Function Compostion
(define (codewalk-within-capacity? cws)
  (andmap
   ; CodeWalk -> Boolean
   ; Returns true if cw has students according to it's capacity
   ; STRATEGY : Data Decomposition on cw : CodeWalk
   (λ (x)
     (<= (length (codewalk-students x)) (codewalk-max x)))
   cws))

;========================= schedule/general ===================================

; schedule/general : Students CodeWalks [X -> X]-> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; Strategy: Generative Recursion
; TERMINATION ARGUMENT: either CodeWalks empty or Students empty
; EXAMPLE :
(begin-for-test
  (check-equal?
   (schedule/general
    (list
     (make-student 'steve (list '2:00pmTues))
     (make-student 'brian (list '2:00pmTues))
     (make-student 'linda (list '2:00pmTues '4:00pmTues)))
    SAMPLE-CWS unavail)
   (list
    (make-codewalk '2:00pmTues empty 3)
    (make-codewalk '4:00pmTues (list 'brian 'steve) 3)
    (make-codewalk '6:00pmTues (list 'linda) 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/general SAMPLE-SUSTWO SAMPLE-CWS unavail)
   FALSE
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/general
    (list
     (make-student 'steve (list '2:00pmTues))
     (make-student 'brian (list '2:00pmTues))
     (make-student 'linda (list '2:00pmTues '4:00pmTues))
     (make-student 'dan (list '2:00pmTues))
     (make-student 'jai (list '2:00pmTues '6:00pmTues '2:00pmWed '4:00pmWed)))
    SAMPLE-CWS unavail)
   (list
    (make-codewalk '2:00pmTues empty 3)
    (make-codewalk '4:00pmTues (list 'jai 'brian 'steve) 3)
    (make-codewalk '6:00pmTues (list 'dan 'linda) 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/general
    (list (make-student 'steve (list TUE600 WED200 WED400))
          (make-student 'brian (list TUE600 WED200 WED400))
          (make-student 'oliver (list TUE600 WED200 WED400))
          (make-student 'dan (list TUE600 WED200 WED400))
          (make-student 'kitty (list TUE600 WED200 WED400))
          (make-student 'tim (list TUE600 WED200 WED400))
          (make-student 'amy (list TUE200 WED200 WED400))
          (make-student 'mimi (list TUE200 WED200 WED400))
          (make-student 'jai (list TUE200 WED200 WED400)))
    SAMPLE-CWS unavail)
   (list
    (make-codewalk '2:00pmTues (list 'oliver 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'tim 'kitty 'dan) 3)
    (make-codewalk '6:00pmTues (list 'jai 'mimi 'amy) 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "CodeWalk schedule succeeded")
  (check-equal?
   (schedule/general
    (list (make-student 'steve (list TUE600 WED200 WED400))
          (make-student 'brian (list TUE600 WED200 WED400))
          (make-student 'oliver (list TUE600 WED200 WED400))
          (make-student 'dan (list TUE600 WED200 WED400))
          (make-student 'kitty (list TUE600 WED200 WED400))
          (make-student 'tim (list TUE600 WED200 WED400))
          (make-student 'amy (list TUE200 WED200 WED400))
          (make-student 'mimi (list TUE200 WED200 WED400))
          (make-student 'jai (list TUE200 TUE600 WED200 WED400)))
    SAMPLE-CWS unavail)
   false
   "CodeWalk schedule failed")
  (check-equal?
   (schedule/general
    (list (make-student 'steve (list TUE600 WED200 WED400)))
    empty unavail)
   false
   "CodeWalk schedule failed because empty CodeWalk"))
; STRATEGY : Generative Recursion
; Halting Measure : length of the list students is always decreasing.
(define (schedule/general students cws f)
  (cond
    [(empty? cws) FALSE]
    [(empty? students) cws]
    [else
     (local
       (; holds current student
        (define student (first students))
        ; checks if the preferences of the current student are all 
        ; out of capacity.
        (define all-pref-full?
          (cws-full? (get-preferred-cws (student-prefs student) cws f)))
        ; holds all the possible codewalks combinations that student can
        ; be allocated to.
        (define possible-lst-cws (add-student-to-all-prefs student cws f))
        ; holds one of the candidate codewalk that works for the student.
        (define candidate
          (schedule/general/list possible-lst-cws (rest students) f)))
       (if all-pref-full? 
           FALSE
           candidate))]))

;========== schedule/general/list ==========

; schedule/general/list : 
;                 ListOf<CodeWalks> Students [X -> X] -> Maybe<CodeWalks>
; Creates a CodeWalk by calling back schedule/general with students and the
; first possible CodeWalks. If the CodeWalk isn't working, call the next
; possible CodeWalks
; EXAMPLE :
(begin-for-test
  (check-equal?
   (schedule/general/list
    (list
     (list
      (make-codewalk '2:00pmTues '(steve) 3)
      (make-codewalk '4:00pmTues '() 3)
      (make-codewalk '6:00pmTues '() 3)
      (make-codewalk '2:00pmWed '() 3)
      (make-codewalk '4:00pmWed '() 3))
     (list
      (make-codewalk '2:00pmTues '() 3)
      (make-codewalk '4:00pmTues '(steve) 3)
      (make-codewalk '6:00pmTues '() 3)
      (make-codewalk '2:00pmWed '() 3)
      (make-codewalk '4:00pmWed '() 3)))
    (list
     (make-student 'brian '(6:00pmTues 2:00pmWed 4:00pmWed))
     (make-student 'oliver '(6:00pmTues 2:00pmWed 4:00pmWed))
     (make-student 'dan '(6:00pmTues 2:00pmWed 4:00pmWed))
     (make-student 'kitty '(6:00pmTues 2:00pmWed 4:00pmWed))
     (make-student 'tim '(6:00pmTues 2:00pmWed 4:00pmWed))
     (make-student 'amy '(2:00pmTues 2:00pmWed 4:00pmWed))
     (make-student 'mimi '(2:00pmTues 2:00pmWed 4:00pmWed))
     (make-student 'jai '(2:00pmTues 2:00pmWed 4:00pmWed)))
    unavail)
   (list
    (make-codewalk '2:00pmTues (list 'oliver 'brian 'steve) 3)
    (make-codewalk '4:00pmTues (list 'tim 'kitty 'dan) 3)
    (make-codewalk '6:00pmTues (list 'jai 'mimi 'amy) 3)
    (make-codewalk '2:00pmWed empty 3)
    (make-codewalk '4:00pmWed empty 3))
   "Returns a list of CodeWalks"))
; STRATEGY : Data Decomposition on lst-cws : ListOf<CodeWalks>
(define (schedule/general/list lst-cws students f)
  (cond
    [(empty? lst-cws) FALSE]
    [else (local
            (; holds the result of the students getting allocated to the 
             ; codewalks.
             (define result/#f (schedule/general students (first lst-cws) f)))
            (if (false? result/#f)
                (schedule/general/list (rest lst-cws) students f)
                result/#f))]))

;========== cws-full? ==========

; cws-full? : CodeWalks -> Boolean
; Returns true if all the codewalk in cws are full in capacity.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (cws-full? (list
               (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
               (make-codewalk '4:00pmTues (list 'steven 'tammy 'daniel) 3)
               (make-codewalk '6:00pmTues (list 'steven 'tammy 'daniel) 3)))
   TRUE "")
  (check-equal?
   (cws-full? (list
               (make-codewalk '2:00pmTues (list 'steve 'amy 'dan) 3)
               (make-codewalk '4:00pmTues (list 'steven 'daniel) 3)
               (make-codewalk '6:00pmTues (list 'steven 'tammy 'daniel) 3)))
   FALSE ""))
; STRATEGY : Function Composition
(define (cws-full? cws)
  (andmap 
   ; CodeWalk -> Boolean
   ; Returns true if cw is out of seat.
   ; STRATEGY : Data Decomposition on cw : CodeWalk
   (λ (cw) (= (length (codewalk-students cw))(codewalk-max cw)))
          cws))

;========== add-student-to-pref-cw ==========

; add-student-to-pref-cw : 
;            Time Student CodeWalks [X -> X] -> CodeWalks
; Returns the CodeWalks that has su assigned to pref CodeWalk
; EXAMPLE :
(begin-for-test
  (check-equal?
   (add-student-to-pref-cw
    '4:00pmTues
    (make-student 'jai '(2:00pmTues 2:00pmWed 4:00pmWed))
    (list
     (make-codewalk '2:00pmTues (list 'oliver 'brian 'steve) 3)
     (make-codewalk '4:00pmTues (list 'tim 'kitty 'dan) 3)
     (make-codewalk '6:00pmTues (list 'jai 'mimi 'amy) 3)
     (make-codewalk '2:00pmWed empty 3)
     (make-codewalk '4:00pmWed empty 3)) unavail)
   empty
   "Empty because both 'jai's preferred time are full"))
; STRATEGY : Data Decomposition on cws : CodeWalks
(define (add-student-to-pref-cw pref su cws f)
  (cond
    [(empty? cws) EMPTY-LIST]
    [else
     (local
       ((define result (add-student-to-pref-cw pref su (rest cws) f)))
       (if (symbol=? (codewalk-time (first cws)) pref)
           (add-student-to-codewalks pref su cws f)
           (next-codewalk pref su cws f result)))]))

; add-student-to-codewalks : ListOf<Time> Student CodeWalks [X -> X]
; Adds the student to the CodeWalk if the capacity hasn't been met
; EXAMPLE : Refer test case above
; STRATEGY : Data Decomposition on cws : CodeWalks
(define (add-student-to-codewalks pref su cws f)
  (if (capacity-ok? (first cws))
      (cons (assign-student-codewalk su (first cws)) (rest cws))
      (add-student-to-pref-cw pref su empty f)))

; next-codewalk : ListOf<Time> Student CodeWalks [X -> X] Boolean
; Returns the next CodeWalks if result isn't empty
; EXAMPLE : Refer test case above
; STRATEGY : Data Decomposition on cws : CodeWalks
(define (next-codewalk pref su cws f result)
  (if (empty? result)
      result
      (cons (first cws) (add-student-to-pref-cw pref su (rest cws) f))))

;========== add-student-to-all-prefs ==========

; add-student-to-all-prefs : 
;                         Student CodeWalks [X -> X] -> ListOf<CodeWalks>
; Adds su to his preferred CodeWalk Time
; EXAMPLE :
(begin-for-test
  (check-equal?
   (add-student-to-all-prefs
    (make-student 'jai '(2:00pmTues 2:00pmWed 4:00pmWed)) 
    (list
     (make-codewalk '2:00pmTues '(oliver brian steve) 3)
     (make-codewalk '4:00pmTues '(tim kitty dan) 3)
     (make-codewalk '6:00pmTues '(mimi amy) 3)
     (make-codewalk '2:00pmWed '() 3)
     (make-codewalk '4:00pmWed '() 3)) unavail)
   (list
    (list
     (make-codewalk '2:00pmTues '(oliver brian steve) 3)
     (make-codewalk '4:00pmTues '(tim kitty dan) 3)
     (make-codewalk '6:00pmTues '(jai mimi amy) 3)
     (make-codewalk '2:00pmWed '() 3)
     (make-codewalk '4:00pmWed '() 3)))
   "Adds student to CodeWalks"))
; STRATEGY : Function Composition
(define (add-student-to-all-prefs su cws f)
  (flatten (map 
            ; Time -> CodeWalks
            ; Adds the student to his/her preferred CodeWalk Time
            ; STRATEGY : Function Composition
            (λ (t)
              (add-student-to-pref-cw t su cws f))
            (get-preferred-time su cws f))))

;========== assign-student-codewalk ==========

; assign-student-codewalk : Student CodeWalk -> CodeWalk
; Adds/Assigns the student to the given codewalk cw
; STRATEGY : Data Decomposition on cw : CodeWalk
; EXAMPLE :
(begin-for-test
  (check-equal?
   (assign-student-codewalk
    (make-student 'tim (list TUE400))
    (make-codewalk TUE400 (list 'ryan 'steve) 3))
   (make-codewalk '4:00pmTues (list 'tim 'ryan 'steve) 3)
   "Assigns the s to the given CodeWalk with the time matching in CodeWalks")
  (check-equal?
   (assign-student-codewalk
    (make-student 'tim (list TUE400))
    empty)
   empty
   "Empty CodeWalk"))
; STRATEGY : Data Decomposition on cw : CodeWalk
(define (assign-student-codewalk s cw)
  (if (empty? cw)
      empty
      (make-codewalk (codewalk-time cw) 
                     (append (list (student-id s))(codewalk-students cw)) 
                     (codewalk-max cw))))

;========== get-preferred-time ==========

; get-preferred-time : Student CodeWalks [X -> X]-> List<Time>
; Returns su's preferred time for CodeWalks
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-preferred-time
    (make-student 'jai '(2:00pmTues 2:00pmWed 4:00pmWed)) SAMPLE-CWS unavail)
   (list '4:00pmTues '6:00pmTues)
   "Returns the preferred time for the student"))
; STRATEGY : Function Composition
(define (get-preferred-time su cws f)
  (map 
   ; CodeWalk -> Time
   ; pulls time of the codewalk.
   ; STRATEGY : Function Composition
   (λ (cw) (codewalk-time cw))
       (filter (λ (cw) (preferred-codewalk? cw su f)) cws)))

;========== flatten ==========

; flatten : List<X> -> List<X>
; Remove the empty from the list
; EXAMPLE :
(begin-for-test
  (check-equal?
   (flatten (list 1 empty 3 empty 5 6))
   (list 1 3 5 6)
   " Remove the empty from the list"))
; STRATEGY : Function Composition
(define (flatten lst)
  (filter 
   ; X -> Boolean
   ; checks if x is empty or not
   ; STRATEGY : Function Composition
   (λ (x) (not (empty? x))) lst)) 

;========== preferred-codewalk? ==========

; preferred-codewalk? : CodeWalk Student [X-> X]-> Boolean
; Returns true if the codewalk is su's preferred CodeWalk.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (preferred-codewalk?
    (make-codewalk '2:00pmTues (list 'oliver 'brian 'steve) 3)
    (make-student 'jai '(2:00pmTues 2:00pmWed 4:00pmWed)) unavail)
   false
   "Not at 'jai's preferred time"))
; STRATEGY : Data Decomposition on cw : CodeWalk 
;                      student : Student
(define (preferred-codewalk? cw student f)
  (f (member? (codewalk-time cw) (student-prefs student))))

;========== capacity-ok? ==========

; capacity-ok? CodeWalk -> Boolean
; Returns true if the CodeWalk students number is
; less than the CodeWalks's max number
; EXAMPLE :
(begin-for-test
  (check-equal?
   (capacity-ok?
    (make-codewalk TUE400 (list 'ryan 'steve) 3))
   TRUE
   "Not full"))
; STRATEGY : Data Decomposition on cw : CodeWalk
(define (capacity-ok? cw)
  (< (length (codewalk-students cw)) (codewalk-max cw)))

;========== get-preferred-cws ==========

; get-preferred-cws : ListOf<Time> CodeWalks [X -> X]-> CodeWalks
; Returns a list of preferred codewalks.
; EXAMPLE :
(begin-for-test
  (check-equal?
    (get-preferred-cws (list '2:00pmTues '4:00pmTues) SAMPLE-CWS unavail)
   (list  CW-TUE600 CW-WED200 CW-WED400)
   "given preferences such that first two of the codewalks are not preferred")
  (check-equal?
   (get-preferred-cws empty SAMPLE-CWS unavail)
   SAMPLE-CWS
   "student allows all the codewalks as preferrable")
  (check-equal?
   (get-preferred-cws
    (list '2:00pmTues '4:00pmTues '6:00pmTues '2:00pmWed '4:00pmWed) 
    SAMPLE-CWS unavail)
   empty
   "student is not available in any of the time slots provided"))
; STRATEGY : Data Decomposition on cws : CodeWalks
(define (get-preferred-cws lot cws f)
  (cond
    [(empty? cws) EMPTY-LIST]
    [else (append (if (preferred-cw? lot (first cws) f)
                      (list (first cws))
                      empty)
                  (get-preferred-cws lot (rest cws) f))])) 

;========== preferred-cw? ==========

; preferred-cw? : ListOf<Time> CodeWalk [X -> X] -> Boolean
; Returns true if the cw is a preferred codewalk.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (preferred-cw? SAMPLE-PREFONE CW-TUE200 unavail)
   FALSE
   "Not a preferred codewalk")
  (check-equal?
   (preferred-cw? SAMPLE-PREFONE CW-TUE400 unavail)
   TRUE
   "Preferred codewalk")
  (check-equal?
   (preferred-cw? SAMPLE-PREFTWO CW-TUE400 unavail)
   FALSE
   "Preferred codewalk")
  (check-equal?
   (preferred-cw? SAMPLE-PREFTWO CW-WED200 unavail)
   TRUE
   "Preferred codewalk"))
; STRATEGY : Function Composition
(define (preferred-cw? lot cw f)
  (if (eq? f unavail)
      (andmap 
       ; Time -> Boolean
       ; Returns true if the given time is preferred time.
       ; STRATEGY : Data Decomposition on cw : CodeWalk
       (λ (x) (f (symbol=? x (codewalk-time cw)))) lot) 
      (ormap 
       ; Time -> Boolean
       ; Returns true if the given time is preferred time.
       ; STRATEGY : Data Decomposition on cw : CodeWalk
       (λ (x) (f (symbol=? x (codewalk-time cw)))) lot)))

;========== avail ==========

; avail : X -> X
; For avail, we simply returns the original value
; EXAMPLE :
(begin-for-test
  (check-equal?
   (avail 1)
   1
   "Returns given value"))
; STRATEGY : Function Composition
(define (avail x) x)

;========== unavail ==========

; unavail : X -> X
; For unavail, we returns the negated value
; EXAMPLE :
(begin-for-test
  (check-equal?
   (unavail true)
   false
   "Returns negated value"))
; STRATEGY : Function Composition
(define (unavail x) (not x))