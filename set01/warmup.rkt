;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require  2htdp/image)
(define TIME-ON-TASK 21 ); Time taken to finish the task in hours

;;exercise 13------------------------------------------------------------------
;; X-CoOrdinate is a real number
;;WHERE: X-CoOrdinate can be less, equal or greater than Zero.  
;;INTERP: X-CoOrdinate represents the position of a point on a 2-D plane with 
;;respect to X axis.
;; Y-CoOrdinate is a real number
;;WHERE: Y-CoOrdinate can be less, equal or greater than Zero.
;;INTERP: CoOrdinate represent the position of a point on a 2-D plane with 
;;respect to Y axis.
;; dist-origin : X-Coordinate Y-Coordinate -> Distance from Origin.
;;Computes the distance of a point(X,Y) in a 2-D plane from it's origin 
;;using the formula Distance
;;from Origin = Square Root of (X^2 + Y^2)
;;EXAMPLE: (dist-origin 3 4) -> 5
(define (dist-origin x y)
  (sqrt(+ (* x x)(* y y ))))
;;Test Case 1 : positive X & Y CoOrdinates.
(begin-for-test (check-equal? (dist-origin 3 4) 5) 
                "The Distance of Point(3,4) from origin is 5")
;;Test Case 2 : negative X & positive Y CoOrdinates.
(begin-for-test (check-equal? (dist-origin -3 4) 5) 
                "The Distance of Point(-3,4) from origin is 5")
;;Test Case 3 : negative X & negative Y CoOrdinates.
(begin-for-test (check-equal? (dist-origin -3 -4) 5) 
                "The Distance of Point(-3,-4) from origin is 5")
;;Test Case 4 : fractional X & Y CoOrdinates.
(begin-for-test (check-equal? (dist-origin -3/2 -4/2) 2.5) 
                "The Distance of Point(-3/2,-4/2) from origin is 2.5")

;;Exercise 14------------------------------------------------------------------
;;cube-edge-length is a real
;;WHERE: cube-edge-lenth > 0
;;INTERP: cube-edge-length represents the edge lenth of a cube.
;;volume is a real
;;INTERP: volume represents the volume of the cube depending on the length 
;;of the cube edge.
;;surface-area is a real
;;INTERP: surface-area represents the the surface area of the cube depending 
;;on the length of 
;;the cube edge.
;;cube-volume: cube-edge-length -> volume
;;computes the volume of the cube given the length of the edges (l), 
;;using the formula Volume of cube = l^3.
;;EXAMPLE:(cube-volume 3) -> 27 
(define (cube-volume l)
  (* l (sqr l)))
;;Test-Case-1: Check with a positive length of cube edge.
(begin-for-test (check-equal? (cube-volume 3) 27) 
                "The volume of the cube with edges of 
                 length 3 units is 27 units^3")
;;Test-Case-2: Check with a length of cube edge = Zero (Boundary condition).
(begin-for-test (check-equal? (cube-volume 0) 0) 
                "The volume of the cube with edges of 
                 length 0 units is 0 units^3")

;;cube-surface: cube-edge-length -> surface-area
;;computes the surface area of the cube given the length of the edges (l), 
;;using the formula 
;;Surface-Area of cube = 6*l^2
;;EXAMPLE: (cube-surface 3) -> 54
(define (cube-surface l)
  (* 6 (sqr l)))
;;Test-Case-1: Check with a positive length of cube edge.
(begin-for-test (check-equal? (cube-surface 3) 54 ) 
                "The volume of the cube with edges of 
                 length 3 units is 27 units^2")
;;Test-Case-2: Check with a length of cube edge = Zero (Boundary condition).
(begin-for-test (check-equal? (cube-surface 0) 0) 
                "The volume of the cube with edges of 
                 length 0 units is 0 units^2")

;;Exercise 15 & 16-------------------------------------------------------------
;;sentence is a string
;;WHERE: sentence cannot be empty
;;first-character is a character
;;WHERE: first-character represents the first character in the sentence
;;last-character is a character
;;WHERE: last-character represents the last character in the given sentence
;;string-first: sentence -> first character
;;returns the first character fc present in the given sentence s 
;;using a predefined function (string-ith string s interger i),which returns 
;;the ith character of the string
;;EXAMPLE: (string-first "hello world") -> "h"
(define (string-first s)
  (string-ith s 0))

;;Test-Case-1: Check with a non empty string.
(begin-for-test (check-equal? (string-first "hello world") "h" ) 
                "The first character of 'hello world' is 'h'")
;;string-last: sentence -> last-character
;;returns the last character lc of the given sentence s using a predefined 
;;function (string-ith string s interger i),which returns the ith character 
;;of the string & string-length string s) whic returns the length of the string
;;EXAMPLE:(string-last "hello world") -> "d" 
(define (string-last s)
  (string-ith s (- (string-length s) 1)))
;;Test-Case-1: Check with a non empty string.
(begin-for-test (check-equal? (string-last "hello world") "d" ) 
                "The first character of 'hello world' is 'd'")

;;exercise 17------------------------------------------------------------------
(define b1 #false);hard coded to #false
(define b2 #true); hard coded to #true
;;bool-imply: b1,b2 -> boolean(#true)
;;computes the OR operation on the boolean values b1 and b2
;;EXAMPLE:(bool-imply b1 b2) -> #true
(define (bool-imply b1 b2) (or b1 b2))
;;Test-Case-1: Check with the given values of b1 & b2 i.e. b1=#false & b2=#true
(begin-for-test (check-equal? (bool-imply b1 b2) #true ) 
                "b1 OR b2 => #true")

;;exercise 18 & 19-------------------------------------------------------------
;;image is an image
;;image-height is a real 
;;WHERE: image-height represents the height of the image in pixels
;;image-width is a real 
;;WHERE: image-width represents the width of the image in pixels
;;image-size is the rectangular area in which image resides
;;image-area: image -> image-size
;;Calculates the image-area ia, of a given image i, by multiplying the 
;;image-height ih (retrieved by image-height function) and 
;;image-width (retrieved by image-width function) iw
;;EXAMPLE: (image-area (rectangle 2 3 "solid" "red")) -> 6
(define (image-area i)
  ( * (image-width i) (image-height i)))
;;Test-Case-1: Checks the function for correct area for an image
(begin-for-test (check-equal? (image-area (rectangle 2 3 "solid" "red")) 6 ) 
                "The pixel area of the image is 6")

;;image-classify: image -> one of ("tall", "wide" or "square")
;;The function image cassify takes an image as input and determines whether it 
;;is "tall", "wide" or "square" depending on the height and width of the image
;;EXAMPLE: (rectangle 3 2 "solid" "red")) "wide"
(define tall "tall"); Defining the tall placeholder
(define wide "wide"); Defining the wide placeholder
(define sq "square"); Defining the square placeholder
(define (image-classify i)
  (cond
    [(= (image-height i) (image-width i)) sq]
    [(> (image-height i) (image-width i)) tall]
    [(< (image-height i) (image-width i)) wide]))

;;Test-Case-1: Checks the function for notifying whether an image is wide
(begin-for-test (check-equal? 
                 (image-classify (rectangle 3 2 "solid" "red")) "wide" ) 
                "The rectangle is wide")
;;Test-Case-2: Checks the function for notifying whether an image is tall
(begin-for-test (check-equal? 
                 (image-classify (rectangle 2 3 "solid" "red")) "tall" ) 
                "The rectangle is tall")
;;Test-Case-3: Checks the function for notifying whether an image is square
(begin-for-test (check-equal? 
                 (image-classify (rectangle 3 3 "solid" "red")) "square" ) 
                "The rectangle is a square")

;;exercise 20-----------------------------------------------------------------
;;string1 is a string
;;WHERE : string1 can be either empty or non-empty
;;INTERP: string1 represents the first string of the resulting appended string
;;string2 is a string
;;WHERE : string2 can be either empty or non-empty
;;INTERP: string2 represents the second string of the resulting appended 
;;string, which will come after "-'
;;appended-string: String1"-"String2
;;string-join: String1 String2 -> appended-string
;;the function appends the string2 with string1 separated by a hyphen "-"
;;result is a appended string s1"-"s2
;;EXAMPLE: (string-join "hello" "world") -> "hello-world" 
(define (string-join s1 s2)
  (string-append s1 "-" s2))
;;Test-Case-1: Checks the function for correct concatenation of the given string
(begin-for-test (check-equal? (string-join "hello" "world") "hello-world" ) 
                "The two non-empty strings and concatenated correctly")
;;Test-Case-2: Checks the function for correct concatenation of the given 
;;empty string
(begin-for-test (check-equal? (string-join "" "") "-" ) 
                "The two empty strings and concatenated correctly")

;;exercise 21------------------------------------------------------------------
;;String1 is a string
;;WHERE: String1 cannot be empty
;;INTERP: string1 represents the string to which a hyphen "-" is to be added.
;;posn is a integer
;;WHERE: 0=< posn = length of the given string
;;INTERP: posn is the position in the string String1 where an hyphen "-" is 
;;to be inserted
;;string-insert: String1 posn -> String
;;the function takes String1 s and posn p as an input and gives string with 
;;a hyphen "-" inserted in the string s1 at posn p
;;EXAMPLE:(string-insert "hello" 2) -> "he-llo" 
(define (string-insert s p)
  (string-append (substring s 0 p) "-" (substring s p (string-length s))))
;;Test-Case-1: Checks the function for correct insertion of hyphen "-" at
;;given position of a non-empty string 
(begin-for-test (check-equal? (string-insert "hello" 2) "he-llo" ) 
                "The \"-\" was inserted correctly in the given position")

;;exercise 22------------------------------------------------------------------
;;String1 is a string
;;WHERE: String1 cannot be empty
;;INTERP: string1 represents the string from which a character is to be deleted
;;posn is a integer
;;WHERE: 0=< posn < length of the given string
;;INTERP: posn is the position of the character which has to be deleted from 
;;the string1
;;string-delete: String1 posn -> String
;;the function takes String1 s and posn p as an input and gives string with 
;;deleted character from the given position
;;EXAMPLE: (string-delete "hello" 2) "helo"
(define (string-delete s p)
  (string-append (substring s 0 p) (substring s (+ p 1) (string-length s))))
;;Test-Case-1: Checks the function for correct insertion of hyphen "-" at 
;;given position of a non-empty string 
(begin-for-test (check-equal? (string-delete "hello" 2) "helo" ) 
                "A character was deleted correctly from the given position")





