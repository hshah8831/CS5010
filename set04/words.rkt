;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname words) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Importing Packages
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 20 ); Time taken to finish the task in hours

;;Providing the functions to other packages
(provide arrangements)
(provide insert-everywhere/in-all-words)
(provide arrangement-main)
(check-location "04" "words.rkt")

;;A Word is either 
;;– '() or
;;– (cons 1String Word)
;;INTERP : A word represents the collection of alphabets.
;;TEMPLATE :
;;(define (word-fn wrd)
;;  (cond
;;    [(empty? wrd) ...]
;;    [else (...(first wrd)...(word-fn (rest wrd))...)]))

(define SAMPLE-WORD (list "e" "r" "d"))

;;A List-of-word is either
;;- '() or
;;- (cons Word List-of-word)
;;INTERP : A List-of-word represents the collection of Word
;;TEMPLATE :
;;(define (List-of-word-fn alow)
;;  (cond
;;    [(empty? alow) ...]
;;    [else (... (first alow)...(List-of-word-fn (rest alow))...)]))

(define SAMPLE-LOW (list
				(list "e" "r" "d")
				(list "e" "d" "r")
				(list "d" "e" "r")
				(list "r" "e" "d")
				(list "r" "d" "e")
				(list "d" "r" "e")))

;;A List-of-string is either
;;- '() or
;;- (cons String List-of-string)
;;INTERP : A List-of-string represents the collection of Strings
;;TEMPLATE :
;;(define (List-of-string-fn alow)
;;  (cond
;;    [(empty? alow) ...]
;;    [else (... (first alow)...(List-of-string-fn (rest alow))...)]))

(define SAMPLE-LOS (list "red" "rde" "dre" "erd" "edr" "der" empty))

;;arrangements : Word -> List-of-words
;;creates a list of all rearrangements of the letters in w : word
(begin-for-test (check-equal? (arrangements (list "d" "e" "r")) 
                               (list
                                (list "r" "e" "d")
                                (list "r" "d" "e")
                                (list "d" "r" "e")
                                (list "e" "r" "d")
                                (list "e" "d" "r")
                                (list "d" "e" "r"))
                            "a list of 1String, i.e. a word is given as input"))
;;STRATEGY : Data Decomposition on w : word
(define (arrangements w)
  (cond
    [(empty? (rest w))  w ]
    [else (insert-everywhere/in-all-words (first w)(arrangements (rest w)))]))

;;insert-everywhere/in-all-words : 1String List-of-word -> List-of-word
;;returns a list of words that has the first argument inserted at the beginning 
;;between all letters, and at the end of all words of the incoming list.
(begin-for-test (check-equal? 
                 (insert-everywhere/in-all-words "d" (cons (list "e" "r")
                                                      (cons (list "r" "e")
                                                             '()))) 
                               (list
				(list "e" "r" "d")
				(list "e" "d" "r")
				(list "d" "e" "r")
				(list "r" "e" "d")
				(list "r" "d" "e")
				(list "d" "r" "e"))
                              "Inserts d at the end and the beginning of word"))
;;STRATEGY : Data ecomposition on alow : List-of-word
(define (insert-everywhere/in-all-words letter alow)
  (cond
   [(empty? (rest alow)) (check-1String-list-handler letter (first alow))]
   [else (append (insert-everywhere/in-string letter (implode(first alow)) 
                                          (length (first alow))) 
          (insert-everywhere/in-all-words letter (rest alow)))]))

;;check-1String-list-handler : 1String Word -> List-Of-Word
;;If the Word input is a 1string, it will send the 1string to insert-everywhere
;;as it is else it the word will be imploded and sent to 
;;insert-everywhere/in-string on the word.
(begin-for-test (check-equal? 
                 (check-1String-list-handler "p" "d")
                 (list (list "d" "p") (list "p" "d")) 
                 "Inserts d at the end and the 
                               beginning of 1String"))
(begin-for-test (check-equal? 
                 (check-1String-list-handler "p" (list "e" "r" "d"))
                 (list
                  (list "e" "r" "d" "p")
                  (list "e" "r" "p" "d")
                  (list "e" "p" "r" "d")
                  (list "p" "e" "r" "d"))
                              "Inserts d at every possible place in Word"))
;;STRATEGY : Function Composition 
(define (check-1String-list-handler letter w)
  (if (string? w)
      (insert-everywhere/in-string letter w 1)
      (insert-everywhere/in-string letter (implode w)
                                   (length w))))

;;insert-everywhere/in-string : 1String String PosInt -> List-Of-Word
;;function takes a 1String String and length of the String and returns a 
;;List-of-Word which has 1String inserted at all the possible place in the 
;;String.
(begin-for-test (check-equal? 
                 (insert-everywhere/in-string "p" "abc" 3)
                 (list
                  (list "a" "b" "c" "p")
                  (list "a" "b" "p" "c")
                  (list "a" "p" "b" "c")
                  (list "p" "a" "b" "c"))
                              "Inserts p at every possible place in String"))
;;STATEGY : Function Composition 
(define (insert-everywhere/in-string letter  s len)
  (cond
    [(zero? len) (list (explode (string-insert s len letter)))]
    [(positive? len) (append 
                      (list (explode (string-insert s len letter)))
                      (insert-everywhere/in-string letter s (sub1 len)))])) 

    
;;string-insert : String PosInt 1String
;;takes a string, a position of the character after which the 1String is to be 
;;inserted and the 1String and returns a string with 1String letter inserted at 
;;Position p in String s.
(begin-for-test (check-equal? 
                 (string-insert "abc" 2 "d") "abdc"
                              "Inserts d after second place in String"))
;;STRATEGY : Function Composition
(define (string-insert s p letter)
  (string-append (substring s 0 p) letter (substring s p (string-length s))))

;;arrangement-main : String -> List-of-word
;;consumes a String and produces all of its possible 
;;re-arrangements as a list of Strings.
;;STRATEGY : Function Composition
(begin-for-test (check-equal? (arrangement-main "der") 
                              (list "red" "rde" "dre" "erd" "edr" "der" empty)
                               "A string is passed as input"))
(define (arrangement-main s)
  (list-implode(arrangements (explode s))))

;;list-implode : List-of-word -> List-of-String
;;consumes a List-of-word and converts each word in it 
;;to a string and stores them in a list
(begin-for-test (check-equal? (list-implode(list
                  (list "a" "b" "c" "p")
                  (list "a" "b" "p" "c")
                  (list "a" "p" "b" "c")
                  (list "p" "a" "b" "c"))) 
                              (list "abcp" "abpc" "apbc" "pabc" empty)
                               "A List-of-Word is passed as input"))
;;STRATEGY : Data Decomposition on alow : List-of-word
(define (list-implode alow)
  (cond
    [(empty? alow) (list '())]
    [else (cons (implode (first alow)) (list-implode(rest alow)))]))

;;Alternate Definitions
;;The approach of any arrangement would be as follows:
;;a.> Take a String
;;b.> Extract each 1String from the String.
;;c.> Create a List of String with extracted 1String inserted at all 
;;possible posistion

;;1.> using List-of-String instead of List-of-List-of-1String

;;Pros: The part where we convert String into List-of-1String & 
;;convert it back to String is avoided. Since the insertion of 1String is 
;;done using string-append utility again the conversion would decrease.

;;Cons: Keeping track and extracting each 1String in the String is  
;;complex with this approach.

