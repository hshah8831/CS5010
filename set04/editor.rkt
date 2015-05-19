;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Importing Packages
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 20 ); Time taken to finish the task in hours

;;Providing the functions to other packages
(provide edit)
(provide string->editor)
(provide editor-pre)
(provide editor-post)
(provide editor-pos)

;;Physical Constants:
(define WIDTH 200); The empty scene width in pixel
(define HEIGHT 20); The empty scene height in pixel
 
;;Graphical Constant:
(define MT (empty-scene WIDTH HEIGHT)); Empty Scene
;;Editor Constant
(define CURSOR (rectangle 1 20 "solid" "red")); Cursor Image
(define FONT-SIZE 16); Size of font in pixel
(define FONT-COLOR "Black"); Font color

;;An Lo1S is one of: 
;;– empty 
;;– (cons 1String Lo1S)
;;TEMPLATE:
;;List-of-1string-fn : Lo1S -> Lo1S
;(define (List-of-1string-fn alo1s )
;  (cond
;    [(empty? alo1s) ...]
;    [else (... (first alo1s) ...
;           ... (List-of-1string-fn (rest alo1s)) ...)]))
(define OLLEH (cons  "o" (cons "l" (cons "l" (cons "e" (cons "h" empty)))))) 
(define WORLD (cons  "w" (cons "o" (cons "r" (cons "l" (cons "d" empty))))))

(define-struct editor [pre post])
;;An Editor is (make-editor Lo1S Lo1S) 
;;WHERE: (string-append (implode ( reverse pre)) (implode post)) does not go 
;;past the right limit of the empty scene.
;;INTERP: (make-editor s t) means the text in the editor is
;;(string-append (implode (s)) (implode (t))) with the cursor displayed 
;;between (implode s) and (implode t).
(define INITIAL-EDITOR (make-editor OLLEH WORLD)) ; Initial Editor

;;TEMPLATE:
;;editor-fn : editor -> ???
(define (editor-fn ed)
  (... (editor-pre ed) ... (editor-post ed) ...))

;;main : String -> Editor
;;launches the editor given some initial string.
;;STRATEGY : Function Composition
(define (run s)
  (big-bang (create-editor s "")
            [to-draw render]
            [on-key  edit ]))

;;string->editor : String -> Editor
;;consumes a String and produces an Editor with cursor at the end of the text.
(begin-for-test (check-equal? (string->editor "hello" )
                              (make-editor empty (list "h" "e" "l" "l" "o"))
                 "editor made from a strings"))
;;STRATEGY : Funnction Composition
(define (string->editor s )
  (make-editor empty (explode s)))

;;create-editor : String String -> Editor
;;consumes two strings and produces an Editor.
(begin-for-test (check-equal? (create-editor "hello" "world" )
                              (make-editor
                (cons "o" (cons "l" (cons "l" (cons "e" (cons "h" empty)))))
                (cons  "w" (cons "o"(cons "r" (cons "l" (cons "d" empty))))))
                 "editor made from pair of strings"))
;;STRATEGY : Funnction Composition
(define (create-editor s t)
  (make-editor (reverse (explode s)) (explode t)))

;;edit : Editor KeyEvent -> Editor
;;reacts to the different keys pressed on the keyboard.
(begin-for-test (check-equal? (edit INITIAL-EDITOR "left" )
                              (make-editor
                (cons "l" (cons "l" (cons "e" (cons "h" empty))))
        (cons "o" (cons  "w" (cons "o"(cons "r" (cons "l" (cons "d" empty)))))))
                 "cursor moved left"))
(begin-for-test (check-equal? (edit INITIAL-EDITOR "right")
                              (make-editor
        (cons "w" (cons "o"(cons "l" (cons "l" (cons "e" (cons "h" empty))))))
        (cons "o" (cons "r" (cons "l" (cons "d" empty)))))
                 "cursor moved right"))
(begin-for-test (check-equal? (edit INITIAL-EDITOR "\b" )
                              (make-editor
        (cons "l" (cons "l" (cons "e" (cons "h" empty))))
        (cons  "w" (cons "o" (cons "r" (cons "l" (cons "d" empty))))))
                 "deleting \"o\" from pre"))
(begin-for-test (check-equal? (edit INITIAL-EDITOR "\t" )
                              (make-editor
          (cons "o" (cons "l" (cons "l" (cons "e" (cons "h" empty)))))
         (cons  "w" (cons "o"(cons "r" (cons "l" (cons "d" empty))))))
                 "the tab key is ignored"))
(begin-for-test (check-equal? (edit INITIAL-EDITOR "\r" )
                              (make-editor
          (cons "o" (cons "l" (cons "l" (cons "e" (cons "h" empty)))))
         (cons  "w" (cons "o"(cons "r" (cons "l" (cons "d" empty))))))
                 "the rubout key is ignored"))
(begin-for-test (check-equal? (edit INITIAL-EDITOR "e")
                              (make-editor
        (cons "e" (cons  "o" (cons "l" (cons "l" (cons "e" (cons "h" empty))))))
        (cons  "w" (cons "o" (cons "r" (cons "l" (cons "d" empty))))))
                 "inserting \"e\" to pre"))
(begin-for-test (check-equal? (edit INITIAL-EDITOR "rshift")
                              (make-editor
          (cons "o" (cons "l" (cons "l" (cons "e" (cons "h" empty)))))
         (cons  "w" (cons "o"(cons "r" (cons "l" (cons "d" empty))))))
                 "the irrelevant keys is ignored"))
;;STRATEGY : Data Decomposition on k : KeyEvent
(define (edit ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)] 
    [else ed]))

;;editor-lft : Editor -> Editor
;;moves the cursor position one 1String left, if possible
(begin-for-test (check-equal? (editor-lft INITIAL-EDITOR )
                              (make-editor
                (cons "l" (cons "l" (cons "e" (cons "h" empty))))
        (cons "o" (cons  "w" (cons "o"(cons "r" (cons "l" (cons "d" empty)))))))
                 "removes \"o\" from pre and adds it to past"))
(begin-for-test (check-equal? (editor-lft (make-editor '() OLLEH ) ) 
        (make-editor '() 
                   (cons "o"(cons "l" (cons "l" (cons "e" (cons "h" empty))))))
                 "cursor moved right"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (editor-lft ed)
  (if (empty? (editor-pre ed)) 
              ed
              (make-editor (remove-first-from-list (editor-pre ed)) 
                           (add-first-to-post ed))))

;;remove-first-from-list : Lo1S -> Lo1S
;;returns rest of the input string, the end effect is as if first 
;;item of the list is removed.
(begin-for-test (check-equal? (remove-first-from-list OLLEH ) 
                (cons "l" (cons "l" (cons "e" (cons "h" empty))))
                 "removes \"o\" from pre"))
(begin-for-test (check-equal? (remove-first-from-list '() ) 
                '()
                 "cursor reached the left end of the text"))
;;STRATEGY : Data Decomposition on s : Lo1S
(define (remove-first-from-list s)
  (cond
    [(empty? s) s ]
    [else (rest s)]))

;;add-first-to-post : Editor -> Lo1S
;;returns editor-post after addition of (first editor-pre)
(begin-for-test (check-equal? (add-first-to-post INITIAL-EDITOR ) 
         (cons "o" (cons  "w" (cons "o"(cons "r" (cons "l" (cons "d" empty))))))
                         "removes \"o\" from pre & adds to post"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (add-first-to-post ed)
   (cons (get-first-of-list (editor-pre ed)) (editor-post ed)))

;;editor-rgt : Editor -> Editor
;;moves the cursor position one 1String right, if possible
(begin-for-test (check-equal? (editor-rgt INITIAL-EDITOR ) 
        (make-editor
        (cons "w" (cons "o"(cons "l" (cons "l" (cons "e" (cons "h" empty))))))
        (cons "o" (cons "r" (cons "l" (cons "d" empty)))))
                 "cursor moved right"))
(begin-for-test (check-equal? (editor-rgt (make-editor OLLEH '()) ) 
        (make-editor
        (cons "o"(cons "l" (cons "l" (cons "e" (cons "h" empty))))) '())
                 "cursor moved right"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (editor-rgt ed)
  (if (empty? (editor-post ed)) ed
      (make-editor  (add-first-to-pre ed) 
                (remove-first-from-list (editor-post ed)))))

;;add-first-to-pre : Editor -> Lo1S
;;returns editor-pre after addition of (fiirst editor-post)
(begin-for-test (check-equal? (add-first-to-pre INITIAL-EDITOR ) 
         (cons  "w" (cons "o"(cons "l" (cons "l" (cons "e" (cons "h" empty))))))
                         "removes \"w\" from post & adds to pre"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (add-first-to-pre ed)
  (cons (get-first-of-list (editor-post ed)) (editor-pre ed)))

;;get-first-of-list : Lo1S -> 1String
;;consumes a list of 1String and returns the first item in the list
(begin-for-test (check-equal? (get-first-of-list OLLEH ) "o"
                         "OLLEH is given as in input with o at first"))
(begin-for-test (check-equal? (get-first-of-list '() ) '()
                         "empty list is given as input"))
;;STRATEGY : Data Decomposition on lo1s : Lo1S
(define (get-first-of-list lo1s)
  (cond 
   [(empty? lo1s) lo1s]
   [else (first lo1s)]))

;;editor-del : Editor -> Editor
;;deletes one 1String to the left of the cursor, if possible
(begin-for-test (check-equal? (editor-del INITIAL-EDITOR ) 
        (make-editor
        (cons "l" (cons "l" (cons "e" (cons "h" empty))))
        (cons  "w" (cons "o" (cons "r" (cons "l" (cons "d" empty))))))
                 "deleting \"o\" from pre"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (editor-del ed) 
  (make-editor (remove-first-from-list (editor-pre ed)) (editor-post ed)))

;;editor-ins : Editor 1String -> Editor
;;insert the 1String k between pre and post
(begin-for-test (check-equal? (editor-ins INITIAL-EDITOR "e") 
        (make-editor
        (cons "e" (cons  "o" (cons "l" (cons "l" (cons "e" (cons "h" empty))))))
        (cons  "w" (cons "o" (cons "r" (cons "l" (cons "d" empty))))))
                 "inserting \"e\" to pre"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (editor-ins ed k)
  (make-editor (add-1string-to-pre ed k) (editor-post ed)))

;;add-1string-to-pre : Editor 1String -> Lo1S
;;adds 1String to (editor-pre ed)
(begin-for-test (check-equal? (add-1string-to-pre INITIAL-EDITOR "e") 
        (cons "e" (cons  "o" (cons "l" (cons "l" (cons "e" (cons "h" empty))))))
                 "inserting \"e\" to pre"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (add-1string-to-pre ed k)
  (cons k (editor-pre ed)))
    
;;render : Editor -> Image
;;renders an image of the incoming editor
(begin-for-test (check-equal? (render INITIAL-EDITOR) 
                              (place-image/align
    (beside (text (implode (reverse OLLEH)) FONT-SIZE FONT-COLOR)
            (rectangle 1 20 "solid" "red")
            (text (implode WORLD) FONT-SIZE FONT-COLOR))
            1 1 "left" "top" (empty-scene WIDTH HEIGHT))
                 "editor is INITIAL-EDITOR "))
;;STRATEGY : Data Decomposition on ed : Editor
(define (render e)
  (place-image/align
    (beside (editor-text (reverse(editor-pre e)))
            CURSOR
            (editor-text (editor-post e))) 1 1 "left" "top"
                                           MT))

;;editor-text : Lo1s -> Image
;;renders a list of 1Strings as a text image 
(begin-for-test (check-equal? (editor-text OLLEH) 
                              (text (implode OLLEH) FONT-SIZE FONT-COLOR)
                               "editor has OLLEH in pre"))
(begin-for-test (check-equal? (editor-text empty) 
                              (text "" FONT-SIZE FONT-COLOR)
                               "editor has OLLEH in pre"))

;;STRATEGY : Data Decomposition on s : Lo1S
(define (editor-text s)
  (cond
    [(empty? s) (text "" FONT-SIZE FONT-COLOR)]
    [else (text (implode s) FONT-SIZE FONT-COLOR)]))

;;editor-pos : Editor -> Number
;;returns the position of the cursor in editor
(begin-for-test (check-equal? (editor-pos INITIAL-EDITOR) 5
                 "Cursor placed after Hello"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (editor-pos ed)
  (string-length (implode (editor-pre ed))))
(begin-for-test (check-equal? (editor-pos INITIAL-EDITOR) 5
                 "Cursor placed after Hello"))

;;Alternate Data Definitions
;;1.> Comparison of List approach with two strings approach

;;The cursor movement in the List approach is easier than two string 
;;approach. 
;;Function Example:
;;Function to remove last character from pre and add it to post
;;(define (add-char-to-post ed)
;;  (string-append (substring (editor-pre ed) 
;;                            (- (editor-pos ed) 1) 
;;                            (editor-pos ed)) 
;;                 (editor-post ed)))

;;v/s

;;(define (add-first-to-post ed)
;;   (cons (get-first-of-list (editor-pre ed)) (editor-post ed)))
;;(define (get-first-of-list lo1s)

;;following function is re-used for right movement as well 
;;  (cond 
;;   [(empty? lo1s) lo1s]
;;   [else (first lo1s)]))

;;2.> Comparison of List approach with a String and a Cursor Position approach
;;rendering in list approach is easier as cursor position comes naturally 
;;between pre and post list. In the a Strnig and Cursor approach the render 
;;function has to 
;;->take the position of the cursor 
;;->divide the String at the position 
;;->then render the all of the above





