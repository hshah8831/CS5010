;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Importing Packages
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 5 ); Time taken to finish the task in hours

;;Providing the functions to other packages
(provide render)
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
(define CURSOR (rectangle 1 20 "solid" "red")); Cursor Image
(define LEFT-GUTTER-MARGIN (text " " 16 "white")); Left Gutter margin
(define BUFFER 12); buffer to kept at the right end of the editor in pixel
(define FONT-SIZE 16); Size of font in pixel

;;editor : (make-editor String String)
;;WHERE: (string-append String String ) does not pass the right 
;;limit of the empty scene 
;;INTERP: (make-editor s t) means the text in the editor is
;;(string-append s t) with the cursor displayed between s and t
(define-struct editor [pre post])

; TEMPLATE:
; editor-fn : editor -> ???
(define (editor-fn ed)
  (... (editor-pre ed) ... (editor-post ed) ...))

;;Defining Initial State of editor
(define INITIAL-EDITOR (make-editor "" "" ))

;;------------------------------------------------------------------------------
;;main : Editor -> Image
;;The function applies big-bang to simulate a one line text editor
;;GIVEN: Editor
;;RETURNS: Image
;;EXAMPLES: 
;;STRATEGY: Function Composition
(define (main ed)
  (big-bang ed
            [to-draw render]
            [on-key  edit ]))

;;------------------------------------------------------------------------------
;;edit editor key-event -> editor
;;The function takes editor ed and keyevent ke from bigbang 
;;and returns new state of editor ed depending on the key event.
;;GIVEN: Editor ed0 , Keyevent ke
;;RETURNS: Editor ed1
;;EXAMPLE: edit(make-editor "hello" "world" ) "a") -> (make-editor "helloa" "world")
;;         edit(make-editor "" "") "b") -> (make-editor "b" "")
;;         edit(make-editor "" "") "\b") -> (make-editor "" "")
;;         edit(make-editor "hello" "world") "left") -> (make-editor "hell" "oworld")
;;         edit(make-editor "hello" "world") "right") -> (make-editor "hellow" "orld")
;;         edit(make-editor "hello" "world") "\t") -> (make-editor "hello" "world")
;;         edit(make-editor "hello" "world") "\b") -> (make-editor "hell" "world")
;;STRATEGY: Function Composition
(define (edit ed ke )
  (cond
    [(key=? "\b" ke) (string-bksp ed) ]
    [(ignore? ke) ( do-nothing ed)]
    [(key=? "left" ke) (move-cursor-left ed)]
    [(key=? "right" ke) (move-cursor-right ed)]
    [else (insert-char ed ke) ]))

;;Test Case 1 : Cursor in the middle of the text and a 
;;              backspace entered as keyevent
(begin-for-test (check-equal? (edit (make-editor "Hello" "World") "\b") 
                              (make-editor "Hell" "World" )  
                "The last character of the text before cursor deleted "))

;;Test Case 2 : Cursor in the middle of the text and  
;;              left key entered as keyevent
(begin-for-test (check-equal? (edit (make-editor "Hello" "World" )  "left") 
                              (make-editor "Hell" "oWorld" )  
                "The movement of cursor to left "))

;;Test Case 3 : Cursor in the middle of the text and  
;;              right key entered as keyevent
(begin-for-test (check-equal? (edit (make-editor "Hello" "World") "right") 
                              (make-editor "HelloW" "orld" )  
                "The movement of cursor right "))

;;Test Case 4 : Cursor in the middle of the text and the   
;;              tab key is entered as keyevent
(begin-for-test (check-equal? (edit (make-editor "Hello" "World") "\t") 
                              (make-editor "Hello" "World" )  
                "The editor state remains unchanged "))

;;Test Case 5 : Cursor in the middle of the text and key other than  
;;              backspace, left & right arrow and ignore keys is entered   
(begin-for-test (check-equal? (edit (make-editor "Hello" "World") "a") 
                              (make-editor "Helloa" "World" )  
                "The character inserted at cursor "))

;;------------------------------------------------------------------------------
;;editor-pos : Editor -> Natural
;;Returns the position of the cursor in editor e, uses 0-based indexing.  
;;GIVEN: Editor ed
;;RETURN: Natural p
;;EXAMPLE: editor-pos(make-editor "hello" "world") -> 5
;;       : editor-pos(make-editor "" "HelloWorld") -> 10
;;       : editor-pos(make-editor "HelloWorld" "") -> 0
;;STRATEGY: Data Decomposition
(define (editor-pos ed)
  (string-length (editor-pre ed)))

;;Test Case 1 : Cursor in the middle of the string
(begin-for-test (check-equal? (editor-pos (make-editor "Hello" "World")) 5  
                "The cursor position returned"))

;;Test Case 2 : Cursor at the end of the string
(begin-for-test (check-equal? (editor-pos (make-editor "HelloWorld" "")) 10  
                "The cursor position returned"))

;;Test Case 3 : Cursor at the beginning of the string
(begin-for-test (check-equal? (editor-pos (make-editor "" "HelloWorld")) 0  
                "The cursor position returned"))

;;------------------------------------------------------------------------------
;;string-bksp : Editor -> Editor
;;The function omits the character immediately left to the 
;;cursor in the current world.If the cursor is present in the left most position
;;then the editor state remains the same
;;GIVEN: editor ed0
;;RETURNS: editor ed1 
;;EXAMPLES: (string-bksp (make-editor "hello" "world") "\b") -> (make-editor "hell" "world")
;;STRATEGY: Data Decomposition
(define (string-bksp ed)
  (if (= (editor-pos ed) 0) 
      ed 
      (make-editor (substring (editor-pre ed) 
                              0 
                              (- (editor-pos ed) 1)) 
                   (editor-post ed))))

;;Test Case 1 : Input is in such a way that the cursor is present on the left 
;;              most position of the text
(begin-for-test (check-equal? (string-bksp (make-editor "" "HelloWorld")) 
                              (make-editor "" "HelloWorld" ) 
                "The editor state remains unchanged "))
                              
;;Test Case 2 : Input is in such a way that the cursor is present in the 
;;              middle of the text  
(begin-for-test (check-equal? (string-bksp (make-editor "Hello" "World")) 
                              (make-editor "Hell" "World" ) 
                "This removes the character immediate left to the cursor "))

;;------------------------------------------------------------------------------
;;Ignore? : Char -> Boolean
;;The function checks if the given character is a one of the following
;; "shift", "rshift", "up", "down", "\t", "u007F" 
;;GIVEN: Char a
;;RETURNS: Boolean
;;EXAMPLES: (ignore? "\t") -> #true
;;          (ignore? "b") -> #false
;;          (ignore? "shift") -> #true
;;STRATEGY: Function Composition
(define (ignore? a) 
  (if (or 
       (string=? a "shift") 
       (string=? a "rshift") 
       (string=? a "up") 
       (string=? a "down")
       (string=? a "control")
       (string=? a "\t")
       (string=? a "\r")
       (string=? a "u007F" )) 
       true false)) 

;;Test Case 1 : Input is one of the value present in the set
(begin-for-test (check-equal? (ignore? "u007F") #true ) 
                "The output is #true as the input is one of the value 
                 present in the set ")

;;Test Case 2 : Input is one of the value NOT present in the set
(begin-for-test (check-equal? (ignore? "a") #false ) 
                "The output is #true as the input is one of the value 
                 present in the set ")
;;------------------------------------------------------------------------------
;;insert-char : Editor Keyevent -> Editor
;;The function takes the current state of editor and a keyevent and returns 
;;a new state of editor which has the keyevent appended to editor-pre 
;;and editor-post is kept as is.
;;GIVEN: Editor ed0 ke
;;RETURNS: Editor ed1
;;EXAMPLES: (insert-char (make-editor "Hello" "World") "a") -> (make-editor "Helloa" "World")
;;STRATEGY: Data Decomposition
(define (insert-char ed ke)
     (if (> (text-image-size ed) (- WIDTH BUFFER)) ed
     (make-editor (string-append (editor-pre ed) ke) (editor-post ed))))
;;  (make-editor (string-append (editor-pre ed) ke) (c)))

;;Test Case 1 : Snapshot of a worldstate
(begin-for-test (check-equal? (insert-char (make-editor "Hello" "World") "a") 
                              (make-editor "Helloa" "World") ) 
                "The character added porperly at appropriate position ")

;;Test Case 2 : Snapshot of a worldstate
(begin-for-test (check-equal? (insert-char (make-editor "HardikShahisanawesomepro" "") "a") 
                              (make-editor "HardikShahisanawesomepro" "") ) 
                "The character ignored as left gutter reached ")

;;------------------------------------------------------------------------------
;;text-image-size : Editor -> NonNegReal
;;Returns the size of the image of the text before the cursor 
;;GIVEN: Editor ed0
;;RETURNS: NonNegReal i
;;EXAMPLES: (text-image-size "Hello" "World") -> 32
;;          (text-image-size "HelloWorld" "") -> 69 
;;STRATEGY:Data Decomposition
(define (text-image-size ed)
  ( image-width(text (editor-pre ed) 16 "black")))

;;Test Case 1 : Snapshot of a worldstate
(begin-for-test (check-equal? (text-image-size (make-editor "Hello" "World")) 
                              35) 
                "The size of the image of the text before the cursor ")

;;------------------------------------------------------------------------------
;;do-nothing : Editor -> Editor
;;This function does nothing to the worldstate and is used when the 
;;editor is to be kept as is.
;;GIVEN: Editor ed0
;;RETURNS: Editor ed0
;;EXAMPLES: (do-nothing ed0) -> ed1
;;STRATEGY: Function Composition
(define (do-nothing ed ) ed )

;;Test Case 1 : Snapshot of a worldstate
(begin-for-test (check-equal? (do-nothing (make-editor "Hello" "World")) 
                              (make-editor "Hello" "World") ) 
                "The Unchanged Editor is returned ")
 
;;------------------------------------------------------------------------------
;;move-cursor-left : Editor -> Editor
;;The function takes the current worldstate and returns a new worldstate with 
;;last character removed from (editor-pre ed0) and added to (editor-post ed1) 
;;at frist position
;;GIVEN: Editor ed1
;;RETURNS: Editor ed0
;;EXAMPLES: (move-cursor-left (make-editor "hello" "world")) -> (make-editor "hell" "world")
;;STRATEGY: Data Decomposition
(define (move-cursor-left ed)
  (if (= (editor-pos ed) 0) 
      ed 
      (make-editor (delete-char-from-pre ed) (add-char-to-post ed))))

;;Test Case 1 : Snapshot of a worldstate when the cursor is NOT at the 
;;              leftmost of the text
(begin-for-test (check-equal? (move-cursor-left (make-editor "Hello" "World")) 
                              (make-editor "Hell" "oWorld") ) 
                "The cursor is moved to left ")

;;Test Case 2 : Snapshot of a worldstate when the cursor is at the leftmost
;;              of the text
(begin-for-test (check-equal? (move-cursor-left (make-editor "" "HelloWorld")) 
                              (make-editor "" "HelloWorld") ) 
                "The cursor remains where it was before ")

;;------------------------------------------------------------------------------
;;move-cursor-right : Editor -> Editor
;;The function takes the current worldstate and returns a new worldstate with 
;;first character removed from (editor-post ed0) and appended to (editor-pre ed1)
;;GIVEN: Editor ed0
;;RETURNS: Editor ed1
;;EXAMPLES: (move-cursor-right (make-editor "Hello" "World")) -> (make-editor "HelloW" "orld")
;;STRATEGY: Data Decomposition
(define (move-cursor-right ed)
  (if (>= (editor-pos ed) (text-length ed))  
          ed (make-editor(add-char-to-pre ed) (delete-char-from-post ed))))

;;Test Case 1 : Snapshot of a worldstate when the cursor is NOT at the rightmost
(begin-for-test (check-equal? (move-cursor-right (make-editor "Hello" "World")) 
                              (make-editor "HelloW" "orld") ) 
                "The cursor is movedd to right ")

;;Test Case 2 : Snapshot of a worldstate when the cursor is at 
;;              the rightmost position
(begin-for-test (check-equal? (move-cursor-right (make-editor "HelloWorld" "")) 
                              (make-editor "HelloWorld" "") ) 
                "The cursor moves to the right ")

;;------------------------------------------------------------------------------
;;text-length : Editor-> NonNegReal
;;The function gives the length of the text entered in the editor
;;GIVEN: Editor ed0
;;RETURNS: NonNegReal
;;EXAMPLES: (text-length (make-editor "hello" "world") -> 10
;;          (text-length (make-editor "" "") -> 0
;;STRATEGY: Data Decomposition
(define (text-length ed)
  (+ (string-length (editor-pre ed)) (string-length (editor-post ed))))

;;Test Case 1 : Initial World
(begin-for-test (check-equal? (text-length (make-editor "" "")) 0 ) 
                "The character added porperly at appropriate position ")

;;Test Case 2 : Screenshot of a worldstate
(begin-for-test (check-equal? (text-length (make-editor "Hello" "World")) 10 ) 
                "The character added porperly at appropriate position ")
                
;;------------------------------------------------------------------------------
;;delete-char-from-pre : Editor -> String
;;The function omits the last character of the string (editor-pre ed0) 
;;and returns the rest of it.
;;GIVEN: Editor ed0
;;RETURNS: String
;;EXAMPLES: (delete-char-from-pre  (make-editor "Hello" "World")) -> "Hell" 
;;STRATEGY: Data Decomposition
(define (delete-char-from-pre ed)
  (substring (editor-pre ed) 0 (- (editor-pos ed) 1)))

;;Test Case 1 : Screenshot of a worldstate
(begin-for-test (check-equal? 
                 (delete-char-from-pre (make-editor "Hello" "World")) 
                 "Hell" ) 
                "The character added porperly at appropriate position ")

;;------------------------------------------------------------------------------
;;delete-char-from-post : Editor -> String
;;The function omits the first character of the string (editor-post ed0) 
;;and returns the rest of it.
;;GIVEN: Editor ed0
;;RETURNS: String
;;EXAMPLES: (delete-char-from-post (make-editor "Hello" "World")) -> "orld"
;;STRATEGY: Data Decomposition
(define (delete-char-from-post ed)
  (substring (editor-post ed) 1 (string-length (editor-post ed))))

;;Test Case 1 : Screenshot of a worldstate
(begin-for-test (check-equal? 
                 (delete-char-from-post (make-editor "Hello" "World"))
                 "orld" ) 
                "The character added porperly at appropriate position ")
;;------------------------------------------------------------------------------
;;add-char-to-post : Editor -> String
;;The function adds the last character from the (editor-pre ed0) 
;;to (editor-post ed0) at the first position of (editor-post ed0)
;;FUNCTION INVARIANT: The (editor-pre ed) and (editor-post ed0) will 
;;never be empty
;;GIVEN: Editor ed0
;;RETURNS: String
;;EXAMPLES: (add-char-to-post (make-editor "Hello" "World")) -> "oWorld" 
;;STRATEGY: Data Decomposition
(define (add-char-to-post ed)
  (string-append (substring (editor-pre ed) 
                            (- (editor-pos ed) 1) 
                            (editor-pos ed)) 
                 (editor-post ed)))

;;Test Case 1 : Screenshot of a worldstate
(begin-for-test (check-equal? 
                 (add-char-to-post (make-editor "Hello" "World")) 
                 "oWorld" ) 
                "The character added porperly at appropriate position ")

;;------------------------------------------------------------------------------
;;add-char-to-pre : Editor -> String
;;The function adds the first character from the (editor-post ed0) 
;;to (editor-pre ed0) at the last position of (editor-pre ed0)
;;FUNCTION INVARIANT: The (editor-pre ed) and (editor-post ed0) will 
;;never be empty
;;GIVEN: Editor ed0
;;RETURNS: String
;;EXAMPLES: (add-char-to-pre (make-editor "Hello" "World")) -> "HelloW"
;;STRATEGY: Data Decomposition
(define (add-char-to-pre ed)
  (string-append (editor-pre ed) (substring (editor-post ed) 0 1)))

;;Test Case 1 : Screenshot of a worldstate
(begin-for-test (check-equal? 
                 (add-char-to-pre (make-editor "Hello" "World")) 
                 "HelloW" ) 
                "The character added porperly at appropriate position ")

;;------------------------------------------------------------------------------
;;render : Editor -> Image
;;The function renders the image with text and cursor at appropriate position 
;;over an empty space.
;;GIVEN: Editor ed0
;;RETURNS: Image
;;EXAMPLES: (render (make-editor "Hello" "World")) -> (overlay/align "left" 
;;                                           "center" 
;;                                           (beside 
;;                                            (text " " 16 "white") 
;;                                            (text "Hello" 16 "black") 
;;                                            (rectangle 1 20 "solid" "red")  
;;                                            (text "World" 16 "black")) 
;;                                            MT) 
;;STRATEGY: Data Decomposition
(define (render ed)
   (overlay/align "left" 
                  "center" 
                  (beside 
                   LEFT-GUTTER-MARGIN
                   (text (editor-pre ed) FONT-SIZE "black") 
                    CURSOR 
                    (text (editor-post ed) FONT-SIZE "black")) 
                  MT))

;;Test Case 1 : INITIAL-EDITOR
(begin-for-test (check-equal? (render (make-editor "" "")) (overlay/align "left" 
                                           "center" 
                                           (beside 
                                            (text " " 16 "white")
                                            (text "" 16 "black") 
                                            (rectangle 1 20 "solid" "red")  
                                            (text "" 16 "black")) 
                                            MT) ) 
                "The INITIAL-EDITOR is succesfully Rendered")

;;Test Case 2 : Screen-shot of a world
(begin-for-test (check-equal? (render (make-editor "Hello" "World")) 
                              (overlay/align "left" 
                                             "center"
                                             (beside 
                                              (text " " 16 "white")
                                              (text "Hello" 16 "black") 
                                              (rectangle 1 20 "solid" "red")  
                                              (text "World" 16 "black"))
                                             MT)) 
                "The check to render a screenshot of world")

;;------------------------------------------------------------------------------
;;string->editor : String -> Editor
;;Returns an Editor containing text str and cursor at position 0.
;;GIVEN: String str
;;RETURNS: Editor ed0
;;EXAMPLES: (string->editor "HelloWorld") -> (make-editor "" "HelloWorld")
;;STRATEGY:Function Composition 
(define (string->editor str)
  (make-editor "" str ))

;;Test Case 1 : This tests whether the given string is converted to a 
;;              Editor type correctly or not
(begin-for-test (check-equal? (string->editor "HelloWorld") 
                              (make-editor "" "HelloWorld")) 
                "The string is converted correctly to editor type successfuly")

;;------------------------------------------------------------------------------
;;run : String -> Editor
;;Returns an Editor containing text str and cursor at the end of the string.
;;GIVEN: String str
;;RETURNS: Editor ed0
;;EXAMPLES: (run "HelloWorld") -> (make-editor "HelloWorld" "")
;;STRATEGY:Function Composition
(define (run str)
  (main (make-editor str "")))





 
  
  
