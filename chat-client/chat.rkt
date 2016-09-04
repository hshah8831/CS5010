;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; importing required teachpacks
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require racket/trace)
(define TIME-ON-TASK 25) 

; providing required functions
(provide mk-world)
(provide receive)
(provide key-handler)
(provide get-users)
(provide get-editor)
(provide get-editor-pre)
(provide get-editor-post)
(provide get-chat-history)

; Display Constants
(define USER-DISPLAY (empty-scene 100 400 "white"))
(define EDITOR (empty-scene 300 20 "white"))
(define CHAT-AREA (empty-scene 300 380 "white"))
(define CHAT-ROOM (beside USER-DISPLAY (above CHAT-AREA EDITOR)))

; Font Color Constants
(define BLUE-FONT "blue")
(define BLACK-FONT "black")
(define GRAY-FONT "gray")
(define RED-FONT "red")

(define USERNAME-UPPER-LIMIT 12)
(define USERNAME-LOWER-LIMIT 1)

(define CURSOR (rectangle 1 20 "solid" "red")) 

; Font Size Constants
(define FONT-SIZE 12)

; Text Limits
(define USERLIST-DISPLAY-LIMIT 400)
(define CHAT-EVENT-DISPLAY-LIMIT 380)
(define EDITOR-RIGHT-LIMIT 300)

; Event Constants
(define EMPTY-STRING "")
(define COLON ":")
(define ARROW "->")
(define SPACE " ")
(define JOINING-MSG " joined")
(define LEAVING-MSG " left the chat")

(define EVENT-LIMIT 25)

; A UserName is a String, consisting of only letters and numbers,
; and is between 1 and 12 characters long.
; Represents a chat room participant.

; A Message is a String

; MsgToServer is a:
; - (list 'broadcast Message)
; - (list 'private UserName Message)
; The user in the private message is the intended recipient of the message.

; TEMPLATE:
; MsgToServer -> ???
;(define (msgtoserver-fn mts)
;  (cond
;    [(eq? 'broadcast (first mts))...(second mts)...]
;    [(eq? 'private (first mts))...(second mts)...(third mts)...]))

; A MsgFromServer is a:
; - (list 'userlist ListOf<UserName>) ; all the current chat participants
; - (list 'join UserName) ; the user that just joined
; - (list 'leave UserName) ; the user that just left
; - (list 'error Message) ; an error message from the server
; - (list 'private UserName Message) ; a priv msg from the specified user
; - (list 'broadcast UserName Message) ; a broadcast msg from the specified user

; TEMPLATE:
; MsgFromServer -> ???
;(define (msgfromserver-fn mfs)
;  (cond
;    [(eq? 'userlist (first mfs))...(second mfs)...]
;    [(eq? 'join (first mfs))...(second mfs)...]
;    [(eq? 'leave (first mfs))...(second mfs)...]
;    [(eq? 'error (first mfs))...(second mfs)...]
;    [(eq? 'broadcast (first mfs))...(second mfs)...(third mfs)...]
;    [(eq? 'private (first mfs))...(second mfs)...(third mfs)...]))

; A HandlerResult is a:
; - World
; - (make-package World MsgToServer)

; An Event is a:
; (make-event ListOf<String> image-color?)
(define-struct event [msg color])
; INTERP: 
; msg represents the message along appended with the UserName sent by.
; color represents the font color of the message in the CHAT-AREA
; TEMPLATE:
; Event -> ???
(define (event-fn w)
  (...(event-msg w)...(event-color w)...))

;;An EditorPart is (make-editorpart ListOf<1String> ListOf<1String>) 
(define-struct editorpart [nondisp disp])
; INTERP: 
; nondisp represents the non displayable part of the EditorPart.
; disp represents the displayable part of the EditorPart
; TEMPLATE:
; EditorPart -> ???
(define (editorpart-fn edprt)
  (...(editorpart-nondisp edprt)...(editorpart-disp edprt)...))

; An Editor is (make-editor EditorPart EditorPart)
(define-struct editor [pre post])
; INTERP: 
; pre represents the part of the Editor that comes before the cursor.
; post represents the part of the editor that comes after the cursor.
; TEMPLATE:
; Editor -> ???
(define (editor-fn ed)
  (...(editor-pre ed)...(editor-post ed)...))

(define INITIAL-EDITOR (make-editor (make-editorpart empty empty) 
                                    (make-editorpart empty empty)))

; A World is a:
; (make-world ListOf<UserName> ListOf<Event>)
(define-struct world [name users events editor])

; TEMPLATE:
; World -> ???
(define (world-fn w)
  (...(world-name w)...(world-users w)...
      (world-events w)...(world-editor w)...))

; run : UserName IPAddress -> World
; Connect to the given chat server with user name nam.
; STRATEGY : Function Composition
(define (run nam server)
  (big-bang (mk-world nam)
            (on-receive receive)
            (to-draw render)
            (on-key key-handler)
            (name nam)
            (register server)
            (port 5010))) 

; mk-world : UserName -> World
; Returns the initial world state for user name.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (mk-world "archana")
   (make-world "archana" (list "archana") empty 
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given valid username"))
; STRATEGY : Function Composition
(define (mk-world nam) 
  (make-world nam (list nam) empty INITIAL-EDITOR))

; receive : World MsgFromServer -> HandlerResult
; Handles messages received from the server.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (receive (make-world "archana" (list "archana") empty 
                        (make-editor (make-editorpart '() '())
                                     (make-editorpart '() '())))
            (list 'userlist (list "sharayu" "hardik")))
   (make-world "archana" (list "sharayu" "hardik") '()
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given a valid world")
  (check-equal?
   (receive (make-world "archana" (list "archana") empty 
                        (make-editor (make-editorpart '() '())
                                     (make-editorpart '() '())))
            (list 'join "Sampath"))
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given handlerresult which triggers a join event")
  (check-equal? 
   (receive (make-world "archana" (list "archana" "Sampath")
                        (list (make-event (list "Sampath joined") "gray"))
                        (make-editor (make-editorpart '() '())
                                     (make-editorpart '() '())))
            (list 'leave "Sampath"))
   (make-world "archana" (list "archana")
               (list (make-event (list "Sampath left the chat") "gray")
                     (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given handlerresult which triggers a leave event")
  (check-equal? 
   (receive (make-world "archana" (list "archana" "Sampath")
                        (list (make-event (list "Sampath joined") "gray"))
                        (make-editor (make-editorpart '() '())
                                     (make-editorpart '() '())))
            (list 'error "Error Occured"))
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Error Occured") "red")
                     (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given handlerresult which triggers a error event")
  (check-equal? 
   (receive (make-world "archana" (list "archana" "Sampath")
                        (list (make-event (list "Sampath joined") "gray"))
                        (make-editor (make-editorpart '() '())
                                     (make-editorpart '() '())))
            (list 'broadcast "archana" "Hi"))
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "< archana >Hi") "black")
                     (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given handlerresult which triggers a broadcast event")
  (check-equal? 
   (receive (make-world "archana" (list "archana" "Sampath")
                        (list (make-event (list "Sampath joined") "gray"))
                        (make-editor (make-editorpart '() '())
                                     (make-editorpart '() '())))
            (list 'private "archana" "Hi"))
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "< archana >Hi") "blue")
                     (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given handlerresult which triggers a private event"))
; STRATEGY : Data Decomposition on mfs : MsgFromServer
(define (receive w mfs)
  (cond
    [(eq? 'userlist (first mfs))
     (make-world (world-name w) (second mfs) (world-events w) (world-editor w))]
    [(eq? 'join (first mfs)) 
     (add-username-to-world w (second mfs))]
    [(eq? 'leave (first mfs)) 
     (remove-username-from-world w (second mfs))]
    [(eq? 'error (first mfs)) 
     (add-event-to-world w (make-error-event (second mfs)))]
    [(eq? 'broadcast (first mfs))
     (add-event-to-world w (make-broadcast-event (second mfs) (third mfs)))]
    [(eq? 'private (first mfs))
     (add-event-to-world w (make-private-event EMPTY-STRING EMPTY-STRING 
                                               (second mfs) (third mfs)))]))

; add-error-event : Message -> World
; returns an error event.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (make-error-event "Error")
   (make-event (list "Error") "red")
   "given an error message"))
; STRATEGY : Function Composition
(define (make-error-event msg)
  (make-event (wrap-message msg) RED-FONT))

; make-broadcast-event : UserName Message -> Event
; returns a broadcast event.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (make-broadcast-event "archana" "Hi")
   (make-event (list "< archana >Hi") "black")
   "given the user and message from broadcast package"))
; STRATEGY : Function Composition 
(define (make-broadcast-event user msg)
  (make-event (wrap-message(string-append "< "user " >" msg)) BLACK-FONT))

; make-private-event : UserName String UserName Message -> World
; returns a private event from the given user1 and user2 and msg.
; WHERE: arrow? can be "" or "->" 
; user1 can be "" or host username.
; user2 is the guest username.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (make-private-event "archana" "" "Sampath" "Hi")
   (make-event (list "< archanaSampath >Hi") "blue")
   "given user and message from private package")
  (check-equal?
   (make-private-event "archana" "->" "sampath" "Hi")
   (make-event (list "< archana->sampath >Hi") "blue")
   "given sending and reciever user for private message"))
; STRATEGY : Function Composition
(define (make-private-event user1 arrow? user2 msg)
  (make-event (wrap-message (string-append "< " user1 arrow? user2 " >" msg))
              BLUE-FONT))

; add-event-to-world : World Event -> World
; returns a World with an added given event to w.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (add-event-to-world 
    (make-world "archana" (list "archana" "Sampath")
                (list (make-event (list "Sampath joined") "gray"))
                (make-editor (make-editorpart '() '())
                             (make-editorpart '() '())))
    (make-event (list "Sampath Joined") "gray"))
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath Joined") "gray")
                     (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given a valid event and world"))
; STRATEGY : Data Decomposition on w : World
(define (add-event-to-world w event)
  (make-world (world-name w)
              (world-users w)
              (add-event-to-events event (world-events w))
              (world-editor w)))

; add-event-to-events : Event ListOf<Event> -> ListOf<Event>
; adds an event to loe.
; EXAMPLE : 
(begin-for-test
  (check-equal? 
   (add-event-to-events 
    (make-event (list "< yeager >hi") "black") 
    (list
     (make-event
      (list "< tom >incorrect spelling jager")
      "black")
     (make-event (list "yeager left the chat") "gray")
     (make-event (list "Shadow left the chat") "gray")
     (make-event (list "< yeager >") "black")
     (make-event
      (list
       "< hshah >soryy .... just checking the 25 limit of events")
      "black")
     (make-event (list "< yeager >hi") "black")
     (make-event (list "yeager joined") "gray")
     (make-event (list "< Shadow >ddt ") "black")
     (make-event (list "< hshah >hi") "black")
     (make-event (list "< tom >stop spaming") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")
     (make-event (list "< hshah >") "black")))
   (list
    (make-event (list "< yeager >hi") "black")
    (make-event
     (list "< tom >incorrect spelling jager")
     "black")
    (make-event (list "yeager left the chat") "gray")
    (make-event (list "Shadow left the chat") "gray")
    (make-event (list "< yeager >") "black")
    (make-event
     (list
      "< hshah >soryy .... just checking the 25 limit of events")
     "black")
    (make-event (list "< yeager >hi") "black")
    (make-event (list "yeager joined") "gray")
    (make-event (list "< Shadow >ddt ") "black")
    (make-event (list "< hshah >hi") "black")
    (make-event (list "< tom >stop spaming") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black")
    (make-event (list "< hshah >") "black"))
   "given list 25 events to a an event to ")) 
; STRATEGY : Function Composition
(define (add-event-to-events event loe)
  ( if (= EVENT-LIMIT (length loe))
       (append (list event) (remove-last-from-list loe))
       (append (list event) loe)))  

; add-username-to-world : World UserName -> World
; adds the given UserName to the list of current users in the World
; and adds a joining event.
; EXAMPLE:
(begin-for-test
  (check-equal? 
   (add-username-to-world 
    (make-world "archana" (list "archana" "Sampath")
                (list (make-event (list "Sampath joined") "gray"))
                (make-editor (make-editorpart '() '())
                             (make-editorpart '() '())))
    "Shah")
   (make-world "archana" (list "archana" "Sampath" "Shah")
               (list (make-event (list "Shah joined") "gray") 
                     (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given a valid world and a username"))
; STRATEGY : Data Decomposition on w : World
(define (add-username-to-world w user)
  (make-world (world-name w)
              (sort (append (list user) (world-users w)) string-ci<?) 
              (add-join-event (world-events w) user)
              (world-editor w)))

; add-join-event : ListOf<Event> UserName -> ListOf<Event>
; returns a list of Event with a new UserName joining event added to it.
; EXAMPLE:
(begin-for-test
  (check-equal? 
   (add-join-event (list (make-event (list "Shah joined") "gray")
                         (make-event (list "Sampath joined") "gray"))
                   "archana")
   (list (make-event (list "archana joined") "gray")
         (make-event (list "Shah joined") "gray")
         (make-event (list "Sampath joined") "gray"))
   "given a valid username to create a join event"))
; STRATEGY : Data Decomposition on loe : ListOf<Event>
(define (add-join-event loe user)
  (add-event-to-events 
   (make-event (wrap-message (string-append user JOINING-MSG))GRAY-FONT) loe))

; remove-username-from-world : World UserName -> World
; returns a world with the given UserName removed from it.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (remove-username-from-world 
    (make-world "archana" (list "archana" "Sampath")
                (list (make-event (list "Sampath joined") "gray"))
                (make-editor (make-editorpart '() '())
                             (make-editorpart '() '())))
    "archana")
   (make-world "archana" (list "Sampath")
               (list (make-event 
                      (list "archana left the chat") "gray")
                     (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   "given a valid world and a username to create a leaving event"))
; STRATEGY : Data Decomposition on w : World
(define (remove-username-from-world w user)
  (make-world (world-name w)
              (remove user (world-users w))
              (add-remove-event (world-events w) user)
              (world-editor w)))

; add-remove-event : ListOf<Event> UserName -> ListOf<Event>
; returns a list of Event with a new user added to loe.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (add-remove-event (list (make-event (list "Shah joined") "gray")
                           (make-event (list "Sampath joined") "gray"))
                     "archana")
   (list (make-event (list "archana left the chat") "gray")
         (make-event (list "Shah joined") "gray")
         (make-event (list "Sampath joined") "gray"))
   "given a valid username and list of event"))
; STRATEGY : Data Decomposition on loe : ListOf<Event>
(define (add-remove-event loe user)
  (add-event-to-events 
   (make-event (wrap-message (string-append user LEAVING-MSG)) GRAY-FONT) loe))

; wrap-message : String -> ListOf<String>
; returns a list of strings if the given string does not fit the right limit 
; of the chat history pane.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (wrap-message "archana")
   (list "archana")
   "given a lessage that fits inside the window")
  (check-equal?
   (wrap-message 
    "this is a really long message to test text wrapping. hopefully it works")
   (list
    "this is a really long message to test text wrapping. hop"
    "efully it works")
   "given a long message that does not fit the window"))
; STRATEGY : Function Composition
(define (wrap-message s)
  (wrap-lo1s (explode s)))  

; wrap-lo1s : ListOf<1String> -> ListOf<String>
; returns a list of string all of which fit in prefectly in the chat pane.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (wrap-lo1s (list "a" "b" "c"))
   (list "abc")
   "given a lessage that fits inside the window")
  (check-equal?
   (wrap-lo1s (list "a"))
   (list "a")
   "given a lessage that fits inside the window")
  (check-equal?
   (wrap-lo1s 
    (explode 
     "this is a really long message to test text wrapping. hopefully it works"))
   (list
    "this is a really long message to test text wrapping. hop"
    "efully it works")
   "given a long message that does not fit the window"))
; STRATEGY : Function Composition
(define (wrap-lo1s lo1s0)
  (local (; wrap-lo1s/a : ListOf<1String> ListOf<String> -> ListOf<String>
          ; returns a list of string which prefectly fit the chat pane.
          ; WHERE :
          ; los holds the strings that fit the chat pane so far from the lo1s
          ; until all of the lo1s0 is wrapped.
          ; STRATEGY : Data Decomposition on lo1s : ListOf<1String>
          (define (wrap-lo1s/a lo1s los)
            (cond
              [(empty? lo1s) los]
              [else (decide-on-appending lo1s los)]))
          ; decide-on-appending : ListOf<1String> ListOf<String> 
          ;                                             -> ListOf<String>
          ; string-appends the first of lo1s to last of los if image-width is
          ; less than limit, else list appends to los.
          ; STRATEGY : Data Decomposition on lo1s : ListOf<1String>
          (define (decide-on-appending lo1s los)
            (if (< (image-width 
                    (string->image 
                     (last (append-in-last-sring (first lo1s) los)))) 
                   EDITOR-RIGHT-LIMIT)
                (wrap-lo1s/a (rest lo1s) 
                             (append-in-last-sring (first lo1s) los))
                (wrap-lo1s/a (rest lo1s) 
                             (append los (list (first lo1s)))))))
    (wrap-lo1s/a lo1s0 (list ""))))   

; append-in-last-sring : 1String ListOf<String> -> ListOf<String>
; returns a list of string which has s appended to the end of the last 
; string of the los.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (append-in-last-sring "a" (list "b" "c"))
   (list "b" "ca")))
; STRATEGY : Data Decomposition on los :ListOf<String>
(define (append-in-last-sring s los)
  (cond
    [(empty? (rest los)) 
     (append (list (string-append (first los) s)) (rest los))]
    [else (append (list (first los)) (append-in-last-sring s (rest los)))]))

; string->image : String -> Image
; returns an image representation of s
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (string->image "archana")
   (text "archana" 12 BLACK-FONT)))
; STRATEGY : Function Composition
(define (string->image s)
  (text s FONT-SIZE BLACK-FONT))

; last : NEListOf<Any> -> Any
; returns the last element of the list.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (last (list "archana"))
   "archana")
  (check-equal?
   (last (list "archana" "sampath"))
   "sampath"))
; STRATEGY : Data Decomposition on lst : ListOf<Any> 
(define (last lst)
  (cond
    [(empty? (rest lst)) (first lst)]
    [else (last (rest lst))]))

; key-handler : World KeyEvent -> HandlerResult
; Handles keyboard user input.
; EXAMPLE :
(begin-for-test
  (check-equal? 
   (key-handler (make-world "archana" (list "archana" "Sampath")
                            (list (make-event (list "Sampath joined") "gray"))
                            (make-editor (make-editorpart (list "h") '())
                                         (make-editorpart '() '())))
                "left")
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart  '() '())
                            (make-editorpart '() (list "h"))))
   "given valid world and left key")
  (check-equal? 
   (key-handler (make-world "archana" (list "archana" "Sampath")
                            (list (make-event (list "Sampath joined") "gray"))
                            (make-editor (make-editorpart '() '())
                                         (make-editorpart (list "h") '())))
                "right")
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart  '() (list "h"))
                            (make-editorpart '() '())))
   "given valid world and right key")
  (check-equal? 
   (key-handler (make-world "archana" (list "archana" "Sampath")
                            (list (make-event (list "Sampath joined") "gray"))
                            (make-editor (make-editorpart (list "h") '())
                                         (make-editorpart '() '())))
                "\b")
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart  '() '())
                            (make-editorpart '() '())))
   "given valid world and backspace key")
  (check-equal? 
   (key-handler (make-world "archana" (list "archana" "Sampath")
                            (list (make-event (list "Sampath joined") "gray"))
                            (make-editor (make-editorpart (list "a") '())
                                         (make-editorpart '() '())))
                "\t")
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath joined") "gray"))
               (make-editor 
                (make-editorpart  (list "a") 
                                  (list "a" "n" "a" "h" "c" "r"))
                (make-editorpart '() '())))
   "given valid world and tab key")
  (check-equal? 
   (key-handler (make-world "archana" (list "archana" "Sampath")
                            (list (make-event (list "Sampath joined") "gray"))
                            (make-editor (make-editorpart (list "a") '())
                                         (make-editorpart '() '())))
                "t")
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart (list "a") (list "t"))
                            (make-editorpart '() '())))
   "given valid world and t key")
  (check-equal? 
   (key-handler (make-world "archana" (list "archana" "Sampath")
                            (list (make-event (list "Sampath joined") "gray"))
                            (make-editor (make-editorpart (list "a") '())
                                         (make-editorpart '() '())))
                "up")
   (make-world "archana" (list "archana" "Sampath")
               (list (make-event (list "Sampath joined") "gray"))
               (make-editor (make-editorpart (list "a") '())
                            (make-editorpart '() '())))
   "given valid world and up key that will be ignored")
  (check-equal? 
   (key-handler (make-world "archana" (list "archana" "Sampath")
                            (list (make-event (list "Sampath joined") "gray"))
                            (make-editor (make-editorpart (list "a") '())
                                         (make-editorpart '() '())))
                "\r")
   (make-package (make-world "archana" (list "archana" "Sampath")
                             (list (make-event (list "< archana >a") "black")
                                   (make-event (list "Sampath joined") "gray"))
                             (make-editor (make-editorpart  '() '()) 
                                          (make-editorpart '() '())))
                 (list 'broadcast "a"))
   "given valid world and return key"))
; STRATEGY : Data Decomposition on k : KeyEvent
(define (key-handler w k)
  (cond
    [(key=? k "left") (modified-editor w editor-lft)]
    [(key=? k "right") (modified-editor w editor-rgt)]
    [(key=? k "\b") (modified-editor w editor-del)]
    [(key=? k "\t") (try-autocomplete w)]
    [(key=? k "\r") (post-message w)]
    [(= (string-length k) 1) (insert-char-in-world w k)] 
    [else w]))

; get-users : World -> ListOf<UserName>
; Returns a list of current chat participants, in lexicographic order.
; EXAMPLES:
(begin-for-test
  (check-equal? 
   (get-users (make-world "archana" (list "archana" "Sampath")
                          (list (make-event (list "Sampath joined") "gray"))
                          (make-editor (make-editorpart (list "a") '())
                                       (make-editorpart '() '()))))
   (list "archana" "Sampath")))
; STRATEGY : Data Decomposirion on w : World
(define (get-users w)
  (sort (world-users w)string-ci<?)) 

; get-editor : World -> Editor
; Returns a representation of the chat client's input area.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (get-editor (make-world "archana" (list "archana" "Sampath")
                           (list (make-event (list "Sampath joined") "gray"))
                           (make-editor (make-editorpart (list "a") '())
                                        (make-editorpart '() '()))))
   (make-editor (make-editorpart (list "a") '())
                (make-editorpart '() '()))))
; STRATEGY : Data Deocmposition on w : World
(define (get-editor w)
  (world-editor w)) 

; get-editor-pre : Editor -> String
; Returns an editor's content before the cursor.
; EXAMPLE:
(begin-for-test 
  (check-equal?
   (get-editor-pre (make-editor (make-editorpart (list "a") '())
                                (make-editorpart '() '())))
   "a")
  (check-equal?
   (get-editor-pre 
    (make-editor
     (make-editorpart
      empty
      (list "i" "h" "t" " " "i" "h" " " ":" "1" "P" "D" "P"))
     (make-editorpart
      empty
      (list "s" " " "i" "s" " " "s" "h" "i" "n" "i" "g" "a" "m" "i"))))
   "PDP1: hi thi"))
; STRATEGY : Data Decomposition on ed : Editor
(define (get-editor-pre ed)
  (pre-editorpart->string (editor-pre ed)))

; get-editor-post : Editor -> String
; Returns an editor's content after the cursor.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (get-editor-post (make-editor (make-editorpart (list "a") (list "b"))
                                 (make-editorpart (list "c") '())))
   "c")
  (check-equal?
   (get-editor-post 
    (make-editor
     (make-editorpart
      empty
      (list "i" "h" "t" " " "i" "h" " " ":" "1" "P" "D" "P"))
     (make-editorpart
      empty
      (list "s" " " "i" "s" " " "s" "h" "i" "n" "i" "g" "a" "m" "i"))))
   "s is shinigami"))
; STRATEGY : Data Decomposition on ed : Editor
(define (get-editor-post ed)
  (post-editorpart->string (editor-post ed)))

; get-chat-history : World -> ListOf<String>
; Returns a list of chat events, rendered to string, 
; where each string format is the same as when the event is
; rendered to the chat window.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (get-chat-history 
    (make-world "archana" (list "archana" "Sampath")
                (list (make-event (list "Sampath joined") "gray"))
                (make-editor (make-editorpart (list "a") '())
                             (make-editorpart '() '()))))
   (list "Sampath joined")))
; STRATEGY : Data Decomposition on w : World
(define (get-chat-history w)
  (get-strings-from-events (world-events w)))

; get-strings-from-events : ListOf<Event> -> ListOf<String>
; returns a list of strings that contains all the messages of all the events.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (get-strings-from-events (list (make-event (list "Sampath joined") "gray")))
   (list "Sampath joined"))
  (check-equal?
   (get-strings-from-events (list
                             (make-event (list "< hshah > Hi") "blue")
                             (make-event (list "< hshah->hshah > Hi") "blue")
                             (make-event (list "< hshah >Hi") "black")
                             (make-event (list "PDP1 left the chat") "gray")))
   (list
    "PDP1 left the chat"
    "< hshah >Hi"
    "< hshah->hshah > Hi"
    "< hshah > Hi")))
; STRATEGY : Data Decomposition on events : ListOf<Event>
(define (get-strings-from-events events)
  (reverse (map get-event-msg events))) 

; get-event-msg : Event : String
; returns a string that is formed by appending all the elements 
; of the (event-msg event).
; EXAMPLE:
(begin-for-test
  (check-equal?
   (get-event-msg (make-event (list "Sampath joined") "gray"))
   "Sampath joined"))
; STRATEGY : Data Decomposition on event : Event
(define (get-event-msg event)
  (append-strings-in-msg (event-msg event)))

; append-strings-in-msg : ListOf<String> -> String
; returns a single string after appending all the elemental strings of los.
(begin-for-test
  (check-equal?
   (append-strings-in-msg (list "archana" "sampath"))
   "archanasampath"))
; STRATEGY : Data Decomposition on los : ListOf<String>
(define (append-strings-in-msg los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los)(append-strings-in-msg (rest los)))]))

; render : World -> Image
; returns the image representation of the world in the given state.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (render (make-world "archana" (list "archana" "Sampath")
                       (list (make-event (list "Sampath joined") "gray"))
                       (make-editor (make-editorpart (list "a") '())
                                    (make-editorpart '() '()))))
   (beside (show-users (list "archana" "Sampath")) 
           (above/align 
            "left"  
            (render-events (list (make-event (list "Sampath joined") "gray")))
            (render-editor (make-editor (make-editorpart (list "a") '())
                                        (make-editorpart '() '())))))
   "renders an image representation of a state of world"))
; STRATEGY : Data Decomposition on w : World
(define (render w)
  (beside (show-users (world-users w)) 
          (above/align "left"  (render-events (world-events w))
                       (render-editor (world-editor w)))))


; render-events : ListOf<Event> -> Image
; returns the image represenation of the events.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (render-events (list (make-event (list "Sampath joined") "gray")))
   (show-chat-history (list (make-event (list "Sampath joined") "gray")))
   "renders an image representation of one event")
  (check-equal?
   (render-events (list (make-event (list "Sampath joined") "gray")
                        (make-event (list "yeager left the chat") "gray")
                        (make-event (list "Shadow left the chat") "gray")))
   (show-chat-history (list (make-event (list "Sampath joined") "gray")
                            (make-event (list "yeager left the chat") "gray")
                            (make-event (list "Shadow left the chat") "gray")))
   "renders an image representation of more than one event")
  (check-equal?
   (render-events empty)
   CHAT-AREA
   "renders an image representation of no event"))
; STRATEGY : Function Composition 
(define (render-events events)
  (if (empty? events)
      CHAT-AREA
      (show-chat-history events)))

; show-users : NEListOf<UserName> -> Image
; returns the image of UserName Pane.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (show-users (list  "notDan"  "guessWho"  "Stephen"  "Dan"  "test"  "UserB"
                      "hshah"))
   (overlay/align "left" "top" 
                  (display-list-of-strings 
                   (sort (list  "notDan"  "guessWho"  
                                "Stephen"  "Dan"  "test"  "UserB"
                                "hshah")string-ci<?)) 
                  USER-DISPLAY)
   "renders more than one users in the userlist pane")
  (check-equal?
   (show-users (list  "notDan"  "guessWho"  "Stephen"  "Dan"  "test"  "UserB"
                      "hshah" "a" "b" "c" "d" "e" "f" "zebra" "giraffe" "lion"
                      "cred" "ned" "sunny" "dude123" "angel" "devil" "god" 
                      "james" "lars" "kirk" "rob" "dave" "dimebag"))
   (overlay/align "left" "top" 
                  (display-list-of-strings 
                   (sort 
                    (list  
                     "notDan"  "guessWho"  "Stephen"  "Dan"  
                     "hshah" "a" "b" "c" "d" "e" "f"  "giraffe" "lion"
                     "cred" "ned" "sunny" "dude123" "angel" "devil" "god" 
                     "james" "lars" "kirk" "rob" "dave" "dimebag")string-ci<?)) 
                  USER-DISPLAY)
   "renders userlist that exceeds the userlist pane height"))
; STRATEGY : Function Composition
(define (show-users users)
  (if (>(image-height (display-list-of-strings users)) USERLIST-DISPLAY-LIMIT)
      (show-users (remove-last-from-list (sort users string-ci<?)))
      (overlay/align "left" "top" 
                     (display-list-of-strings (sort users string-ci<?)) 
                     USER-DISPLAY))) 

; display-list-of-strings : NEListOf<UserName> -> Image
; returns an image of UserName stacked above each other.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (display-list-of-strings 
    (list  "notDan"  "guessWho"  "Stephen"  "Dan"  "test"  "UserB"
           "hshah"))
   (foldl strings-above-each-other 
          (text (first (list  "notDan"  "guessWho"  
                              "Stephen"  "Dan"  "test"  "UserB"
                              "hshah")) FONT-SIZE BLACK-FONT) 
          (list  "guessWho"  
                 "Stephen"  "Dan"  "test"  "UserB"
                 "hshah"))
   "renders string in the list over each other"))
; STRATEGY : Data Decomposition on users : ListOf<UserName>
(define (display-list-of-strings users)
  (foldl strings-above-each-other 
         (text (first users) FONT-SIZE BLACK-FONT) (rest users)))

; strings-above-each-other : UserName Image -> Image
; returns an image of user stacked under the incoming image.
(begin-for-test
  (check-equal?
   (strings-above-each-other  "notDan"  USER-DISPLAY)
   (above/align "left" USER-DISPLAY (text "notDan" FONT-SIZE BLACK-FONT))
   "renders string over an empty scene")) 
; STRATEGY : Function Composition
(define (strings-above-each-other user image)
  (above/align "left" image (text user FONT-SIZE BLACK-FONT)))

; show-chat-history : ListOf<Event> -> Image
; returns an image of chat-area with the chat events over it.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (show-chat-history  (list (make-event (list "< hshah > this") "blue")))
   (overlay/align
    "left" "top" 
    (display-list-of-events 
     (list (make-event (list "< hshah > this") "blue"))) CHAT-AREA)
   "renders chat history in chat pane")
  (check-equal?
   (show-chat-history  (list
                        (make-event (list "PDP left the chat") "gray")
                        (make-event (list "lalala left the chat") "gray")
                        (make-event (list "< PDP >hi") "blue")
                        (make-event (list "< PDP >cgcgffgfdfsfdf") "black")
                        (make-event (list "PDP joined") "gray")
                        (make-event (list "< hshah > be") "blue")
                        (make-event (list "< hshah->hshah > be") "blue")
                        (make-event (list "< hshah > to") "blue")
                        (make-event (list "< hshah->hshah > to") "blue")
                        (make-event (list "< hshah > going") "blue")
                        (make-event (list "< hshah->hshah > going") "blue")
                        (make-event (list "< hshah > this") "blue")
                        (make-event (list "< hshah->hshah > this") "blue")
                        (make-event (list "< hshah > fine") "blue")
                        (make-event (list "< hshah->hshah > fine") "blue")
                        (make-event (list "< hshah > am") "blue")
                        (make-event (list "< hshah->hshah > am") "blue")
                        (make-event (list "< hshah > i") "blue")
                        (make-event (list "< hshah->hshah > i") "blue")
                        (make-event (list "< hshah > you") "blue")
                        (make-event (list "< hshah->hshah > you") "blue")
                        (make-event (list "< hshah >hs") "black")
                        (make-event (list "< hshah > are") "blue")
                        (make-event (list "< hshah->hshah > are") "blue")
                        (make-event (list "< hshah > how") "blue")
                        (make-event (list "< hshah > how") "blue")
                        (make-event (list "< hshah > how") "blue")
                        (make-event (list "< hshah > how") "blue")))
   (overlay/align
    "left" "top" 
    (display-list-of-events (list
                             (make-event (list "PDP left the chat") "gray")
                             (make-event (list "lalala left the chat") "gray")
                             (make-event (list "< PDP >hi") "blue")
                             (make-event (list "< PDP >cgcgffgfdfsfdf") "black")
                             (make-event (list "PDP joined") "gray")
                             (make-event (list "< hshah > be") "blue")
                             (make-event (list "< hshah->hshah > be") "blue")
                             (make-event (list "< hshah > to") "blue")
                             (make-event (list "< hshah->hshah > to") "blue")
                             (make-event (list "< hshah > going") "blue")
                             (make-event (list "< hshah->hshah > going") "blue")
                             (make-event (list "< hshah > this") "blue")
                             (make-event (list "< hshah->hshah > this") "blue")
                             (make-event (list "< hshah > fine") "blue")
                             (make-event (list "< hshah->hshah > fine") "blue")
                             (make-event (list "< hshah > am") "blue")
                             (make-event (list "< hshah->hshah > am") "blue")
                             (make-event (list "< hshah > i") "blue")
                             (make-event (list "< hshah->hshah > i") "blue")
                             (make-event (list "< hshah > you") "blue")
                             (make-event (list "< hshah->hshah > you") "blue")
                             (make-event (list "< hshah >hs") "black")
                             (make-event (list "< hshah > are") "blue")
                             (make-event (list "< hshah->hshah > are") "blue")
                             (make-event (list "< hshah > how") "blue"))) 
    CHAT-AREA)
   "renders events that exceeds the chat pane height"))
; STRATEGY : Function Composition
(define (show-chat-history loe)
  (if (>(image-height (display-list-of-events loe)) CHAT-EVENT-DISPLAY-LIMIT)
      (show-chat-history (remove-last-from-list loe))
      (overlay/align "left" "top" 
                     (display-list-of-events loe) CHAT-AREA))) 

; display-list-of-events : ListOf<Event> -> Image
; returns an image of all the events in the list stacked over each other.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (display-list-of-events  (list (make-event (list "< hshah > this") "blue")))
   (foldl event-above-each-other 
          (event->image (first 
                         (reverse 
                          (list (make-event (list "< hshah > this") "blue"))))) 
          (rest (reverse (list (make-event (list "< hshah > this") "blue")))))
   "renders chat history over each other"))
; STRATEGY : Data Decomposition on loe : ListOf<Event> 
(define (display-list-of-events loe)
  (foldl event-above-each-other 
         (event->image (first (reverse loe))) 
         (rest (reverse loe))))

; event-above-each-other : Event Image -> Image
; returns an Image with the given Event stacked under the given Image.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (event-above-each-other  (make-event (list "< hshah > this") "blue")
                            (event->image 
                             (make-event (list "< hshah > are") "blue")))
   (above/align "left" (event->image 
                        (make-event (list "< hshah > are") "blue")) 
                (event->image (make-event (list "< hshah > this") "blue")))
   "renders onr event over another"))
; STRATEGY : Function Composition
(define (event-above-each-other event image)
  (above/align "left" image (event->image event)))

; event->image : Event -> Image
; returns an Image representation of the Event.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (event->image  (make-event (list "< hshah > this") "blue"))
   (show-an-event (list "< hshah > this") "blue")
   "renders one event"))
; STRATEGY : Data Decomposition on event : Event
(define (event->image event)
  (show-an-event (event-msg event) (event-color event)))

; show-an-event : ListOf<String> String -> Image
; WHERE : String is the color?.
; INTERP : renders an image of an event.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (show-an-event (list "< hshah > this") "blue")
   (text "< hshah > this" 12 "blue")
   "renders one event")
  (check-equal?
   (show-an-event (list "< hshah > this" "is awesome") "blue")
   (above/align "left" (text "< hshah > this" 12 "blue") 
                (text "is awesome" 12 "blue"))
   "renders more than one events"))
; STRATEGY : Function Composition
(define (show-an-event msg color)
  (local(; strings-above-each-other : String Image  
         ; returns an Image where s is stacked below image.
         ; STRATEGY : Function Composition
         (define (strings-above-each-other s image)
           (above/align "left" image (text s FONT-SIZE color))))
    (foldl strings-above-each-other 
           (text (first msg) FONT-SIZE color) 
           (rest msg))))

; modified-editor : World [Editor -> Editor] -> World
; returns a World with required changes in the editor.
; STRATEGY : Data Decomposition on w : World
(define (modified-editor w f)
  (make-world (world-name w)
              (world-users w)
              (world-events w)
              (f (world-editor w))))

; insert-char-in-world : World 1String -> World
; returns a World whose editor part has an added 1String to pre
; STRATEGY : Data Decomposition on w : World
(define (insert-char-in-world w k)
  (make-world (world-name w)
              (world-users w)
              (world-events w)
              (editor-ins (world-editor w) k)))

; editor-lft : Editor -> Editor
; moves the cursor position one 1String left, if possible.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (editor-lft 
    (make-editor (make-editorpart empty empty)
                 (make-editorpart
                  (list "r")
                  (list "k" "s" "h" "a" "h" " " "i" "s" " " 
                        "a" "w" "e" "s" "o" "m" "e" " " 
                        "p" "r" "o" "g" "r" "a" "m" "e"))))
   (editor-lft 
    (make-editor (make-editorpart empty empty)
                 (make-editorpart
                  (list "r")
                  (list "k" "s" "h" "a" "h" " " "i" "s" " " 
                        "a" "w" "e" "s" "o" "m" "e" " " 
                        "p" "r" "o" "g" "r" "a" "m" "e"))))
   "pre of the editor is empty")
  (check-equal?
   (editor-lft 
    (make-editor (make-editorpart (list "i" "d" "r" "a" "h") empty)
                 (make-editorpart
                  (list "r")
                  (list "k" "s" "h" "a" "h" " " "i" "s" " " 
                        "a" "w" "e" "s" "o" "m" "e" " " 
                        "p" "r" "o" "g" "r" "a" "m" "e"))))
   (make-editor
    (make-editorpart (list "d" "r" "a" "h") empty)
    (make-editorpart
     (list "r")
     (list "i" "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e")))
   "pre of the editor is not empty, but disp of pre is empty")
  (check-equal?
   (editor-lft 
    (make-editor (make-editorpart (list "i" "d" "r" "a" "h") (list "k"))
                 (make-editorpart
                  (list "r")
                  (list "k" "s" "h" "a" "h" " " "i" "s" " " 
                        "a" "w" "e" "s" "o" "m" "e" " " 
                        "p" "r" "o" "g" "r" "a" "m" "e"))))
   (make-editor
    (make-editorpart (list "i" "d" "r" "a" "h") empty)
    (make-editorpart
     (list "r")
     (list "k" "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e")))
   "pre of the editor is not empty, but disp of pre is empty"))
; STRATEGY : Data Decomposition on ed : Editor
(define (editor-lft ed)
  (if (editor-part-empty? (editor-pre ed)) 
      ed
      (move-cursor-left (editor-pre ed) (editor-post ed))))

; decide-on-disp-nondisp : EditorPart EditorPart -> Editor
; returns prt2 after readjusting the nondisp and disp if needed.
(begin-for-test
  (check-equal?
   (decide-on-disp-nondisp 
    (make-editorpart (list "i" "d" "r" "a" "h") (list " "))
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editorpart
    (list "r")
    (list "i" "k" "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e"))
   "the prt2 is not exceeding the limit")
  (check-equal?
   (decide-on-disp-nondisp 
    (make-editorpart (list "i" "d" "r" "a" "h") (list " "))
    (make-editorpart 
     (list "s" "e" "s") 
     (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
           "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
           " ""h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " "c" "e" "r" 
           "t""a" "i" "n" " " "c" "a")))
   (make-editorpart 
    (list "a" "s" "e" "s") 
    (list "i" "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
          "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
          " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " "c" "e" "r" 
          "t" "a" "i" "n" " " "c"))
   "the prt2 is exceeding the limit"))
; STRATEGY : Function Composition
(define (decide-on-disp-nondisp prt1 prt2)
  (if (image-width-going-past-limit 
       (add-from-nondisp-disp prt1 prt2))
      (move-from-disp-nondisp (add-from-nondisp-disp prt1 prt2))
      (add-from-nondisp-disp prt1 prt2)))

; move-cursor-left : EditorPart EditorPart -> Editor
; move the cursor to the left.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (move-cursor-left 
    (make-editorpart (list "i" "d" "r" "a" "h") (list " "))
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editor
    (make-editorpart (list "i" "d" "r" "a" "h") empty)
    (make-editorpart
     (list "r")
     (list " " "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e"))) 
   "display part of pre is empty")
  (check-equal?
   (move-cursor-left 
    (make-editorpart (list "i" "d" "r" "a" "h") (list "h"))
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editor
    (make-editorpart (list "i" "d" "r" "a" "h") empty)
    (make-editorpart
     (list "r")
     (list "h" "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e"))) 
   "display part of pre is not empty"))
; STRATEGY : Function Composition
(define (move-cursor-left pre post)
  (if (empty? (editorpart-disp pre))
      (make-editor (remove-first-from-nondisp pre) 
                   (decide-on-disp-nondisp pre post))
      (make-editor (remove-first-from-disp pre) 
                   (add-from-disp-disp pre post))))

; remove-first-from-nondisp : EditorPart -> EditorPart
; delete first char from nondisp part.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (remove-first-from-nondisp 
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editorpart
    empty
    (list "k" "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e")) 
   "theres a char present in non disp")
  (check-equal?
   (remove-first-from-nondisp 
    (make-editorpart
     empty
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editorpart
    empty
    (list "k" "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e")) 
   "theres a char not present in non disp"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (remove-first-from-nondisp edprt)
  (make-editorpart (remove-first-from-list (editorpart-nondisp edprt))
                   (editorpart-disp edprt)))

; remove-first-from-disp : EditorPart -> EditorPart
; delete first char from disp part.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (remove-first-from-disp 
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editorpart
    (list "r")
    (list "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e")) 
   "theres a char present in disp"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (remove-first-from-disp edprt)
  (make-editorpart (editorpart-nondisp edprt)
                   (remove-first-from-list (editorpart-disp edprt))))

; get-first-of-disp-nondisp : [EditorPart -> ListOf<1String>] EditorPart 
;                                                                 -> 1String
; returns the first 1String present in disp or nondisp of edprt.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-first-of-disp-nondisp editorpart-disp
                              (make-editorpart
                               (list "r")
                               (list "k" "s" "h" "a" "h" " " "i" "s" " " 
                                     "a" "w" "e" "s" "o" "m" "e" " " 
                                     "p" "r" "o" "g" "r" "a" "m" "e")))
   (list "k") 
   "theres a char present in disp")
  (check-equal?
   (get-first-of-disp-nondisp editorpart-nondisp
                              (make-editorpart
                               empty
                               (list "k" "s" "h" "a" "h" " " "i" "s" " " 
                                     "a" "w" "e" "s" "o" "m" "e" " " 
                                     "p" "r" "o" "g" "r" "a" "m" "e")))
   empty 
   "theres a no char present in disp"))
; STRATEGY : Data Decoposition on edprt : EditorPart
(define (get-first-of-disp-nondisp f edprt)
  (if (empty? (get-first-of-list (f edprt)))
      empty
      (list (get-first-of-list (f edprt)))))

; add-from-nondisp-disp : EditorPart EditorPart : EditorPart
; move from nondisp of prt1 to disp of prt2.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (add-from-nondisp-disp 
    (make-editorpart (list "i" "d" "r" "a" "h") empty)
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editorpart
    (list "r")
    (list "i" "k" "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e")) 
   "theres a char present in nondisp of prt1"))
; STRATEGY : Data Decomposition on prt2 : EditorPart
(define (add-from-nondisp-disp prt1 prt2)
  (make-editorpart (editorpart-nondisp prt2)
                   (append (get-first-of-disp-nondisp editorpart-nondisp prt1)
                           (editorpart-disp prt2))))

; add-from-disp-disp : EditorPart EditorPart : EditorPart
; move from disp of prt1 to disp of prt2.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (add-from-nondisp-disp 
    (make-editorpart (list "i" "d" "r" "a" "h") empty)
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editorpart
    (list "r")
    (list "i" "k" "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e")) 
   "theres a char present in nondisp of prt1"))
; STRATEGY : Data Decomposition on prt2 : EditorPart
(define (add-from-disp-disp prt1 prt2)
  (make-editorpart (editorpart-nondisp prt2)
                   (append (get-first-of-disp-nondisp editorpart-disp prt1)
                           (editorpart-disp prt2))))

; editor-part-empty? : EditorPart -> Boolean
; returns true if the part before cursor is empty.
(begin-for-test
  (check-equal?
   (editor-part-empty? 
    (make-editorpart empty empty)) #true "the editor part is empty")
  (check-equal?
   (editor-part-empty? 
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e"))) 
   #false "the editor part is not empty"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (editor-part-empty? edprt)
  (and (empty? (editorpart-disp edprt)) 
       (empty? (editorpart-nondisp edprt))))

; remove-first-from-list : ListOf<Any> -> ListOf<Any>
; returns rest of the input string, the end effect is as if first 
; item of the list is removed.
(begin-for-test
  (check-equal?
   (remove-first-from-list 
    (list "k" "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e")) 
   (list "s" "h" "a" "h" " " "i" "s" " " 
         "a" "w" "e" "s" "o" "m" "e" " " 
         "p" "r" "o" "g" "r" "a" "m" "e") "given a list with 1String")
  (check-equal?
   (remove-first-from-list 
    empty) 
   empty "given a list with empty "))
; STRATEGY : Data Decomposition on s : ListOf<1String>
(define (remove-first-from-list s)
  (cond
    [(empty? s) s ]
    [else (rest s)]))

; remove-last-from-list : NEListOf<Any> -> ListOf<Any>
; returns a list which does not have the last element of lst.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (remove-last-from-list 
    (list "k" "s" "h" "a" "h" " " "i" "s" " " 
          "a" "w" "e" "s" "o" "m" "e" " " 
          "p" "r" "o" "g" "r" "a" "m" "e")) 
   (list "k" "s" "h" "a" "h" " " "i" "s" " " 
         "a" "w" "e" "s" "o" "m" "e" " " 
         "p" "r" "o" "g" "r" "a" "m" ) "given a list with 1String"))
; STRATEGY : Data Decomposition on lst : ListOf<Any>
(define (remove-last-from-list lst)
  (cond
    [(empty? (rest lst)) '() ]
    [else (append (list (first lst)) (remove-last-from-list(rest lst)))]))

; editor-rgt : Editor -> Editor
; moves the cursor position one 1String right, if possible.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (editor-rgt 
    (make-editor (make-editorpart empty empty)
                 (make-editorpart
                  (list "r")
                  (list "k" "s" "h" "a" "h" " " "i" "s" " " 
                        "a" "w" "e" "s" "o" "m" "e" " " 
                        "p" "r" "o" "g" "r" "a" "m" "e"))))
   (make-editor (make-editorpart empty (list "k"))
                (make-editorpart
                 (list "r")
                 (list "s" "h" "a" "h" " " "i" "s" " " 
                       "a" "w" "e" "s" "o" "m" "e" " " 
                       "p" "r" "o" "g" "r" "a" "m" "e")))
   "post of the editor is not empty empty")
  (check-equal?
   (editor-rgt 
    (make-editor (make-editorpart empty empty)
                 (make-editorpart empty empty)))
   (make-editor (make-editorpart empty empty)
                (make-editorpart empty empty))
   "post of the editor is empty"))
; STRATEGY : Data Decomposition on ed : Editor
(define (editor-rgt ed)
  (if (editor-part-empty? (editor-post ed)) 
      ed
      (move-cursor-right (editor-pre ed) (editor-post ed))))

; move-cursor-right : EditorPart EditorPart -> Editor
; move the cursor to the left.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (move-cursor-right 
    (make-editorpart empty empty)
    (make-editorpart
     (list "r")
     (list "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " 
           "p" "r" "o" "g" "r" "a" "m" "e")))
   (make-editor
    (make-editorpart empty (list "k"))
    (make-editorpart
     (list "r")
     (list "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e")))
   "post of the editor is not empty empty")
  (check-equal?
   (move-cursor-right 
    (make-editorpart empty empty)
    (make-editorpart
     (list "r")
     empty))
   (make-editor
    (make-editorpart empty (list "r"))
    (make-editorpart empty empty))
   "post of the editor is not empty empty"))
; STRATEGY : Data Deccomposition on pre : EditorPart
(define (move-cursor-right pre post)
  (if (empty? (editorpart-disp post))
      (make-editor (decide-on-disp-nondisp post pre)
                   (remove-first-from-nondisp post))
      (make-editor (add-from-disp-disp post pre) 
                   (remove-first-from-disp post))))

; get-first-of-list : ListOf<Any> -> Any
; returns the first of lst if present.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (get-first-of-list (list "a" "b" "c"))
   "a"))
; STRATEGY : Data Decomposition on lst : ListOf<Any>
(define (get-first-of-list lst)
  (cond 
    [(empty? lst) lst]
    [else (first lst)])) 

; get-last-of-list : NEListOf<Any> -> Any
; returns the last element of the lst if non empty.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (get-last-of-list (list "a" "b" "c"))
   "c"))
; STRATEGY : Data Decomposition on lst : ListOf<Any>
(define (get-last-of-list lst)
  (cond 
    [(empty? (rest lst)) (first lst)]
    [else (get-last-of-list (rest lst))]))

; editor-del : Editor -> Editor
; deletes one 1String to the left of the cursor, if possible.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (editor-del (make-editor (make-editorpart (list "a") (list "b"))
                            (make-editorpart (list "c") (list "d"))))
   (make-editor (make-editorpart (list "a") '())
                (make-editorpart '() (list "c" "d"))))
  (check-equal?
   (editor-del (make-editor (make-editorpart '() '())
                            (make-editorpart '() '())))
   (make-editor (make-editorpart '() '())
                (make-editorpart '() '()))))
; STRATEGY : Data Decomposition on ed : Editor
(define (editor-del ed)
  (if (editor-part-empty? (editor-pre ed)) 
      ed
      (do-delete (editor-pre ed) (editor-post ed))))

; do-delete : EditorPart EditorPart -> Editor
; does a deletes from the pre of the editor and adjusts post.
; EXAMLE :
(begin-for-test
  (check-equal?
   (do-delete  (make-editorpart '() '())
               (make-editorpart '() '()))
   (make-editor (make-editorpart '() '())
                (make-editorpart '() '()))))
; STRATEGY : Function Composition
(define (do-delete pre post)
  (if (or (empty? (editorpart-disp pre))
          (image-width-going-past-limit post))
      (make-editor (remove-first-from-nondisp pre)  
                   (move-from-nondisp-disp post))
      (make-editor (remove-first-from-disp pre) 
                   (move-from-nondisp-disp post)))) 

; move-from-nondisp-disp : EditorPart -> EditorPart
; moves the char from nondisp to disp.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (move-from-nondisp-disp (make-editorpart (list "a") (list "b")))
   (make-editorpart '() (list "a" "b"))))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (move-from-nondisp-disp edprt)
  (make-editorpart (remove-first-from-list (editorpart-nondisp edprt)) 
                   (shift-from-nondisp-disp edprt)))

; shift-from-nondisp-disp : EditorPart -> ListOf<1String>
; shifts the first char from nondisp of edprt to disp.
; EXAMPLE:
(begin-for-test
  (check-equal?
   (shift-from-nondisp-disp (make-editorpart (list "a") (list "b")))
   (list "a" "b")))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (shift-from-nondisp-disp edprt)
  (if (empty? (get-first-of-list (editorpart-nondisp edprt)))
      (append empty (editorpart-disp edprt))
      (append (list (get-first-of-list (editorpart-nondisp edprt)))
              (editorpart-disp edprt))))

;;editor-ins : Editor 1String -> Editor
;;insert the 1String k between pre and post.
; EXAMPLE:
(begin-for-test 
  (check-equal?
   (editor-ins (make-editor (make-editorpart (list "a") (list "b"))
                            (make-editorpart (list "c") (list "d"))) "a")
   (make-editor (make-editorpart (list "a") (list "a" "b"))
                (make-editorpart (list "c") (list "d"))))
  (check-equal?
   (editor-ins (make-editor (make-editorpart (list "a") (list "b"))
                            (make-editorpart (list "c") (list "d"))) "a")
   (make-editor (make-editorpart (list "a") (list "a" "b"))
                (make-editorpart (list "c") (list "d"))))
  (check-equal?
   (editor-ins 
    (make-editor
     (make-editorpart empty empty) 
     (make-editorpart 
      (list "s" "e" "s") 
      (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
            "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
            " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
            "c" "e" "r" "t" "a" "i" "n" " " "c" "a")))  "a")
   (make-editor
    (make-editorpart empty (list "a"))
    (make-editorpart
     (list "s" "e" "s")
     (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
           "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
           " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
           "c" "e" "r" "t" "a" "i" "n" " " "c" "a"))) 
   "given is a long message that exceeds the limit")
  (check-equal?
   (editor-ins 
    (make-editor
     (make-editorpart
      empty 
      (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
            "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
            " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
            "c" "e" "r" "t" "a" "i" "n" " " "c" "a")) 
     (make-editorpart 
      (list "s" "e" "s") 
      (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
            "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
            " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
            "c" "e" "r" "t" "a" "i" "n" " " "c" "a")))  "a")
   (make-editor
    (make-editorpart
     (list "a")
     (list "a" "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
           "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
           " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
           "c" "e" "r" "t" "a" "i" "n" " " "c")) 
    (make-editorpart 
     (list "s" "e" "s") 
     (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
           "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
           " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
           "c" "e" "r" "t" "a" "i" "n" " " "c" "a")))             
   "given is a long message that exceeds the limit")
  (check-equal?
   (editor-ins 
    (make-editor
     (make-editorpart
      empty 
      (list "t" "h" "i" "s" " " "i" "s" )) 
     (make-editorpart 
      (list "s" "e" "s") 
      (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
            "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
            " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
            "c" "e" "r" "t" "a" "i" "n" " " "c" "a")))  "a")
   (make-editor
    (make-editorpart
     empty
     (list "a" "t" "h" "i" "s" " " "i" "s"))
    (make-editorpart
     (list "a" "s" "e" "s")
     (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
           "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
           " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
           "c" "e" "r" "t" "a" "i" "n" " " "c")))               
   "given insertion happens in the middle of the editor"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (editor-ins ed k)
  (if (image-width-going-past-limit (add-1string-to-pre (editor-pre ed) k))
      (make-editor (move-from-disp-nondisp
                    (add-1string-to-pre (editor-pre ed) k)) 
                   (editor-post ed))
      (if(>= (image-width (editor->image (editor-pre ed)(editor-post ed)))
             EDITOR-RIGHT-LIMIT)
         (make-editor (add-1string-to-pre (editor-pre ed) k) 
                      (move-from-disp-nondisp (editor-post ed)))
         (make-editor (add-1string-to-pre (editor-pre ed) k) 
                      (editor-post ed))))) 

; image-width-going-past-limit : EditorPart : Boolean
; returns true if image representation of the EditorPart goes out of 
; bounds.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (image-width-going-past-limit
    (make-editorpart
     (list "s" "e" "s")
     (list "t" "h" "i" "s" " " "i" "s" " " "a" " " "l" "o" "n" "g" " " 
           "m" "e" "s" "s" "a" "g" "e" " " "t" "h" "a" "t" " " "w" "i" "l" "l" 
           " " "h" "e" "l" "p" " " "m" "e" " " "t" "e" "s" "t" " " 
           "c" "e" "r" "t" "a" "i" "n" " " "c" "a" "b" "c")))
   #true
   "given a editorpart that exceeds the limit"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (image-width-going-past-limit edprt)
  (>= (image-width (editor-text (editorpart-disp edprt))) EDITOR-RIGHT-LIMIT))

;;add-1string-to-pre : EditorPart 1String -> EditorPart
;;adds 1String to (editor-pre ed)
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (add-1string-to-pre
    (make-editorpart
     (list "s" "e" "s")
     (list "t" "h" "i" "s" )) "A")
   (make-editorpart
    (list "s" "e" "s")
    (list "A" "t" "h" "i" "s"))
   "given a valid editor and a key"))
;;STRATEGY : Data Decomposition on ed : Editor
(define (add-1string-to-pre pre k)
  (make-editorpart (editorpart-nondisp pre)
                   (cons k (editorpart-disp pre))))

; move-from-disp-nondisp : EditorPart -> EditorPart
; moves the char from nondisp to disp.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (move-from-disp-nondisp
    (make-editorpart
     (list "s" "e" "s")
     (list "t" "h" "i" "s" )))
   (make-editorpart (list "s" "s" "e" "s") (list "t" "h" "i"))
   "given a valid editorpart"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (move-from-disp-nondisp edprt)
  (make-editorpart (shift-from-disp-nondisp edprt)
                   (remove-last-from-list (editorpart-disp edprt))))

; shift-from-disp-nondisp : EditorPart -> ListOf<1String> 
; shifts the last char from nondisp of edprt to disp.
; EXAMPLE : 
(begin-for-test 
  (check-equal?
   (shift-from-disp-nondisp
    (make-editorpart
     (list "s" "e" "s")
     (list "t" "h" "i" "s" )))
   (list "s" "s" "e" "s")
   "shifts from disp of editor part to nondisp"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (shift-from-disp-nondisp edprt)
  (append (list (get-last-of-list (editorpart-disp edprt)))
          (editorpart-nondisp edprt)))

; render-editor : Editor -> Image
; renders an image of the incoming editor.
; EXAMPLE : 
(begin-for-test 
  (check-equal?
   (render-editor
    (make-editor
     (make-editorpart (list "i" "d" "r" "a" "h") empty)
     (make-editorpart
      (list "r")
      (list "k" "k" "s" "h" "a" "h" " " "i" "s" " " 
            "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e"))))
   (place-image/align 
    (editor->image 
     (make-editorpart (list "i" "d" "r" "a" "h") empty)
     (make-editorpart
      (list "r")
      (list "k" "k" "s" "h" "a" "h" " " "i" "s" " " 
            "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e")))
    1 1 "left" "top" EDITOR)
   "given a valid editor input"))
; STRATEGY : Data Decomposition on ed : Editor
(define (render-editor ed)
  (place-image/align (editor->image (editor-pre ed)(editor-post ed))
                     1 1 "left" "top" EDITOR))

; editor->image : EditotPart EditorPart -> Image
; returns an image after merging images of pre and post together.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (editor->image
    (make-editorpart (list "i" "d" "r" "a" "h") empty)
    (make-editorpart
     (list "r")
     (list "k" "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e")))
   (beside 
    (editor-text empty)
    CURSOR
    (editor-text 
     (list "k" "k" "s" "h" "a" "h" " " "i" "s" " " 
           "a" "w" "e" "s" "o" "m" "e" " " "p" "r" "o" "g" "r" "a" "m" "e")))
   "given a valid pre and post input"))
; STRATEGY : Data Decomposition on pre, post : EditorPart
(define (editor->image pre post)
  (beside (editor-text (reverse(editorpart-disp pre)))
          CURSOR
          (editor-text (editorpart-disp post))))

;;editor-text : ListOf<1String> -> Image
;;renders a list of 1Strings as a text image.
(begin-for-test 
  (check-equal?
   (editor-text (list "i" "d" "r" "a" "h"))
   (text "idrah" FONT-SIZE BLACK-FONT)
   "given a valid list of 1string"))
;;STRATEGY : Data Decomposition on lo1s : ListOf<1String>
(define (editor-text lo1s)
  (cond
    [(empty? lo1s) (text "" FONT-SIZE BLACK-FONT)]
    [else (text (implode lo1s) FONT-SIZE BLACK-FONT)]))

; editor->string : Editor -> String
; returns the string represention of the editor.
; EXAMPLE : 
(begin-for-test 
  (check-equal?
   (editor->string 
    (make-editor
     (make-editorpart (list "k" "i" "d" "r" "a" "h") (list " "))
     (make-editorpart
      (list "r")
      (list "s" "h" "a" "h" " " "i" "s" " " "a" "w" "e" "s" "o" "m" "e" 
            " " "p" "r" "o" "g" "r" "a" "m" "e"))))
   "hardik shah is awesome programer"
   "given a valid editor with entry"))
; STRATEGY : Data Decomposition on ed : Editor
(define (editor->string ed)
  (string-append (pre-editorpart->string (editor-pre ed)) 
                 (post-editorpart->string (editor-post ed))))

; pre-editorpart->string : EditorPart -> String
; WHERE : the editor part is the pre of Editor.
; returns the string representation of the EditorPart.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (pre-editorpart->string
    (make-editorpart (list "k" "i" "d" "r" "a" "h") (list " ")))
   "hardik "
   "given a valid pre of an editor with entry"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (pre-editorpart->string edprt)
  (string-append (implode (reverse (editorpart-nondisp edprt))) 
                 (implode (reverse (editorpart-disp edprt)))))

; post-editorpart->string : EditorPart -> String
; WHERE : the edprt is the post of Editor.
; returns the string representation of the EditorPart.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (post-editorpart->string
    (make-editorpart (list "t" "h" "i" "s" ) (list " ")))
   " this"
   "given a valid post of an editor with entry"))
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (post-editorpart->string edprt)
  (string-append (implode (editorpart-disp edprt))
                 (implode (editorpart-nondisp edprt)))) 

; post-message : World -> HandlerResult
; returns a HandlerResult which has a Message posted and a modified World
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (post-message (make-world
                  "hshah"
                  (list "pdpbot" "DrRacket" "Dan" "PDP" "hshah")
                  (list (make-event (list "james left the chat") "gray"))
                  (make-editor
                   (make-editorpart
                    empty
                    (list "i" "h" " " ":" "h" "a" "h" "s" "h"))
                   (make-editorpart empty empty))))
   (make-package
    (make-world
     "hshah"
     (list "pdpbot" "DrRacket" "Dan" "PDP" "hshah")
     (list
      (make-event (list "< hshah->hshah > hi") "blue")
      (make-event (list "james left the chat") "gray"))
     (make-editor
      (make-editorpart empty empty)
      (make-editorpart empty empty)))
    (list 'private "hshah" " hi"))
   "given a valid with a private message potential")
  (check-equal?
   (post-message (make-world
                  "hshah"
                  (list "pdpbot" "DrRacket" "Dan" "PDP" "hshah")
                  (list (make-event (list "james left the chat") "gray"))
                  (make-editor
                   (make-editorpart
                    empty
                    (list "i" "h" " " "h" "a" "h" "s" "h"))
                   (make-editorpart empty empty))))
   (make-package
    (make-world
     "hshah"
     (list "pdpbot" "DrRacket" "Dan" "PDP" "hshah")
     (list
      (make-event (list "< hshah >hshah hi") "black")
      (make-event (list "james left the chat") "gray"))
     (make-editor
      (make-editorpart empty empty)
      (make-editorpart empty empty)))
    (list 'broadcast "hshah hi"))
   "given a valid with a broadcast message potential"))
; STATEGY : Data Decomposition on w : World
(define (post-message w)
  (if (private-message? (world-editor w)) 
      (make-private-msg-package w (world-name w) (world-editor w))
      (make-broadcast-msg-package w (world-name w) (world-editor w))))

; make-private-msg-package : World UserName Editor -> HandlerResult
; retrurns a HandlerResult with a broadcasting message.
; EXMAPLE : See test cases above.
; STRATEGY : Function Composition
(define (make-private-msg-package w sender ed) 
  (make-package 
   (reset-editor-of-world 
    (add-event-to-world w (make-private-event 
                           sender 
                           ARROW 
                           (get-receiver(explode (editor->string ed))) 
                           (get-message 
                            (reverse (explode (editor->string ed)))))))
   (list 'private (get-receiver (explode (editor->string ed))) 
         (get-message (reverse (explode (editor->string ed))))))) 

; get-receiver : ListOf<1String> -> String
; returns the first word before COLON as you go left to right.
; EXMAPLE : 
(begin-for-test
  (check-equal?
   (get-receiver (explode "hshah: hi"))
   "hshah"
   "given a input with a colon")
  (check-equal?
   (get-receiver (explode "hshah hi"))
   " "
   "given a input with no colon"))
; STRATEGY : Function Composition
(define (get-receiver lo1s)
  (if (has-colon? lo1s)
      (implode (trackback lo1s COLON)) 
      " ")) 

; has-colon? : ListOf<1String> -> Boolean
; returns true if any of the element in lo1s is a COLON.
; EXAMPLE :see test cases above. 
; STRATEGY : Function Composition 
(define (has-colon? lo1s)
  (ormap colon? lo1s))

; colon? : 1String -> Boolean
; returns true if c is a COLON.
; EXAMPLE : refer test cases above.
; STRATEGY : Function Composition
(define (colon? c)
  (string=? c COLON))

; get-message : ListOf<1String> -> String
; returns the part of (implode lo1s) that comes after COLON
; as you go left to right.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (get-message (reverse (explode "hshah: hi")))
   " hi"
   "given a input with a colon")
  (check-equal?
   (get-message (explode "hshah hi"))
   "ih hahsh"
   "given a input with no colon"))
; STRATEGY : Function Composition
(define (get-message lo1s)
  (implode (reverse (trackback lo1s COLON)))) 

; reset-editor-of-world : World -> World
; resets the editor of the current world to initial state.
; Example : refer test cases for key handler.
; STRATEGY : Data Decompostition on w : World
(define (reset-editor-of-world w)
  (make-world (world-name w)
              (world-users w)
              (world-events w)
              INITIAL-EDITOR))

; make-broadcast-msg-package : World UserName Editor -> HandlerResult
; retrurns a HandlerResult with a broadcasting message.
; Example : refer test cases for key handler
; STRATEGY : Function Composition
(define (make-broadcast-msg-package w user ed) 
  (make-package 
   (reset-editor-of-world (add-event-to-world 
                           w 
                           (make-broadcast-event user (editor->string ed))))
   (list 'broadcast (editor->string ed)))) 

; private-message? : Editor -> Boolean
; returns true id if the private message is tried to be sent.
; EXAMPLE : refer test cases for key handler
; STRATEGY : Data Decomposition on ed : editor
(define (private-message? ed )
  (and 
   (<= (string-length 
        (get-receiver (explode (editor->string ed)))) 
       USERNAME-UPPER-LIMIT)
   (>= (string-length 
        (get-receiver (explode (editor->string ed)))) 
       USERNAME-LOWER-LIMIT)
   (number-and-string? 
    (get-receiver (explode (editor->string ed))))))

; number-and-string? : String -> Boolean
; returns true if every 1String in s is either an integer or an alphabet.
; STRATEGY : Function Composition
(define (number-and-string? s)
  (andmap char-number-or-string (explode s)))

; char-number-or-string : 1String -> Boolean
; returns true if c either an integer or an alphabet.
; EXAMPLE :
(begin-for-test
  (check-equal?
   (char-number-or-string "a") #true "given a valid 1string")
  (check-equal?
   (char-number-or-string "1") #true "given a valid number 1string"))
; STRATEGY : Function Composition
(define (char-number-or-string c)
  (or (and (string-ci<=? c "z") (string-ci>=? c "a"))
      (integer? (string->number c))))   

; try-autocomplete : World -> World
; returns a World where the current word written is replaced by the matching 
; UserName if found a match.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (try-autocomplete (make-world
                      "hshah"
                      (list "pdpbot" "Dan" "Ashish" "ChatNotWrkin" "hshah")
                      (list
                       (make-event (list "< ChatNotWrkin >4") "black")
                       (make-event (list "< ChatNotWrkin >") "black"))
                      (make-editor
                       (make-editorpart empty (list "a"))
                       (make-editorpart empty empty)))) 
   (make-world
    "hshah"
    (list "pdpbot" "Dan" "Ashish" "ChatNotWrkin" "hshah")
    (list
     (make-event (list "< ChatNotWrkin >4") "black")
     (make-event (list "< ChatNotWrkin >") "black"))
    (make-editor
     (make-editorpart empty (list "a"))
     (make-editorpart empty empty)))
   "given a world where autocomplete is not possible")
  (check-equal?
   (char-number-or-string "1") #true "given a valid number 1string"))
; STRATEGY : Data Decomposition on w : World
(define (try-autocomplete w)
  (if (sub-string-match? (world-editor w) (world-users w))
      (make-world (world-name w) (world-users w) (world-events w)
                  (autocomplete (world-editor w) (world-users w)))
      w))

; last-word-typed : EditorPart -> String
; WHERE : edprt is the pre of the Editor
; returns the last word typed in the editor.
; EXAMPLE : 
(begin-for-test 
  (check-equal?
   (last-word-typed 
    (make-editorpart empty (list "a" "b" "h" "i")))
   "ihba"
   "given a one word")
  (check-equal?
   (last-word-typed 
    (make-editorpart empty (list "a" "b" "h" "i" " " "k" "i" "d")))
   "ihba"
   "given a two or more words"))
; STRATEGY : Function Composition
(define (last-word-typed edprt)
  (implode (reverse(trackback (reverse(explode (pre-editorpart->string edprt)))
                              SPACE)))) 

; sub-string-match? : Editor ListOf<UserName> -> Boolean
; returns true if there is a substring match of the latest word typed in the 
; editor with the loun.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (sub-string-match? 
    (make-editor
     (make-editorpart empty (list "a" "b" "h" "s"))
     (make-editorpart empty empty))
    (list "pdpbot" "Dan" "Ashish" "ChatNotWrkin" "hshah"))
   #false
   "given a value that is longer than the smallest username"))
; STRATEGY: Function Composition
(define (sub-string-match? ed loun)
  (local (; string-compare : String -> Boolean
          ; returns true if there is a substring match with s.
          ; STRATEGY : Data Decomposition on ed : Editor
          (define (string-compare s)
            (if (> (string-length (last-word-typed (editor-pre ed)))
                   (string-length s))
                #false
                (string=? 
                 (last-word-typed (editor-pre ed)) 
                 (implode
                  (trim-string 
                   (explode s) 
                   (string-length (last-word-typed (editor-pre ed)))))))))  
    (ormap string-compare loun)))  

; trackback : ListOf<1String> 1String -> ListOf<1String>
; returns a List of 1String that has all the 1String till first delimiter.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (trackback (explode "this is good") SPACE)
   (list "t" "h" "i" "s")
   "given a input with multiple words"))
; STRATEGY : Data Decompositon on lo1s : ListOf<1String>
(define (trackback lo1s del)
  (if (or (empty? lo1s) (string=? del (first lo1s)))
      '()
      (cons (first lo1s) (trackback (rest lo1s) del))))

; autocomplete : Editor ListOf<UserName> -> Editor
; returns a modified Editor which has been autocompleted with the UserName.
; EXAMPLE : 
(begin-for-test
  (check-equal?
   (autocomplete 
    (make-editor
     (make-editorpart (list "k" "i" "d" "r" "a" "h") (list " "))
     (make-editorpart
      (list "r")
      (list "s" "h" "a" "h" " " "i" "s" " " "a" "w" "e" "s" "o" "m" "e" 
            " " "p" "r" "o" "g" "r" "a" "m" "e")))
    (list "guessWho" "notDan" "Dan" "ThisPointer" "hackergirl92" "hshah"))
   (make-editor
    (make-editorpart
     (list "k" "i" "d" "r" "a" "h")
     (list "n" "a" "D" " "))
    (make-editorpart
     (list "r")
     (list "s" "h" "a" "h" " " "i" "s" " " "a" "w" "e" "s" "o" "m" "e" 
           " " "p" "r" "o" "g" "r" "a" "m" "e")))
   "given a input that can be autocompleted"))
; STRATEGY : Data Decomposition on ed : Editor
(define (autocomplete ed loun)
  (make-editor (autocomplete-pre-editorpart (editor-pre ed) 
                                            (sort loun string-ci<?))
               (editor-post ed)))

; autocomplete-pre-editorpart : EditorPart ListOf<UserName> -> EditorPart
; WHERE : edprt is the pre of the Editor
; returns the autocompleted edprt with one of the element in loun.
; EXAMPLE : refer test cases above.
; STRATEGY : Data Decomposition on edprt : EditorPart
(define (autocomplete-pre-editorpart edprt loun)
  (make-editorpart (editorpart-nondisp edprt)
                   (autocomplete-editor-pre edprt loun)))

; autocomplete-editor-pre : EditorPart ListOf<UserName> -> ListOf<1String>
; WHERE : edprt is the pre of the Editor
; returns the autocompleted ListOf<1String>.
; EXAMPLE : refer test cases.
; STRATEGY : Data Decomposition on lo1s : ListOf<1String>
(define (autocomplete-editor-pre edprt loun)
  (append 
   (reverse(remaining-string 
            (explode 
             (first (get-matched-username edprt loun)))
            (string-length (last-word-typed edprt))))
   (editorpart-disp edprt))) 

; trim-string : ListOf<1String> Natural -> ListOf<1String>
; returns the truncated list of lo1s at n.
; EXAMPLE : refer test cases.
; STRATEGY : Data Decomposition on lo1s : ListOf<1String>
(define (trim-string lo1s n)
  (cond
    [(zero? n) '()]
    [else (cons (first lo1s) (trim-string (rest lo1s) (sub1 n)))]))

; remaining-string : ListOf<1String> Natural -> ListOf<1String>
; returns the remaining part of lo1s after n.
; EXAMPLE : refer test cases above.
; STRATEGY : Data Decomposition on lo1s : ListOf<1String>
(define (remaining-string lo1s n)
  (cond
    [(zero? n) lo1s]
    [else (remaining-string (rest lo1s) (sub1 n))]))

; get-matched-username : EditorPart ListOf<UserName> -> ListOf<UserName>
; WHERE : edprt is the pre of the Editor
; returns the list of UserName that has matched the incoming string in 
; ListOf<1String> format.
; EXAMPLE :
(begin-for-test 
  (check-equal?
   (get-matched-username(make-editorpart empty (list "i" "h" "s" "A" ))
                        
                        (list "pdpbot" "Dan" "Ashish" "ChatNotWrkin" "hshah"))
   (list "Ashish")
   "given a value that is present in the list of username"))
; STRATEGY : Function Composition 
(define (get-matched-username edprt los)
  (local (; string-compare : String -> Boolean
          ; returns true if there is a substring match with s.
          ; STRATEGY : Data Decomposition on ed : Editor
          (define (string-compare s)
            (if (> (string-length (last-word-typed edprt))
                   (string-length s))
                #false
                (string=? (last-word-typed edprt) 
                          (implode (trim-string 
                                    (explode s) (string-length 
                                                 (last-word-typed edprt))))))))
    (filter string-compare los)))     
