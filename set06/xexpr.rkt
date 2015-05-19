;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xexpr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; importing Packages
(require rackunit)
(require "extras.rkt")
(require 2htdp/batch-io)
(require 2htdp/image)
(define TIME-ON-TASK 25); Time taken to finish the task in hours

; providing the required functions
(provide xexpr-element?)
(provide xexpr-tag)
(provide xexpr-attributes)
(provide xexpr-content)
(provide attribute-value)
(provide get-value)
(provide xexpr-find)
(provide xexpr-depth)
(provide xexpr-elements)

; An Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)
; Represents an XML element.

(define HTML-EMPTY '(html ((lang "en-US")) (body (p) (br) (p) (br))))
(define HTML-WITH-TEXT
  '(html
    ((lang "en-US"))
    (body
     (p "Here is assignment " 5)
     (br)
     (p "This is a " (b "data") " definition."))))
(define IMG '(img ((src "balls.png") (width "100") (height "50"))))
(define LIST1 '(list (list 'lang "en-US")))
(define LI 'lang)

; TEMPLATE: 
; xexpr-fn : Xexpr -> ???
;(define (xexpr-fn xe)
;(local (; parse-loa-lox : ListOf<Any> -> ??? 
;        ; WHERE : ListOf<Any> can be either 
;        ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>. 
;        ; this function parses the (cons ListOf<Attribute> ListOf<Xexpr>)
;        ; or ListOf<Xexpr>
;        ; STRATEGY : Data Decomposition on xe : ListOf<Any>
;         (define (parse-loa-lox xe)
;           (cond
;             [(empty? xe) ...]
;             [(list-of-attributes? (first xe))
;              (...(first xe)...(lox-fn(rest xe))...)]
;             [else ... (lox-fn xe)...]))
;            
;          ; lox-fn : ListOf<Xexpr> -> ???
;          ; parses ListOf<Xexpr>
;          ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr> 
;          (define (lox-fn lox)
;            (cond
;              [(empty? lox) ...]
;              [else (...(rest lox)...(xexpr-fn (first lox))...)])))
;    ; — IN —
;    (cond
;      [(symbol? xe) ...]
;      [(string? xe) ...]
;      [(number? xe) ...]
;      [(empty? xe) ...]
;      [(symbol? (first xe)) (parse-loa-lox (rest xe))])))


; A Tag is a Symbol, representing the name of an XML element.
(define H1 'h1)
(define P 'p)
(define BR 'br)
 
; An Attribute is a (list AttrName AttrValue)
; representing an attribute name-value pair in an XML element.
(define HREF-NEU (list 'href "http://northeastern.edu"))
(define IMG-SRC (list 'src "ball.gif"))

; An AttrName is a Symbol, 
; representing the name of an XML attribute.
; An AttrValue is a String, 
; representing the value associated with an XML attribute.

; xexpr-element? : Any -> Boolean
; Returns true if the argument is a valid XML element.
; EXAMPLE :
(begin-for-test (check-equal? (xexpr-element? HTML-WITH-TEXT) #true 
                              "the input case has a nested X-expressions 
                               with attributes"))
(begin-for-test (check-equal? (xexpr-element? HTML-WITH-TEXT) #true 
                              "the input case has a nested X-expressions 
                               with attributes"))
(begin-for-test (check-equal? (xexpr-element? '())
                              #t
                              "The input case is empty"))
(begin-for-test (check-equal? (xexpr-element? '( list '*))
                              #t
                              "The list doesnt start with a tag"))
(begin-for-test (check-equal? (xexpr-element? (list '(bold "hello")))
                              #f
                              "The input is a ListOf<Attribute>"))
; STRATEGY : Function Composition
(define (xexpr-element? xe)
  (local (; check-first-symbol : Any -> Boolean
          ; checks if the first of the input list is symbol
          ; STRATEGY : Function Composition 
          (define (check-first-symbol xe)
            (if (symbol? (first xe)) (list-xexpr-element? (rest xe)) #false))
          ; list-xexpr-element? : ListOf<Any> -> Boolean
          ; WHERE : ListOf<Any> can be either 
          ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>.
          ; this function parses the (cons ListOf<Attribute> ListOf<Xexpr>)
          ; or ListOf<Xexpr>?, and returns false if invalid Xexpr
          ; STRATEGY : Data Decomposition on xe : ListOf<Any>
          (define (list-xexpr-element? xe)
            (cond
              [(empty? xe) #true]
              [(list-of-attributes? (first xe))
               (and (andmap attribute? (first xe)) 
                               (andmap xexpr-element? (rest xe)))]
              [else (andmap xexpr-element? xe)])))
    ; — IN —
    (if (or (symbol? xe)(string? xe)(number? xe)(empty? xe)) #true
        (check-first-symbol xe))))

; attribute? : ListOf<Any> -> Boolean
; consumes any data class and returns true if it belongs to Attribute 
; data class.
; EXAMPLE :
(begin-for-test (check-equal? (attribute? '(bold 5)) #false
                              "an invalid attribute"))
(begin-for-test (check-equal? (attribute? '(bold "bright")) #true
                              "an valid attribute"))
(begin-for-test (check-equal? (attribute? (list '* '1 '2))
                              #f
                              "an invalid attribute"))
(begin-for-test (check-equal? (attribute? (list '(bold "bright")))
                              #f
                              "an invalid attribute"))
; STRATEGY : Data Decomposition on a : ListOf<Any>
(define (attribute? a)
  (and (symbol? (first a)) (string? (second a)) (= 2 (length a))))

; list-of-attributes: ListOf<Any> -> Boolean
; returns true if the given value a list of attributes else false
; EXAMPLE :
(begin-for-test (check-equal? (list-of-attributes? '())
                              #t
                              "the list is empty so true"))
(begin-for-test (check-equal? (list-of-attributes? (list "hello"))
                              #f
                              "the list is a string so false"))
(begin-for-test (check-equal? (list-of-attributes? '1)
                              #f
                              "the list has the number so false"))
(begin-for-test (check-equal? (list-of-attributes? (list (list 'bold "hello")))
                              #t
                              "the list is a valid xexpression"))
; STRATEGY : Data Decomposition on x : ListOf<Any>
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [(string? x) #false]
    [(number? x) #false]
    [(symbol? x) #false]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))
 
; xexpr-tag : Xexpr -> Maybe<Tag>
; Returns the tag of element xe.
; EXAMPLE :
(begin-for-test(check-equal? (xexpr-tag HTML-WITH-TEXT) 'html
                              "input expression with 'html as first tag"))
(begin-for-test(check-equal? (xexpr-tag 'html) #false
                              "input expression with just a tag"))
(begin-for-test(check-equal? (xexpr-tag "web") #false
                              "input expression with just a string"))
(begin-for-test(check-equal? (xexpr-tag 5) #false
                              "input expression with just a number"))
(begin-for-test(check-equal? (xexpr-tag empty) #false
                              "input expression with just an empty"))
; STRATEGY : Data Decomposition on xe : Xexpr
(define (xexpr-tag xe)
    (cond
      [(symbol? xe) #false]
      [(string? xe) #false]
      [(number? xe) #false]
      [(empty? xe) #false]
      [(symbol? (first xe)) (first xe)]))
     
; xexpr-attributes : Xexpr -> ListOf<Attribute>
; Returns the attributes of the given XML element.
; EXAMPLE : 
(begin-for-test (check-equal? (xexpr-attributes HTML-WITH-TEXT)
                              (list (list 'lang "en-US"))
                              "the input has an attribute of the X-expression"))
(begin-for-test (check-equal? (xexpr-attributes '())
                             #f
                             "Since the xexpr is empty we get a false"))
(begin-for-test (check-equal? (xexpr-attributes "hello")
                             #f
                             "Since the xexpr is a string we get a false"))
(begin-for-test (check-equal? (xexpr-attributes '1) 
                              #f
                              "Since the xexpr is a number its false"))
(begin-for-test (check-equal? (xexpr-attributes '*)
                              #f
                              "Since the xexpr is a symbol its false"))

; STRATEGY : Data Decomposition on xe : Xexpr
(define (xexpr-attributes xe)
   (local (; list-xexpr-element? : ListOf<Any> -> ListOf<Attribute> 
           ; WHERE : ListOf<Any> can be either 
           ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>.
           ; returns list of Attribute if present after parsing
           ; input. retrurns empty if no Attribute present. 
           ; STRATEGY : Data Decomposition on xe : ListOf<Any>
          (define (list-xexpr-element? xe)
            (cond
              [(empty? xe) '()]
              [(list-of-attributes? (first xe)) (first xe)])))
    ; — IN —
    (cond
      [(symbol? xe) #false]
      [(string? xe) #false]
      [(number? xe) #false]
      [(empty? xe) #false]
      [(symbol? (first xe)) (list-xexpr-element? (rest xe))])))

; xexpr-content : Xexpr -> ListOf<Xexpr>
; Extracts the content of an XML element.
; An element's tag and attributes are not part of its content.
; EXAMPLE : 
(begin-for-test (check-equal? (xexpr-content HTML-EMPTY)
                              (list (list 'body (list 'p) (list 'br) (list 'p)
                                          (list 'br)))
                              "input expression with no text paras in given"))
(begin-for-test (check-equal? (xexpr-content '())
                              #f
                            "Since the xexpr is empty the result is a false"))
(begin-for-test (check-equal? (xexpr-content "hello")
                              #f
                           "Since the xexpr is a string the result is a false"))
(begin-for-test (check-equal? (xexpr-content '5)
                              #f
                          "Since the xexpr is a number the result is a false"))
(begin-for-test (check-equal? (xexpr-content '*)
                              #f
                           "Since the xexpr is a symbol the result is a false"))
(begin-for-test (check-equal? (xexpr-content (list 'bold "hello"))
                              "hello"
                              "The list is not a nested one"))
; STRATEGY : Data Decomposition on xe : Xexpr
(define (xexpr-content xe)
  (local (; list-xexpr-element? ListOf<Any> -> ListOf<Xexpr> 
          ; WHERE : ListOf<Any> can be either 
          ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>.
          ; returns the ListOf<Xexpr> if present in the input.
          ; STRATEGY : Data Decomposition on xe : ListOf<Any>
          (define (list-xexpr-element? xe)
            (cond
              [(empty? xe) '()]
              [(list-of-attributes? (first xe)) (rest xe)]
              [else (first xe)])))
    ; — IN —
    (cond
      [(symbol? xe) #false]
      [(string? xe) #false]
      [(number? xe) #false]
      [(empty? xe) #false]
      [(symbol? (first xe)) (list-xexpr-element? (rest xe))])))
 
; attribute-value : ListOf<Attribute> AttrName -> Maybe<AttrValue>
; Returns the value of first attribute in loa named aname.
; Returns #f if no attribute in loa has the name aname.
; EXAMPLE :
(begin-for-test (check-equal? (attribute-value '() empty)
                              #f
                              "Since the LoA is empty the result is a false"))
(begin-for-test (check-equal? (attribute-value (list (list 'lang "en-US")) LI)
                              '"en-US"
                              "The LOA has an attribute names LI"))
(begin-for-test (check-equal? (attribute-value (list (list 'lang "en-US"))
                                               HTML-WITH-TEXT)
                              #f
                              "No Attribute of the same name"))
;STRATEGY : Data Decomposition on loa : ListOf<Attribute>
(define (attribute-value loa aname)
  (cond
    [(empty? loa) #false]
    [else (if (attribute-name-match? (first loa) aname)
              (get-attribute-value (first loa)) 
              (attribute-value (rest loa) aname))]))

; attribute-name-match? Attribute AttrName -> Boolean
; returns true if the given AttrName matches the AttrName of the given Attribute
; EXAMPLE :
(begin-for-test (check-equal? (attribute-name-match? (list '()) empty)
                              #t
                              "Name match"))
(begin-for-test (check-equal? (attribute-name-match?
                               '(list (list 'lang "en-US")) LI)
                              #f
                              "The name doesnt match"))
; STRATEGY : Data Decompsoition on a : Attribute
(define (attribute-name-match? a aname)
  (eq? (first a) aname))

; get-attribute-value : Attribute -> AttrValue
; returns the attribute value of the given Attribute
; EXAMPLE :
(begin-for-test (check-equal? (get-attribute-value '(list (list 'lang "en-US")))
                              (list
                               'list
                               (list 'quote 'lang)
                               "en-US")
                              "Extracts the second value"))
(begin-for-test (check-equal? (get-attribute-value (list '* '1 '2))
                              1
                              "The second value is extracted"))                
; STRATEGY : Data Decomposition on a : Attribute
(define (get-attribute-value a)
  (second a))
 
; get-value : Xexpr AttrName -> Maybe<AttrValue>
; Returns the value of the first attribute in xe named aname.
; EXAMPLE : 
(begin-for-test (check-equal? (get-value HTML-WITH-TEXT 'lang) "en-US"
                              "input given with atleast one attribute"))
(begin-for-test (check-equal? (get-value empty '())
                              #f
                              "Since the xexpr is empty it returns empty"))
(begin-for-test (check-equal? (get-value HTML-WITH-TEXT 'html)
                             #f
                             "Since the xexpr is a string"))
(begin-for-test (check-equal? (get-value HTML-WITH-TEXT '2)
                              #f
                              "The xexpr is a number"))
(begin-for-test (check-equal? (get-value  HTML-WITH-TEXT "hello")
                              #f
                              "The xexpr is a string"))
(begin-for-test  (check-equal? (get-value 
                               '(body
                                 (p ((bold "dark")) "Here is assignment " 5)
                                 (br)
                                 (p 
                                  ((bold "light")) "This is a " 
                                  (b ((bold "bright")) "data") " definition.")) 
                               'bold)
                               "dark"))
(begin-for-test (check-equal? (get-value HTML-WITH-TEXT '())
                              #f
                              "Empty xexpr"))
(begin-for-test (check-equal? (get-value 'text '())
                              #f
                              "symbol xexpr"))
; STRATEGY : Data Decomposition on xe : Xexpr
(define (get-value xe aname)
    (local (; list-xexpr-element? : ListOf<Any> -> MayBe<AttrValue> 
            ; WHERE : ListOf<Any> can be either 
            ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>
            ; this function parses the input and returns the AttrValue for the
            ; corrresponding AttrName if present else returns false.
            ; STRATEGY : Data Decomposition on xe : ListOf<Any>
            (define (list-xexpr-element? xe)
              (cond
                [(empty? xe) #false]
                [(list-of-attributes? (first xe))
                 (if (eq? #false (attribute-value (first xe) aname))
                     (get-value-lox (rest xe) aname) 
                     (attribute-value (first xe) aname))]
                [else (get-value-lox xe aname)]))
            ; get-value-lox : ListOf<Xexpr> AttrName -> Maybe<AttrValue>
            ; returns the AttrValue if the corresponding AttrName is present 
            ; else returns a #false.
            ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr> 
            (define (get-value-lox lox aname)
              (cond
                [(empty? lox) #false]
                [else (if (eq? #false (get-value (first lox) aname))
                          (get-value-lox (rest lox) aname)
                          (get-value (first lox) aname))])))
      ; — IN —
      (cond
        [(symbol? xe) #false]
        [(string? xe) #false]
        [(number? xe) #false]
        [(empty? xe) #false]
        [(symbol? (first xe)) (list-xexpr-element? (rest xe))])))
 
; xexpr-find : Xexpr AttrName -> ListOf<AttrValue>
; Searches xe and nested elements in xe for attributes named aname
; and returns all the values for these attributes.
; EXAMPLE :
(begin-for-test (check-equal? (xexpr-find HTML-WITH-TEXT 'lang) 
                              (list "en-US")
                             "input contains a expression with one attribute"))
(begin-for-test (check-equal? (xexpr-find  
                               '(body
                                 (p ((bold "dark")) "Here is assignment " 5)
                                 (br)
                                 (p 
                                  ((bold "light")) "This is a " 
                                  (b ((bold "bright")) "data") " definition.")) 
                               'bold)
                              (list "light" "bright" "dark")
                             "input contains a expression with three 
                              matching attribute"))
(begin-for-test (check-equal? (xexpr-find HTML-WITH-TEXT empty)
                              empty
                              "The valid xexpr"))
; STRATEGY : Data Decomposition on xe : Xexpr
(define (xexpr-find xe aname) 
  (local (; list-xexpr-element? : ListOf<Any> -> ListOf<AttrValue> 
          ; WHERE : ListOf<Any> can be either 
          ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>.
          ; this function parses the input 
          ; and returns ListOf<AttrValue> matched with AttrName.
          ; STRATEGY : Data Decomposition on xe : ListOf<Any>
          (define (list-xexpr-element? xe)
            (cond
              [(empty? xe) '()]
              [(list-of-attributes? (first xe))
               (if (eq? #false (attribute-value (first xe) aname))
                   (get-value-lox (rest xe) aname) 
                   (cons (attribute-value (first xe) aname) 
                         (get-value-lox (rest xe) aname)))]
              [else (get-value-lox xe aname)])) 
          ; get-value-lox : ListOf<Xexpr> AttrName -> ListOf<AttrValue>
          ; returns the list of AttrValue from all the Attributes present in 
          ; the ListOf<Xexpr>. 
          ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr>
          (define (get-value-lox lox aname)
            (cond
              [(empty? lox) '()]
              [else (append (get-value-lox (rest lox) aname)
                        (xexpr-find (first lox) aname))]
              )))
    ; — IN —
    (cond
      [(symbol? xe) '()]
      [(string? xe) '()]
      [(number? xe) '()]
      [(empty? xe) '()]
      [(symbol? (first xe)) (list-xexpr-element? (rest xe))]
      [else '()])))
 
; xexpr-depth : Xexpr -> Natural
; Returns the depth of the deepest nested Xexpr. 
; An Xexpr with no nested elements has depth 0.
; EXAMPLE :
(begin-for-test (check-equal? (xexpr-depth HTML-WITH-TEXT)
                4
                "The valid depth"))
(begin-for-test (check-equal? (xexpr-depth '())
                             0
                             "No depth since the list is empty"))
(begin-for-test (check-equal? (xexpr-depth 'text)
                             0
                             "input with a symbol"))
;STRATEGY : Data Decomposition on xe : Xexpr 
(define (xexpr-depth xe)
    (local (; list-xexpr-element? ListOf<Any> -> Number 
            ; WHERE : ListOf<Any> can be either 
            ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>
            ; this function parses the input and finds the depth.
            ; STRATEGY : Data Decomposition on xe : ListOf<Any>
            (define (list-xexpr-element? xe)
              (cond
                [(empty? xe) 0]
                [(list-of-attributes? (first xe)) 
                 (get-max-depth-lox (rest xe))]
                [else (get-max-depth-lox xe)]))
            ; get-max-depth-lox : ListOf<Xexpr> -> Number
            ; returns the depth of the most deep Xexpr in the list
            ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr>
            (define (get-max-depth-lox lox)
              (cond
              [(empty? lox) 0]
              [else (max (xexpr-depth (first lox)) 
                            (get-max-depth-lox (rest lox))
                            )])))
      ; — IN —
      (cond
        [(symbol? xe) 0]
        [(string? xe) 0]
        [(number? xe) 0]
        [(empty? xe) 0]
        [(symbol? (first xe)) 
         (+ 1 (list-xexpr-element? (rest xe)))])))

; xexpr-elements : Xexpr Tag -> ListOf<Xexpr>
; Returns a list of all elements in xe whose tag is t.
; An element with tag t may be nested inside another element with 
; the same tag and this function returns both.
(begin-for-test (xexpr-elements HTML-WITH-TEXT 'p) 
                (list
                 (list 'p "Here is assignment " 5)
                 (list 'p "This is a " (list 'b "data") " definition."))
                "given an expression with 'p tag represting at 
                 two different places")
; STRATEGY : Data Decomposition on xe : Xexpr
(define (xexpr-elements xe t) 
      (local (; list-xexpr-element? : ListOf<Any> Tag -> ListOf<Xexpr> 
              ; WHERE : ListOf<Any> can be either 
              ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>
              ; this function parses the input and returns a ListOf<Xexpr>
              ; which starts with a input tag.
              ; STRATEGY : Data Decomposition on xe : ListOf<Any>
              (define (list-xexpr-element? xe t)
                (cond
                  [(empty? xe) '()]
                  [(list-of-attributes? (first xe)) 
                   (get-xexpr-lox (rest xe) t)]
                  [else (get-xexpr-lox xe t)]))
              ; get-xexpr-lox : ListOf<Xexpr> Tag -> ListOf<Xexpr>
              ; returns the list of matched Xexpr from the given ListOf<Xexpr>
              ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr>
              (define (get-xexpr-lox lox t)
                (cond
                  [(empty? lox) '()]
                  [else (append (xexpr-elements (first lox) t) 
                                (get-xexpr-lox (rest lox) t)
                                )])))
        ; — IN —
        (cond
          [(symbol? xe) '()]
          [(string? xe) '()]
          [(number? xe) '()]
          [(empty? xe) '()]
          [(symbol? (first xe)) (if (eq? (first xe) t)
                                 (cons xe (list-xexpr-element? (rest xe) t))
                                 (list-xexpr-element? (rest xe) t))])))

;----------------------------EndOfRequiredFunctions-----------------------------
;----------------------------StartOfSecondPartOfProblem-------------------------
;Subsequent part fetches a pic of the string given in the literal SEARCH-NAME 
;below. It fetches the first pic that it finds in the wikipedia database that  
;is tagged with the name given.

(define URL-PREFIX "https://en.wikipedia.org")
(define WIKI "/wiki/")
(define HTTP "https:")
(define SEARCH-NAME "Matthias_Felleisen")
(define SEARCH-URL (string-append URL-PREFIX WIKI SEARCH-NAME))
(define IMAGE-NOT-FOUND (text "IMAGE-NOT-FOUND" 10 "black"))
(define LOSRC 
  (xexpr-find 
   (read-xexpr/web SEARCH-URL) 'src))

(define LENGTH-OF-JPEG 4)

; get-rel-pic-url : ListOf<AttrValue> -> String
; retuns a relative url of the first pic from the wikipedia XML that has 
; SEARCH-NAME tagged.
(begin-for-test (check-equal? (get-rel-pic-url 
                 (list
                  "//www.googletagmanager.com/ns.html?id=GTM-WGQLLJ"
                  "http://www.ccs.neu.edu/wp-content/themes/northeastern-ccs
                  /assets/javascripts/build/script.min.js?ver=2.0.13"))
                ""
                "input with no .jpg"))
; STRATEGY : Data Decomposition on losrc : ListOf<AttrValue>
(define (get-rel-pic-url losrc)
  (cond
    [(empty? losrc) ""]
    [else (if (string-has-jpg (first losrc))
              (first losrc) (get-rel-pic-url (rest losrc)))]))

; string-has-jpg : AttrValue -> Boolean
; returns true if the given AttrValue ends with a .jpg
(begin-for-test (check-equal? (string-has-jpg "ab") #false 
                                       "a string smaller than 4 is given"))
; STRATEGY : Data Decomposition on s : AttrValue
(define (string-has-jpg s)
     (if (< (string-length s) LENGTH-OF-JPEG) #false
      (string=? ".jpg" (substring s ( - (string-length s) LENGTH-OF-JPEG)))))

; display-pic : ListOf<AttrValue> -> Image
; returns an Image that is fetched from one of the AttrValue present in the list
(begin-for-test (image? (display-pic LOSRC)))
; STRATEGY : Function Composition
(define (display-pic losrc)
  (bitmap/url (string-append HTTP (get-rel-pic-url losrc))))

;(display-pic LOSRC)

;----------------------------EndOfSecondPartOfProblem---------------------------
;----------------------------StartOfAlternateDataDefinition---------------------

;Refining the given data definition by adding Xwords in the definition


; An Xexpr is one of:
; - Symbol
; - XWord
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)
; Represents an XML element.


; A XWord is '(word ((text String))).
;Everything else is same as the previous data definition.

;The change in template is as follows:

; TEMPLATE: 
; xexpr-fn : Xexpr -> ???
;(define (xexpr-fn xe)
;(local (; parse-loa-lox : ListOf<Any> -> ???
;        ; WHERE : ListOf<Any> can be either 
;        ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>
;        ; this function parses the (cons ListOf<Attribute> ListOf<Xexpr>)
;        ; or ListOf<Xexpr>
;        ; STRATEGY : Data Decomposition on xe : ListOf<Any>
;          (define (parse-loa-lox xe)
;            (cond
;              [(empty? xe) ...]
;              [(list-of-attributes? (first xe))
;               (...(first xe)...(rest xe)...)]
;              [else ... (xexpr-fn (rest xe))...]))
;            
;          ; lox-fn : ListOf<Xexpr> -> ???
;          ; parses ListOf<Xexpr>
;          ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr> 
;          (define (lox-fn lox)
;            (cond
;              [(empty? lox) ...]
;              [else (...(rest lox)...(xexpr-fn (first lox))...)])))
;    ; — IN —
;    (cond
;      [(symbol? xe) ...]
;      [(xword? xe) ...]
;      [(number? xe) ...]
;      [(empty? xe) ...]
;      [(symbol? (first xe)) (parse-loa-lox (rest xe))]
;      [else ...])))


;; xword? : Any -> Boolean
;; takes Any data and checks whether it is a valid XWord or not, 
;; returns true if valid and false if not
;; EXAMPLE :
;(begin-for-test (check-equal? (xword? '(word ((text "String"))))
;                              #true "a invalid input is given"))
;(begin-for-test (check-equal? (xword? '(word ((text string))))
;                              #false "a invalid input is given"))
;(begin-for-test (check-equal? (xword? empty)
;                              #false "an empty is given"))
;(begin-for-test (check-equal? (xword? "string")
;                              #false "an invalid input is given"))
;; STRATEGY  : Function Composition
;(define (xword? xe)
;  (if (and (not(empty? xe))
;        (eq? 'word (first xe))) (validate-xword (second xe)) #false))
;
;; validate-xword : ListOf<Any> -> Boolean
;; takes Any data checks whether it is a valid content if a XWord,
;; ie (second '(word ((text String)))) is valid. returns true if it is
;; else returns false.
;; EXAMPLE : 
;(begin-for-test (check-equal? (validate-xword 
;                               (list (list 'text "String")))
;                              #true "a valid input is given"))
;(begin-for-test (check-equal? (validate-xword 
;                               (list (list (list 'text 'string))))
;                              #false "a invalid input is given"))
;; STRATEGY : Data Decomposition on loxw : Any
;(define (validate-xword loxw)
;  (cond
;    [(empty? (first loxw)) #false]
;    [(andmap valid-word? loxw) #true]
;    [else #false]))
;
;; valid-word? : ListOf<Any> -> Boolean 
;; returns true if the input list is of the form (list 'text String) else
;; returns false
;; EXAMPLE: 
;(begin-for-test (check-equal? (valid-word? (list 'text "String")) #true
;                              "a valid input is given"))
;(begin-for-test (check-equal? (valid-word? (list 'text 'string)) #false
;                              "a invalid input is given"))
;; STRATEGY : Data Decomposition on word : ListOf<Any>
;(define (valid-word? word)
;  (and (eq? 'text (first word)) (string? (second word))))

; every function using this TEMPLATE will have the above changes.
; Following are two functions to show the changes

;(define (xexpr-find xe aname) 
;  (local (; list-xexpr-element? ListOf<Any> -> ListOf<AttrValue>
;          ; WHERE : ListOf<Any> can be either 
;          ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>
;          ; this function parses the (cons ListOf<Attribute> ListOf<Xexpr>)
;          ; or ListOf<Xexpr>
;          ; STRATEGY : Data Decomposition on xe : ListOf<Any>
;          (define (list-xexpr-element? xe)
;            (cond
;              [(empty? xe) '()]
;              [(list-of-attributes? (first xe))
;               (if (eq? #false (attribute-value (first xe) aname))
;                   (get-value-lox (rest xe) aname) 
;                   (cons (attribute-value (first xe) aname) 
;                         (get-value-lox (rest xe) aname)))]
;              [else (get-value-lox xe aname)])) 
;          ; get-value-lox : ListOf<Xexpr> AttrName -> ListOf<AttrValue>
;          ; returns the list of AttrValue from all the Attributes present in 
;          ; the ListOf<Xexpr>. 
;          ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr>
;          (define (get-value-lox lox aname)
;            (cond
;              [(empty? lox) '()]
;              [else (append (get-value-lox (rest lox) aname)
;                        (xexpr-find (first lox) aname))]
;              )))
;    ; — IN —
;    (cond
;      [(symbol? xe) '()]
;      [(xword? xe) '()]
;      [(number? xe) '()]
;      [(empty? xe) '()]
;      [(symbol? (first xe)) (list-xexpr-element? (rest xe))])))

;(define (xexpr-elements xe t) 
;      (local (; list-xexpr-element? ListOf<Any> -> ListOf<Xexpr> 
;              ; WHERE : ListOf<Any> can be either 
;              ; (cons ListOf<Attribute> ListOf<Xexpr>) or ListOf<Xexpr>
;              ; this function parses the (cons ListOf<Attribute> ListOf<Xexpr>)
;              ; ListOf<Xexpr>
;              ; STRATEGY : Data Decomposition on xe : ListOf<Any>
;              (define (list-xexpr-element? xe t)
;                (cond
;                  [(empty? xe) '()]
;                  [(list-of-attributes? (first xe)) 
;                   (get-value-lox (rest xe) t)]
;                  [else (get-value-lox xe t)]))
;          ; get-value-lox : ListOf<Xexpr> -> ListOf<Xexpr>
;          ; returns the list of AttrValue from all the Attributes present in 
;          ; the ListOf<Xexpr>. 
;          ; STRATEGY : Data Decomposition on lox : ListOf<Xexpr>
;              (define (get-value-lox lox t)
;                (cond
;                  [(empty? lox) '()]
;                  [else (append (xexpr-elements (first lox) t) 
;                                (get-value-lox (rest lox) t)
;                                )])))
;        ; — IN —
;        (cond
;          [(symbol? xe) '()]
;          [(xword? xe) '()]
;          [(number? xe) '()]
;          [(empty? xe) '()]
;          [(symbol? (first xe)) (if (eq? (first xe) t)
;                                 (cons xe (list-xexpr-element? (rest xe) t))
;                                 (list-xexpr-element? (rest xe) t))])))

; As we can see the only difference in the previous functions on previous data 
; definition and the new is that the clause checking (string? xe) is 
; replaced by (xword? xe).

;----------------------------EndOfAlternateDataDefinition---------------------