####Problem Description

These exercises are derived from exercises 303-313 in the textbook.

 - Here are data definitions for Xexpr and Attribute. They are equivalent to Xexpr.v3 and Attribute from the textbook, except slightly modified with aliases and ourListOf<X> syntax.

```
; An Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)```

```
; Represents an XML element.
(define HTML-EMPTY 
	'(html ((lang "en-US")) (body (p) (br) (p) (br))))
(define HTML-WITH-TEXT
  '(html ((lang "en-US")) 
	(body (p "Here is assignment " 5) (br) (p "This is a " (b "data") " definition.")))) (define IMG '(img ((src "balls.png") (width "100") (height "50"))))
```

```; A Tag is a Symbol, representing the name of an XML element. 
(define H1 'h1) 
(define P 'p) 
(define BR 'br)
```

```
; An Attribute is a (list AttrName AttrValue)
; representing an attribute name-value pair in an XML element.
(define HREF-NEU (list 'href "http://northeastern.edu"))
(define IMG-SRC (list 'src "ball.gif"))
```

```
; An AttrName is a Symbol, 
; representing the name of an XML attribute.; An AttrValue is a String, 
; representing the value associated with an XML attribute.
```

Complete the Data Design with templates and other requirements.  
Define the functions below. Some of them may be definable in terms of the others. (And some may already be given to you in the textbook.)  

```
; xexpr-element? : Any -> Boolean
; Returns true of x is a valid XML element.
(define (xexpr-element? x) ...)```


``` 
; xexpr-tag : Xexpr -> Maybe<Tag>
; Returns the tag of element xe.
(define (xexpr-tag xe) ...)```

 
```
; xexpr-attributes : Xexpr -> ListOf<Attribute>
; Returns the attributes of the given XML element
(define (xexpr-attributes xe) ...)```

``` 
; xexpr-content : Xexpr -> ListOf<Xexpr>
; Extracts the content of an XML element.
; An element's tag and attributes are not part of its content
(define (xexpr-content xe) ...)```

``` 
; attribute-value : ListOf<Attribute> AttrName -> Maybe<AttrValue>
; Returns the value of first attribute in loa named aname.
; Returns #f if no attribute in loa has the name aname.
(define (attribute-value loa aname) ...)```

 
```
; get-value : Xexpr AttrName -> Maybe<AttrValue>
; Returns the value of the first attribute in xe named aname.
(define (get-value xe aname) ...)```

```
; xexpr-find : Xexpr AttrName -> ListOf<AttrValue>
; Searches xe and nested elements in xe for attributes named aname
; and returns all the values for these attributes.(define (xexpr-find xe aname) ...)```

```
; xexpr-depth : Xexpr -> Natural
; Returns the depth of the deepest nested Xexpr. 
; An Xexpr with no nested elements has depth 0.(define (xexpr-depth xe) ...)```

``` 
; xexpr-elements : Xexpr Tag -> ListOf<Xexpr>
; Returns a list of all elements in xe whose tag is t.
; An element with tag t may be nested inside another element with ; the same tag and this function returns both.
(define (xexpr-elements xe t) ...)```

Finally, use the functions you wrote, and the read-xexpr/web function from the2htdp/batch-io library to write a function that consumes a string url and scrapes and returns some interesting information from the web. (Use the Google Finance example from the textbook if you are struggling to come up with ideas.) This function should false if there is a problem retrieving content from the web. We will check what you did during your code walk.

