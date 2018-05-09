#lang scheme 
;2015400123
(define (RAILWAY-CONNECTION location) ; This and all the functions in 3.1 uses tail recursion.
  (define (iter list) ; They define a driver function inside them and call that recursive function only one with LOCATIONS or TRAVELERS
    (cond ; If elseif else statement here
      [(eqv? (length list) 0) '()] ; base condition, return '() length of the list is zero i.e we could not find location in LOCATIONS
      [(eqv? (car (list-ref list 0)) location) (caddr (list-ref list 0))] ; If we find it return third element in that list.
      [else (iter (rest list))] ; else recursively call iter again with the rest of the list
      )
    )
  (iter LOCATIONS)) ;here one call of iter.
(define (ACCOMMODATION-COST location) 
  (define (iter list)
    (cond
      [(eqv? (length list) 0) 0] ; only difference is that it returns 0 here.
      [(eqv? (car (list-ref list 0)) location) (cadr (list-ref list 0))]
      [else (iter (rest list))]
      )
    )
  (iter LOCATIONS)
  )
(define (INTERESTED-CITIES traveler)
  (define (iter list)
    (cond
      [(eqv? (length list) 0) '()] ;same as railway-connection
      [(eqv? (caar list) traveler) (cadar list)]
      [else (iter (rest list))]
      )
    )
  (iter TRAVELERS)
  )
(define (INTERESTED-ACTIVITIES traveler)
  (define (iter list)
    (cond
      [(eqv? (length list) 0) '()] ; same as railway-connection
      [(eqv? (caar list) traveler) (caddr (car list))]
      [else (iter (rest list))]
      )
    )
  (iter TRAVELERS)
  )
(define (HOME traveler)
  (define (iter list)
    (cond
      [(eqv? (length list) 0) ""] ; only difference is that this returns empty string 
      [(eqv? (caar list) traveler) (cadddr (car list))]
      [else (iter (rest list))]
      )
    )
  (iter TRAVELERS)
  )
(define (TRAVELER-FROM location)
  (define (iter lst formedlist) ; This method constructs a list which is formedlist.
    (cond
      [(eqv? (length lst) 0) formedlist] ; If list is finished return formedlist until now.
      [(eqv? (cadddr (car lst)) location) (append (list (caar lst)) (iter (rest lst) formedlist))] ; If we found a entry with given location then we append it to the result of the rest of the iteration
      [else (iter (rest lst) formedlist)] ; If we cant find in this iteration continue with the rest of the list.
      )
    )
  (iter TRAVELERS '()) ; We need to traverse TRAVELERS to find people's hometowns.
  )
(define (INTERESTED-IN-CITY location)
  (define (iter lst formedlist)
    (define (iter2 lst bool) ; Recursive func for iterating interested in cities list of a traveler.
      (cond ; start value of bool is false.
        [(eqv? (length lst) 0) bool];  If the list is empty return the same value.
        [(eqv? (car lst) location) #t] ; If the list contains location in the first location, return true.
        [else (iter2 (rest lst) bool)] ; else continue iterating.
        )
      )
    (cond
      [(eqv? (length lst) 0) formedlist] ; again if the elements in TRAVELERS is finished return formedlist so far.
      [(eqv? (iter2 (cadar lst) #f) #t) (append (iter (rest lst) formedlist) (list (caar lst)))   ] ; If location is in the list (cadar lst) i.e interested cities list of a traveler
      [else (iter (rest lst) formedlist)] ; cont of above- Then return a list which is get by appending two lists, the list only contains the traveler name
      );cont of above- and the list formed returned by the rest of the recursion.
    )
  (iter TRAVELERS '()) ;
  
  )
(define (INTERESTED-IN-ACTIVITY activity) ; instead of location have written activity
  (define (iter lst formedlist)
    (define (iter2 lst bool) 
      (cond 
        [(eqv? (length lst) 0) bool]
        [(eqv? (car lst) activity) #t] 
        [else (iter2 (rest lst) bool)] 
        )
      )
    (cond
      [(eqv? (length lst) 0) formedlist]
      [(eqv? (iter2 (caddar lst) #f) #t) (append (iter (rest lst) formedlist) (list (caar lst)))   ] ; only difference with INTERESTED-IN-CITY:
      [else (iter (rest lst) formedlist)] ; The line above has caddar lst insteadd of cadar lst.
      )
    )
  (iter TRAVELERS '())
  
  )


(define (isValidTra traveler) ; This checks if there exist a traveler "traveler" in TRAVELERS.
  (define (isIn lst bool) (cond
                           [(eqv? (length lst) 0) #f] 
                           [(eqv? (caar lst) traveler) #t]
                           [else (isIn (rest lst) bool)]
                          ))


(isIn TRAVELERS #f)

 
  )
(define (isValidLoc location) ; This checks if there exist a location "location" in LOCATIONS.
  (define (isIn lst bool) (cond
                           [(eqv? (length lst) 0) #f] 
                           [(eqv? (caar lst) location) #t]
                           [else (isIn (rest lst) bool)]
                          ))
(

isIn LOCATIONS #f

 )
  )
(define (ACTIVITIES-IN loc) ; Returns the list of activities in the location loc
  (define (iter list)
    (cond
      [(eqv? (length list) 0) '()] 
      [(eqv? (caar list) loc) (last (car list))]
      [else (iter (rest list))]
      )
    )
  (iter LOCATIONS)
  )

(define (ACCOMMODATION-EXPENSES traveler location)
  
  (

 cond[(or (false? (isValidLoc location)) (false? (isValidTra traveler))) 0] ; If inputs are not in database return 0
     [(eqv? (HOME traveler) location) 0] ; If home return 0
     [(equal? '() (filter (lambda (x) (memv x (ACTIVITIES-IN location))) (INTERESTED-ACTIVITIES traveler))) (ACCOMMODATION-COST location)]
     ;If the traveler's interested activities are not present in the current city return accommodation cost of the city
     [else (* 3 (ACCOMMODATION-COST location))] ; Then there is a corresponding interesting activity in the city. Return three day accom. cost.
  )
)
(define (ALL-LOCATIONS) ; Returns the list of all locations.
  (define (iter lst formedlist)
    (cond
      [(eqv? (length lst) 0) formedlist] 
      [else (iter (rest lst) (append formedlist (list (caar lst))))]
      )
    )
  (iter LOCATIONS '())
  )


(define (breadthfirstsearch city queue formedlist) ; Very recursive breadthfirst connectedness test function.
    (cond 
      [(eqv? (length queue) 0) formedlist] ; If queue is empty return the formed list
      [(equal? (sort formedlist symbol<?) (sort (ALL-LOCATIONS) symbol<?)) formedlist]  ; If formedlist contains all the cities return it
      [(eqv? (length (filter (lambda (x)(not (memv x formedlist))) (RAILWAY-CONNECTION (car queue)))) 0) (breadthfirstsearch city (cdr queue) formedlist)]
; If there are new cities (not in formedlist) around the current city that will be processed now, add them to the queue and append all the neighbours to formedlist (unique onesi.e combine them)
      [else (breadthfirstsearch city (cdr queue) (append formedlist (filter (lambda (x)(not (memv x formedlist))) (RAILWAY-CONNECTION (car queue)))))]
      ; If there are not the call the procedure with one less element in the queue.
      )
  )
   


(define (connected city)

   (breadthfirstsearch city (RAILWAY-CONNECTION city) '()) ; this returns connected cities to the city "city" as a list.
  )

(define (TRAVEL-EXPENSES traveler location) ; Computes travel expense of a traveler to a location "location"
  (
   cond[(eqv? location (HOME traveler)) 0 ] ; If we are here already return 0
       [(memv location (connected (HOME traveler))) 100] ; if location is connected to the home pay 100
       [else 200] ; else pay 200
   )
  )
(define (EXPENSES traveler location) (+ (ACCOMMODATION-EXPENSES traveler location) (TRAVEL-EXPENSES traveler location))) ; Sums accommodation expenses and travel expenses

(define (IN-BETWEEN num1 num2) (filter (lambda (x) (and (>= (ACCOMMODATION-COST x) num1) (<= (ACCOMMODATION-COST x) num2))) (ALL-LOCATIONS)))


