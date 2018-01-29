;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname binheap-refined) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
#|
Author: Joe Severini
Written in ASL (advanced student language) in Racket.

Implements a comlplete binary heap data structure.
Public functions: (E = element) (H = heap)
 heap:create       N [Order] -> heap  Creates a heap of size N with ordering [Order]
 heap:insert!      H E -> Void        Inserts the element at the end of the heap and then 'bubbles-up' to restore invariant.
 heap:find-min     H -> E             Returns the minimum element in the heap. Does NOT change the heap itself.
 heap:remove-min!  H -> Void          Removes the minimum element in the heap by replacing it with the last element in 
                                      the heap and then 'percolating-down' to restore the invariant.
Time complexity: (N = size of the heap)
 heap:insert!       O(log(N))     Must bubble-up, so proportional to height of the tree
 heap:find-min      O(1)          Access index zero of the heap, so constant time
 heap:remove-min!   O(log(N))     Must percolate-down, so proportional to height of the tree

Dictionary:
 N  : the set of natural numbers
 N+ : the set of positive integers
 R  : the set of real numbers

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete binary heap functions (i.e. public functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct heap [size lt? data])

; heap:create : N [Order] -> Heap
; creates a heap structure of size capacity. As the heap is empty, all components of the heap 
; are initiliazed to #false
(define (heap:create capacity lt?)
  (make-heap 0 lt? (make-vector capacity #false)))

; heap:insert! : Heap X -> Void
; Adds new element to the end of the heap and then 'bubbles up' to restore
; the heap invariant.
(define (heap:insert! heap new-element)
  (local
    [(define oldSize (heap-size heap))]
    (cond
      [(> (+ (heap-size heap) 1) (vector-length (heap-data heap))) ;; expand if necessary
       (begin
         (expansion heap (heap-size heap))              ;; brand new heap
         (set-heap-size! heap (+ oldSize 1) )          
         (heap:set! heap (heap-size heap) new-element)  ;; add it to the end of the heap
         (heap:bubble-up! heap (heap-size heap) ) )]    ;; bubble it up
      [else                                             ;; do not need to expand
       (begin
         (set-heap-size! heap (+ (heap-size heap) 1) )
         (heap:set! heap (heap-size heap) new-element)  ;; add it to the end of the heap
         (heap:bubble-up! heap (heap-size heap) ) )]    ;; bubble it up
      )))

; heap:find-min : heap -> X
; Returns the smallest (according to the heap's order) element in the heap
; (which is at index 0)
(define (heap:find-min heap)
  (if (= (heap-size heap) 0) (error "the heap is empty") (heap:ref heap 1)))

; heap:remove-min! : heap -> void
; Removes the smallest element from the given heap
; Will throw an error if the heap is empty
(define (heap:remove-min! heap)
  (cond
  [(> (heap-size heap) 0)
    (begin
      (heap:set! heap 1 (heap:ref heap (heap-size heap))) ; step1 replace the root with the last node
      (set-heap-size! heap (- (heap-size heap) 1) )
      (heap:percolate-down! heap 1))]
   [else 
    (error "the heap is empty")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions (i.e. private functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; heap:bubble-up! : heap N -> Void
; Restores the heap invariant by bubbling up the element at index 'index'.
(define (heap:bubble-up! heap index)
  (local
    [(define p1 (heap:parent index))]
    (if
       (and (and (> p1 0) (> index 1)) ((heap-lt? heap) (heap:ref heap index) (heap:ref heap p1)))
        (begin
            (heap:swap! heap index p1)
            (heap:bubble-up! heap p1))
       void)
))

; heap:find-smaller-child : heap N -> N
; Returns the index of the smaller child of the element at index 'index'.
; Will return #false if no children.
(define (heap:find-smaller-child heap index)
  (local
    [(define child1 (heap:left index))
     (define child2 (heap:right index))]
    (cond 
      [( < (heap-size heap) child1) #false]   ; no children
      [( = (heap-size heap) child1) child1]   ; only left child
      [else (if ((heap-lt? heap) (heap:ref heap child1) (heap:ref heap child2)) child1 child2)]
)))

; heap:lt? : heap N N -> Bool
; Is the heap element at index 'i' less than the heap element at index 'j'?
; (using the heap's order)
(define (heap:lt? heap i j)
  (if ((heap-lt? heap) (heap:ref heap i) (heap:ref heap j))  #true #false))

; heap:percolate-down! : heap N
; Restores the heap invariant by checking if the invariant holds, and swapping otherwise, starting with
; the element at index 0 in the heap
(define (heap:percolate-down! heap index)
  (local
    [(define smallerChild  (heap:find-smaller-child heap index))]
    (cond
      [(number? smallerChild) ; if no smaller child, smallerChild will be #false
       (cond
         [(and (and (> index 0) (or (< smallerChild (heap-size heap)) (= smallerChild (heap-size heap)) ) ) ((heap-lt? heap) (heap:ref heap smallerChild)  (heap:ref heap index)) )
          (begin
            (heap:swap! heap index smallerChild)
            (heap:percolate-down! heap smallerChild))]
         [else void])]
       [else void] )))

; heap:ref : heap N -> X
; Returns the element in the heap that is at index N
; NOTE: N is greater than or equal to 1
(define (heap:ref heap N)
    (vector-ref (heap-data heap) (- N 1)) ) ; other functions act like the first array index is 1, but racket implementation says first is index 0, so must adjust

; heap:set! : heap N R -> void
; Sets the heap element at index 'pos' to be 'val'
(define (heap:set! heap pos val)
   (vector-set! (heap-data heap) (- pos 1) val))  

; heap:swap! : heap N N -> void
; Swaps the heap element at position 'vert1' with the heap element at position 'vert2'
(define (heap:swap! heap vert1 vert2)
  (local
    [(define val1 (heap:ref heap vert1))
     (define val2 (heap:ref heap vert2))]
  (begin
    (heap:set! heap vert1 val2)
    (heap:set! heap vert2 val1)
)))

; heap:left : N -> N
; Returns the index of the left child of the given index
(define (heap:left i)
  (* 2 i))

; heap:right : N -> N
; Returns the index of the left child of the given index.
(define (heap:right i)
  (+ (* 2 i) 1))

; heap:parent : N -> N
; Returns the index of the parent of the given index.
(define (heap:parent i)
 (floor (/ i 2)))

; expansion: heap N -> void
; If expansion  is necessary, create twice as large heap and then take out values
; from inital and put them into the expanded heap.
(define (expansion heap N)
  (cond  
    [(> (+ N 1) (vector-length (heap-data heap)))
     (local
       [(define anotherHeap (heap:create (* (heap-size heap) 2) (heap-lt? heap)) )]
       (recursiveExpansion heap anotherHeap))]
    [else void]))

; recursiveExpansion : heap heap -> void
; Takes values out of initialHeap and puts them into expandedHeap (recursively)
(define (recursiveExpansion initialHeap expandedHeap)
  (cond
    [(> (heap-size initialHeap) 0)
     (begin
         (heap:insert! expandedHeap (heap:find-min initialHeap))
         (heap:remove-min! initialHeap)
         (recursiveExpansion initialHeap expandedHeap))]
    [else 
       (set-heap-data! initialHeap (heap-data expandedHeap))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unit testing for the helper functions

(check-expect 
 (local
   [(define heap1 (heap:create 10 <))]
   (begin
     (heap:insert! heap1 1)
     (heap:insert! heap1 2)
     (heap:insert! heap1 3)   
     (heap:find-smaller-child heap1 1))) 2)

(check-expect 
 (local
   [(define heap1 (heap:create 10 >))]
   (begin
     (heap:insert! heap1 1)
     (heap:insert! heap1 2)
     (heap:insert! heap1 3)   
     (heap:find-smaller-child heap1 1)))  3)

(check-expect 
 (local
   [(define heap1 (heap:create 10 <))]
   (begin
     (heap:insert! heap1 6)
     (heap:insert! heap1 5)
     (heap:insert! heap1 4)
     (heap:insert! heap1 3)
     (heap:insert! heap1 2)
     (heap:insert! heap1 1)     
     (heap:find-smaller-child heap1 3)))  6)


(check-expect 
 (local
   [(define heap1 (heap:create 10 <))]
   (begin
     (heap:insert! heap1 1)
     (heap:insert! heap1 2)
     (heap:insert! heap1 3)   
     (heap:ref heap1 2)))  2)

(check-expect 
 (local
   [(define heap1 (heap:create 10 <))]
   (begin
     (heap:insert! heap1 10)
     (heap:insert! heap1 9)
     (heap:insert! heap1 8)
     (heap:insert! heap1 7)
     (heap:insert! heap1 6)
     (heap:insert! heap1 5)     
     (heap:ref heap1 4)))  10)

(check-expect 
 (local
   [(define heap1 (heap:create 10 <))]
   (begin
     (heap:insert! heap1 10)
     (heap:insert! heap1 9)
     (heap:insert! heap1 8)
     (heap:insert! heap1 7)
     (heap:insert! heap1 6)
     (heap:insert! heap1 5)     
     (heap:lt? heap1 1 3)))  #true)

(check-expect 
 (local
   [(define heap1 (heap:create 10 <))]
   (begin
     (heap:insert! heap1 10)
     (heap:insert! heap1 9)
     (heap:insert! heap1 8)
     (heap:insert! heap1 7)
     (heap:insert! heap1 6)
     (heap:insert! heap1 5)     
     (heap:lt? heap1 4 3)))  #false)

(check-expect (heap:left 1) 2)
(check-expect (heap:left 4) 8)
(check-expect (heap:left 5) 10)

(check-expect (heap:right 2) 5)
(check-expect (heap:right 4) 9)

(check-expect (heap:parent 8) 4) ;; array starts at 1 (according to the formula)
(check-expect (heap:parent 3) 1)


;; Complete testing of Public Functions

(define-struct testingCommand [command obj])

; testing-script! : heap [List-of-testingCommand] -> [List-of-bools]
; Carries out a series of heap commands seqeuentially and returns a list of the return values.
; (i.e. the testing apparatus)
(define (testing-script! heap ListOfCommands)
  (begin
    (if (empty? ListOfCommands) '()
        (begin
          (local
            [(define currentCommand (first ListOfCommands))]
            (cond 
              [(symbol=? (testingCommand-command currentCommand) 'insert)
                (begin
                  (heap:insert! heap (testingCommand-obj currentCommand))
                  ;(write uf)
                  (testing-script! heap (rest ListOfCommands))
                  )]
              [(symbol=? (testingCommand-command currentCommand) 'find-min)
                (local
                  [(define currentResult (heap:find-min heap))]
                  (cons currentResult (testing-script! heap (rest ListOfCommands))))]
              [(symbol=? (testingCommand-command currentCommand) 'remove-min)
                (begin
                  (heap:remove-min! heap)
                  ;(write uf)
                  (testing-script! heap (rest ListOfCommands)))]     
))))))

(check-expect 
  (testing-script! (heap:create 50 string<?) '()) 
   '() )

(check-expect 
 (testing-script!
     (heap:create 100 <)
     (cons (make-testingCommand 'insert 10) '()))
 '() )

(check-expect 
 (testing-script!
     (heap:create 5 <)
     (cons (make-testingCommand 'insert 10) 
     (cons (make-testingCommand 'insert 12) 
     (cons (make-testingCommand 'insert 14) 
     (cons (make-testingCommand 'insert 16) 
     (cons (make-testingCommand 'insert 18) 
     '()))))))
 '() )

(check-expect 
 (testing-script!
  (heap:create 100 <)
     (cons (make-testingCommand 'insert 10)
     (cons (make-testingCommand 'find-min #false) '())))
   '(10) )

(check-error 
   (testing-script!
    (cons (heap:create 10 <)
    (cons (make-testingCommand 'remove-min 0) '()))))
    

(check-expect 
 (testing-script!
     (heap:create 100 <)
     (cons (make-testingCommand 'insert 10)
     (cons (make-testingCommand 'remove-min 0) '())))
   '() )

(check-expect 
 (testing-script!
  (heap:create 100 <)
     (cons (make-testingCommand 'insert -20)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'insert 30)
     (cons (make-testingCommand 'find-min #false) '()))))))
   '(-20 30) )

(check-expect 
 (testing-script!
  (heap:create 100 <)
     (cons (make-testingCommand 'insert -20)
     (cons (make-testingCommand 'insert 30)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false) '()))))))
   '(-20 30) )

(check-expect 
 (testing-script!
  (heap:create 100 <)
     (cons (make-testingCommand 'insert 30)
     (cons (make-testingCommand 'insert -20)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false) '()))))))
   '(-20 30) )

(check-expect 
 (testing-script!
  (heap:create 15 <)
     (cons (make-testingCommand 'insert 0) 
     (cons (make-testingCommand 'insert 1) 
     (cons (make-testingCommand 'insert 2)
     (cons (make-testingCommand 'insert 3) 
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0) '())))))))))))))
   '( 0 1 2 3) )


(check-expect 
 (testing-script!
  (heap:create 150 >)
     (cons (make-testingCommand 'insert 0) 
     (cons (make-testingCommand 'insert 1) 
     (cons (make-testingCommand 'insert 2)
     (cons (make-testingCommand 'insert 3) 
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0) '())))))))))))))
   '( 3 2 1 0) )

(check-expect 
 (testing-script!
  (heap:create 150 <)
     (cons (make-testingCommand 'insert 7) 
     (cons (make-testingCommand 'insert 2) 
     (cons (make-testingCommand 'insert 3)
     (cons (make-testingCommand 'insert 1) 
     (cons (make-testingCommand 'insert 11) 
     (cons (make-testingCommand 'insert 6) 
     (cons (make-testingCommand 'insert 4) 
     (cons (make-testingCommand 'insert 17) 
     (cons (make-testingCommand 'insert 9) 
     (cons (make-testingCommand 'insert 10) 
     (cons (make-testingCommand 'insert 5) 
     (cons (make-testingCommand 'insert 20) 
     (cons (make-testingCommand 'insert 13) 
     (cons (make-testingCommand 'insert 14) 
     (cons (make-testingCommand 'insert 19) 
     (cons (make-testingCommand 'insert 16) 
     (cons (make-testingCommand 'insert 8) 
     (cons (make-testingCommand 'insert 18) 
     (cons (make-testingCommand 'insert 15) 
     (cons (make-testingCommand 'insert 12) 
     (cons (make-testingCommand 'insert 0) 
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
           '()))))))))))))))))))))))))))))))))))))))))
   '(0 1 2 3 4 5 6 7 8) )

(check-expect 
 (testing-script!
  (heap:create 22  string<?)
     (cons (make-testingCommand 'insert "arrowHead") 
     (cons (make-testingCommand 'insert "bear") 
     (cons (make-testingCommand 'insert "carrot")
     (cons (make-testingCommand 'insert "django") 
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0) '())))))))))))))
   '( "arrowHead" "bear" "carrot" "django") )

(check-expect 
 (testing-script!
  (heap:create 22  string<?)
     (cons (make-testingCommand 'insert "arrowHead") 
     (cons (make-testingCommand 'insert "carrot") 
     (cons (make-testingCommand 'insert "bear")
     (cons (make-testingCommand 'insert "django") 
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0) '())))))))))))))
   '( "arrowHead" "bear" "carrot" "django") )

(check-expect 
 (testing-script!
  (heap:create 22  string>?)
     (cons (make-testingCommand 'insert "arrowHead") 
     (cons (make-testingCommand 'insert "bear") 
     (cons (make-testingCommand 'insert "carrot")
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false) '())))))))))
   '("carrot" "bear"  "arrowHead" ))

;; tests structures
(define-struct structForTesting [main secondary tertiary])
(check-expect 
 (local
   [(define (comp struc1 struc2)
      (< (structForTesting-main struc1)(structForTesting-main struc2)))]
 (testing-script!
  (heap:create 22  comp)
     (cons (make-testingCommand 'insert (make-structForTesting 10 1 1)) 
     (cons (make-testingCommand 'insert (make-structForTesting 11 1 1)) 
     (cons (make-testingCommand 'insert (make-structForTesting 12 1 1))
     (cons (make-testingCommand 'insert (make-structForTesting 5 1 1))
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false)
     (cons (make-testingCommand 'remove-min 0)
     (cons (make-testingCommand 'find-min #false) '())))))))))))
     
     (cons (make-structForTesting 5 1 1)
     (cons (make-structForTesting 10 1 1)
     (cons (make-structForTesting 11 1 1) '()))) 
)

;stress test
(check-expect 
 (local
   [(define insertList  '())
    (define removeList       '())]
    (begin
         (let again [(j 0)]
           (when (< j 10000)
             (begin
               (set! insertList (cons (make-testingCommand 'insert j) insertList)) 
               (again (+ j 1)))))
         (let again [(j 0)]
           (when (< j 20)
             (begin
               (set! removeList (cons (make-testingCommand 'remove-min 0) removeList))
               (set! removeList (cons (make-testingCommand 'find-min #false) removeList)) 
               (again (+ j 1)))))
 (testing-script!
    (heap:create 10001 <)
     (append insertList removeList))
 ))
 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) )
      
;heap expansion
(check-expect 
 (local
   [(define insertList  '())
    (define removeList       '())]
    (begin
         (let again [(j 0)]
           (when (< j 20)
             (begin
               (set! insertList (cons (make-testingCommand 'insert j) insertList)) 
               (again (+ j 1)))))
         (let again [(j 0)]
           (when (< j 9)
             (begin
               (set! removeList (cons (make-testingCommand 'remove-min 0) removeList))
               (set! removeList (cons (make-testingCommand 'find-min #false) removeList)) 
               (again (+ j 1)))))
 (testing-script!
    (heap:create 1 <)
     (append insertList removeList))
 ))
 '(0 1 2 3 4 5 6 7 8) )
