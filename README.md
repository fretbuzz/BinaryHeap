# BinaryHeap
Implements a complete binary heap data structure in Racket (specifically in Advanced Student Language)

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

NOTE: While this was assigned as a homework assignemnt for EECS214 at Northwestern University in the Fall of 2015, ALL code (including comments, 
function descriptions, the testing apparatus, and the tests as well as the actual code) are the sole work of the author.
