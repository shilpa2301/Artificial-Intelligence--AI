;; STRIPS Instance problem for the Star Puzzle with 4 disks

(define (problem star-puzzle-4)
  (:domain star-puzzle)
  (:objects PA PB PC PO
    D1 D2 D3 D4)
  (:init
   ;; types
   (post_o PO) ; initializing Post O

  ;; locations and properties
   (smaller D1 PA) (smaller D2 PA) (smaller D3 PA) (smaller D4 PA)
   (smaller D1 PB) (smaller D2 PB) (smaller D3 PB) (smaller D4 PB)
   (smaller D1 PC) (smaller D2 PC) (smaller D3 PC) (smaller D4 PC)
   (smaller D1 PO) (smaller D2 PO) (smaller D3 PO) (smaller D4 PO)
   (smaller D1 D2) (smaller D1 D3) (smaller D1 D4) 
   (smaller D2 D3) (smaller D2 D4) 
   (smaller D3 D4)
   
   (clear PB) (clear PC) (clear PO) (clear D1)
   (on D4 PA) (on D3 D4) (on D2 D3) (on D1 D2))

  (:goal
   (and (on D4 PC) (on D3 D4) (on D2 D3) (on D1 D2)))

)