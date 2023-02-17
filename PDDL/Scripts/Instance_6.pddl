;; STRIPS Instance problem for the Star Puzzle with 6 disks

(define (problem star-puzzle-4)
  (:domain star-puzzle)
  (:objects PA PB PC PO
    D1 D2 D3 D4 D5 D6)
  (:init
   ;; types
   (post_o PO) ; initializing Post O

  ;; locations and properties
   (smaller D1 PA) (smaller D2 PA) (smaller D3 PA) (smaller D4 PA) (smaller D5 PA) (smaller D6 PA)
   (smaller D1 PB) (smaller D2 PB) (smaller D3 PB) (smaller D4 PB) (smaller D5 PB) (smaller D6 PB)
   (smaller D1 PC) (smaller D2 PC) (smaller D3 PC) (smaller D4 PC) (smaller D5 PC) (smaller D6 PC)
   (smaller D1 PO) (smaller D2 PO) (smaller D3 PO) (smaller D4 PO) (smaller D5 PO) (smaller D6 PO)
   (smaller D1 D2) (smaller D1 D3) (smaller D1 D4) (smaller D1 D5) (smaller D1 D6)
   (smaller D2 D3) (smaller D2 D4) (smaller D2 D5) (smaller D2 D6)
   (smaller D3 D4) (smaller D3 D5) (smaller D3 D6)
   (smaller D4 D5) (smaller D4 D6)
   (smaller D5 D6)
   
   (clear PB) (clear PC) (clear PO) (clear D1)
   (on D6 PA) (on D5 D6) (on D4 D5) (on D3 D4) (on D2 D3) (on D1 D2))

  (:goal
   (and (on D6 PC) (on D5 D6) (on D4 D5) (on D3 D4) (on D2 D3) (on D1 D2)))

)