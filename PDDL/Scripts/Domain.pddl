;; STRIPS domain of the STAR Puzzle

(define (domain star-puzzle)
  (:requirements :strips)
  (:predicates 
        (post_o ?o)  ; is at Post O?
        (on ?x ?y) ; x is on y
    	(smaller ?s ?l) ; s is smaller than l
    	(clear ?z) ; clear to move or clear to be moved on
    )

    ; Move from a post to post O
  (:action MOVETO
    :parameters (?disk ?from ?to)
    :precondition (and (post_o ?to) (smaller ?disk ?to) (on ?disk ?from) (clear ?disk) (clear ?to))
    :effect  (and (on ?disk ?to) (clear ?from) (not (on ?disk ?from)) (not (post_o ?to)) (post_o ?disk) (not (clear ?to))))

    ; Move from post O to a post 
  (:action MOVEFROM
    :parameters (?disk ?from ?to)
    :precondition (and (post_o ?disk) (smaller ?disk ?to) (on ?disk ?from) (clear ?disk) (clear ?to))
    :effect  (and (on ?disk ?to) (clear ?from) (not (on ?disk ?from)) (not (post_o ?disk)) (post_o ?from) (not (clear ?to))))

)