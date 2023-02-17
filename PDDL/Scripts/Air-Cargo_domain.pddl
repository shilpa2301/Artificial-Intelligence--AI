;; STRIPS domain of the Air-Cargo

(define (domain air-cargo)
  (:requirements :strips)
  (:predicates 
        (At ?c ?a)  ;  c is at airport
        (In ?c ?p) ;  c is in plane p
        (Cargo ?c) ; c is a cargo
        (Airport ?a) ; a is an airpot
        (Plane ?p) ; p is a plane
    	
    )

    ; Load cargo to plane
  (:action LOAD
    :parameters (?cargo ?plane ?airport)
    :precondition (and (At ?cargo ?airport) (At ?plane ?airport) (Cargo ?cargo) (Plane ?plane) (Airport ?airport))
    :effect  (and (not (At ?cargo ?airport)) (In ?cargo ?plane)))
    
	;unload cargo from plane
  (:action UNLOAD
    :parameters (?cargo ?plane ?airport)
    :precondition (and (In ?cargo ?plane) (At ?plane ?airport) (Cargo ?cargo) (Plane ?plane) (Airport ?airport))
    :effect  (and (At ?cargo ?airport) (not (In ?cargo ?plane))))

    ; Fly from one airport to another 
  (:action FLY
    :parameters (?plane ?from ?to)
    :precondition (and (At ?plane ?from) (Plane ?plane) (Airport ?from) (Airport ?to))
    :effect  (and (not (At ?plane ?from)) (At ?plane ?to)))

)