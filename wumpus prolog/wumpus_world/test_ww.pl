% -------------------------------------------------



initialize_world(map_1) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(2)),
  addto_ww_init_state(gold(1,1)),
  ww_initial_state(L),
  assert_list(L).



% -------------------------------------------------



initialize_world(map_2) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(4,4)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(2,2)),
  ww_initial_state(L),
  assert_list(L).



% -------------------------------------------------



initialize_world(map_3) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(2,2)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(3,1)),
  ww_initial_state(L),
  assert_list(L).



% -------------------------------------------------



initialize_world(map_4) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(4,4)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(3,3)),
  addto_ww_init_state(pit(2,1)),
  addto_ww_init_state(pit(2,2)),
  addto_ww_init_state(pit(1,2)),
  ww_initial_state(L),
  assert_list(L).



% -------------------------------------------------



initialize_world(map_5) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(4,1)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(4,4)),
  addto_ww_init_state(pit(1,4)),
  ww_initial_state(L),
  assert_list(L).



% -------------------------------------------------



initialize_world(map_6) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(1,3)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(2,3)),
  addto_ww_init_state(pit(3,1)),
  addto_ww_init_state(pit(3,3)),
  addto_ww_init_state(pit(4,4)),
  ww_initial_state(L),
  assert_list(L).



% -------------------------------------------------



initialize_world(map_7) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(3,1)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(3,2)),
  addto_ww_init_state(pit(1,3)),
  addto_ww_init_state(pit(3,3)),
  addto_ww_init_state(pit(4,4)),
  ww_initial_state(L),
  assert_list(L).

