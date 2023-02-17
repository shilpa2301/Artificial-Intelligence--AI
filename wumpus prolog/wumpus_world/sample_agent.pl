%my_agent.pl

%   this procedure requires the external definition of two procedures:
%
%     init_agent: called after new world is initialized.  should perform
%                 any needed agent initialization.
%
%     run_agent(percept,action): given the current percept, this procedure
%                 should return an appropriate action, which is then
%                 executed.
%
% This is what should be fleshed out

:- use_module(library(clpfd)).

%% Variables
% Agent variables
:- dynamic agent_loc/2, agent_orient/1.
:- dynamic rich/1, arrow/1.
:- dynamic past/1.
:- dynamic killer/1.
:- dynamic shot_origin/2, shot_angle/1.
% Board variables
:- dynamic safe/2.
:- dynamic wall/2.
:- dynamic has_stench/2, has_breeze/2.
:- dynamic no_stench/2, no_breeze/2.
:- dynamic has_wumpus/2, has_pit/2.
:- dynamic maybe_wumpus/2, maybe_pit/2.
:- dynamic no_wumpus/2, no_pit/2.
:- dynamic has_glitter/2, no_glitter/2.
:- dynamic seen/2.
:- dynamic path/1.

%% Agent Variable Descriptions
% -- agent_loc: (X, Y), where X is row on board and Y
% is column on board. (1,1) is the entrance.
% -- agent_orient: A, where A is the direction the agent is
% facing on the board. 0 = North, 1 = East, 2 = South, 3 = West.
% -- rich: "yes" if agent has gold, otherwise "no".
% -- arrow: "yes" if agent has arrow, otherwise "no".
% -- past: Past action taken by agent.
% -- killer: "yes" if we've killed the wumpus, otherwise "no".
% -- shot_origin: (X, Y) of arrow shot location
% -- shot_angle: A of arrow shot angle

%% Board Variable Descriptions
% -- safe: (X, Y) where we KNOW it's safe to step on.
% -- wall: (X, Y) where we KNOW there is a wall (out of bounds).
% -- has_stench: (X, Y) where we noticed a stench
% -- has_breeze: (X, Y) where we noticed a breeze
% -- no_stench: (X, Y) where we KNOW there's no stench.
% -- no_breeze: (X, Y) where we KNOW there's no breeze.
% -- has_wumpus: (X, Y) where we KNOW the wumpus is.
% -- has_pit: (X, Y) where we KNOW a pit is.
% -- has_glitter: (X, Y) where we KNOW glitter is
% -- no_glitter: (X, Y) where we KNOw there's no glitter
% -- no_wumpus: (X, Y) where we KNOW there's no wumpus
% -- no_pit: (X, Y) where we KNOW there's no pit
% -- maybe_wumpus: (X, Y) where a wumpus COULD be (based on stench)
% -- maybe_pit: (X, Y) where a pit COULD be (based on breeze)
% -- seen: (X, Y) where we've been to a space before (prioritize going to unseen spaces)
% -- path: Path of actions that bring us back to the entrance (in reverse order)

%% Tracking Percepts
% These functions update our knowledge base based on our past
% action and what we just observed
% Percept options are:
% [Stench, Breeze, Glitter, Bump, Scream]
% Actions are:
% goforward, turnleft, turnright, grab, shoot, climb
track_percepts(PastAction, [Stench, Breeze, Glitter, Bump, Scream]):-
  track_movement(PastAction, Bump), % Important that this happens first, updates our position
  track_rotate(PastAction),
  track_stench(PastAction, Stench),
  track_breeze(PastAction, Breeze),
  track_glitter(PastAction, Glitter),
  track_scream(PastAction, Scream).


%track_stench(Past, Stench).
% Stench
track_stench(_, yes):-
  agent_loc(X, Y),
  assert(has_stench(X, Y)).
% No stench
track_stench(_, no):-
  agent_loc(X, Y),
  assert(no_stench(X, Y)).


% track_breeze(Past, Breeze).
% Breeze
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
track_breeze(_, yes):-
  agent_loc(X, Y),
  assert(has_breeze(X, Y)).
% No breeze
track_breeze(_, no):-
  agent_loc(X, Y),
  assert(no_breeze(X, Y)).


% track_glitter(Past, Glitter).
% Mark glitter spot
track_glitter(_, yes):-
  agent_loc(X, Y),
  assert(has_glitter(X, Y)).
% We took the glitter, we're rich!
track_glitter(grab, no):-
  agent_loc(X, Y),
  retract(has_glitter(X, Y)),
  assert(no_glitter(X, Y)),
  assert(rich(yes)).
% Do nothing
track_glitter(_, no):-
  agent_loc(X, Y),
  assert(no_glitter(X, Y)).


% track_bump(Action, Bump).
% Don't move, mark wall
track_movement(goforward, yes):-
  agent_loc(X, Y),
  agent_orient(A),
  goforward(X,Y,A,X1,Y1),
 % step_forward(X, Y, A, X1, Y1),
  assert(seen(X1, Y1)), % Technically mark wall as seen
  assert(wall(X1, Y1)). % Mark wall
% Move forward
track_movement(goforward, no):-
  agent_loc(X, Y),
  agent_orient(A),
  assert(seen(X, Y)), % Mark previous location as seen
 % step_forward(X, Y, A, X1, Y1),
  goforward(X,Y,A,X1,Y1),
  retract(agent_loc(X, Y)),
  assert(agent_loc(X1, Y1)),
  path(P), % Track path return
  retract(path(P)),
  append([turnleft, turnleft, goforward], P, NewP),
  assert(path(NewP)).
% Do nothing (we didn't move forward)
track_movement(_, _).


% track_rotate(Past).
% No associated percept with this
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
track_rotate(_):-
  agent_loc(X,Y).
 % agent_orient(A),
 %   turn_right(X,Y);
 %   turn_left(X, Y)).



% track_scream(Past, Scream).
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
track_scream(_,yes):-
 % agent_loc(X, Y),
  killer(yes).

track_scream(_,no):-
 % agent_loc(X, Y),
  killer(no).



%% Our logical rules based on our Percepts
% A cell is safe if there's no wumpus or pit there
safe(X, Y):-
  no_wumpus(X, Y),
  no_pit(X, Y).

%shilpa
not_safe(X, Y):-
  has_wumpus(X, Y);
  has_pit(X, Y).



% A cell has no wumpus if there's no stench in at least
% one of its adjacent squares
% Note that we need #= instead of is because of
% https://stackoverflow.com/questions/23815952/prolog-arguments-are-not-sufficiently-instantiated
no_wumpus(X, Y):-
  XUp #= X+1,
  XDown #= X-1,
  YRight #= Y+1,
  YLeft #= Y-1,
  (
    no_stench(XUp, Y);
    no_stench(XDown, Y);
    no_stench(X, YRight);
    no_stench(X, YLeft)
  ).
% Wumpus can't be alive if we've killed it
no_wumpus(_, _):-
  killer(yes).


% A cell might have a wumpus if we don't know if there's a wumpus
% there and one of the adjacent squares has a stench
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
might_have_wumpus(X,Y):-
  maybe_wumpus(X,Y),
  XUp #= X+1,
  XDown #= X-1,
  YRight #= Y+1,
  YLeft #= Y-1,
  (
    has_stench(XUp, Y);
    has_stench(XDown, Y);
    has_stench(X, YRight);
    has_stench(X, YLeft)
  ).

% A wumpus is surrounded by at least 3 stenches or against a wall/pit
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
has_wumpus(X,Y):-
  XUp #= X+1,
  XDown #= X-1,
  YRight #= Y+1,
  YLeft #= Y-1,
  ((wall(X,YLeft);
   has_pit(X, YLeft)),
   has_stench(XUp,Y);
   has_stench(X,YRight);
   has_stench(XDown,Y)),
  ((wall(XUp,Y);
   has_pit(XUp, Y)),
  has_stench(X,YRight);
 has_stench(XDown,Y);
  has_stench(X,YLeft)   ),
  ( (wall(X,YRight);
   has_pit(X, YRight)),
  has_stench(XDown,Y);
 has_stench(X,YLeft);
  has_stench(XUp,Y)  ),
  (   (wall(XDown,Y);
   has_pit(XDown, Y)),
  has_stench(X,YLeft);
 has_stench(XUp,Y);
  has_stench(X,YRight)).

% If X, Y has two adjacent stenchs, but its diagonal doesn't have a wumpus,
% then X, Y must have the wumpus
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
%
has_wumpus(X,Y):-
 (  Count #= 0,
   (assertz(has_stench(X-1,Y)), Count is Count+1;
   assertz(has_stench(X+1,Y)), Count is Count+1;
   assertz(has_stench(X,Y-1)), Count is Count+1;
   assertz(has_stench(X,Y+1)), Count is Count+1
   ),
    assertz(Count=:=2),
  ( no_wumpus(X-1,Y-1),
    no_wumpus(X-1,Y+1),
    no_wumpus(X+1,Y-1),
    no_wumpus(X+1,Y+1))
  ).


% If there is a stench on either side of X, Y, then the wumpus must be there
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
has_wumpus(X,Y):-
  (   (has_stench(X-1,Y),
    has_stench(X+1,Y)
    );
   (has_stench(X,Y-1),
    has_stench(X,Y+1)
   )).

% A cell has no pit if there's no breeze in at least
% one of its adjacent squares
no_pit(X, Y):-
  XUp #= X+1,
  XDown #= X-1,
  YRight #= Y+1,
  YLeft #= Y-1,
  (
    no_breeze(XUp, Y);
    no_breeze(XDown, Y);
    no_breeze(X, YRight);
    no_breeze(X, YLeft)
  ).


% A cell might have a pit if we don't know if there's a pit
% there and one of the adjacent squares has a breeze
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
might_have_pit(X,Y):-
 ( maybe_pit(X,Y),
  (has_breeze(X-1,Y);
  has_breeze(X+1,Y);
  has_breeze(X,Y-1);
  has_breeze(X,Y+1))).


% A pit is surrounded by breezes or against a wall
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
has_pit(X,Y):-
  ( has_breeze(X-1,Y);wall(X-1,Y)  ),
  ( has_breeze(X,Y-1);wall(X,Y-1) ),
  ( has_breeze(X+1,Y);wall(X+1,Y)  ),
  ( has_breeze(X,Y+1);wall(X,Y+1)  ).


% Check if the arrow hit a point
% We can assume the arrow also hits the point
% it was shot from
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
arrow_hit(X,Y,XOrigin,YOrigin):-

% Angle 0 (North): Y must match and X >= XOrigin
  (    assert(Y=:=YOrigin, X>=XOrigin),

%%%%%%%%%% Your Code Here %%%%%%%%%%
% Angle 1 (East): X must match and Y >= YOrigin
  assert(Y>=YOrigin, X=:=XOrigin)).

  %assert(Y=:=YOrigin, X<=XOrigin).

%%%%%%%%%% Your Code Here %%%%%%%%%%
% Angle 3 (West): X must match and Y =< YOrigin
%%%%%%%%%% Your Code Here %%%%%%%%%%
 % assert(Y<=YOrigin, X=:=XOrigin)).

% Calculate rotations
%%%%%%%%%% Your Code Here %%%%%%%%%%


% North
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
is_north(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=:=0.

not_north(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=\=0.


% East
%%%%%%%%%% Your Code Here %%%%%%%%%%
%%shilpa
is_east(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=:=1.

not_east(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=\=1.




% South
%%%%%%%%%% Your Code Here %%%%%%%%%%
%%shilpa
is_south(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=:=2.

not_south(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=\=2.


% West
%%%%%%%%%% Your Code Here %%%%%%%%%%
%%shilpa
is_west(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=:=3.

not_west(X,Y,A):-
  agent_loc(X,Y),
  agent_orient(A),
  A=\=3.




% Look at what our step would be
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
%get_action(Action):
 % agent_loc(X,Y),
  %agent_orient(A),
  %(
  %is_north(X,Y,A),goforward(X, Y, A, X, Y+1),Action:goforward;
  %is_east(X,Y,A),goforward(X, Y, A, X+1, Y),Action:goforward;
  %is_south(X,Y,A),goforward(X, Y, A, X, Y-1),Action:goforward;
  %is_west(X,Y,A),goforward(X, Y, A, X-1, Y),Action:goforward
  %).


% We can't move anywhere :(
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
is_stuck(_,yes):-
  agent_loc(X,Y),
  (
  has_stench(X,Y);has_breeze(X,Y)
  ),
  (not_safe(X+1,Y); wall(X+1,Y); maybe_wumpus(X+1,Y),maybe_pit(X+1,Y)),
  (not_safe(X,Y+1); wall(X,Y+1);maybe_wumpus(X,Y+1),maybe_pit(X,Y+1)   ),
  (not_safe(X-1,Y); wall(X-1,Y);maybe_wumpus(X-1,Y),maybe_pit(X-1,Y)),
  (not_safe(X,Y-1); wall(X,Y-1);maybe_wumpus(X,Y-1),maybe_pit(X,Y-1)).

is_stuck(_,no):-
  agent_loc(X,Y),
  (
  no_stench(X,Y);no_breeze(X,Y)
  ),
  (safe(X+1,Y); maybe_wumpus(X+1,Y),maybe_pit(X+1,Y)),
  (safe(X,Y+1); maybe_wumpus(X,Y+1),maybe_pit(X,Y+1)   ),
  (safe(X-1,Y); maybe_wumpus(X-1,Y),maybe_pit(X-1,Y)),
  (safe(X,Y-1); maybe_wumpus(X,Y-1),maybe_pit(X,Y-1)).

 % track_movement(_,_).


% If number of safe and visited cells are equal
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
safe_visited_count():-
 ( count_safe#=0,
  count_visited#=0,
  forall(agent_loc(X,Y),
           (safe(X,Y)->count_safe is count_safe+1,
           seen(X,Y)->count_seen is count_seen+1)),
  assert(count_safe =:= count_seen)).



%% Action deciders
% Note that we only need Action since
% we've already updated our knowledge base
% Actions are:
% goforward, turnleft, turnright, grab, shoot, climb


% If we're on the gold, grab it!
get_action(Action):-
  agent_loc(X, Y),
  has_glitter(X, Y),
  Action=grab.


% If we have at least one gold and we're at the
% entrance, climb!
get_action(Action):-
  rich(yes),
  agent_loc(X,Y),
  X =:= 1,
  Y =:= 1,
  Action=climb.


% If nowhere left to go, climb
get_action(Action):-
  all_explored,
  agent_loc(X, Y),
  X =:= 1,
  Y =:= 1,
  Action=climb.

%shilpa
%turnleft action
turn_left(X,Y):-
  agent_loc(X,Y),
  agent_orient(A),
  (
      is_north(X,Y,A),A is 3;
      is_east(X,Y,A),A is 0;
      is_south(X,Y,A),A is 1;
      is_west(X,Y,A),A is 2
  ).

get_action(Action):-
  agent_loc(X,Y),
  turn_left(X,Y),
  Action=turnleft.


%turnright action
turn_right(X,Y):-
  agent_loc(X,Y),
  agent_orient(A),
  (
      is_north(X,Y,A),A is 1;
      is_east(X,Y,A),A is 2;
      is_south(X,Y,A),A is 3;
      is_west(X,Y,A),A is 0
  ).

get_action(Action):-
  agent_loc(X,Y),
  turn_right(X,Y),
  Action=turnright.




% If there's nowhere left to explore, go home
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
all_explored:-
  forall(agent_loc(X,Y),
           (assert(seen(X,Y)))).

%shilpa
goforward(X,Y,A, X1,Y1):-
  agent_loc(X,Y),
  agent_orient(A),
  (
  agent_orient(A)=:=0,X1=:=X,Y1=:=Y+1,agent_loc(X1,Y1);
  agent_orient(A)=:=1, X1=:=X+1, Y1=:=Y, agent_loc(X1,Y1);
  agent_orient(A)=:=2,X1=:=X,Y1=:=Y-1,agent_loc(X1,Y1);
  agent_orient(A)=:=3,X1=:=X-1, Y1=:=Y, agent_loc(X1,Y1)
  ).


% Move forward to an unexplored space
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
move_forward_to_unexplored(X,Y):-
  agent_loc(X,Y),
  agent_orient(A),
  (
  (   not_north(X,Y,A), seen(X,Y+1));goforward(X, Y, A, X, Y+1);
  (   not_east(X,Y,A), seen(X+1,Y)); goforward(X, Y, A, X+1, Y );
  (   not_south(X,Y,A), seen(X,Y-1)); goforward(X, Y, A, X, Y-1);
  (   not_west(X,Y,A), seen(X-1,Y));goforward(X, Y, A, X-1, Y)
  ).


% Turn towards unexplored space
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
%turn_to_unexplored(X,Y):-
  %preference given to clockwise position first, then the anticlockwise position
 % agent_loc(X,Y),
 % agent_orient(A),
 % (
 %     (
 %     (agent_orient(A)=:=0,not_seen(X+1,Y))->A is 1,!;
  %    (agent_orient(A)=:=0,not_seen(X-1,Y))->A is 3
  %    );
  %    (
  %    (agent_orient(A)=:=1,not_seen(X,Y-1))->A is 2,!;
  %    (agent_orient(A)=:=1,not_seen(X,Y+1))->A is 0
  %    );
  %    (
  %    (agent_orient(A)=:=2,not_seen(X,Y-1))->A is 3,!;
  %    (agent_orient(A)=:=2,not_seen(X,Y+1))->A is 1
  %    );
  %    (
  %    (agent_orient(A)=:=3,not_seen(X,Y+1))->A is 0,!;
  %   (agent_orient(A)=:=3,not_seen(X,Y-1))->A is 2
  %    )
  %).

% If we're facing the wumpus, fire!
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  agent_orient(A),
  has_wumpus(XWumpus,YWumpus),
  (
      (    not_north(X,Y,A); X=\=XWumpus; Y>=YWumpus; Action=shoot );
      (   not_east(X,Y,A); X>=XWumpus; Y=\=YWumpus; Action=shoot   )
      %(   not_south(X,Y,A); X=\=XWumpus; Y<=YWumpus; Action=shoot   )
      %(   not_west(X,Y,A); X<=XWumpus; Y=\=YWumpus; Action=shoot   )

   ).



% If we're next to the wumpus and we have the arrow, face it!
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  agent_orient(A),
  has_stench(X,Y),
  (
        (has_wumpus(X+1,Y),arrow(_),
          (   is_north(X,Y,A),Action=turnright;
              is_south(X,Y,A),Action=turnleft;
              is_west(X,Y,A),Action=[turnleft,turnleft]
           )
        );
      (has_wumpus(X,Y+1),arrow(_),
        (   is_west(X,Y,A),Action=turnright;
            is_east(X,Y,A),Action=turnleft;
            is_south(X,Y,A) ,Action=[turnleft,turnleft]

         )
       );
       (has_wumpus(X-1,Y),arrow(_),
          (   is_south(X,Y,A),Action=turnright ;
              is_north(X,Y,A),Action=turnleft;
              is_west(X,Y,A) ,Action=[turnleft,turnleft]
         )
      );

       (has_wumpus(X,Y-1),arrow(_),
          (   is_east(X,Y,A),Action=turnright;
              is_west(X,Y,A),Action=turn_left;
              is_north(X,Y,A),Action=[turnleft,turnleft]
         )
      )

   ).


% No new spot to explore and no wumpus to kill, Move forward if we can do so safely
% and without bumping a wall
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  all_explored,
  no_wumpus(_,_),
  Action=track_movement(goforward,_).


% If we're stuck and facing where a wumpus might be, fire!
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  assert(is_stuck(X,Y)),
  might_have_wumpus(X,Y),
  Action=shoot.


% If we're stuck and out of ammo, we might've missed the wumpus, just leave :(
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  assert(is_stuck(X,Y)),
  arrow(no),
  killer(no),
  Action=none.

% If we're stuck and out of ammo, we might be surrounded by pits, just leave :(
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
 agent_loc(X,Y),
 assert(is_stuck(X,Y)),
 arrow(no),
 (
      has_pit(X+1,Y);
      has_pit(X-1,Y);
      has_pit(X,Y+1);
      has_pit(X,Y-1)
  ),
 Action=track_movement(_,_).


% If forward isn't safe or there's a wall,
% look to the left. If its safe and there's no wall,
% rotate left
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  agent_orient(A),
  (
  (is_north(X,Y,A),(not_safe(X,Y+1); wall(X,Y+1)),safe(X-1,Y),turn_left(X,Y), Action=turnleft);
  (is_east(X,Y,A),(not_safe(X+1,Y); wall(X+1,Y)),safe(X,Y+1),turn_left(X,Y), Action=turnleft );
  (is_south(X,Y,A), (not_safe(X,Y-1);wall(X,Y-1)),safe(X+1,Y),turn_left(X,Y),Action=turnleft  );
  (is_west(X,Y,A), (not_safe(X-1,Y);wall(X-1,Y)),safe(X,Y-1),turn_left(X,Y),Action=turnleft  )
  ).


% If there's literally nothing else to do, rotate right.
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  turn_right(X,Y),
  Action=turnright.

% Reset some variables
reset:-
  retractall(agent_loc(_,_)),
  retractall(agent_orient(_)),
  retractall(rich(_)),
  retractall(arrow(_)),
  retractall(past(_)),
  retractall(killer(_)),
  retractall(shot_origin(_,_)),
  retractall(shot_angle(_)),
  retractall(wall(_,_)),
  retractall(has_stench(_,_)),
  retractall(has_breeze(_,_)),
  retractall(no_stench(_,_)),
  retractall(no_breeze(_,_)),
  retractall(seen(_,_)),
  retractall(path(_)).

%% Interface
% evaluate_agent(1, Score, Time).
init_agent:-
  format('===== Starting game ====='),
  display_world,
  reset,
  assert(agent_loc(1,1)),
  assert(agent_orient(1)),
  assert(rich(no)),
  assert(arrow(yes)),
  assert(past(spawn)),
  assert(killer(no)),
  assert(no_wumpus(1,1)),
  assert(no_pit(1,1)),
  assert(safe(1,1)).

run_agent(Percepts, Action):-
  display_world,
  %sleep(1),
  past(P),
  track_percepts(P, Percepts),
  get_action(H),
  retract(past(P)),
  assert(past(H)),
  Action=H.


















