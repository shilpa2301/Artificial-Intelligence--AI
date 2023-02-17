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
:- dynamic opposite_movement/1.
:- dynamic stuck_turn_value/1.


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
% -- opposite_movement: "yes" if we're return opposite movement back to
% the entrance, otherwise "no"
% -- stuck_turn_value: Number of times we've been stuck in a loop
% after taking turns at the same space

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
  track_shoot(PastAction),
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
  go_forward(X,Y,A,X1,Y1),
 % step_forward(X, Y, A, X1, Y1),
  assert(seen(X1, Y1)), % Technically mark wall as seen
  assert(wall(X1, Y1)). % Mark wall
% Move forward
track_movement(goforward, no):-
  agent_loc(X, Y),
  agent_orient(A),
  assert(seen(X, Y)), % Mark previous location as seen
 % step_forward(X, Y, A, X1, Y1),
  go_forward(X,Y,A,X1,Y1),
  retract(agent_loc(X, Y)),
  assert(agent_loc(X1, Y1)),
  path(P), % Track path return
  (
 opposite_movement(yes);
  (
  append([goforward], P, NewP),
   retract(path(P)),
   assert(path(NewP))
   )
   ).
% Do nothing (we didn't move forward)
track_movement(_, _).

%shilpa
track_shoot(shoot):-
  retract(arrow(yes)),
  assert(arrow(no)).

track_shoot(_).


% track_rotate(Past).
% No associated percept with this
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa _check new orientation
track_rotate(turnleft):-
  agent_orient(A),
 NewOrientation is (A + 3) mod 4,
  retract(agent_orient(A)),
  assert(agent_orient(NewOrientation)),
  % Change the path after modifying orientation
  path(P),
  (
    opposite_movement(yes);
    (
      append([turnright], P, NewP), % Append the reverse for navigating to the start.
      retract(path(P)),
      assert(path(NewP))
    )
  ).

track_rotate(turnright):-
 %change orientation
 agent_orient(A),
 NewOrientation is (A+1) mod 4,
 retract(agent_orient(A)),
 assert(agent_orient(NewOrientation)),
 %change path
 path(P),
 (
     opposite_movement(yes);
     (
         append([turnleft], P, NewP),
         retract(path(P)),
         assert(path(NewP))

   )
 ).

track_rotate(_).




% track_scream(Past, Scream).
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
track_scream(_,yes):-
 % agent_loc(X, Y),
  assert(killer(yes)).

%no changes needed if agent didnt die
track_scream(_,_).
 % agent_loc(X, Y),
 % killer(no).



%% Our logical rules based on our Percepts
% A cell is safe if there's no wumpus or pit there
safe(X, Y):-
  no_wumpus(X, Y),
  no_pit(X, Y).

%shilpa
%not_safe(X, Y):-
 % has_wumpus(X, Y);
 % has_pit(X, Y).



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
maybe_wumpus(X,Y):-
 % maybe_wumpus(X,Y),
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
%shilpa_check


% If X, Y has two adjacent stenchs, but its diagonal doesn't have a wumpus,
% then X, Y must have the wumpus
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
%
has_wumpus(X,Y):-
    XUp #= X+1,
    XDown #= X-1,
    YLeft #= Y-1,
    YRight #=Y+1,
     (
    (has_stench(XUp, Y), has_stench(X, YLeft), no_wumpus(XUp, YLeft));
    (has_stench(X, YLeft), has_stench(XDown, Y), no_wumpus(XDown, YLeft));
    (has_stench(XDown, Y), has_stench(X, YRight), no_wumpus(XDown, YRight));
    (has_stench(X, YRight), has_stench(XUp, Y), no_wumpus(XUp, YRight))
  ).



% If there is a stench on either side of X, Y, then the wumpus must be there
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
has_wumpus(X,Y):-
  (   (has_stench(X-1,Y),    has_stench(X+1,Y) );
      (has_stench(X,Y-1),    has_stench(X,Y+1) )
  ).

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
maybe_pit(X,Y):-
 \+ has_pit(X,Y),
  (has_breeze(X-1,Y);
  has_breeze(X+1,Y);
  has_breeze(X,Y-1);
  has_breeze(X,Y+1)).


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
%shilpa_check A

% Angle 0 (North): Y must match and X >= XOrigin



%%%%%%%%%% Your Code Here %%%%%%%%%%
%Angle 1 (East): X must match and Y >= YOrigin


% Angle 2 (South): Y must match and X =< XOrigin
%%%%%%%%%% Your Code Here %%%%%%%%%%


 % assert(Y<=YOrigin, X=:=XOrigin)).


%%%%%%%%%% Your Code Here %%%%%%%%%%
% Angle 3 (West): X must match and Y =< YOrigin
%%%%%%%%%% Your Code Here %%%%%%%%%%

returnheadlist([Head|Tail],Head,Tail).
 % track_movement(_,_).



% Calculate rotations
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
%paremeters:(Xmovement,Ymovement, angle,action)
action_wrt_position( 0,  1,   0, goforward).
action_wrt_position( 1,  0,   1, goforward).
action_wrt_position(0,  -1,   2, goforward).
action_wrt_position(-1,  0,   3, goforward).

action_wrt_position( 1, 0,   0, turnright).
action_wrt_position( 0,  -1,  1, turnright).
action_wrt_position( -1,  0,   2, turnright).
action_wrt_position(0,  1,   3, turnright).

action_wrt_position(_,_,_,turnleft).

%shilpa
%To go to a particular coordinate X1,Y1
get_action_to_coordinate(X1,Y1, Action):-
  agent_loc(X,Y),
  agent_orient(A),
  Xdiff #= X1 - X,
  Ydiff #= Y1 - Y,
  action_wrt_position(Xdiff, Ydiff, A, Action).




% North
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
go_forward(X,Y,0,X,Y1):-
    Y1 is Y+1.


% East
%%%%%%%%%% Your Code Here %%%%%%%%%%
%%shilpa
go_forward(X,Y,1,X1,Y):-
    X1 is X+1.

% South
%%%%%%%%%% Your Code Here %%%%%%%%%%
%%shilpa
go_forward(X,Y,2,X,Y1):-
    Y1 is Y-1.


% West
%%%%%%%%%% Your Code Here %%%%%%%%%%
%%shilpa
go_forward(X,Y,3,X1,Y):-
    X1 is X-1.




% Look at what our step would be
%%%%%%%%%% Your Code Here %%%%%%%%%%


% We can't move anywhere :(
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
is_stuck(X,Y):-
  XUp #= X+1,
  XDown #= X-1,
  YRight #= Y+1,
  YLeft #= Y-1,
  (\+ no_pit(XUp, Y); \+ no_wumpus(XUp, Y); wall(XUp, Y)),
  (\+ no_pit(X, YRight); \+ no_wumpus(X, YRight); wall(X, YRight)),
  (\+ no_pit(XDown, Y); \+ no_wumpus(XDown, Y); wall(XDown, Y)),
  (\+ no_pit(X, YLeft); \+ no_wumpus(X, YLeft); wall(X, YLeft)),
   !.

% If number of safe and visited cells are equal
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
safe_visited_count():-
  setof((X, Y), safe(X,Y), SafeCells),
  length(SafeCells, SafeNum),
  setof((X1,Y1), seen(X1,Y1), SeenCells),
  length(SeenCells, SeenNum),
  SafeNum = SeenNum.



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
  (   all_explored,
    (stuck_turn_value(S), S > 3)
  ),
  agent_loc(X, Y),
  X =:= 1,
  Y =:= 1,
  Action=climb.

% Move forward to an unexplored space
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  agent_orient(A),
  go_forward(X, Y, A, X1, Y1),
  safe(X1, Y1),
  \+ seen(X1, Y1),
  write('MOVING FORWARD: to safe unexplored space'), nl,
  Action=goforward,!,
  % Reset the stuck counter since we can move, hence not stuck
  retractall(stuck_turn_value(_)),
  assert(stuck_turn_value(0)).

% If we're facing the wumpus, fire!
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  agent_orient(A),
  arrow(yes),
  go_forward(X,Y,A, XWumpus, YWumpus),
  has_wumpus(XWumpus,YWumpus),
  Action=shoot.


% If we're next to the wumpus and we have the arrow, face it!
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
    %wumpus is never behind agent, since agent moved from behind to forward
  agent_loc(X,Y),
  agent_orient(A),
  arrow(yes),
  has_wumpus(XWumpus, YWumpus),
  XMovement is XWumpus - X,
  YMovement is YWumpus - Y,
  action_wrt_position(XMovement, YMovement, A, Action).

% Turn towards unexplored space
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X, Y),
  agent_orient(A),
  % You can either turn left or right, when forward fails
  (
    (
      LeftTurn is (A+3) mod 4,
      go_forward(X, Y, LeftTurn, XLeft, YLeft),
      safe(XLeft, YLeft),
      \+ seen(XLeft, YLeft),
      write('Unexplored space present to the left'),nl,
      Action=turnleft
    );
    (
      RightTurn is (A + 1) mod 4,
      go_forward(X, Y, RightTurn, XRight, YRight),
      safe(XRight, YRight),
      \+ seen(XRight, YRight),
      write('Unexplored space present to the right'),nl,
      Action=turnright
    )
  ).


% If there's nowhere left to explore, go home
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  ( all_explored;
    (stuck_turn_value(S), S > 3) % If we've been stuck for 3 turns, ignore
  ),
  (
    (
      opposite_movement(no),
      % Optimize the path
      path_optimization,
      % Append a opposite turn for the path
      path(P),
      retract(path(P)),
      append([turnleft], P, NewP),
      assert(path(NewP)),
      retractall(opposite_movement(_)),
      assert(opposite_movement(yes)),
      write('Nothing left to explore'),nl,
      Action=turnleft % We need 2 left turns to go back. So we append both.
  );
    (
      opposite_movement(yes),
      path(P),
      returnheadlist(P,Action,PNew),
      retractall(path(_)),
      assert(path(PNew))
    )
  ).

% No new spot to explore and no wumpus to kill, Move forward if we can do so safely
% and without bumping a wall
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
 % all_explored,
  agent_loc(X, Y),
  agent_orient(A),
  go_forward(X, Y, A, X1, Y1),
  \+ wall(X1, Y1),
  safe(X1, Y1),
  Action=goforward.
% If we're stuck and facing where a wumpus might be, fire!
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  agent_orient(A),
  is_stuck(X, Y),
  go_forward(X, Y, A, XWumpus, YWumpus),
  maybe_wumpus(XWumpus, YWumpus),
  arrow(yes),
  write(' we are stuck and facing where a wumpus might be, so we fire'),
  Action=shoot.

% If we're stuck and out of ammo, we might've missed the wumpus, just leave :(
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X, Y),
  is_stuck(X, Y),
  arrow(no),
  killer(no),
  opposite_movement(no),
  path(P),
  retract(path(P)),
  append([turnleft], P, NewP),
  assert(path(NewP)),
  retractall(opposite_movement(_)),
  assert(opposite_movement(yes)),
  write('Stuck and out of ammo and Wumpus alive. just leave'),nl,
  Action=turnleft.

% If we're stuck and out of ammo, we might be surrounded by pits, just leave :(
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X, Y),
  is_stuck(X, Y),
  arrow(no),
  opposite_movement(no),
  path(P),
  retract(path(P)),
  append([turnleft], P, NewP),
  assert(path(NewP)),
  retractall(opposite_movement(_)),
  assert(opposite_movement(yes)),
  write('Stuck and out of ammo. might be surrounded by pits, just leave'),nl,
  Action=turnleft.

% If forward isn't safe or there's a wall,
% look to the left. If its safe and there's no wall,
% rotate left
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  agent_loc(X,Y),
  agent_orient(A),
  go_forward(X, Y, A, X1, Y1),
  (\+ safe(X1, Y1); wall(X1, Y1)),
   LeftTurn is (A + 3) mod 4,
  go_forward(X, Y, LeftTurn, XLeft, YLeft),
  safe(XLeft, YLeft),
  \+ wall(XLeft, YLeft),
  write('left:safe, forward: not safe'),nl,
  Action=turnleft.


% If there's literally nothing else to do, rotate right.
%%%%%%%%%% Your Code Here %%%%%%%%%%
%shilpa
get_action(Action):-
  write('Nothing to do, rotating right'),nl,
  stuck_turn_value(S),
  retractall(stuck_turn_value(_)),
  NewC is S + 1,
  assert(stuck_turn_value(NewC)),
  Action=turnright.


all_explored():-
  %forall(agent_loc(X,Y),
   %        (assert(seen(X,Y)))).
    opposite_movement(yes);
    safe_visited_count().



remove_turns([],[]).

remove_turns(Path,NewP):-
    returnheadlist(Path,Head,Tail),
  (
    % Remove pairs of left and right turns
    (Head=turnleft, returnheadlist(Tail,turnright,NewP));
    (Head=turnright, returnheadlist(Tail,turnleft,NewP))
  ).

path_optimization():-
  %Remove turns that are opposite once path taken
  path(P),
  retractall(path(_)),
  remove_turns(P,NewP),
  assert(path(NewP)).

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
  retractall(path(_)),
  retractall(stuck_turn_value(_)),
  retractall(opposite_movement(_)).


init_agent:-
  format('\n=====================================================\n'),
  format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),
  format('=====================================================\n\n'),
  display_world,
  reset,
  assert(agent_loc(1,1)),
  assert(agent_orient(0)), % Change the angle to 0 because we are measuring angles in degrees.
  assert(rich(no)),
  assert(arrow(yes)),
  assert(past(spawn)),
  assert(killer(no)),
  assert(no_wumpus(1,1)),
  assert(no_pit(1,1)),
  assert(opposite_movement(no)),
  assert(path([])),
  assert(stuck_turn_value(0)),
  assert(safe(1,1)).
%run_agent(Percept,Action):-
%run_agent(_, goforward ):-
 % format('\n=====================================================\n'),
 % format('This is run_agent(.,.):\n\t It gets called each time step.\n\tThis default one simply moves forward\n'),
  %format('You might find "display_world" useful, for your debugging.\n'),
  %display_world,
  %format('=====================================================\n\n').

run_agent(Percepts,Action):-
    format('\nExecuting custom agent......\n\n'),
   display_world,
  %sleep(1),
  past(P),
  track_percepts(P, Percepts),
  get_action(H),
  retract(past(P)),
  assert(past(H)),
  Action=H,
  format("\ndone....").



