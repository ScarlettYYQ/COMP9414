% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N)  :- 
% insert_legs(), head_member(), build_path()
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal_g(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (state_s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).







% pathsearch.pl

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% This file provides code for insert_legs(), head_member() and build_path()
% used by bfsdijkstra(), ucsdijkstra(), greedy() and astar().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).





% Question 1
% set the different distance of land and sea,and select location of seas to the list.In base case we assert the initial location  
% and confirm the location of monster,then choose the goals to list of intents.
% If goal(X1,Y1) is land, We set the cost of passing this point to 1.


mandist(X/Y, X1/Y1, D) :-      % D is Manhattan Dist between two positions
    diff(X, X1, Dx),
    diff(Y, Y1, Dy),
    D is Dx + Dy.

diff(A, B, D) :-                % D is |A-B|
    D is A-B, D >= 0, !.

diff(A, B, D) :-                % D is |A-B|
    D is B-A.


state_s(goal(X1,Y1),goal(X2,Y2),1):-
	land_or_dropped(X2,Y2),
	mandist(X1/Y1,X2/Y2,1).
% If goal(X1,Y1) is land, We set the cost of passing this point to 10000.
state_s(goal(X1,Y1),goal(X2,Y2),1000):-
    X_minus is X1-1,
    X_plus is X1+1,
    Y_minus is Y1-1,
    Y_plus is Y1+1,
	between(Y_minus,Y_plus,Y2),
	between(X_minus,X_plus,X2),
	not(land(X2,Y2)),
	mandist(X1/Y1,X2/Y2,1).


% remove the coordinates of land.
%Only the coordinates in the water and the coordinates of the monster
intentions_i([],[]).
intentions_i([goal(X,Y)|Path],I):-
	not(land(X,Y)),
	I = [[goal(X,Y),[]]|I_new],
	intentions_i(Path,I_new).
intentions_i([goal(X,Y)|Path],I):-
	land(X,Y),
	I = I,
	intentions_i(Path,I).

%Base case
initial_intentions(intents(L,[])):-
    monster(X0,Y0),
    assert(goal_g(goal(1,1))),
	solve(goal(X0,Y0),Path,_,_),
	intentions_i(Path,I),
    retractall(goal_g(goal(1,1))),
	L = I.
%Question 2
% Write a Prolog procedure trigger(Percepts, Goals) which takes lists of location of stones 
% and add these coordinates to the goal list.
%Base case

trigger([], []).
trigger(Percepts, Goals):-
	Percepts=[Head_p|Tail_p],
	Head_p=stone(X1,Y1),
	Head_g=goal(X1,Y1),
    Goals=[Head_g|Goals_new],
    trigger(Tail_p,Goals_new).

% write rules find_G() to find the feasible goals which are not seas and use insert_new_goal() to arrange these goals
% in decreasing order, and then use incorporate_goals to add goals to intentions list.
%Base case
incorporate_goals([],Intentions_new , Intentions_new):-!.
incorporate_goals([Goals_Head|Goals_Tail], intents(Int_drop,Int_pick), Intentions_new):-
    agent_at(X0,Y0),
    assert(goal_g(goal(X0,Y0))),
    find_G(Goals_Head,Int_pick,New_pick),
    retractall(goal_g(goal(X0,Y0))),
    Intentions_new=Intentions_temp,
    incorporate_goals(Goals_Tail, intents(Int_drop,New_pick), Intentions_temp).

find_G(Goals_Head,Int_pick,New_pick):-
    find_old_solve(Goals_Head,_Path,New_G),
    New_G=<1000,
    insert_new_goal(Goals_Head,New_G,Int_pick,New_pick),!.
find_G(Goals_Head,Int_pick,New_pick):-
    find_old_solve(Goals_Head,_Path,New_G),
    New_G>1000,
    New_pick=Int_pick.

insert_new_goal(Goals_Head,_,[[Goals_Head,Goals_plan]|Tail],[[Goals_Head,Goals_plan]|Tail]).
insert_new_goal(Goals_Head,_,[],[[Goals_Head,[]]]).
insert_new_goal(Goals_Head,New_G,[[Goals_diff,Goals_plan_diff]|Tail],New_pick):-
    find_old_solve(Goals_diff,_Path,G),
    G > New_G,
    New_pick=[[Goals_Head,[]]|[[Goals_diff,Goals_plan_diff]|Tail]].
insert_new_goal(Goals_Head,New_G,[[Goals_diff,Goals_plan_diff]|Tail],New_pick):-
    find_old_solve(Goals_diff,_Path,G),
    G =< New_G,
    New_pick=[[Goals_diff,Goals_plan_diff]|Tail_next],
    insert_new_goal(Goals_Head,New_G,Tail,Tail_next).

% a function to call solve function and determine if the target and start coordinates are in the same location.
find_old_solve(goal(X,Y),Path,G):-
    not(agent_at(X,Y)),
    solve(goal(X,Y),Path,G,_N),!.
find_old_solve(goal(X,Y),Path,G):-
    agent_at(X,Y),
    Path=[],
    G=0.

%Question 4
%The aim of the base case is to catch the next action of agent and path in order to pick 
% or drop the stone using get_action(), we set append_function() to add list, and write pick_plan and drop_plan
% respectively in order to add feasible coordinates to the list. And we use move_one_point() to solve the situation of
% stone staying at agent_point,moving one point in order to pick the stone.
% change goal(_,_) to move(_,_).
find_all_move([],[]).
find_all_move([goal(X,Y)|Plan_Tail],Move_Plan):-
    Move_Plan=[move(X,Y)|New_move_plan],
    find_all_move(Plan_Tail,New_move_plan).

append_function([],X,X).
append_function([Head|Tail],X,[Head|Tail1]) :-
    append_function(Tail,X,Tail1).

%Base case
get_action(intents(Int_drop,Int_pick), intents(Int_drop_new,Int_pick_new), Action):-
    agent_stones(0),
    agent_at(X,Y),
    assert(goal_g(goal(X,Y))),
    pick_plan(Int_pick,Int_pick_new,Action),
    Int_drop_new=Int_drop,
    retractall(goal_g(goal(X,Y))),!.

get_action(intents(Int_drop,Int_pick), intents(Int_drop_new,Int_pick_new), Action):-
    agent_stones(1),
    agent_at(X,Y),
    assert(goal_g(goal(X,Y))),
    drop_plan(Int_drop,Int_drop_new,Action),
    Int_pick_new = Int_pick,
    retractall(goal_g(goal(X,Y))),!.


pick_plan([],[],Action):-
    agent_at(X,Y),
    Action=move(X,Y).
pick_plan([[goal(X,Y),[Next_action|Tail_plan]]|Tail_pick],[[goal(X,Y),New_plan_Tail]|Tail_pick],Action):-
    (applicable(Next_action)
    ->Action=Next_action,
    New_plan_Tail=Tail_plan
    ;find_old_solve(goal(X,Y),Path,_G),
    append_function([_Agent_point|Right_path_no_goal],[_Goal_point],Path),
    find_all_move(Right_path_no_goal,Move_Plan_no_goal),
    append_function(Move_Plan_no_goal,[pick(X,Y)],[Action|Move_Plan_Tail]),
    New_plan_Tail=Move_Plan_Tail
    ).
pick_plan([[goal(X,Y),[]]|Tail_pick],[[goal(X,Y),New_plan_Tail]|Tail_pick],Action):-
    find_old_solve(goal(X,Y),Path,_G),
    Path\=[],
    append_function([_Agent_point|Right_path_no_goal],[_Goal_point],Path),
    find_all_move(Right_path_no_goal,Move_Plan_no_goal),
    append_function(Move_Plan_no_goal,[pick(X,Y)],[Action|Move_Plan_Tail]),
    New_plan_Tail=Move_Plan_Tail.
% if agent_at(X,Y)=stone(X,Y)
pick_plan([[goal(X,Y),[]]|Tail_pick],[[goal(X,Y),New_plan_Tail]|Tail_pick],Action):-
    find_old_solve(goal(X,Y),Path,_G),
    Path=[],
    move_one_point(goal(X,Y),Move_one),
    Action=Move_one,
    New_plan_Tail=[pick(X,Y)].


move_one_point(goal(X,Y),Move_one):-
    X+1<10,
    X0 is X+1,
    land_or_dropped(X0,Y),
    Move_one=move(X0,Y),!.
move_one_point(goal(X,Y),Move_one):-
    X-1>0,
    X0 is X-1,
    land_or_dropped(X0,Y),
    Move_one=move(X0,Y),!.
move_one_point(goal(X,Y),Move_one):-
    Y+1<10,
    Y0 is Y+1,
    land_or_dropped(X,Y0),
    Move_one=move(X,Y0),!.
move_one_point(goal(X,Y),Move_one):-
    Y-1<10,
    Y0 is Y-1,
    land_or_dropped(X,Y0),
    Move_one=move(X,Y0).


drop_plan([],[],Action):-
    agent_at(X,Y),
    Action=move(X,Y).
drop_plan([[goal(X,Y),[Next_action|Tail_plan]]|Tail_pick],[[goal(X,Y),New_plan_Tail]|Tail_pick],Action):-
    (applicable(Next_action)
    ->Action=Next_action,
    New_plan_Tail=Tail_plan
    ;find_old_solve(goal(X,Y),Path,_G),
    append_function([_Agent_point|Right_path_no_goal],[_Goal_point],Path),
    find_all_move(Right_path_no_goal,Move_Plan_no_goal),
    append_function(Move_Plan_no_goal,[drop(X,Y)],[Action|Move_Plan_Tail]),
    New_plan_Tail=Move_Plan_Tail
    ).
drop_plan([[goal(X,Y),[]]|Tail_pick],[[goal(X,Y),New_plan_Tail]|Tail_pick],Action):-
    find_old_solve(goal(X,Y),Path,_G),
    append_function([_Agent_point|Right_path_no_goal],[_Goal_point],Path),
    find_all_move(Right_path_no_goal,Move_Plan_no_goal),
    append_function(Move_Plan_no_goal,[drop(X,Y)],[Action|Move_Plan_Tail]),
    New_plan_Tail=Move_Plan_Tail.

%Question 5
% Update three posibilities of action in different ways,if agent do nothing, change nothing;
% if agent dropped stone, delete first drop location; if agent picked stone, delete first pick location.

update_intentions(at(_,_),intents(Int_Drop,Int_Pick),intents(Int_Drop,Int_Pick)).
update_intentions(dropped(_,_),intents([_|Tail],Int_Pick),intents(Tail,Int_Pick)).
update_intentions(picked(_,_),intents(Int_Drop,[_|Tail]),intents(Int_Drop,Tail)).