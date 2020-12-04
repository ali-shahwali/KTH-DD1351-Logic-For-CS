verify(Input) :-
  see(Input),
  read(T),
  read(L),
  read(S),
  read(F),
  seen,
  check(T, L, S, [], F).
  
  % check(T, L, S, U, F)
  % T - The transitions in form of adjacency lists
  % L - The labeling
  % S - Current state
  % U - Currently recorded states
  % F - CTL Formula to check.
  
% Literals
check(_, L, S, [], X) :-
  member([S, LabelList], L),
  member(X, LabelList).

% NEG
check(_, L, S, [], neg(X)) :-
  member([S, LabelList], L),
  \+ member(X, LabelList).

% OR
check(T, L, S, [] , or(X, Y)) :-
  check(T, L, S , [], X);
  check(T, L, S , [], Y).

% And
check(T, L, S, [], and(X, Y)) :-
  check(T, L, S , [], X),
  check(T, L, S , [], Y).


%%% For all next states
% AX 
check(T, L, S, [], ax(X)) :-
  check_transAll(T, L, S, [], X).

% AG 1.
check(_, _, S, U, ag(_)) :-
  member(S, U).

% AG 2.
check(T, L, S, U, ag(X)):-
  \+member(S, U),                                         %s får inte tillhöra U.
  check(T, L, S, [], X),                %starttillstånd
  check_transAll(T, L, S, [S|U], ag(X)).

% AF 1,
check(T, L, S, U, af(X)):-
  \+member(S,U),                                        % s får inte tillhöra U.
  check(T, L, S, [], X).                          % kollar att vi har premissen.

% AF 2
check(T, L, S, U, af(X)):-
  \+member(S, U),
  check_transAll(T, L, S, [S|U], af(X)).           % bygger up


%%% For some next states
% EX
check(T, L, S, [], ex(X)) :-
  check_transSome( T, L, S, [], X).

% EG1
check(_, _, S, U, eg(_)) :-
  member(S, U).

% EG2
check(T, L, S, U, eg(X)):-
  \+member(S, U),
  check(T, L, S, [], X),
  check_transSome(T, L, S, [S|U], eg(X)).

% EF1
check(T, L, S, U, ef(X)):-
  \+member(S, U),
  check(T, L, S, [] , X).

% EF2
check(T, L, S, U, ef(X)):-
  \+member(S, U),
  check_transSome(T, L, S, [S|U], ef(X)).



%%%%% "For all" transitions
check_transAll(T, L, S, U, F) :-
  member([S, AdjList], T),               % kollar om det finns en lista i T där AdjList innehåller alla states som nuvarande state S kan gå till
  check_statesAll(T, L, U, F, AdjList).

% kollar alla states som current state kan gå till. AdjList itereras igenom
check_statesAll(_, _, _, _, []).
check_statesAll(T, L, U, F, [Head|Tail]) :-
  check(T, L, Head, U, F),              % skickar vidare för att verifiera current state H, försöker matcha nästkommande states
  check_statesAll(T, L, U, F, Tail).    % loopar runt alla states i AdjList



%%%%% "For some" transition
check_transSome(T, L, S, U, F) :-
  member([S, AdjList], T),              % kollar att det finns en lista i T som där N innehåller alla states som scurrent state kan gå till
  check_statesSome(T, L, U, F, AdjList),!.

%% kollar något state som nuvarande state kan gå till, AdjList itereras igenom
check_statesSome(T, L, U, F, [Head|Tail]) :-
  check(T, L, Head, U, F);              % skickar vidare för att verifiera current state H. försöker matcha något nästkommande states
  check_statesSome(T, L, U, F, Tail),!. % annars loopar man runt alla states i AdjList tills man hittar nåt
