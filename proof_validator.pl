%%% Verifera om bevis är sann eller falskt

verify(InputFileName) :- 
	see(InputFileName),
	read(Prems), 
	read(Goal), 
	read(Proof),
	seen,
	check_goal(Goal, Proof),
	valid_proof(Prems, Goal, Proof,[]),
	write('predikatet uppfyllt').

check_goal(Goal, [[_ ,X, _]|[]]):-
	Goal = X.
  
check_goal(Goal,[_|Tail]):-
	check_goal(Goal,Tail).

%%%%%%%%%% Kontrollerar om varje steg i bevis är korrekt

valid_proof(_, _, [], _).

valid_proof(Prems, Goal, [Head|Tail], PrevLines) :-
	check_line(Prems, Goal, Head, PrevLines),
	valid_proof(Prems, Goal, Tail, [Head|PrevLines]).

%%% Check line används för att kolla alla raders bevisregler 

%%% premise
check_line(Prems, _, [_, X, premise], _) :-
	member(X, Prems).

%%% and / and-introduction / and-elimination
check_line(_, _, [_, and(X,Y), andint(I,J)], PrevLines) :-
	member([I, X, _], PrevLines),
	member([J, Y, _], PrevLines).

check_line(_, _, [_, X, andel1(I)], PrevLines) :-
	member([I, and(X,_), _], PrevLines).

check_line(_, _, [_, X, andel2(I)], PrevLines) :-
	member([I, and(_,X), _], PrevLines).

%%% implies / implies introduction / implies elimination
check_line(_, _, [_, imp(X,Y), impint(I,J)], PrevLines) :-
	member(AList, PrevLines),
	member([I, X, assumption], AList),
	member([J, Y, _], AList).

check_line(_, _, [_, X, impel(I,J)], PrevLines) :-
	member([I, Y, _], PrevLines),
	member([J, imp(Y,X), _], PrevLines).

%%% or / or-introduction / or-elimination
check_line(_, _, [_, or(X,_), orint1(I)], PrevLines) :-
	member([I, X, _], PrevLines).

check_line(_, _, [_, or(_,Y), orint2(I)], PrevLines) :-
	member([I, Y, _], PrevLines).

check_line(_, _, [_, X, orel(I, J, K, U, V)], PrevLines) :-
	member(AList1, PrevLines),
	member(AList2, PrevLines),
	member([I, or(A,B), _], PrevLines),
	member([J, A, assumption], AList1),
	member([K, X, _], AList1),
	member([U, B, assumption], AList2),
	member([V, X, _], AList2).

%%% negation / negation-introduction / negation-elimination / double negation elimination/introduction
check_line(_, _, [_, X, negnegel(I)], PrevLines) :-
	member([I, neg(neg(X)), _], PrevLines).

check_line(_, _, [_, neg(neg(X)), negnegint(I)], PrevLines) :-
	member([I, X, _], PrevLines).

check_line(_, _, [_, cont, negel(I,J)], PrevLines) :-
	member([I, X, _], PrevLines),
	member([J, neg(X), _], PrevLines).

check_line(_, _, [_, neg(X), negint(I,J)], PrevLines) :-
	member(AList, PrevLines),
	member([I, X, assumption], AList),
	member([J, cont, _], AList).

%%% copy
check_line(_, _, [_, X, copy(I)], PrevLines):-
	member([I, X, _], PrevLines).

%%% contradiction / contradiction elimination
check_line(_, _, [_, _, contel(I)], PrevLines) :-
	member([I, cont, _], PrevLines).

%%% modus tollens
check_line(_, _, [_, neg(X), mt(I,J)], PrevLines) :-
	member([I, imp(X,Y), _], PrevLines),
	member([J, neg(Y), _], PrevLines).

%%% proof by contradiction
check_line(_, _, [_, neg(X), pbc(I,J)], PrevLines) :-
	member(AList, PrevLines),
	member([I, X, assumption], AList),
	member([J, cont, _], AList).

check_line(_, _, [_, X, pbc(I,J)], PrevLines) :-
	member(AList, PrevLines),
	member([I, neg(X), assumption], AList),
	member([J, cont, _], AList).

%%% law of excluded middle
check_line(_, _, [_, or(X, neg(X)), lem], _).


%%% assumption (open box)
check_line(Prems, Goal, [[_,_,assumption]|Tail], PrevLines) :-
	valid_proof(Prems, Goal, Tail,[[_, _, assumption]|PrevLines]).
