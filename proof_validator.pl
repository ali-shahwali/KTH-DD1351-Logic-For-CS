%%% Verifera om bevis är sann eller falskt

verify(InputFileName) :- 
	see(InputFileName),
	read(Prems), 
	read(Goal), 
	read(Proof),
	seen,
	valid_proof(Prems, Goal, Proof,[]),
	member(Goal, PrevLines),
	write('predikatet uppfyllt').

%%% Kontrollerar om varje steg i bevis är korrekt

valid_proof(_, _, [], _).
valid_proof(Prems, Goal, [Head|Tail], PrevLines) :-
	check_line(Prems, Goal, Head, PrevLines),
	valid_proof(Prems, Goal, Tail, [Head|PrevLines]).

%%% Check line används för att kolla alla raders bevisregler 

%%% Kontrollerar om premiss finns i "premiss-listan" 
check_line(Prems, _, [_, X, premise], _) :-
	member(X, Prems).

%%% and / and-introduction / and-elimination
check_line(_, _, [_, and(X,Y), andint(I,J)], PrevLines) :-
	member([I, X, _], PrevLines),
	member([J, Y, _], PrevLines).


%%% implies / implies introduction / implies elimination
check_line(_, _, [_, imp(X,Y), impint(I,J)], PrevLines) :-
	member(Lista1, PrevLines),
	member([I, X, assumption], Lista1),
	member([J, Y, _], Lista1).

%%% assumption (open box)
check_line(Prems, Goal, [[_,_,assumption]|Tail], PrevLines) :-
	valid_proof(Prems, Goal, Tail,[[_, _, assumption]|PrevLines]).






		

