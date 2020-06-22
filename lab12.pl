% A tic-tac-toe table is encoded as a list [ [X1,Y1,Z1], [X2,Y2,Z2], [X3,Y3,Z3] ] 
% where a variable is bound to atom x or z (zero)
% if the position has been played or it is uninstantiated otherwise. 

% 1. Write the predicate diags(Tbl,R) which computes the list R of diagonals in table Tbl.
diags_aux([],_,_,[],[]).
diags_aux([H|T], N, N2, [No1|S],[No2|S2]) :- nth0(N, H, No1), nth0(N2, H, No2),
								 			 Np1 is N + 1, Np2 is N2 - 1, 
											 diags_aux(T, Np1, Np2, S, S2).

diags([H|T], R) :- append([DiagPrinc],[DiagSec], R), length(H, L), Id2 is L - 1, 
				   diags_aux([H|T], 0, Id2, DiagPrinc, DiagSec).

% 2. Write the predicate cols(Tbl,R) which computes the list of columns in table Tbl. 
% Hint: use maplist/3. 
head([], []).
head([H|_], H).

tail([], []).
tail([_|T], T).

cols([[]|_], []).
cols(Table, [L|R]) :- maplist(head, Table, L),
				      maplist(tail, Table, Table2),
				      cols(Table2, R), !.