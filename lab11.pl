/*
 1. Write the predicate sublist/2 which constructs each sublist of a given list. 
 Hint: use append.
 2. Rewrite the predicate such that the empty list is reported only once. 
?- sublist([7,2,9],R).
% R will subsequently be bound to [], [7], [2], [9], [7,2], [2,9], [7,2,9] 
*/
/*
VARIANTA IMPERATIVA
sublist(_,[]).
sublist(L,R) :- append(R,S, L), R \= [], S \= [].
sublist(L,R) :- append(S,R, L), R \= [], S \= [].
sublist(L,L).
*/
% VARIANTA GENERATIVA
sublist(_,[]).
sublist(L,[H|T]) :- append(R1,_,L), append(_,[H|T],R1).
/*
 3. Write the predicate natlist/1 which generates each finite list 
 of natural number in ascending order.
?- natlist(R).
R = [0] ;
R = [0,1] ;
R = [0,1,2] ; 
...
*/
natlist([0]).
natlist(R) :- natlist(L), reverse(L,[H2|T]), H1 is H2 + 1, reverse([H1|[H2|T]],R).
/*
 4. Write the predicate oddOnly/2 which removes all even integers from a list.

?- evenOnly([1,2,3,4],R).
R = [2,4].
*/
evenOnly([],[]).
evenOnly([H|T], [H|R]) :- 0 is mod(H,2), evenOnly(T, R).
evenOnly([H|T], R) :- 1 is mod(H,2), evenOnly(T,R).

% 5. Write the predicate oddList/1 which 
% generates all finite lists of odd numbers, in ascending order.
oddList([1]).
oddList(R) :- oddList(L), reverse(L,[H2|T]), H1 is H2 + 2, reverse([H1|[H2|T]],R).
/*
 6. Write a predicate eqelem/1 which generates all lists of 
 uninstantiated variables which must be equal. Example:
?- eqelem(L), length(L,3), L=[0|_].
L = [0,0,0].
*/
eqelem([]).
eqelem([H|T]) :- eqelem(X), append(X, [H], [H|T]).

% 7. Use the previous predicate to write repeat/3 
% which returns a list containing the same value X repeated K times.
repeat(N, X, L) :- eqelem(L), length(L,N), L=[X|_].

% 8. Write a predicate pal/1 which generates all lists of palindromes.
% Hint: use only append.
pal([]).
pal([_]).
pal([H|T]) :- append(Tp,[H],T), pal(Tp).

% 9. Write a predicate ksubset/3 where ksubset(C,K,V) 
% generates all sets (represented as lists) C with K elements from the list V. 
% Hint: build the recursion scheme after K. 
ksubset([], 0, _).
ksubset([A|NTail], N, [X|L]):- member(A,[X|L]), Np is N - 1, ksubset(NTail, Np, L), 
							   not(member(A, NTail)).

% 10. Write a predicate subset where subset(C,V) generates all subsets of V.
% Hint: build the recursion scheme after each element in V. 
subset([X|R],[X|T]) :- subset(R,T), not(member(X,R)).
subset(R,[_|T]) :- subset(R,T).

% 11. What is the difference between ?- length(C,K), subset(C,V). and ?- ksubset(C,K,V).? 
% where K is an instantiated variable (with a value less or equal to the size of V). 
% Write your answer down. 
% combinari de k luate cate n vs aranjamente de k luate cate n

% 12. Implement connected/4, where connected(X,Y,P,G)
% generates all paths P between nodes X and Y in graph G.

edge(X,Y,[_,E]) :- member([X,Y],E).
edge(X,Y,[_,E]) :- member([Y,X],E).

connected2(X,Y,G,[X,Y],Visited) :- not(member(Y,Visited)), edge(X,Y,G).
connected2(X,Y,G,[X|Path],Visited) :- edge(X,Z,G), not(member(Z,Visited)),
                                      connected2(Z,Y,G,Path,[Z|Visited]).

connected(X,Y,G,[X|Path]) :- connected2(X,Y,G,[X|Path],[X]).
