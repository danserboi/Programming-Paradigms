nat(zero).
nat(X) :- X = succ(Y), nat(Y).

toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

% 1. Implement the predicate add/3 which adds two natural numbers

add(zero,Y,Y).
add(succ(X),Y,succ(Res)) :- add(X,Y,Res).

% toNat(5,X), toNat(3,Y), add(X, Y, S), fromNat(S,R).

% 2. Implement the predicate minus/3 which substracts one natural number from another. 
% Substraction only needs to work as long as the result is a natural number. 

minus(Y, zero, Y).
minus(succ(X),succ(Y), Res) :- minus(X,Y,Res).

% toNat(5,X), toNat(3,Y), minus(X, Y, S), fromNat(S,R).

% 3. Implement min/3 which computes the minimum of two natural numbers. Example usage: 
% -? toNat(7,X), toNat(4,Y), min(X,Y,R), fromNat(R,O).

min(zero, _, zero).
min(_, zero, zero).
min(succ(X), succ(Y), R) :- min(X,Y,Rez), R = succ(Rez).

% 4. Implement max/3 which computes the maximum of two natural numbers. 
max(zero, Y, Y).
max(X, zero, X).
max(succ(X), succ(Y), R) :- max(X,Y,Rez), R = succ(Rez).

% 5. Implement gt/2 which is satisfied if the first natural number is strictly greater than the second.

gt(X, zero) :- X \= zero.
gt(succ(X), succ(Y)) :- gt(X,Y).

% 6. Implement leq/2 which is satisfied if the first natural number is less than or equal to the second. 

leq(zero, _).
leq(succ(X), succ(Y)) :- leq(X,Y).

% 7. Implement div/3 which implements the div operator.

div(X, Y, R) :- gt(Y, X), R = zero.
div(X, Y, succ(Res)) :- gt(X, Y), minus(X, Y, S), div(S,Y,Res).

% 8. Implement mod/3. 

mod(X, Y, R) :- gt(Y, X), R = X.
mod(X, Y, Res) :- gt(X, Y), minus(X, Y, S), mod(S,Y,Res).

%  9. Implement gcd/3 which computes the greatest common divisor of two natural numbers.

gcd(X, zero, Res) :- Res = X.
gcd(zero, Y, Res) :- Res = Y.
gcd(X, Y, Res) :- leq(X, Y),  minus(Y, X, S), gcd(S,X,Res).
gcd(X, Y, Res) :- gt(X, Y), minus(X, Y, S), gcd(S,Y,Res).


% Consider the following representation of lists, expressed by the predicate isList shown below.

isList(void).
isList(cons(_,T)) :- isList(T).

% 10. Implement head/2 and tail/2. 

head(cons(H,_), H).

tail(cons(_,T), T).

% 11. Implement size/2 which determines the length of a list as a Prolog integer. 

size(void, 0).
size(cons(_,T), Rp) :- size(T,R), Rp is R + 1.

% 12. Implement concat/3 which concatenates two lists.

concat(void,L,L).
concat(cons(H,T),L, cons(H,Rp)) :- concat(T,L,Rp).

% 13. Implement reverse/2 which reverses a list. 

% Use accumulators instead of concatenation and an auxiliary predicate. 

% cand ajung sa nu mai am niciun element in lista, obtin rezultatul
rev(void,R,R).
% altfel adaug H la accumulator
rev(cons(H,T),R,Acc) :- rev(T,R,cons(H, Acc)).

reverse(L, R) :- rev(L, R, void).

% Consider the following conversion functions from the above list representation to Prolog lists. 
fromList(void,[]).
fromList(cons(H,T),[H|R]) :- fromList(T,R).

% 14. Write the predicate toList/2 which converts a Prolog list to our list representation. 

toList([],void).
toList([H|T], cons(H,R)) :- toList(T,R).

% 15. Implement kelem/3 which determines the k-th element of a Prolog list. 

kelem([H|_], 1, H).
kelem([_|T], N, R) :- Np is N-1, kelem(T, Np, R).

% 16. Implement rem/2 which removes consecutive duplicates from a Prolog list. 
rem([],[]).
% adaug head-ul la lista rezultat doar daca urmatorul element din lista prelucrata e diferit
rem([H|T], [H|T1]) :- T \= [H|_], rem(T, T1).
% altfel, nu il adaug
rem([H,H|T], L) :- rem([H|T], L).

/*
-? rem([1,1,1,2,2,3,4,4],R).
R = [1,2,3,4].
*/

% 17. Implement flatten/2 which flattens nested Prolog lists. Do not use auxiliary predicates. 
flatten([], []).
flatten([[H|T1]|T2], R) :- flatten([H|[T1|T2]], R).
flatten([[]|T], R) :- flatten(T, R).
flatten([H|T], [H|R]) :- H \= [], H \= [_|_], flatten(T, R).
/*
?- flatten([1,2,[3,4,[5]], [[6],[]], [7]], R).
*/

%  18. Implement pack/2 which groups consecutive duplicates into sublists.
pack([], []).
pack([H|T], [[H]|R]) :- T \= [H|_], pack(T, R).
pack([H,H|T], [[H|RH]|RT]) :- pack([H|T], [RH|RT]).

/*
?- pack([1,1,1,2,3,3,1,1,4,5,5,5,5],R).
 R = [[1,1,1],[2],[3,3],[1,1],[4],[5,5,5,5]]
 */

 %  19. Implement slice/4 which returns the elements of a list between two positions.
slice(_, 0, -1, []).
slice([H|T], 0, I, [H|R]) :- I >= 0, Ip is I-1, slice(T, 0, Ip, R), !.
slice([_|T], S, I, R) :- S >= 0, I >= 0, Sp is S-1, Ip is I-1, slice(T, Sp, Ip, R), !.
/*
?- slice([1,2,3,4,5,6,7,8,9,10],3,7,R).
R = [4,5,6,7,8]
*/

%  20. Implement rotate/3 which rotates a list by a given number of positions. 
/*
?- rotate([1,2,3,4,5,6,7,8],3,R).
R = [4,5,6,7,8,1,2,3]
*/
% Rotirea inseamna impartirea listei initiala in 2 liste in care prima are lungimea N
% si care se va inversa cu cea de-a doua
rotate(List, N, Res) :- append(Lleft, Lright, List), append(Lright, Lleft, Res), length(Lleft, N), !.