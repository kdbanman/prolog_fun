xlast([X], X).

xlast([_|T], X) :- xlast(T, X).


xreverse([X], [X]).

xreverse([H|T], R) :- 
    xreverse(T, Rt), 
    append(Rt, [H], R).


xunique([X], [X]).

xunique([H|T], [H|U]) :-
    xunique(T, U).

/*
xunique(L, U) :-
    xunique_accum(L, U, []).

xunique_accum([X], [X], _).

xunique_accum([H|T], [H|U], A) :-
    xunique_accum(T, U, [H|A]).
*/

/*
xunique_accum([H1|T], [H2|U], A) :-
    H1 \= H2,
    xmember(H1, A),
    xunique_accum(T, [H2|U], A).
*/

xmember(X, [X|_]).

xmember(X, [_|T]) :- xmember(X, T).
    
