:- module(basic, [xreverse/2, xunique/2, xunion/3]).

xlast([X], X).

xlast([_|T], X) :- xlast(T, X).


xreverse([], []).

xreverse([H|T], R) :- 
    xreverse(T, Rt), 
    append(Rt, [H], R).


xunique([], []).

xunique([H|T], [H|U]) :-
    delete(T, H, Tp),
    xunique(Tp, U).

   
xunion([], [], []).

xunion(L1, [], L) :-
    xunique(L1, L).

xunion([], L2, L) :-
    xunique(L2, L).
