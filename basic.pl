:- module(basic, [xreverse/2, xunique/2, xunion/3, removeLast/3]).

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

   
xunion(L1, L2, L) :-
    append(L1, L2, ALL),
    xunique(ALL, L).


removeLast([X], [], X).

removeLast([H|T], [H|T1], Last) :-
    removeLast(T, T1, Last).
