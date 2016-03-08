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

   

