xlast([X], X).

xlast([_|T], X) :- xlast(T, X).


xreverse([], []).

xreverse([H|T], R) :- 
    xreverse(T, Rt), 
    append(Rt, [H], R).


% Base case, TODO one element or empty.
xunique([], []).

% Eliminate from U if not in T
xunique([H|T], U) :-
    select(H, U, Up),
    xmember(H, T),
    xunique(T, U).

xunique([H|T], U) :-
    select(H, U, Up),
    xnotmember(H, T),
    xunique(T, Up).


xnotmember(X, []).

xnotmember(X, [H|T]) :-
    X \= H,
    xnotmember(X, T).


xmember(X, [X|_]).

xmember(X, [_|T]) :- xmember(X, T).
    
