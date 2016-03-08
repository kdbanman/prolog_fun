xlast([X], X).

xlast([_|T], X) :- xlast(T, X).


xreverse([], []).

xreverse([H|T], R) :- 
    xreverse(T, Rt), 
    append(Rt, [H], R).


% Base case, TODO one element or empty.
xunique([], []).

% If the head of the list is repeated in the tail, do not alter the 
% unique member list.
xunique([H|T], U) :-
    xmember(H, T),
    xunique(T, U).

% If the head of the list is not repeated in the tail
xunique([H|T], U) :-
    xnotmember(H, T),
    select(H, U, Up),
    xunique(T, Up).


xu([], [], _).

% If the head is not repeated in the tail, drop it from the unique members.
xu([H|T], [H|U], A) :-
    xnotmember(H, T),
    append(A, [H], Ap),
    xu(T, U, Ap).

% If the head is repeated in the tail, don't drop it from the unique members.
xu([H|T], [H|U], A) :-
    xmember(H, T),
    xu(T, [H|U], A).

xu([H|T], U, A) :-
    xmember(H, A),
    xu(T, U, A).

xu([H|T], U, A) :-
    xmember(H, U),
    xu(T, U, A).


xnotmember(X, []).

xnotmember(X, [H|T]) :-
    X \= H,
    xnotmember(X, T).


xmember(X, [X|_]).

xmember(X, [_|T]) :- xmember(X, T).
    
