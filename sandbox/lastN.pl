lastN(L, N, R) :-
    length(L, N),
    R = L.

lastN([H|T], N, R) :-
    length([H|T], Nt),
    Nt > N,
    lastN(T, N, R).
