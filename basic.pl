last([X], X).

last([H|T], X) :- last(T, X).

xreverse([X], [X]).

xreverse([X, Y], [Y, X]).

%xreverse(append(L, [X], [
