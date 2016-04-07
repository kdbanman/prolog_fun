
:- use_module(constraint).

    
:- begin_tests(constraint).

    test(fourSquares_1) :-
        fourSquares(0, [0,0,0,0]).
    test(fourSquares_2) :-
        fourSquares(1, [0,0,0,1]).
    test(fourSquares_3) :-
        fourSquares(20, [0,0,2,4]).
    test(fourSquares_4) :-
        fourSquares(20, [1,1,3,3]).

    test(disarm_1, [nondet]) :-
        disarm([1,5], [6], [[[1,5], [6]]]).
    test(disarm_2, [nondet]) :-
        disarm([1,5,2], [6,1,1], [[[2], [1,1]],[[1,5], [6]]]).
    test(disarm_3) :-
        \+ disarm([1,5,2], [6,1,1], [[[1,5], [6]], [[2], [1,1]]]).
    test(disarm_3) :-
        \+ disarm([1,5,3], [6,1,1], _).
    test(disarm_4) :-
        \+ disarm([1,5,2], [6,2,1], _).

:- end_tests(constraint).
