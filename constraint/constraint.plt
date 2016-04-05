
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

    test(custDomain_1) :-
        trivial(3).

:- end_tests(constraint).
