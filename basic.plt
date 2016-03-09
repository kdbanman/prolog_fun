
:- use_module(basic).

:- begin_tests(basic).
    test(1) :-
        \+ xreverse([b,b], [b,a]).
    test(2) :-
        xreverse([a,b], [b,a]).
:- end_tests(basic).

