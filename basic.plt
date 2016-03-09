
:- use_module(basic).

:- begin_tests(basic).
    test(reverse_1) :-
        \+ xreverse([b,b], [b,a]).
    test(reverse_2) :-
        xreverse([a,b], [b,a]).
    test(reverse_3) :-
        xreverse([], []).
    test(reverse_4) :-
        xreverse([1], [1]).
    test(reverse_5) :-
        xreverse([1, 2, [1, 2, 3]], [[1, 2, 3], 2, 1]).

    test(unique_1) :-
        xunique([], []).
    test(unique_2) :-
        xunique([1], [1]).
    test(unique_3) :-
        xunique([1,1,2,1,3,2,3,3,3,1,2], [1,2,3]).

    test(union_1) :-
        xunion([], [], []).
    test(union_2) :-
        xunion([1, 1, 2, 1], [], [1, 2]).
    test(union_3) :-
        xunion([], [2, a], [2, a]).
    test(union_4) :-
        xunion([1], [2], [1, 2]).
    test(union_5) :-
        xunion([1, 1, 3], [2, 1, 3, 4], [1, 3, 2, 4]).

    test(removeLast_1, [nondet]) :-
        removeLast([a,c,a,d], [a,c,a], d).
    test(removeLast_2) :-
        \+ removeLast([a,c,a,d], [a,c,a], [d]).
    test(removeLast_3, [nondet]) :-
        removeLast([a], [], a).
    test(removeLast_4, [nondet]) :-
        removeLast([[a,b,c]], [], [a,b,c]).
:- end_tests(basic).

