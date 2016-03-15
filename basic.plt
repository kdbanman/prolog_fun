
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

    test(subset_1) :-
        xsubset([], []).
    test(subset_2) :-
        xsubset([], [a, 1]).
    test(subset_3, [nondet]) :-
        xsubset([a], [a, 1]).
    test(subset_4, [nondet]) :-
        xsubset([a], [b, a, 1]).
    test(subset_4, [nondet]) :-
        xsubset([b, a, 1], [b, a, 1]).
    test(subset_5) :-
        \+ xsubset([a], [b, 1]).
    test(subset_6) :-
        \+ xsubset([a], [b, [a], 1]).

    test(connected_1, [nondet]) :-
        connected(a, []).
    test(connected_2, [nondet]) :-
        connected(a, [b]).
    test(connected_3, [nondet]) :-
        connected(a, [b, c]).
    test(connected_4, [nondet]) :-
        connected(c, [a]).
    test(connected_5) :-
        \+ connected(c, [b]).
    test(connected_6) :-
        \+ connected(c, [c]).

    test(allConnected_1) :-
        allConnected([]).
    test(allConnected_2, [nondet]) :-
        allConnected([a]).
    test(allConnected_3, [nondet]) :-
        allConnected([a, b]).
    test(allConnected_4, [nondet]) :-
        allConnected([a, c]).
    test(allConnected_5) :-
        \+ allConnected([b, c]).
    test(allConnected_6, [nondet]) :-
        allConnected([a3, b3, c3, e3]).
    test(allConnected_7) :-
        \+ allConnected([a3, b3, c3, d3, e3]).
        
    test(clique_1, [nondet]) :-
        clique([]).
    test(clique_2, [nondet]) :-
        clique([a]).
    test(clique_3, [nondet]) :-
        clique([b]).
    test(clique_4, [nondet]) :-
        clique([c]).
    test(clique_5, [nondet]) :-
        clique([a, b]).
    test(clique_6, [nondet]) :-
        clique([a, c]).
    test(clique_7, [nondet]) :-
        \+ clique([b, c]).
    test(clique_8, [nondet]) :-
        clique([a3, b3, c3, e3]).

    test(noStrictSuperset_1, [nondet]) :-
        noStrictSuperset([a, b, c], [[], [a], [a, b], [a, c]]).
    test(noStrictSuperset_2, [nondet]) :-
        noStrictSuperset([a, d], [[], [a], [a, b], [a, c]]).
    test(noStrictSuperset_3, [nondet]) :-
        noStrictSuperset([a, d], [[], [a], [a, b], [a, d]]).
    test(noStrictSuperset_4) :-
        \+ noStrictSuperset([a, d], [[], [a], [a, b], [a, c, d]]).

    test(notSubsetStrict_1, [nondet]) :-
        notSubsetStrict([a], []).
    test(notSubsetStrict_2, [nondet]) :-
        notSubsetStrict([a], [b]).
    test(notSubsetStrict_3, [nondet]) :-
        notSubsetStrict([a], [a]).
    test(notSubsetStrict_4, [nondet]) :-
        notSubsetStrict([a, b], [a, b]).
    test(notSubsetStrict_5) :-
        \+ notSubsetStrict([a, b], [a, b, c]).
    test(notSubsetStrict_6) :-
        \+ notSubsetStrict([a, c], [a, b, c]).
    test(notSubsetStrict_7, [nondet]) :-
        notSubsetStrict([a, d], [a, b, c]).

    test(singleMaxClique_1, [nondet]) :-
        singleMaxClique(4, [a3, b3, c3, e3]).
    test(singleMaxClique_2, [nondet]) :-
        singleMaxClique(2, [a, b]).
    test(singleMaxClique_3, [nondet]) :-
        singleMaxClique(2, [a, c]).
    test(singleMaxClique_4, [nondet]) :-
        \+ singleMaxClique(3, [a3, b3, c3]).
    test(singleMaxClique_5, [nondet]) :-
        \+ singleMaxClique(2, [b, c]).
:- end_tests(basic).

