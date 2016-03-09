:- module(basic, [xreverse/2, xunique/2, xunion/3, removeLast/3, allConnected/1, connected/2, xsubset/2, xappend/3, clique/1]).

/*
* xreverse/2:
*   All terms are lists. Predicate is false unless:
*
*   - The first list is the reverse of the second list (and vice-versa).
*/
xreverse([], []).

xreverse([H|T], R) :- 
    xreverse(T, Rt), 
    append(Rt, [H], R).


/*
* xunique/2:
*   All terms are lists.  Predicate is false unless:
*
*   - The first list is any list.
*   - The second list is a list of the unique elements from the first list.
*   - The elements in the second list occur in the same order that they occur
*     in the first list.
*   - Nested lists in the first list occur unchanged in the second list, and 
*     vice-versa.
*   - The second list is technically a set.
*/
xunique([], []).

xunique([H|T], [H|U]) :-
    delete(T, H, Tp),
    xunique(Tp, U).

   
/*
* xunion/3:
*   All terms are lists.  Predicate is false unless:
*
*   - The first and second lists are any list.
*   - The third list is the set-union of the elements that occur in the first
*     and second lists.
*/
xunion(L1, L2, L) :-
    append(L1, L2, ALL),
    xunique(ALL, L).


/*
* removeLast/3:
*   The first two terms are lists.  Predicate is false unless:
*
*   - The first list is any list.
*   - The second list is the first list without its last element.
*   - The third term is the last element of the first list.
*/
removeLast([X], [], X).

removeLast([H|T], [H|T1], Last) :-
    removeLast(T, T1, Last).


/*
* allConnected/1:
* The input term is a list.  Predicate is false unless: 
*
* - 
*/
allConnected([]).


/*
* connected/2:
* The second term is a list.  Predicate is false unless:
*
* - The first term is connected to each element of the second term by edge/2.
*/
connected(_, []).

connected(A, [H|T]) :-
    edge(A, H),
    connected(A, T).

connected(A, [H|T]) :-
    edge(H, A),
    connected(A, T).


xappend([], L, L).
xappend([H|T], L, [H|R]) :-
    xappend(T, L, R).


xsubset([], _).

xsubset([X|Xs], Set) :-
    xappend(_, [X|Set1], Set),
    xsubset(Xs, Set1).


clique(L) :-
    findall(X, node(X), Nodes),
    xsubset(L, Nodes),
    allConnected(L).
