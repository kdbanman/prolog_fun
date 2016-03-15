:- module(basic, [xreverse/2, xunique/2, xunion/3, removeLast/3, allConnected/1, connected/2, xsubset/2, xappend/3, clique/1, maxClique/2, singleMaxClique/2, allCliques/1, noStrictSuperset/2, notSubsetStrict/2, nonMember/2]).

:- use_module(graphs).

/*
* xreverse/2:
*   All terms are lists. Predicate is true iff:
*
*   - The first list is the reverse of the second list (and vice-versa).
*/
xreverse([], []).

xreverse([H|T], R) :- 
    xreverse(T, Rt), 
    append(Rt, [H], R).


/*
* xunique/2:
*   All terms are lists.  Predicate is true iff:
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
*   All terms are lists.  Predicate is true iff:
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
*   The first two terms are lists.  Predicate is true iff:
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
* The input term is a list.  Predicate is true iff: 
*
* - 
*/
allConnected([]).

allConnected([H|T]) :-
    connected(H, T),
    allConnected(T).


/*
* connected/2:
* The second term is a list.  Predicate is true iff:
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


/*
* xappend/3:
* All terms are lists.  Predicate is true iff:
*
* - The first list and second list are any lists.
* - The third list is the elements of the first list followed by the elements
*   of the second list.
*/
xappend([], L, L).

xappend([H|T], L, [H|R]) :-
    xappend(T, L, R).


/*
* xsubset/2:
* Both terms are lists.  Predicate is true iff:
*
* - The first list is a subset of the second list.
* - The second list is any list whose elements do not repeat.
*/
xsubset([], _).

xsubset([X|Xs], Set) :-
    xappend(_, [X|Set1], Set),
    xsubset(Xs, Set1).


/*
* clique/1:
* The term is a list.  Predicate is true iff:
*
* - Each element in the list is connected to all other elements in the list.
*/
clique(L) :-
    findall(X, node(X), Nodes),
    xsubset(L, Nodes),
    allConnected(L).


/*
* maxClique/2:
* First term is an integer, second term is a list.  Predicate is true iff:
*
* - The second list contains all cliques (lists of nodes) of size N that are
*   not subsets of any other clique.
*/
maxClique(Size, [CliquesHead | CliquesTail]) :-
    nonMember(CliquesHead, CliquesTail),
    clique(CliquesHead),
    length(CliquesHead, Size),
    allCliques(AllCliques),
    delete(AllCliques, CliquesHead, OtherCliques),
    noStrictSuperset(CliquesHead, OtherCliques),
    maxClique(Size, CliquesTail).

maxClique(_, []).


/*
 * singleMaxClique/2:
 * First term is an integer, second term is a list.  Predicate is true iff:
 *
 * - The first term equals the number of elements in the second term.
 * - The second term contains a clique (list of nodes) that is not a subset of
 *   any other clique.
 */
singleMaxClique(Size, L) :-
    clique(L),
    length(L, Size),
    allCliques(AllCliques),
    noStrictSuperset(L, AllCliques).

/*
 * allCliques/1:
 * Term is a list.  Predicate is true iff:
 *
 * - The term represents all cliques from node/1 and edge/2 facts.
 */
allCliques(Gc) :-
    findall(C, clique(C), Gc).


/*
* noStrictSuperset/2:
* Both terms are lists.  Predicate is true iff:
*
* - Each list in the second list is *not* a strict superset.
*/
noStrictSuperset(_, []).

noStrictSuperset(S, [H|T]) :-
   notSubsetStrict(S, H),
   noStrictSuperset(S, T).


/*
* notSubsetStrict/2:
* Both terms are lists.  Predicate is true iff:
*
* - The first list contains one or more element that is not in the second list.
*/
notSubsetStrict(S1, S2) :-
    S1 == S2.

notSubsetStrict([H|T], L) :-
    nonMember(H, L);
    notSubsetStrict(T, L).


/*
 * nonMember/2:
 * Last term is a list.  Predicate is true iff:
 *
 * - The first term is NOT a member of L (nesting ignored).
 */
nonMember(E, L) :-
    delete(L, E, R),
    length(L, N),
    length(R, N).
