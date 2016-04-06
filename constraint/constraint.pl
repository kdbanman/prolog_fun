:- module(constraint, [fourSquares/2, disarm/3]).

:- use_module(library(clpfd)).

/*
 * fourSquares/2:
 *  First term is integer, second is list.  Predicate is true iff:
 *
 *  - The integer N is >= 0.
 *  - The list is four integers whose squares sum to N.
 */
fourSquares(N, Squares) :-
    Squares = [S1, S2, S3, S4],
    Squares ins 0..N,
    S1*S1 + S2*S2 + S3*S3 + S4*S4 #= N,
    S1 #=< S2,
    S2 #=< S3,
    S3 #=< S4,
    label(Squares).

/*
 * disarm/3:
 * All terms are lists.  Predicate is true iff:
 *
 * - First and Second lists are flat lists of positive integers.
 * - Solution is a list of disarmaments, where a disarmament is a list
 *   of two lists [Ad, Bd] such that:
 *   - Ad and Bd are lists of integers.
 *   - One of Ad or Bd is length 1 or 2, and the other is 2 or 1, respectively.
 *   - Ad and Bd have equal sums.
 *   - Said sums are in increasing order through the list of disarmaments.
 *
 * Whew!
 */
disarm([], [], []).

disarm(A, B, D) :-
    selectTwo(DisarmA, A, RemainingA),
    selectOne(DisarmB, B, RemainingB),
    singleDisarm(DisarmA, DisarmB, RemainingA, RemainingB, D).


disarm(A, B, D) :-
    selectOne(DisarmA, A, RemainingA),
    selectTwo(DisarmB, B, RemainingB),
    singleDisarm(DisarmA, DisarmB, RemainingA, RemainingB, D).



selectTwo(Selected, Source, Remaining) :-
    select(E1, Source, Tmp),
    select(E2, Tmp, Remaining),
    E1 =< E2,
    Selected = [E1, E2].


selectOne(Selected, Source, Remaining) :-
    select(E, Source, Remaining),
    Selected = [E].


singleDisarm(DisarmA, DisarmB, RemainingA, RemainingB, [DisarmHead | DisarmTail]) :-
    sumlist(DisarmA, SumA),
    sumlist(DisarmB, SumB),
    SumA = SumB,
    DisarmHead = [DisarmA, DisarmB],
    disarm(RemainingA, RemainingB, DisarmTail).


sumCompare(C, L1, L2) :-
    sumlist(L1, S1),
    sumlist(L2, S2),
    compare(C, S1, S2).
