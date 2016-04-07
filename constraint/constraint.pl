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
    singleDisarm(DisarmA, DisarmB, RemainingA, RemainingB, UnorderedD),
    predsort(compareDisarm, UnorderedD, D).

disarm(A, B, D) :-
    selectOne(DisarmA, A, RemainingA),
    selectTwo(DisarmB, B, RemainingB),
    singleDisarm(DisarmA, DisarmB, RemainingA, RemainingB, UnorderedD),
    predsort(compareDisarm, UnorderedD, D).


/*
* selectTwo/3:
* All terms are lists.  Predicate is true iff:
*
* - Selected is length 2, each element of which is a unique element of Source
* - Source is a list of at least length 2
* - Remaining is Source less the elements in Selected.
*/
selectTwo(Selected, Source, Remaining) :-
    select(E1, Source, Tmp),
    select(E2, Tmp, Remaining),
    E1 =< E2,
    Selected = [E1, E2].


/*
* selectOne/3:
* All terms are lists.  Predicate is true iff:
*
* - Selected is length 1, and the element is an element of Source
* - Source is a list of at least length 1
* - Remaining is Source less the element in Selected.
*/
selectOne(Selected, Source, Remaining) :-
    select(E, Source, Remaining),
    Selected = [E].


/*
* singleDisarm/5:
* All terms are lists.  Predicate is true iff:
*
* - DisarmA and DisarmB have equal sums
* - The head of the last term is a list of DisarmA and DisarmB
* - RemainingA and RemainingB are disarmed by the tail of the last term
*/
singleDisarm(DisarmA, DisarmB, RemainingA, RemainingB, [DisarmHead | DisarmTail]) :-
    sumlist(DisarmA, SumA),
    sumlist(DisarmB, SumB),
    SumA = SumB,
    DisarmHead = [DisarmA, DisarmB],
    disarm(RemainingA, RemainingB, DisarmTail).


/*
* compareDisarm/3:
* First term is atom, rest of terms are lists.  Predicate is true iff:
*
* - C is < if the sum of L1 is less than the sum of L2
* - C is > if the sum of L1 is more than the sum of L2
* - C is = if the sum of L1 is equal to the sum of L2
* - Lists are possibly nested lists of integers
*/
compareDisarm(C, L1, L2) :-
    flatten(L1, Flat1),
    flatten(L2, Flat2),
    sumlist(Flat1, Sum1),
    sumlist(Flat2, Sum2),
    compare(C, Sum1, Sum2).
