:- module(constraint, [fourSquares/2, disarm/3, inList/2, asDomain/2]).

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
    select(A1, A, A_A1),
    select(A2, A_A1, A_A1A2),
    select(B1, B, B_B1),
    A1 + A2 #= B1,
    A1 #=< A2,
    ddd([A1, A2], [B1], A_A1A2, B_B1, D).


disarm(A, B, D) :-
    select(B1, B, B_B1),
    select(B2, B_B1, B_B1B2),
    select(A1, A, A_A1),
    B1 + B2 #= A1,
    B1 #=< B2,
    ddd([A1], [B1, B2], A_A1, B_B1B2, D).

% test strategy - will build all options and find valid ones
%increasingDisarmaments(D) :-
%    map to sum
%    true if sorted

% construct strategy - will take almost correct option and correct it
%increasingDisarmaments(D, Dsorted) :-
%    map to sum
%    sort with parallel swaps


ddd(DisarmA, DisarmB, RemainingA, RemainingB, [DisarmHead | DisarmTail]) :-
    DisarmHead = [DisarmA, DisarmB],
    disarm(RemainingA, RemainingB, DisarmTail).

inList(E, L) :-
    asDomain(L, Ld),
    E in Ld.


asDomain([E], E).

asDomain([E|T],  '\\/'(E, TDomain)) :-
    asDomain(T, TDomain).
    
