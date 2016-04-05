:- module(constraint, [fourSquares/2, disarm/3, trivial/1, asDomain/2]).

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
disarm(A, B, Disarm) :-
    [DH | DT] = Disarm.


trivial(X) :-
    X in 1\/2\/3.

asDomain([E], E).

asDomain([E|T],  '\\/'(E, TDomain)) :-
    asDomain(T, TDomain).
    
