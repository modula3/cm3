/* Last modified on Fri Jun 12 15:45:14 PDT 1992 by heydon                   */

/* This module defines basic operations on sets. Sets are represented by
   lists (containing no duplicates). The empty list denotes the empty set.

   The code in this module is taken from Clocksin and Mellish, pgs 153--154.
   It defines the predicates member/2, subset/2, intersection/3, and union/3.
*/

/* member(X,Y) - true iff element X is a member of set Y */
member(X,[X|_]).
member(X,[_|Y]) :- member(X,Y).

/* subset(X,Y) - true iff set X is a (not nec. proper) subset of set Y */
subset([],Y).
subset([A|X],Y) :- member(A,Y), subset(X,Y).

/* intersection(X,Y,Z) - true iff X intersected with Y = Z */
intersection([],X,[]).
intersection([X|R],Y,[X|Z]) :- member(X,Y), !, intersection(R,Y,Z).
intersection([X|R],Y,Z) :- intersection(R,Y,Z).

/* union(X,Y,Z) - true iff X union Y = Z */
union([],X,X).
union([X|R],Y,Z) :- member(X,Y), !, union(R,Y,Z).
union([X|R],Y,[X|Z]) :- union(R,Y,Z).
