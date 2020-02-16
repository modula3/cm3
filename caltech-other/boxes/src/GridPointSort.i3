
(* $Id$ *)

INTERFACE GridPointSort; 
IMPORT GridPoint AS Elem;

TYPE   Comparer = PROCEDURE (READONLY a, b : Elem.T) : [-1..1];

PROCEDURE Sort(VAR a: ARRAY OF Elem.T; lo, hi : CARDINAL;
                cmp : Comparer);
(* Sort a subarray of "a" using the order defined by "cmp". *)
(* lo and hi are the indices of the first and last element to sort *)

END GridPointSort.

(* "Sort(a, cmp)" permutes the elements of "a" such that:

| FIRST(a) <= i < j <= LAST(a)

   implies

| cmp(a[i], a[j]) <= 0.
   
   The algorithm used is QuickSort:
   <UL>
   <LI>It is not stable.
   <LI>On average, it requires "O(N ln N)" comparison and assignment
         operations.  In the worst case it may require "O(N*N)" operations.
   </UL>

   For an expanded description of QuickSort, see <A REL=BIB.ENTRY HREF="../../../html/references.html#Sedgewick:Alg"> [Sedgewick:Alg] </A>.
*)

