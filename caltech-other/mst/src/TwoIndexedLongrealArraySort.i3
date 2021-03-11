
INTERFACE TwoIndexedLongrealArraySort;
IMPORT TwoIndexedLongreal AS Elem;

PROCEDURE Sort(VAR a: ARRAY OF Elem.T; num := -1);
(* Sort the elements of "a" using the order defined by "cmp". *)
(* num is the number of elements of the array, starting from a[0],
   to sort.  -1 means sort all elements *)

END TwoIndexedLongrealArraySort.

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

