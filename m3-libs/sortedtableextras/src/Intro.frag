(* Created on Sun Nov 23 08:55:42 PST 1997 by heydon *)
(* Last modified on Thu Nov 27 11:35:04 PST 1997 by heydon *)

(*
The standard Modula-3 library includes two generic interfaces named
"Table" and "SortedTable" \cite[Sections 3.5 and 3.6]{SRC-113}.
A "Table.T" is a table of key-value pairs. A "SortedTable.T" is
a subtype of "Table.T" that also provides methods for iterating
over the elements of the table in increasing or decreasing key order
and for seeking to an arbitrary key in the iteration sequence.

The default sorted table implementation, "SortedTable.Default", is
implemented using a randomized data structure called a heap-ordered
binary tree, or ``treap'' \cite{aragon-seidel-treaps}. This paper
describes the implementations of two other well-known data structures,
one based on red-black trees, and one based on skip lists. These
implementations are provided by the "sortedtableextras" package, which
also includes templates for instantiating the generics as described
below. The paper concludes by comparing the performance of the four
implementations. *)
