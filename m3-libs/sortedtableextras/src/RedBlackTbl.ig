(* Created on Tue Nov 18 17:15:02 PST 1997 by heydon       *)
(* Last modified on Thu Nov 27 11:39:51 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

(* A "RedBlackTbl.T" is a subtype of a "SortedTable.T", but it is
   implemented using red-black trees. Red-black trees are self-balancing
   binary search trees. *)

GENERIC INTERFACE RedBlackTbl(Key, Value, SortedTbl);
(* Where the same requirments exist on the "Key" and "Value" interfaces
   as those described in the generic "SortedTable" interface and where
   "SortedTbl" is the generic instance "SortedTable(Key, Value)". *)

CONST Brand = "(RedBlackTbl " & Key.Brand & " " & Value.Brand & ")";
(* The type "T" is revealed to have brand "Brand". *)

TYPE
  T <: Public;
  Public = SortedTbl.T OBJECT METHODS
    init(): T;
    keyCompare(READONLY k1, k2: Key.T): [-1..1];
  END;

  Iterator <: IteratorPublic;
  IteratorPublic = SortedTbl.Iterator OBJECT METHODS
    reset();
  END;

END RedBlackTbl.

(*
\subsection{Method Specifications}

The expression "NEW(T).init()" evaluates to a new table with no
elements. The "init" method may also be invoked on an existing table
to delete all of its entries.

The implementation calls the "keyCompare" method to compare two keys.
The default "keyCompare" method simply returns "Key.Compare(k1, k2)".
However, subtypes may wish to override the "keyCompare" method to
effect a new key ordering. "keyCompare" is required to implement a
total order.

The "iterate" method returns an iterator of type "Iterator", a
subtype of "SortedTbl.Iterator". Its "reset" method resets the
iterator. This allows clients to iterate over a table multiple times
without having to allocate a new "Iterator" object on each pass.

\subsection{Synchronization}

For efficiency, red-black tables and their iterators are not
monitored, so a client accessing a table from multiple threads
must ensure that if two operations are active concurrently, then
neither of them has side-effects on the same table or iterator.
The "init", "put", and "delete" methods are the only ones
with side-effects on the table. All three of an iterator's
"reset", "next", and "seek" methods have side-effects on the
iterator.

\subsection{Quake Instantiation Procedures}

The "sortedtableextras" package includes a quake template
that defines quake procedures for instantiating instances of
the "RedBlackTbl" generic interface and implemenation. The
two procedures are:

| redblack_table (nm, key, value)
| RedBlack_table (nm, key, value)

The only difference between these two procedures is that tables
instantiated by the former are private to the package in which
they are built, while those instantiated by the latter are exported.

These procedures create and include the two generic instantiation files
"RedBlack<nm>Tbl.i3" and "RedBlack<nm>Tbl.m3". The generic
interface and implementation are instantiated with the interfaces
named "key" and "value". "nm" should be a string representing the
concatenation of the names "key" and "value", possibly in abbreviated
form; it must be the same name that is used to instantiate the generic
"Table" and "SortedTable" interfaces. Here are some examples: uses

| redblack_table ("IntInt", "Integer", "Integer")
| redblack_table ("IntText", "Integer", "Text")
| redblack_table ("RealRef", "RealType", "Refany")

For example, the last procedure call would create the two derived
files "RedBlackRealRefTbl.i3" and "RedBlackRealRefTbl.m3".

In order for a program that includes a "RedBlackTbl" instantiation to link
successfully, it must also instantiate the generic "Table" and "SortedTable"
interfaces with the same "nm", "key", and "value" arguments.

\subsection{Performance and Implementation}

A red-black table's "get", "put", and "delete" methods
take "O(log n)" time in the worst case, where "n" is the number
of elements in the table. The other table methods take constant
time. An iterator's "reset", "next", and "seek" methods also take
"O(log n)" time in the worst case. As opposed to seeking on a
"SortedTbl.Default", seeking in a red-black table has the same
cost whether seeking forward or backward.

This implementation is based on the description of red-black
trees in a well-known algorithms text
\cite[Chapter 14]{cormen-leiserson-rivest}. In this implementation,
the tree is only rebalanced on insertions and deletions, not on
searches or iterations.

The space requirements of a red-black table are dominated by the
space costs for each of its entries. The space required for
each entry is the space required for the key and the value plus
the space for three "REF"s and the space for the color bit. Stricly
speaking, the color bit should require only 1 bit. However, due to
alignment restrictions, it probably requires "Word.Size" bits in
practice.
*)
