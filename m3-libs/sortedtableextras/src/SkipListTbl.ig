(* Created on Fri Nov 21 08:43:22 PST 1997 by heydon       *)
(* Last modified on Thu Nov 27 11:43:46 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

(* A "SkipListTbl.T" is a subtype of a "SortedTable.T", but it
   is implemented using skip lists. Skip lists are randomized
   data structures that have logarithmic expected-time performance. *)

GENERIC INTERFACE SkipListTbl(Key, Value, SortedTbl);
(* Where the same requirments exist on the "Key" and "Value" interfaces
   as those described in the generic "SortedTable" interface and where
   "SortedTbl" is the generic instance "SortedTable(Key, Value)". *)

CONST Brand = "(SkipListTbl " & Key.Brand & " " & Value.Brand & ")";
(* The type "T" is revealed to have brand "Brand". *)

TYPE
  T <: Public;
  Public = SortedTbl.T OBJECT METHODS
    init(maxSizeHint: CARDINAL := 10000; fixedSeed := FALSE): T;
    keyCompare(READONLY k1, k2: Key.T): [-1..1];
  END;

  Iterator <: IteratorPublic;
  IteratorPublic = SortedTbl.Iterator OBJECT METHODS
    reset();
  END;

END SkipListTbl.

(*
\subsection{Method Specifications}

The expression "NEW(T).init(maxSizeHint, fixedSeed)" evaluates to
a new table with no elements. The "init" method may also be invoked
on an existing table to delete all of its entries.

The "maxSizeHint" parameter should be an estimate of the table's
maximum size. If the estimate is too small, the table will perform
poorly, so it is better to over-estimate. The cost of over-estimating
is that the table will consume more space than necessary.

Each "SkipListTbl.T" uses its own random number generator. The
generator is initialized with a fixed seed if and only if the
"fixedSeed" parameter is "TRUE". Use of a fixed seed is only
recommended for testing purposes.

The implementation calls the "keyCompare" method to compare two keys.
The default "keyCompare" method simply returns "Key.Compare(k1, k2)".
However, subtypes may wish to override the "keyCompare" method to
effect a new key ordering. "keyCompare" is required to implement a
total order.

The "iterate" method returns an iterator of type "Iterator", a subtype of
"SortedTbl.Iterator". Its "reset" method resets the iterator. This allows
clients to iterate over a table multiple times without having to allocate a
new "Iterator" object on each pass.

\subsection{Synchronization}

For efficiency, skip list tables and their iterators are not
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
the "SkipListTbl" generic interface and implemenation. The
two procedures are:

| skiplist_table (nm, key, value)
| SkipList_table (nm, key, value)

The only difference between these two procedures is that tables
instantiated by the former are private to the package in which
they are built, while those instantiated by the latter are exported.

These procedures create and include the two generic instantiation files
"SkipList<nm>Tbl.i3" and "SkipList<nm>Tbl.m3". The generic
interface and implementation are instantiated with the interfaces
named "key" and "value". "nm" should be a string representing the
concatenation of the names "key" and "value", possibly in abbreviated
form; it must be the same name that is used to instantiate the generic
"Table" and "SortedTable" interfaces. Here are some examples:

| skiplist_table ("IntInt", "Integer", "Integer")
| skiplist_table ("IntText", "Integer", "Text")
| skiplist_table ("RealRef", "RealType", "Refany")

For example, the last procedure call would create the two derived
files "SkipListRealRefTbl.i3" and "SkipListRealRefTbl.m3".

In order for a program that includes a "SkipListTbl" instantiation
to link successfully, it must also instantiate the generic "Table"
and "SortedTable" interfaces with the same "nm", "key", and
"value" arguments.

\subsection{Performance and Implementation}

A skip list table's "get", "put", and "delete" methods
take "O(log n)" expected time, where "n" is the number of
elements in the table. The other table methods take constant
time. An iterator's "reset", "next", and "seek" methods also
take "O(log n)" expected time.

Skip lists were invented by William Pugh \cite{pugh-skiplist,
pugh-skiplist-cookbook}. This implementation of skip lists uses:
%
\begin{itemize}
\item
A "p" value of 1/4 as recommended in Pugh's papers.

\item
An extra back-pointer per node to allow downward iterations.

\item
The extra test desribed in section 3.5 of Pugh's ``Cookbook''
paper \cite{pugh-skiplist-cookbook} for minimizing the number of
key comparisons. If key comparisons are cheap, including this test
is unnecessary and hurts performance slightly, but in a generic
implementation where the cost of key comparisons is potentially
unbounded, including the test seems prudent. 
\end{itemize}

There is no well-defined value of type "Key.T" that exceeds all
other keys. Hence, the use of a ``nil'' sentinel as described in
Pugh's papers could not be used. Instead, some extra tests against
"NIL" are required.

The space requirements of a skip list table are dominated by the
space costs for each of its entries. The space required for each
entry is the space for the key and the value plus the space for the
foward and backward "REF"s. According to Pugh, the expected number
of forward "REF"s per entry with a value for "p" of 1/4 is 1.333.
However, since the number of forward "REF"s per entry may vary,
and since this is a safe implementation, the forward "REF"'s are
represented as a "REF ARRAY" of "REF"s. Hence, each node requires an
extra "REF" for the "REF ARRAY" plus the runtime's space overhead for
the "REF ARRAY" itself, which includes its typecode and its size.
All told then, the expected number of "REF"s per entry is 3.333,
and there is an additional space cost per entry of the runtime
"REF ARRAY" space overhead.
*)
