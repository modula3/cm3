(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Sep 22 19:27:13 PDT 1994 by heydon   *)
(*      modified on Mon Nov  8 16:51:45 PST 1993 by mcjones  *)
(*      modified on Wed Oct  7 16:16:18 PDT 1992 by johnh    *)

(* "SortedTable" is a generic interface defining partial maps over
   a totally ordered domain.
   \index{map!updatable}
*)

GENERIC INTERFACE SortedTable(Key, Tbl);
(* Where "Key.T" is not an open array type, "Tbl" is a generic
   instance "Table(Key, Value)" (for some "Value" defining a type
   "T" that is not an open array type), both "Key" and "Tbl" contain

| CONST Brand = <text-constant>;

   and "Key" additionally contains

| PROCEDURE Compare(k1, k2: Key.T): [-1..1];

   "Brand" must be a text constant. It will be used to construct a brand for
   the opaque type "SortedTable.Default" and any generic types instantiated
   with the "SortedTable" interface. For a non-generic interface, we recommend
   choosing the name of the interface.

   "Compare" must be a total order.

   "Compare" may be declared with a parameter mode of either "VALUE"
   or "READONLY", but not "VAR".
*)

CONST
  Brand = "(Sorted " & Tbl.Brand & ")";
  DefaultBrand = "(Default " & Brand & ")";
  (* A "SortedTable.Default" is revealed to have the brand "DefaultBrand". *)

TYPE
  T = Tbl.T OBJECT METHODS
    iterateOrdered(up: BOOLEAN := TRUE): Iterator
  END;
  Iterator = Tbl.Iterator OBJECT METHODS
    seek(READONLY key: Key.T)
  END;
  Default <: T OBJECT METHODS
    init(): Default;
    keyCompare(READONLY k1, k2: Key.T): [-1..1]
  END;
END SortedTable.

(* A "SortedTable(Key, Table(Key, Value)).T", or sorted table, is a
   "Table(Key, Value).T" together with a total (linear) order on the
   keys of the table.  Formally, a sorted table "tbl" has the
   additional component:

| le(tbl) `a total order on the values of "Key.T"`

   The total order "le(tbl)" must be time-invariant.

   The methods have the following specifications: 

   The call "tbl.iterateOrdered(up)" returns an iterator, which is an
   object that can be used to iterate over all the key-value pairs in
   "tbl", ordered by key.  The order is increasing if "up" is "TRUE",
   decreasing otherwise.

   If "i" is the result of the call "tbl.iterateOrdered(up)", then the
   call "i.next(k, v)" sets "k" and "v" to the key and value of the
   next pair and returns "TRUE".  If no entries remain, the call
   returns "FALSE" without setting "k" or "v".  It is a checked
   runtime error to call "next" or "seek" after "next" has returned
   "FALSE".  The client must ensure that while an iterator is in use,
   the parent table is not modified.
   
   The call "i.seek(k)" skips past zero or more key-value pairs
   (either forward or backward) so that a subsequent call of "next"
   returns the first pair with key greater than or equal to "k" if "i"
   is in increasing order or with key less than or equal to "k" if "i"
   is in decreasing order.

   The type "Default" is an implementation of "T" using randomized
   heap-ordered binary trees or ``treaps'' (see \cite{Aragon}).  In
   this implementation, seeking forward (relative to the iterator's
   order) is more efficient than seeking backward.  If a forward seek
   skips over "d" key-value pairs, the expected time for the seek is
   "O(log d)".  The time for a backward seek is
   "O(log(table.size()))", no matter how far back it skips.

   The call "dflt.init()" returns "dflt" after initializing it to an
   empty table.

   The call "dflt.keyCompare(k1, k2)" returns "Key.Compare(k1, k2)".
   The other methods call "keyCompare" whenever they need to consult
   "le(tbl)".  This means a subtype of "Default" can determine
   "le(tbl)" by overriding "keyCompare", providing "keyCompare"
   implements a total order.

   For efficiency, sorted tables and their iterators are not
   monitored, so a client accessing a table from multiple threads must
   ensure that if two operations are active concurrently, then neither
   of them has side effects on the same table or iterator.  The
   "T.put", "T.delete", and "Default.init" methods are the only ones
   with side effects on the table.  An iterator's "next" method has
   side-effects on the iterator.

*)
