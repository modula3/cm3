(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Sep 22 11:05:54 PDT 1994 by heydon   *)
(*      modified on Mon Nov  8 15:14:32 PST 1993 by mcjones  *)
(*      modified on Sun Jan 19 18:07:07 PST 1992 by gnelson  *)

(* "Table" is a generic interface defining partial maps that support
   update and iteration.
   \index{map!updatable}
*)

GENERIC INTERFACE Table(Key, Value);
(* Where "Key.T" and "Value.T" are types that are not open array types, both
   "Key" and "Value" contain

| CONST Brand = <text-constant>;

   and "Key" additionally contains

| PROCEDURE Equal(k1, k2: Key.T): BOOLEAN;
| PROCEDURE Hash(k: Key.T): Word.T;

   "Brand" must be a text constant. It will be used to construct a brand for
   the opaque type "Table.Default" and any generic types instantiated with the
   "Table" interface. For a non-generic interface, we recommend choosing the
   name of the interface.

   "Equal" must be an equivalence relation and "Hash" must respect
   that equivalence relation, in other words, if "Equal(k1, k2)", then
   "Hash(k1)=Hash(k2)".

   "Hash" and "Equal" may be declared with a parameter mode of either
   "VALUE" or "READONLY", but not "VAR".
*)

IMPORT Word;

CONST
  Brand = "(Table " & Key.Brand & " " & Value.Brand & ")";
  DefaultBrand = "(Default " & Brand & ")";
  (* A "Table.Default" is revealed to have the brand "DefaultBrand". *)

TYPE
  T = OBJECT METHODS
    get(READONLY k: Key.T; VAR v: Value.T): BOOLEAN;
    put(READONLY k: Key.T; READONLY v: Value.T): BOOLEAN;
    delete(READONLY k: Key.T; VAR v: Value.T): BOOLEAN;
    size(): CARDINAL;
    iterate(): Iterator
  END;
  Iterator = OBJECT METHODS
    next(VAR k: Key.T; VAR v: Value.T): BOOLEAN
  END;
  Default <: T OBJECT METHODS
    init(sizeHint: CARDINAL := 0): Default;
    keyEqual(READONLY k1, k2: Key.T): BOOLEAN;
    keyHash(READONLY k: Key.T): Word.T
  END;
END Table.

(* A "Table(Key, Value).T", or table, is a partial map from "Key.T"s
   to "Value.T"s.  Actually, it turns out to be useful for a table to
   treat two different keys as if they are the same whenever they are
   equivalent according to some specified equivalence relation.  For
   example, if you are creating a table with a "Key.T" of "TEXT", you
   are likely to want "Text.Equal" as the equivalence relation.

   Formally, a table "tbl" has the components:

| canon(tbl) `a map on elements of "Key.T"`
| map(tbl)   `a map from elements of "Key.T" to elements of "Value.T"`

   "canon(tbl)" represents an equivalence relation: "canon(tbl)(k)" is
   the {\em canonical representative} of all the keys that are
   equivalent to "k".  The domain of "map(tbl)" includes only
   canonical representatives, that is, elements in the range of
   "canon(tbl)".  The equivalence relation underlying "canon(tbl)"
   must be time-invariant.  For example, it can't depend on the values
   of particular references since some garbage collectors move "REF"
   values.

   The methods of an object "tbl" of type "Table.T" have the following
   specifications:

   The call "tbl.get(k, v)" sets "v" to "map(tbl)(canon(tbl)(k))" and
   returns "TRUE" if "canon(tbl)(k)" is in "dom(map(tbl))".  Otherwise,
   it returns "FALSE" without changing "v".

   The call "tbl.put(k, v)" changes "map(tbl)(canon(tbl)(k))" to "v"
   and returns "TRUE" if "canon(k)" is in "dom(map(tbl))".  Otherwise,
   it sets the value of "map(tbl)(canon(tbl)(k))" to "v", and returns
   "FALSE".

   The call "tbl.delete(k, v)" sets "v" to "map(tbl)(canon(tbl)(k))",
   removes "(canon(tbl)(k), v)" from "map(tbl)", and returns "TRUE" if
   "canon(tbl)(k)" is in "dom(map(tbl))".  Otherwise, it returns
   "FALSE" without changing "v".

   The call "tbl.size()" returns the size of "dom(map(tbl))", that is,
   the number of entries in "tbl".

   The call "tbl.iterate()" returns an iterator, which is an object
   that can be used to iterate over the key-value pairs in "tbl".  See
   the definition of the type "Iterator" below.

   If "i" is the result of the call "tbl.iterate()", then the call
   "it.next(k, v)" selects an entry from "tbl" that has not already
   been returned by "i", sets "k" and "v" to its key and value, and
   returns "TRUE".  If no entries remain, the call returns "FALSE"
   without setting "k" or "v".  It is a checked runtime error to call
   "next" after it has returned "FALSE".  The client must ensure that
   while an iterator is in use, the parent table is not modified.

   The type "Default" is an implementation of "T" using chained
   hashing.  The methods specific to an object "dflt" of type
   "Default" have the following specifications:

   The call "dflt.init(sizeHint)" returns "dflt" after initializing it
   to a table with an empty "map(dflt)".  If "sizeHint" is greater
   than 0, "init" assumes that "put" will subsequently be called with
   at least "sizeHint" different keys; these calls on "put" may
   execute somewhat faster than if "sizeHint" was 0.  The "init"
   method has side-effects on the table.

   The call "dflt.keyEqual(k1, k2)" returns "Key.Equal(k1, k2)" and
   the call "dflt.keyHash(k)" returns "Key.Hash(k)".  The other
   methods call "keyEqual" and "keyHash" whenever they need to consult
   the table's equivalence relation. This means a subtype of "Default"
   can determine the equivalence relation by overriding "keyEqual" and
   "keyHash", providing "keyEqual" implements an equivalence relation
   and "keyHash" respects that relation.

   For efficiency, tables and their iterators are not monitored, so a
   client accessing a table from multiple threads must ensure that if
   two operations are active concurrently, then neither of them has
   side effects on the same table or iterator.  The "T.put",
   "T.delete", and "Default.init" methods are the only ones with side
   effects on the table.  An iterator's "next" method has side-effects
   on the iterator.
*)

