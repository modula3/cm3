(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Mon Aug 22 08:22:13 PDT 1994 by kalsow   *)
(*      modified on Mon Nov  8 11:39:46 PST 1993 by detlefs  *)

GENERIC INTERFACE TableList(Key, Value, KeyValueTable);
(* Where "Key.T" and "Value.T" are types that are not open array types
   and "Key" contains

| PROCEDURE Equal(k1, k2: Key.T): BOOLEAN;

   and "KeyValueTable" is the instantiation of the generic interface
   "Table" with the interfaces "Key" and "Value".

   "Equal" must be an equivalence relation.

   "Equal" may be declared with a parameter mode of either "VALUE" or
   "READONLY", but not "VAR".

   "TableList.T"'s may be more efficient than "Table.Defaults"'s for
   small tables.
*)

TYPE
  T <: KeyValueTable.T OBJECT METHODS
    init(): T;
    keyEqual(READONLY k1, k2: Key.T): BOOLEAN
  END;

TYPE XX = Value.T;
(* this use of Value.T is just to prevent a warning message.  sigh. *)

END TableList.

