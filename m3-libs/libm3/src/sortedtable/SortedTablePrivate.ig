(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Oct  8 09:28:27 PDT 1992 by mcjones *)
(*      modified on Wed Oct  7 15:51:31 PDT 1992 by johnh   *)

GENERIC INTERFACE SortedTablePrivate(SrtdTbl);
(* Where "SrtdTbl.T" is an instance of a SortedTable. *)

PROCEDURE Validate(table: SrtdTbl.T);
(* Cause a checked runtime error if "table" does not satisfy its invariants. *)

END SortedTablePrivate.
