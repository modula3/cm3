(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Coverage.i3                                           *)
(* Last modified on Fri Aug 21 17:28:49 PDT 1992 by kalsow     *)

(* Coverage generates the tables and code necessary to instrument
   a module for coverage analysis. *)

INTERFACE Coverage;

IMPORT Value;

(***** phase 0 - during Binding *****)

PROCEDURE NoteLine ();
(* note that the current line (Scanner.offset) will be counted *)

PROCEDURE NoteProcedure (v: Value.T);
(* note that procedure v will be counted *)


(***** phase 1 - before code generation *****)

PROCEDURE GenerateTables ();
(* produce the C declarations necessary to hold the profile information *)


(***** phase 2 - during code generation *****)

PROCEDURE CountLine ();
(* generate code to increment the counter associated with the current
   line (Scanner.offset).  For any particular line, NoteLine must be
   called prior to CountLine. *)

PROCEDURE CountProcedure (v: Value.T);
(* generate code to increment the counter associated with procedure v.
   For any procedure v, NoteProcedure must be called prior to
   CountProcedure. *)

PROCEDURE Reset ();

END Coverage.
