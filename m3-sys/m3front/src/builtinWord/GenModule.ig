(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: GenModule.ig                                          *)
(* Last Modified On Tue Mar  2 09:01:49 PST 1993 By kalsow     *)

GENERIC INTERFACE GenModule ();

IMPORT Module;

VAR M: Module.T;

PROCEDURE Initialize (name: TEXT);

END GenModule.
