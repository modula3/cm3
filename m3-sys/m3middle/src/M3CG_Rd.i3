(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Nov 19 09:30:17 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3CG_Rd;

IMPORT M3CG, Rd;

PROCEDURE Inhale (rd: Rd.T;  cg: M3CG.T);
(* Parse the M3CG calls from 'rd' and call 'cg' to implement them. *)

END M3CG_Rd.
