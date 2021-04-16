(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 26 14:21:02 PDT 1994 by isard      *)
(*      modified on Fri Nov 19 09:30:31 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3C;

IMPORT M3CG, Wr, M3ID;

PROCEDURE New0 (cfile: Wr.T): M3CG.T;
PROCEDURE New (library (* or program *): TEXT; source: M3ID.T; cfile: Wr.T; target_name: TEXT): M3CG.T;
(* returns a new code generator that writes to 'cfile'. *)

END M3C.
