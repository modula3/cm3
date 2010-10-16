(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 26 14:21:02 PDT 1994 by isard      *)
(*      modified on Fri Nov 19 09:30:31 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3x86;

IMPORT M3CG, M3ObjFile, Wr;

PROCEDURE New (logfile: Wr.T; obj: M3ObjFile.T): M3CG.T;
(* returns a fresh, initialized code generator that writes its
   log on 'logfile', or is silent if logfile = NIL. *)

END M3x86.
