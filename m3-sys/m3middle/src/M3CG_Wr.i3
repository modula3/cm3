(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Nov 19 09:30:31 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE M3CG_Wr;

IMPORT M3CG, Wr;

PROCEDURE New (wr: Wr.T): M3CG.T;
(* returns a fresh, initialized code generator that writes its
   calls as readable ASCII on 'wr'. *)

END M3CG_Wr.
