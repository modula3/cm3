(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Feb  9 09:05:16 PST 1995 by kalsow                   *)
(*      modified on Wed May  4 11:22:31 PDT 1994 by najork                   *)
(*       Created on Fri Apr 29 15:05:10 PDT 1994 by najork                   *)


INTERFACE HashFmt;

IMPORT FormsVBT AS FV;
IMPORT IntList AS IL;

PROCEDURE FormsVBT (form : FV.T) : TEXT;
PROCEDURE IntList (list : IL.T) : TEXT;

END HashFmt.
