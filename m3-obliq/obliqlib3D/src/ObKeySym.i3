(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 25 13:24:54 PDT 1994 by najork                   *)
(*       Created on Mon Jul 25 13:23:07 PDT 1994 by najork                   *)


INTERFACE ObKeySym;

IMPORT ObValue, VBT;

PROCEDURE M3ToObliq (k : VBT.KeySym) : ObValue.Val;
PROCEDURE ObliqToM3 (val : ObValue.Val) : VBT.KeySym RAISES {ObValue.Error};

END ObKeySym.
