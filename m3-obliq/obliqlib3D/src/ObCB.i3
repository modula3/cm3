(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 25 10:37:00 PDT 1994 by najork                   *)
(*       Created on Mon Jul 25 10:35:16 PDT 1994 by najork                   *)


INTERFACE ObCB;

IMPORT ObValue;

PROCEDURE SetupPackage ();

VAR BadMethod : ObValue.ValException;

END ObCB.
