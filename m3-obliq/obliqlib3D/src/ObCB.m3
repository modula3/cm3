(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jul 25 10:40:03 PDT 1994 by najork                   *)
(*       Created on Mon Jul 25 10:37:16 PDT 1994 by najork                   *)


MODULE ObCB;

IMPORT ObValue;

PROCEDURE SetupPackage () =
  BEGIN
    BadMethod := NEW (ObValue.ValException, name := "CB_BadMethod");
  END SetupPackage;


BEGIN 
END ObCB.
