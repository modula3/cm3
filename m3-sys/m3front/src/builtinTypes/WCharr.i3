(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE WCharr;

IMPORT Type;

VAR T: Type.T;

VAR IsUnicode : BOOLEAN; 
(* ^Value is set from within package cm3, where it is initially known,
   prior to calling Initialize. *) 

PROCEDURE Initialize ();

END WCharr.
