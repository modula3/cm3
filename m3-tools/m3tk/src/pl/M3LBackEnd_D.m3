(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3LBackEnd_D EXPORTS M3LBackEnd;


IMPORT Text;
IMPORT M3Args;
IMPORT M3Context, M3LInitOrder, M3LTypeCodes, M3LBackEnd;

PROCEDURE D_RegisterArgs(<*UNUSED*> t: M3Args.T) RAISES {}=
  BEGIN
  END D_RegisterArgs;

PROCEDURE D_Run(
    <*UNUSED*> c: M3Context.T; 
    <*UNUSED*> h: M3LInitOrder.T;
    <*UNUSED*> tcl: M3LTypeCodes.T
    ): INTEGER RAISES {}=
  BEGIN
    RETURN 0;
  END D_Run;

PROCEDURE D_HardWired(interface: TEXT; <*UNUSED*>proc: TEXT): BOOLEAN
  RAISES {}=
  BEGIN
    RETURN Text.Equal(interface, "Word");
  END D_HardWired;

BEGIN
  M3LBackEnd.Run := D_Run;
  M3LBackEnd.RegisterArgs := D_RegisterArgs;
  M3LBackEnd.HardWired := D_HardWired;
END M3LBackEnd_D.
