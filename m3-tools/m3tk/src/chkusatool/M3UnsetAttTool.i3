(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3UnsetAttTool;

IMPORT M3Args;

CONST
  CheckUnsetAtts_Arg = "CheckUnsetAtts";

PROCEDURE Init() RAISES {};
(* Register tool as an extra pass of the compiler front-end *)

PROCEDURE Get(): M3Args.T RAISES {};

END M3UnsetAttTool.
