(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3ASTPickleTool;

IMPORT M3AST_AS;

CONST
  PickleInterfaces_Arg = "PickleInterfaces";
  PickleModules_Arg = "PickleModules";
  PickleAllInterfaces_Arg = "PickleAllInterfaces";
  PickleAllModules_Arg = "PickleAllModules";

PROCEDURE Init();
(* Register as an extra pass of the front end, interpreting
   the above args and pickling the interfaces/modules
   given by the above arguments. *)

PROCEDURE DoUnit(cu: M3AST_AS.Compilation_Unit);
(* Pickle "cu". *)

END M3ASTPickleTool.
