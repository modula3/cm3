(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3DepDATool;

IMPORT M3Args;

VAR
  tool_g: M3Args.T;

PROCEDURE Get(): M3Args.T RAISES {}=
  BEGIN RETURN tool_g; END Get;

BEGIN
  (* Set up tool_g *)
  tool_g := M3Args.New(Name, "Modula-3 Dependency Analyser", Version);
  M3Args.RegisterStringList(tool_g, CompileInDir_Arg, 
      "compile from given directories (all if empty)", shared := TRUE);
  M3Args.RegisterFlag(tool_g, CompileHeadersOnly_Arg, 
     "compile unit headers only; up to and including IMPORTs");
  M3Args.RegisterFlag(tool_g, NOFilterUnits_Arg, 
     "do not filter units to compile");
  M3Args.RegisterFlag(tool_g, FilterUnitsExact_Arg, 
     "filter units to compile (exact name match);");
  M3Args.RegisterFlag(tool_g, Verbose_Arg, "be verbose", shared := TRUE);
END M3DepDATool.

