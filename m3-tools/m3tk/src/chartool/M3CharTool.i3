(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

INTERFACE M3CharTool;

IMPORT M3Args;

CONST
  TypesToChange_Arg         = "CWTypesToChange";
  StatsToConsider_Arg       = "CWStatsToConsider";
  ExprsToReplace_Arg        = "CWExprsToReplace";
  ExprsToConsider_Arg       = "CWExprsToConsider";
  ExprsDistantlyRelated_Arg = "CWExprsDistantlyRelated";
  ConsiderAll_Arg           = "CWConsiderAll";

  KeepModules_Arg = "KeepModules";

PROCEDURE Init () RAISES {};
  (* Register tools as an extra pass of the compiler front-end *)

PROCEDURE Get (): M3Args.T RAISES {};

END M3CharTool.
