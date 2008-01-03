(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3DepDATool;

IMPORT M3Args;

CONST
  Name = "m3depda";
  Version = "30-Dec-91";
  (* flags *)
  CompileInDir_Arg = "CompileInDir";
  CompileHeadersOnly_Arg = "HeadersOnly";
  FilterUnits_Arg = "FilterUnits";
  NOFilterUnits_Arg = "NOFilterUnits";
  FilterUnitsExact_Arg = "FilterUnitsExact";
  Verbose_Arg = "Verbose";

PROCEDURE Get(): M3Args.T RAISES {};
(* Return the tool for m3depda *)

END M3DepDATool.
