(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE StubGenTool;

IMPORT M3Context, M3Args, M3AST_AS;
IMPORT Type;

VAR tool_g: M3Args.T;

CONST
  StubGen_Arg = "StubGen";
  StubGen_V1_Arg = "v1";
  StubGen_V2_Arg = "v2";
  StubGenTypes_Arg = "StubNetworkObjects";
  StubGenInterfaces_Arg = "StubInterfaces";
  StubGenExists_Arg = "UseStubs";
  StubGenPerf_Arg = "StubPerfMon";

PROCEDURE Init();
(* Register the stub generator with the compiler front end *)

PROCEDURE GetArgs(tool: M3Args.T);
(* Get command line arguments to initialize stubTypes, perfMon, useTypes,
   interfaces *)

VAR (* command-line arguments *)
  stubTypes: REF ARRAY OF Type.Qid;
  useTypes: REF ARRAY OF Type.Qid;
  stubInterfaces : REF ARRAY OF Type.Qid; (* item is NIL in each of these *)
  
PROCEDURE Set(context: M3Context.T; cu: M3AST_AS.Compilation_Unit);
(* Stub generate "cu" *)

END StubGenTool.
