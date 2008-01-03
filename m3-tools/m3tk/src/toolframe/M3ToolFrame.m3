(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3ToolFrame;

IMPORT M3Context, M3Args;
IMPORT WiredStandard, M3CBE_C_Tool, M3PathTool, M3CFETool;

PROCEDURE Init(context: M3Context.T): INTEGER=
  BEGIN
    M3CBE_C_Tool.ToolInit();
    IF M3Args.CheckHelp() THEN RETURN 0 END;
    IF M3CBE_C_Tool.Init() < 0 THEN RETURN -1 END;
    context.put(M3PathTool.Check());
    WiredStandard.Set(context);
    RETURN 1;
  END Init;

PROCEDURE Startup(worker: Worker; compile := TRUE): INTEGER RAISES ANY=
  VAR
    context := M3Context.New();
    compileResult: INTEGER := Init(context);
  BEGIN
    IF compileResult <= 0 THEN RETURN compileResult END;
    IF compile THEN compileResult := M3CFETool.CompileInContext(context) END;
    RETURN worker.work(context, compileResult);
  END Startup;

BEGIN
END M3ToolFrame.
