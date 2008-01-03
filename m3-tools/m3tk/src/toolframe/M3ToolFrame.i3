(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3ToolFrame;

(* Basic framework for an M3TK tool. *)

IMPORT M3Context;

PROCEDURE Init(c: M3Context.T): INTEGER;
(* Initialise the toolkit, using context "c" *)

TYPE 
  Worker = OBJECT
  METHODS 
    work(context: M3Context.T;
         compileResult: INTEGER): INTEGER RAISES ANY;
  END;

PROCEDURE Startup(
    worker: Worker;
    compile := TRUE;
    ): INTEGER RAISES ANY;
(* Initialise the toolkit and then invoke "worker" *)

(* The "Init" procedure  is equivalent to the following code:

|  BEGIN
|    (* register target machine variants *)
|    M3CBE_C_Tool.ToolInit();
|    (* check for -help *)
|    IF M3Args.CheckHelp() THEN RETURN 0 END;
|    (* check for target machine and fail if bad *)
|    IF M3CBE_C_Tool.Init() < 0 THEN RETURN -1 END;
|    (* establish a finder for M3 files ans store in "context" *)
|    context.put(M3PathTool.Check());
|    (* include "standard" interface *)
|    WiredStandard.Set(context);
|    RETURN 1
|  END;

"Init" returns "< 0" if an initialisation error occurs, "0" if help
was requested and "> 0" otherwise.

The "Startup"  procedure is equivalent to the following code:

|  VAR
|    context := M3Context.New();
|    compileResult: INTEGER := Init(context);
|  BEGIN
|    IF compileResult <= 0 THEN RETURN compileResult END;
|    IF compile THEN compileResult := M3CFETool.CompileInContext(context) END;
|    RETURN worker.work(context, compileResult);  (* call user method *)
|  END;

*)


END M3ToolFrame.
