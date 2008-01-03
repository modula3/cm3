(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE Main;

(* The stub generator uses the standard tool framework provided by
   "M3ToolFrame".  
   Each interface given on the command line is processed, and
   stubs are generated for all the network objects it defines
   that can legitimately be marshalled.
*)

IMPORT Process, RTCollector;
IMPORT Stdio, Wr;
IMPORT AstToType;
IMPORT TypeNames, StubGenTool;
IMPORT M3Args, M3Context, M3Conventions, M3AST_AS, M3CUnit, M3CFETool, 
       M3ToolFrame;
IMPORT M3AST_all; (* this cannot be omitted; it defines the particular
                     revelations for all the AST nodes *)

TYPE ContextClosure = M3Context.Closure OBJECT
    wr: Wr.T;
  OVERRIDES callback := VisitUnit;
  END;

PROCEDURE VisitUnit(
    cl: ContextClosure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit)
    RAISES {}=
  BEGIN
    (* if it is a generic instantiation, get to actual instantiated tree *)
    cu := M3CUnit.ToGenIns(cu, ut); 
    IF M3Conventions.PrimarySource IN cu.fe_status AND
          (M3CUnit.Errors * cu.fe_status = M3CUnit.Status{}) THEN
      StubGenTool.Set(cl.context, cu);
    END;
  END VisitUnit;

PROCEDURE DoRun(<*UNUSED*> w: M3ToolFrame.Worker; c: M3Context.T;
                <*UNUSED*> compileResult: INTEGER): INTEGER RAISES {}=
  VAR returnCode: INTEGER;
  BEGIN
    M3Args.SetFlag(StubGenTool.tool_g, StubGenTool.StubGen_Arg, TRUE);
    StubGenTool.GetArgs(StubGenTool.tool_g);
    returnCode := M3CFETool.CompileInContext(c);
    IF returnCode >= 0 THEN
      RTCollector.DisableMotion(); 
                  (* Don't want copying until fix use of RefTable *)
      TypeNames.Preprocess(c);
      IF StubGenTool.stubTypes = NIL THEN
        <*FATAL ANY*>
        BEGIN
          M3Context.Apply(c, NEW(ContextClosure, wr := Stdio.stdout),
                          findStandard := FALSE); (* ignore 'standard' unit *)
        END;
      ELSE
        WITH objects = StubGenTool.stubTypes DO
          FOR i := 0 TO LAST(objects^) DO
            IF objects[i] # NIL THEN
              returnCode := AstToType.OneStub(c, objects[i], Stdio.stdout);
            END;
          END;
        END;
      END;
      RTCollector.EnableMotion(); 
    END;
    RETURN returnCode;
  END DoRun;

  <* FATAL ANY *>
BEGIN
  Process.Exit(ABS(M3ToolFrame.Startup(
                       NEW(M3ToolFrame.Worker, work := DoRun),
                       compile := FALSE)));
END Main.


