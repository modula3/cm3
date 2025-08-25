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

(************************************************************)

IMPORT Debug, AL, Scheme, SchemeM3, ReadLine;
IMPORT Pathname, IP, SchemeNavigatorEnvironment;
IMPORT ReadLineError, NetObj;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT TypeTranslator;
IMPORT Atom, Type;
IMPORT SchemeString; 
IMPORT Csighandler; (* no-readline *)
IMPORT SchemeModula3Types;
IMPORT SchemeEnvironment;
IMPORT SchemeBoolean;

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

PROCEDURE DoRun(<*UNUSED*> w: M3ToolFrame.Worker; 
                c: M3Context.T;
                <*UNUSED*> compileResult: INTEGER): INTEGER 
  RAISES {}=
  VAR 
    returnCode: INTEGER;
  BEGIN
    M3Args.SetFlag(StubGenTool.tool_g, StubGenTool.StubGen_Arg, TRUE);
    StubGenTool.GetArgs(StubGenTool.tool_g);
    returnCode := M3CFETool.CompileInContext(c);
    IF returnCode >= 0 THEN
      RTCollector.DisableMotion(); 
                  (* Don't want copying until fix use of RefTable *)
      TypeNames.Preprocess(c);
          
      IF StubGenTool.stubInterfaces # NIL THEN
        WITH objects = StubGenTool.stubInterfaces^ DO
          FOR i := FIRST(objects) TO LAST(objects) DO
            WITH allSyms = AstToType.GetNames(c, objects[i]) DO
              FOR j := 0 TO allSyms.size()-1 DO
                Debug.Out("Symbol: " & Atom.ToText(allSyms.get(j)));

                returnCode := AstToType.OneStubScm(c, 
                                                   NEW(Type.Qid,
                                                       intf := objects[i].intf,
                                                       item := allSyms.get(j)),
                                                   Stdio.stdout)
              END
            END
          END
        END
      END;

      IF StubGenTool.stubTypes # NIL THEN
        WITH objects = StubGenTool.stubTypes DO
          FOR i := 0 TO LAST(objects^) DO
            IF objects[i] # NIL THEN
              returnCode := AstToType.OneStubScm(c, objects[i], Stdio.stdout)
            END
          END
        END
      END;
      RTCollector.EnableMotion();
    END;
    RETURN returnCode
  END DoRun;

PROCEDURE ScmArgsOrZeroLength() : REF ARRAY OF TEXT =
  BEGIN
    WITH x = M3Args.GetStringList(StubGenTool.tool_g, ScmFlag) DO
      IF x = NIL THEN RETURN NEW(REF ARRAY OF TEXT, 0) ELSE RETURN x END
    END
  END ScmArgsOrZeroLength;

  <* FATAL ANY *>
CONST ScmFlag = "scm";

(* NO-READLINE support follows ........ *)
CONST NoReadlineFlag = "noreadline";

CONST NoInteractiveFlag = "nointeractive";

CONST UnsafeFlag = "unsafe";

TYPE 
  Interrupter = Scheme.Interrupter OBJECT
  OVERRIDES
    interrupt := Interrupt;
  END;

PROCEDURE Interrupt(<*UNUSED*>i : Interrupter) : BOOLEAN =
  BEGIN
    IF Csighandler.have_signal() = 1 THEN 
      Csighandler.clear_signal();
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END Interrupt;

VAR
  env := NEW(SchemeNavigatorEnvironment.T).initEmpty();
BEGIN
  M3Args.RegisterStringList(StubGenTool.tool_g, ScmFlag, "scheme command file", shared := FALSE);
  M3Args.RegisterFlag(StubGenTool.tool_g, NoReadlineFlag, "no readline support", shared := FALSE);
  M3Args.RegisterFlag(StubGenTool.tool_g, NoInteractiveFlag, "no interactive support", shared := FALSE);
  M3Args.RegisterFlag(StubGenTool.tool_g, UnsafeFlag, "unsafe Scheme environment (no thread locking)", shared := FALSE);

  WITH res = ABS(M3ToolFrame.Startup(
                       NEW(M3ToolFrame.Worker, work := DoRun),
                       compile := FALSE)) DO

    IF res # 0 THEN Process.Exit(res) END;

    EVAL SchemeModula3Types.Extend(SchemeM3.GetPrims());

    WITH  args = ScmArgsOrZeroLength()^ DO
      TRY
        IF M3Args.GetFlag(StubGenTool.tool_g, UnsafeFlag) THEN
          env := NEW(SchemeEnvironment.Unsafe).initEmpty()
        END;

        WITH scm = NEW(SchemeM3.T).init(ARRAY OF Pathname.T { "require" }, 
                                        globalEnv := env) DO
          scm.bind("the-types", AstToType.typeList);
          scm.bind("the-exceptions", AstToType.exceptionList);
          scm.bind("the-vars", AstToType.varList);
          scm.bind("the-procs", AstToType.procList);
          scm.bind("the-consts", AstToType.constList);
          scm.bind("the-protos", TypeTranslator.protoList);
          scm.bind("the-basetypes", TypeTranslator.basetypeList);
          scm.bind("the-sourcefiles", AstToType.filenames);
          scm.bind("is-unsafe", SchemeBoolean.FromBool(AstToType.isUnsafe));
          FOR i := FIRST(args) TO LAST(args) DO
            Debug.Out("Loading SCM file " & args[i]);
            WITH str = SchemeString.FromText(args[i]) DO
              EVAL scm.loadFile(str)
            END
          END;

          IF NOT M3Args.GetFlag(StubGenTool.tool_g, NoInteractiveFlag) THEN
            IF M3Args.GetFlag(StubGenTool.tool_g, NoReadlineFlag) THEN
              Csighandler.install_int_handler();
              scm.readEvalWriteLoop(NEW(Interrupter))
            ELSE
              MainLoop(NEW(ReadLine.Default).init(), scm)
            END
          END
        END
      EXCEPT
        Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
      |
        IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
      |
        ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
      |
        NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
          AL.Format(err))
      END
    END;
    Process.Exit(res)
  END
END Main.



