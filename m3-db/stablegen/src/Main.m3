(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Susan Owicki                                     *)
(* Last modified on Mon Sep 26 19:27:14 PDT 1994 by weich      *)

(* The main module of the stable stub generator starts by
   using a "M3ToolFrame.Startup" which starts "DoRun", the
   main working procedure, as call back (see there). *)

MODULE Main;

IMPORT Process, RTCollector;
IMPORT Type, TypeNames, StablegenArgs, StablegenError, GenCode;
IMPORT M3CFETool, M3ToolFrame, M3Context;

(* \subsection{Procedure DoRun}
   "DoRun" reads the run string
   parameter to gather the interface where the "T" which
   shall be made stable is defined (together with tool kit
   options.  It then starts "M3CFETool.CompileInContext()"
   to produce an abstract syntax tree of that type.

   A preprocess run "TypeNames.Preprocess()" will assign an
   attribute on each node of the AST which is a type
   specification.  A ``type specification'' is a type that
   is not just an identifier.  The attribute is the name of
   that type.

   Finally "AstToType.OneStub()" is called to produce the
   implementation of the stable subtype.
*)
PROCEDURE DoRun (<*UNUSED*> w: M3ToolFrame.Worker;
                            c: M3Context.T;
                 <*UNUSED*> compileResult: INTEGER): INTEGER
  RAISES {} =
  BEGIN
    TRY
      VAR object: Type.Qid;
          reveal, impl, rep: TEXT;
      BEGIN
        StablegenArgs.Get(object, reveal, impl, rep);
        IF M3CFETool.CompileInContext(c) < 0 THEN
          RAISE StablegenError.E("errors in object declaration");
        END;
        RTCollector.DisableMotion(); (* because of
                                          "FRefRefTbl"
                                          module *)
        TypeNames.Preprocess(c);
        GenCode.Do(c, object, reveal, impl, rep);
        RTCollector.EnableMotion();
        RETURN 0;
      END
    EXCEPT
      StablegenError.E(msg)=> StablegenError.Failure(msg);
        RETURN 1;
    END;
  END DoRun;

(* \subsection{Startup} *)

<* FATAL ANY *>
BEGIN
  Process.Exit(ABS(M3ToolFrame.Startup(
                     NEW(M3ToolFrame.Worker, work := DoRun),
                     compile := FALSE)));
END Main.


