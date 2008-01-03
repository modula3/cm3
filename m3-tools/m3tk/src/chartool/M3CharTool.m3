(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

MODULE M3CharTool;

IMPORT AST, ASTWalk;
IMPORT M3Args, M3Context, M3CUnit, M3Conventions;
IMPORT M3AST_AS;
IMPORT M3AST_AS_F, M3AST_FE_F;
IMPORT M3CGo;
IMPORT M3Error;
IMPORT M3CharTypesToChange, M3CharStatsToConsider,
       M3CharExprsToReplace, M3CharExprsToConsider;

CONST Version = "Sep-28-92";

VAR tool_g: M3Args.T;

PROCEDURE Get (): M3Args.T RAISES {} =
  BEGIN
    RETURN tool_g;
  END Get;

TYPE
  Extension =
    M3CGo.Extension OBJECT OVERRIDES extend := Extend END;

TYPE
  ASTWalkClosure =
    ASTWalk.Closure OBJECT
      typesToChangeHandle  : M3CharTypesToChange.Handle   := NIL;
      statsToConsiderHandle: M3CharStatsToConsider.Handle := NIL;
      exprsToReplaceHandle : M3CharExprsToReplace.Handle  := NIL;
    OVERRIDES
      callback := Node; END;

PROCEDURE Init () RAISES {} =
  BEGIN
    M3CGo.AddExtension(NEW(Extension));
  END Init;

PROCEDURE Extend (<*UNUSED*> e      : M3CGo.Extension;
                  <*UNUSED*> context: M3Context.T;
                             cu     : M3AST_AS.Compilation_Unit;
                  <*UNUSED*> VAR (*inout*) phases: M3CUnit.Status; )
  RAISES {} =
  VAR
    exprsToConsider_Arg := M3Args.GetFlag(tool_g, ExprsToConsider_Arg);
    typesToChange_Arg := M3Args.GetFlag(tool_g, TypesToChange_Arg);
    statsToConsider_Arg := M3Args.GetFlag(tool_g, StatsToConsider_Arg);
    exprsToReplace_Arg := M3Args.GetFlag(tool_g, ExprsToReplace_Arg);
    exprsDistantlyRelated_Arg := M3Args.GetFlag(
                       tool_g, ExprsDistantlyRelated_Arg);
    all    := M3Args.GetFlag(tool_g, ConsiderAll_Arg);
    do_etc := all OR exprsToConsider_Arg;
    do_edr := all OR exprsDistantlyRelated_Arg;
    any := all OR exprsToConsider_Arg OR typesToChange_Arg OR
               statsToConsider_Arg OR exprsToReplace_Arg OR
               exprsDistantlyRelated_Arg;
  BEGIN
    IF M3Conventions.PrimarySource IN cu.fe_status
         AND M3CUnit.State.SemChecked IN cu.fe_status THEN
      IF any THEN
        IF cu.fe_status * M3CUnit.Errors = M3CUnit.Status{} THEN
          TYPECASE cu.as_root OF
           | NULL =>
             (* no tree to analyse *)

           | M3AST_AS.UNIT_GEN_DEF =>
             (* ignore generic definitions *)

          ELSE
            WITH cl = NEW(ASTWalkClosure) DO
              IF all OR typesToChange_Arg THEN
                cl.typesToChangeHandle :=
                  M3CharTypesToChange.NewHandle(); END;
              IF all OR statsToConsider_Arg THEN
                cl.statsToConsiderHandle :=
                  M3CharStatsToConsider.NewHandle(); END;
              IF all OR exprsToReplace_Arg THEN
                cl.exprsToReplaceHandle :=
                  M3CharExprsToReplace.NewHandle(); END;

              IF any THEN
                <*FATAL ANY*> BEGIN
                  ASTWalk.ModeVisitNodes(
                      cu, cl, ASTWalk.OnEntryAndExit);
                END;
              END;
            END (* with *);
            IF do_etc OR do_edr THEN
              <*FATAL ANY*> BEGIN
                ASTWalk.ModeVisitNodes(
                    cu, M3CharExprsToConsider.NewHandle(do_etc, do_edr),
                    ASTWalk.OnEntryAndExit);
              END;
            END (*if*);
          END; (* typecase *)
        ELSE
          M3Error.Warn(cu.as_root,
                       "'m3chartool' analysis suppressed due to errors");
        END;
      END;
    END;
  END Extend;

PROCEDURE Node (cl: ASTWalkClosure;
                n : AST.NODE;
                vm: ASTWalk.VisitMode) =
  BEGIN
    IF cl.typesToChangeHandle # NIL THEN
      M3CharTypesToChange.Node(cl.typesToChangeHandle, n, vm) END;
    IF cl.statsToConsiderHandle # NIL THEN
      M3CharStatsToConsider.Node(cl.statsToConsiderHandle, n, vm) END;
    IF cl.exprsToReplaceHandle # NIL THEN
      M3CharExprsToReplace.Node(cl.exprsToReplaceHandle, n, vm) END;
  END Node;


BEGIN
  tool_g :=
    M3Args.New(
      "m3char", "analyse code for CHAR size dependencies", Version);
  M3Args.RegisterFlag(
    tool_g, TypesToChange_Arg, "find types to change");
  M3Args.RegisterFlag(
    tool_g, StatsToConsider_Arg, "statements to think about");
  M3Args.RegisterFlag(
    tool_g, ExprsToReplace_Arg,
    "expressions requiring systematic replacement");
  M3Args.RegisterFlag(
    tool_g, ExprsToConsider_Arg, "expressions needing thought");
  M3Args.RegisterFlag(
    tool_g, ExprsDistantlyRelated_Arg,
    "expressions distantly related to CHAR");
  M3Args.RegisterFlag(
    tool_g, ConsiderAll_Arg, "consider all the above situations");
END M3CharTool.

