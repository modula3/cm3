(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3UnsetAttTool;

IMPORT AST, ASTWalk;
IMPORT M3Args, M3Context, M3CUnit, M3Conventions, M3Error;
IMPORT M3AST, M3AST_AS, M3ASTOp_SM;
IMPORT M3AST_AS_F, M3AST_FE_F;
IMPORT M3CGo;

CONST
  Version = "Oct-1-92";

VAR
  tool_g: M3Args.T;

PROCEDURE Get(): M3Args.T RAISES {}=
  BEGIN
    RETURN tool_g;
  END Get;

TYPE 
  Extension = M3CGo.Extension OBJECT OVERRIDES extend := Extend END;
  CheckSetClosure = M3ASTOp_SM.CheckSetClosure OBJECT
  OVERRIDES callback := Report;
  END;
  ASTWalkClosure = ASTWalk.Closure OBJECT
    csl: CheckSetClosure;
  OVERRIDES callback := CheckSet;
  END;

PROCEDURE Init() RAISES {}=
  BEGIN
    M3CGo.AddExtension(NEW(Extension));
  END Init;

PROCEDURE Extend(
    <*UNUSED*> e: M3CGo.Extension;
    <*UNUSED*> context: M3Context.T;
    cu: M3AST_AS.Compilation_Unit;
    <*UNUSED*> VAR (*inout*) phases: M3CUnit.Status;
    ) RAISES {}=
  BEGIN
    IF NOT M3Args.GetFlag(tool_g, CheckUnsetAtts_Arg) THEN RETURN END;

    IF M3Conventions.PrimarySource IN cu.fe_status AND
       M3CUnit.State.SemChecked IN cu.fe_status THEN
      TYPECASE cu.as_root OF
      | NULL =>
          (* no tree to analyse *)

      | M3AST_AS.UNIT_GEN_DEF =>
          (* ignore generic definitions *)
      
      ELSE
        WITH csl = NEW(CheckSetClosure),
             cl = NEW(ASTWalkClosure, csl := csl) DO
          <*FATAL ANY*> BEGIN
            ASTWalk.VisitNodes(cu, cl);
          END;
       END;
      END; (* typecase *)
    END;
  END Extend;

PROCEDURE CheckSet(cl: ASTWalkClosure; n: AST.NODE;
                   <*UNUSED*> vm: ASTWalk.VisitMode)=
  VAR m3n: M3AST.NODE := NARROW(n, M3AST.NODE);
  BEGIN
    m3n.checkSet(cl.csl);
  END CheckSet;

PROCEDURE Report(<*UNUSED*> cl: M3ASTOp_SM.CheckSetClosure; n: M3AST.NODE)=
  BEGIN
    M3Error.Report(n, "unset semantic attribute");
  END Report;

BEGIN
  tool_g := M3Args.New("m3unsetatt", 
      "check for unset attributes in AST", 
      Version);
  M3Args.RegisterFlag(tool_g, CheckUnsetAtts_Arg,
                      "check for unset semantic atributes");

END M3UnsetAttTool.
