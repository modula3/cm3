MODULE M3CWarnTool;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3Context, M3CUnit, M3Args, M3Conventions, M3CGo;
IMPORT AST, ASTWalk, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_FE_F;

IMPORT M3CChkRaises, M3CChkUses, M3CChkReturn, M3CChkNarrow,
       M3CChkObsolete;

CONST
  Version = "09-Oct-92";

VAR
  tool_g: M3Args.T;

PROCEDURE Get(): M3Args.T RAISES {}=
  BEGIN
    RETURN tool_g;
  END Get;

TYPE Extension = M3CGo.Extension OBJECT OVERRIDES extend := Extend END;

PROCEDURE Init() RAISES {}=
  BEGIN
    M3CGo.AddExtension(NEW(Extension));
  END Init;

TYPE ASTWalkClosure = ASTWalk.Closure OBJECT
    usesHandle: M3CChkUses.Handle := NIL;
    raisesHandle: M3CChkRaises.Handle := NIL;
    returnHandle: M3CChkReturn.Handle := NIL;
    narrowHandle: M3CChkNarrow.Handle := NIL;
    obsHandle: M3CChkObsolete.Handle := NIL;
  OVERRIDES
    callback := Node;
  END;

PROCEDURE Extend(
    <*UNUSED*> e: M3CGo.Extension;
    <*UNUSED*> context: M3Context.T;
    cu: M3AST_AS.Compilation_Unit;
    <*UNUSED*> VAR (*inout*) phases: M3CUnit.Status;
    ) RAISES {}=
  VAR
    all := M3Args.GetFlag(tool_g, WA_Arg);
    any := all;
  BEGIN
    IF M3Conventions.PrimarySource IN cu.fe_status AND
       M3CUnit.State.SemChecked IN cu.fe_status THEN
      TYPECASE cu.as_root OF
      | NULL =>
          (* no tree to analyse *)

      | M3AST_AS.UNIT_GEN_DEF =>
          (* ignore generic definitions *)
      
      ELSE
       WITH cl = NEW(ASTWalkClosure) DO
          IF all OR M3Args.GetFlag(tool_g, WU_Arg) THEN
            any := TRUE;
            cl.usesHandle := M3CChkUses.NewHandle(cu,
                M3Args.GetFlag(tool_g, WIF_Arg));
          END;
          IF all OR M3Args.GetFlag(tool_g, WE_Arg) THEN
            any := TRUE;
            cl.raisesHandle := M3CChkRaises.NewHandle(cu);
          END;
          IF all OR M3Args.GetFlag(tool_g, WR_Arg) THEN
            any := TRUE;
            cl.returnHandle := M3CChkReturn.NewHandle(cu);
          END;
          IF all OR M3Args.GetFlag(tool_g, WN_Arg) THEN
            any := TRUE;
            cl.narrowHandle := M3CChkNarrow.NewHandle(cu);
          END;
          IF all OR M3Args.GetFlag(tool_g, WO_Arg) THEN
            any := TRUE;
            cl.obsHandle := M3CChkObsolete.NewHandle(cu);
          END;
          IF any THEN
            <*FATAL ANY*> BEGIN
              ASTWalk.ModeVisitNodes(cu, cl, ASTWalk.OnEntryAndExit);
            END;
            IF cl.usesHandle # NIL THEN M3CChkUses.CloseHandle(cl.usesHandle) END;
          END;
       END;
      END; (* typecase *)
    END;
  END Extend;

PROCEDURE Node(cl: ASTWalkClosure; n: AST.NODE; vm: ASTWalk.VisitMode)=
  BEGIN
    IF cl.usesHandle # NIL THEN M3CChkUses.Node(cl.usesHandle, n, vm) END;
    IF cl.raisesHandle # NIL THEN M3CChkRaises.Node(cl.raisesHandle, n, vm) END;
    IF cl.returnHandle # NIL THEN M3CChkReturn.Node(cl.returnHandle, n, vm) END;
    IF cl.narrowHandle # NIL THEN M3CChkNarrow.Node(cl.narrowHandle, n, vm) END;
    IF cl.obsHandle # NIL THEN M3CChkObsolete.Node(cl.obsHandle, n, vm) END;
  END Node;


BEGIN
  tool_g := M3Args.New("m3cwarn", "analyse code and warn of problems", 
      Version);
  M3Args.RegisterFlag(tool_g, WE_Arg, "check for uncaught exceptions");
  M3Args.RegisterFlag(tool_g, WU_Arg, "check for unused declarations");
  M3Args.RegisterFlag(tool_g, WIF_Arg,
    "don\'t warn about unused FOR loop control variables");
  M3Args.RegisterFlag(tool_g, WR_Arg, "check for missing returns");
  M3Args.RegisterFlag(tool_g, WN_Arg, "check for implicit NARROWs");
  M3Args.RegisterFlag(tool_g, WO_Arg, "check for using OBSOLETEs");
  M3Args.RegisterFlag(tool_g, WA_Arg, "check for all of the above");
END M3CWarnTool.
