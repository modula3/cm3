(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE MODULE WiredStandard;

IMPORT AST_Init;
IMPORT TextRd;
IMPORT M3Context, M3CGo, M3CUnit, M3CUnitRep, M3AST_AS, M3Conventions;

IMPORT M3AST_FE_F;

IMPORT StandardAsText;
IMPORT Date;

CONST
  Version = Date.T{1993, Date.Month.Apr, 2, 0, 0, 0, 0, "", Date.WeekDay.Fri};
  (* increase this when StandardAsText changes *)

PROCEDURE Set(c: M3Context.T) RAISES {}=
  <*FATAL Date.Error*>
  VAR
    s := TextRd.New(StandardAsText.Get());
    cu: M3AST_AS.Compilation_Unit := NEW(M3AST_AS.Compilation_Unit).init();
    phases := M3CUnit.AllPhases;
  BEGIN
    M3Context.SetStandard(cu);
    cu.fe_uid := NEW(M3CUnit.Uid, filename := M3Conventions.Standard,
                     stamp := Date.ToTime(Version));
    M3CGo.CompileUnit(cu, c, s, Void, phases, NIL);
  END Set;


PROCEDURE Void(
    <*UNUSED*> name: TEXT;
    <*UNUSED*> unitType: M3CUnit.Type;
    <*UNUSED*> context: M3Context.T;
    <*UNUSED*> VAR (*out*) cu: M3AST_AS.Compilation_Unit
): BOOLEAN RAISES {}=
  BEGIN
    <*ASSERT FALSE*>
  END Void;

BEGIN

END WiredStandard.
