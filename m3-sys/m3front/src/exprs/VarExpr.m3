(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: VarExpr.m3                                            *)
(* Last modified on Fri Feb 24 16:48:13 PST 1995 by kalsow     *)
(*      modified on Sun Jan 21 10:57:47 1990 by muller         *)

MODULE VarExpr;

IMPORT M3, M3ID, Expr, ExprRep, Type, Value, Variable;

TYPE
  P = Expr.T OBJECT
        v : Variable.T;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NoPrep;
        compileLV    := CompileLV;
        prepBR       := ExprRep.PrepNoBranch;
        compileBR    := ExprRep.NoBranch;
        evaluate     := ExprRep.NoValue;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsAlways;
        isDesignator := ExprRep.IsAlways;
	isZeroes     := ExprRep.IsNever;
	genFPLiteral := ExprRep.NoFPLiteral;
	prepLiteral  := ExprRep.NoPrepLiteral;
	genLiteral   := ExprRep.NoLiteral;
        note_write   := NoteWrites;
      END;

PROCEDURE New (t: Type.T;  name: M3ID.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.v := Variable.New (name, TRUE);
    p.type := Type.Base (t);
    Variable.BindType (p.v, t, indirect := FALSE, readonly := FALSE,
                       open_array_ok := FALSE,  needs_init := TRUE);
    RETURN p;
  END New;

PROCEDURE Obj (e: Expr.T): Variable.T =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN NIL;
    | P(p) => RETURN p.v;
    ELSE      RETURN NIL;
    END;
  END Obj;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    Value.TypeCheck (p.v, cs);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN (a.v = b.v);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE NeedsAddress (p: P) =
  BEGIN
    Variable.NeedsAddress (p.v);
  END NeedsAddress;

PROCEDURE Compile (p: P) =
  BEGIN
    Variable.Load (p.v);
  END Compile;

PROCEDURE CompileLV (p: P) =
  BEGIN
    Variable.LoadLValue (p.v);
  END CompileLV;

PROCEDURE NoteWrites (p: P) =
  BEGIN
    Variable.ScheduleTrace (p.v);
  END NoteWrites;

BEGIN
END VarExpr.
