(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ProcExpr.m3                                           *)
(* Last modified on Fri Feb 24 16:48:44 PST 1995 by kalsow     *)
(*      modified on Tue Oct 10 18:42:24 1989 by muller         *)

MODULE ProcExpr;
(* A PROCEDURE constant. *) 

IMPORT M3, CG, Expr, ExprRep, Type, Value, Procedure, M3Buf;
IMPORT Scope;

TYPE
  P = Expr.T OBJECT
        proc : Value.T;
      OVERRIDES
        typeOf       := TypeOf;
        check        := Check;
        need_addr    := ExprRep.NotAddressable;
        prep         := ExprRep.NoPrep;
        compile      := Compile;
        prepLV       := ExprRep.NotLValue;
        compileLV    := ExprRep.NotLValue;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := ExprRep.Self;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := ExprRep.IsNever;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := ExprRep.NoPrepLiteral;
        genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
      END;

PROCEDURE New (proc: Value.T): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.proc := Value.Base (proc);
    RETURN p;
  END New;

PROCEDURE Split (e: Expr.T;  VAR proc: Value.T): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => proc := p.proc; RETURN TRUE;
    ELSE      RETURN FALSE;
    END;
  END Split;

PROCEDURE Compare (a, b: Expr.T;  VAR sign: INTEGER): BOOLEAN =
  VAR x, y: Value.T;
  BEGIN
    TYPECASE a OF
    | NULL => RETURN FALSE;
    | P(p) => x := p.proc;
    ELSE      RETURN FALSE;
    END;
    TYPECASE b OF
    | NULL => RETURN FALSE;
    | P(p) => y := p.proc;
    ELSE      RETURN FALSE;
    END;
    IF (x = y) THEN sign := 0 ELSE sign :=  -3 END;
    RETURN TRUE;
  END Compare;

PROCEDURE TypeOf (p: P): Type.T =
  BEGIN
    RETURN Value.TypeOf (p.proc);
  END TypeOf;

PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  BEGIN
    Value.TypeCheck (p.proc, cs);
    p.type := Value.TypeOf (p.proc);
  END Check;

PROCEDURE EqCheck (a: P;  e: Expr.T;  <*UNUSED*> x: M3.EqAssumption): BOOLEAN =
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(b) => RETURN Procedure.IsEqual (a.proc, b.proc);
    ELSE      RETURN FALSE;
    END;
  END EqCheck;

PROCEDURE Compile (p: P) =
  BEGIN
    Value.Load (p.proc);
  END Compile;

PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  VAR s: Scope.IDStack;
  BEGIN
    s.top := 0;
    Scope.NameToPrefix (p.proc, s, dots := TRUE);
    Scope.PutStack (buf, s);
  END GenFPLiteral;

PROCEDURE GenLiteral (p: P;  offset: INTEGER;  <*UNUSED*>type: Type.T;
                      is_const: BOOLEAN) =
  BEGIN
    CG.Init_proc (offset, Procedure.CGName (p.proc), is_const);
  END GenLiteral;

BEGIN
END ProcExpr.
