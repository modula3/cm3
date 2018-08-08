(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: SetType.m3                                            *)
(* Last modified on Tue May 23 15:28:01 PDT 1995 by kalsow     *)
(*      modified on Wed Sep 26 19:01:24 1990 by muller         *)

MODULE SetType;

IMPORT M3, CG, Type, TypeRep, Target, TInt, Error, Token, Scanner;
IMPORT Word, TipeMap, TipeDesc, ErrType;

TYPE
  P = Type.T OBJECT
        range      : Type.T;
      OVERRIDES
        check      := Check;
        no_straddle:= TypeRep.ScalarNoStraddle;
        isEqual    := EqualChk;
        isSubtype  := Subtyper;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := TypeRep.InitToZeros;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

(* EXPORTED: *)
PROCEDURE Parse (): Type.T =
  TYPE TK = Token.T;
  VAR p := NEW (P);
  BEGIN
    TypeRep.Init (p, Type.Class.Set);
    Scanner.Match (TK.tSET);
    Scanner.Match (TK.tOF);
    p.range := Type.Parse ();
    RETURN p;
  END Parse;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Set) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

(* EXPORTED: *)
PROCEDURE Split (t: Type.T;  VAR range: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    range := p.range;
    RETURN TRUE;
  END Split;

PROCEDURE Check (p: P) =
  VAR info: Type.Info;
  BEGIN
    p.range := Type.CheckInfo (p.range, info);
    IF NOT Type.IsOrdinal (p.range) THEN
      Error.Msg ("domain of a set type must be an ordinal type");
      p.range := ErrType.T;
    END;

    p.info.size      := SizeOf (p);
    p.info.min_size  := p.info.size;
    p.info.alignment := MAX (Target.Integer.align, Target.Structure_size_boundary);
    p.info.class     := Type.Class.Set;
    p.info.isTraced  := FALSE;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := Word.Times (811, info.hash);
    IF (p.info.size <= Target.Integer.size) THEN
      p.info.mem_type := Target.Word.cg_type;
      p.info.stk_type := Target.Word.cg_type;
    ELSE
      p.info.mem_type  := CG.Type.Struct;
      p.info.stk_type  := CG.Type.Addr;
      p.info.addr_align := Target.Address.align;
    END;
  END Check;

PROCEDURE Compiler (p: P) =
  VAR info: Type.Info;
  BEGIN
    Type.Compile (p.range);
    EVAL Type.CheckInfo (p, info);
    CG.Declare_set (Type.GlobalUID (p), Type.GlobalUID (p.range), info.size);
  END Compiler;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN Type.IsEqual (a.range, b.range, x);
  END EqualChk;

PROCEDURE Subtyper (a: P;  t: Type.T): BOOLEAN =
  BEGIN
    RETURN Type.IsEqual (a, t, NIL);
  END Subtyper;

PROCEDURE SizeOf (p: P): INTEGER =
  VAR n: INTEGER;  Grain := Target.Integer.size;
  BEGIN
    IF NOT TInt.ToInt (Type.Number (p.range), n) THEN RETURN -1 END;
    RETURN (n + Grain - 1) DIV Grain * Grain;
  END SizeOf;

PROCEDURE InitCoster (<*UNUSED*> p: P; <*UNUSED*> zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN 0;
  END InitCoster;

PROCEDURE GenMap (<*UNUSED*> p: P; offset, size: INTEGER;
                  <*UNUSED*> refs_only: BOOLEAN) =
  BEGIN
    TipeMap.Add (offset, TipeMap.Op.Set_1, size DIV Target.Byte);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF TipeDesc.AddO (TipeDesc.Op.Set, p) THEN
      TipeDesc.AddX (Type.Number (p.range));
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag      := "SET";
    x.n_nodes  := 1;
    x.nodes[0] := p.range;
  END FPrinter;
        
BEGIN
END SetType.
