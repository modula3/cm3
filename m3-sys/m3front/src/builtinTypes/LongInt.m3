(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LongInt.m3                                            *)

MODULE LongInt;

IMPORT M3, Type, Target, Tipe, TypeRep, TipeMap, TipeDesc;

TYPE
  P = Type.T BRANDED "LongInt.T" OBJECT
      OVERRIDES
        check      := Check;
        check_align:= TypeRep.ScalarAlign;
        isEqual    := TypeRep.NeverEqual;
        isSubtype  := TypeRep.NoSubtypes;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := TypeRep.InitToZeros;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

PROCEDURE Check (p: P) =
  BEGIN
    p.info.size      := Target.Int64.size;
    p.info.min_size  := Target.Int64.size;
    p.info.alignment := Target.Int64.align;
    p.info.mem_type  := Target.Int64.cg_type;
    p.info.stk_type  := Target.Int64.cg_type;
    p.info.class     := Type.Class.LongInt;
    p.info.isTraced  := FALSE;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := 6;
  END Check;

PROCEDURE Compiler (<*UNUSED*> p: P) =
  BEGIN
  END Compiler;

PROCEDURE InitCoster (<*UNUSED*> t: Type.T; 
                      <*UNUSED*> zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN 0;
  END InitCoster;

PROCEDURE FPrinter (<*UNUSED*> t: Type.T;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag := "$longint";
    x.n_nodes := 0;
  END FPrinter;

PROCEDURE GenMap (<*UNUSED*> p: P; offset: INTEGER; 
                  <*UNUSED*> size: INTEGER; refs_only: BOOLEAN) =
  VAR IntBytes := Target.Int64.bytes;
  BEGIN
    IF    (refs_only)    THEN (* skip *)
    ELSIF (IntBytes = 4) THEN TipeMap.Add (offset, TipeMap.Op.Int_4, 0);
    ELSIF (IntBytes = 8) THEN TipeMap.Add (offset, TipeMap.Op.Int_8, 0);
    ELSIF (IntBytes = 2) THEN TipeMap.Add (offset, TipeMap.Op.Int_2, 0);
    ELSIF (IntBytes = 1) THEN TipeMap.Add (offset, TipeMap.Op.Int_1, 0);
    ELSE  <*ASSERT FALSE *>
    END;
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    EVAL TipeDesc.AddO (TipeDesc.Op.Integer, p);
  END GenDesc;

PROCEDURE Initialize () =
  BEGIN
    T := NEW (P);
    TypeRep.Init (T, Type.Class.LongInt);
    Tipe.Define ("LONGINT", T, TRUE);
  END Initialize;

BEGIN
END LongInt.
