(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LInt.m3                                               *)
(* Last Modified On Tue May 23 15:32:32 PDT 1995 by kalsow     *)
(*      Modified On Fri Dec 21 01:48:33 1990 by muller         *)

MODULE LInt;

IMPORT M3, Type, Target, Tipe, TypeRep, TipeMap, TipeDesc;

TYPE
  P = Type.T BRANDED "LInt.T" OBJECT
      OVERRIDES
        check      := Check;
        no_straddle:= TypeRep.ScalarNoStraddle;
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
    p.info.size      := Target.Longint.size;
    p.info.min_size  := Target.Longint.size;
    p.info.alignment := Target.Longint.align;
    p.info.mem_type  := Target.Longint.cg_type;
    p.info.stk_type  := Target.Longint.cg_type;
    p.info.class     := Type.Class.Longint;
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

PROCEDURE GenMap (<*UNUSED*> p: P; offset, size: INTEGER; refs_only: BOOLEAN) =
  VAR IntBytes := Target.Longint.bytes;
  BEGIN
    <*ASSERT size = Target.Longint.size*>
    IF    (refs_only)    THEN (* skip *)
    ELSIF (IntBytes = 8) THEN TipeMap.Add (offset, TipeMap.Op.Int_8, 0);
    ELSIF (IntBytes = 4) THEN TipeMap.Add (offset, TipeMap.Op.Int_4, 0);
    ELSIF (IntBytes = 2) THEN TipeMap.Add (offset, TipeMap.Op.Int_2, 0);
    ELSIF (IntBytes = 1) THEN TipeMap.Add (offset, TipeMap.Op.Int_1, 0);
    ELSE  <*ASSERT FALSE *>
    END;
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    EVAL TipeDesc.AddO (TipeDesc.Op.Longint, p);
  END GenDesc;

PROCEDURE Initialize () =
  BEGIN
    T := NEW (P);
    TypeRep.Init (T, Type.Class.Longint);
    Tipe.Define ("LONGINT", T, TRUE);
  END Initialize;

BEGIN
END LInt.
