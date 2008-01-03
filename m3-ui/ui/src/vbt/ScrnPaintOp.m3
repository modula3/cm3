(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:58:08 PST 1992 by muller   *)
(*      modified on Wed Sep 11 15:24:17 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE ScrnPaintOp;

IMPORT TrestleComm, Word;

REVEAL T = Public BRANDED OBJECT END; Private = BRANDED OBJECT END;

PROCEDURE ConstructPlanewiseOp(
  pwo: PlaneWiseOracle;
  READONLY bitOps: ARRAY OF BitOp): T 
RAISES {Failure, TrestleComm.Failure} =
  VAR 
    res := pwo.transparent(); temp: T;
    mask := ARRAY [0..31] OF BOOLEAN {FALSE, ..};
    allones := Word.Minus(Word.Shift(1, NUMBER(bitOps)), 1);
    zero := pwo.opaque(0);
    one := pwo.opaque(allones);
    trans := res;
    swap := pwo.swap(0,allones);
  BEGIN
    FOR i := 0 TO LAST(bitOps) DO
      CASE bitOps[i] OF
        BitOp.Zero => temp := zero
      | BitOp.And => temp := pwo.bgfg(zero, trans)
      | BitOp.NotAnd => temp := pwo.bgfg(zero,swap)
      | BitOp.Src => temp := pwo.bgfg(zero,one)
      | BitOp.AndNot => temp := pwo.bgfg(trans, zero)
      | BitOp.Dest =>  temp := trans
      | BitOp.Xor => temp := pwo.bgfg(trans, swap)
      | BitOp.Or => temp := pwo.bgfg(trans,one)
      | BitOp.Nor => temp := pwo.bgfg(swap, zero)
      | BitOp.Equal => temp := pwo.bgfg(swap, trans)
      | BitOp.Invert => temp := swap 
      | BitOp.NotOr => temp := pwo.bgfg(swap,one)
      | BitOp.NotSrc => temp := pwo.bgfg(one,zero)
      | BitOp.OrNot => temp := pwo.bgfg(one,trans) 
      | BitOp.Nand => temp := pwo.bgfg(one,swap) 
      | BitOp.One => temp := one
      END;
      IF res = trans THEN 
        res := temp
      ELSIF temp # trans THEN
        res := pwo.planewise(SUBARRAY(mask, 0, NUMBER(bitOps)), res, temp)
      END;
      mask[i] := TRUE
    END;
    RETURN res
  END ConstructPlanewiseOp;

BEGIN END ScrnPaintOp.
