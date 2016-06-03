(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE SimFP;

IMPORT Long;

CONST
  kUint64MSB = 16_8000000000000000L;
  
REVEAL

  GFP = Public BRANDED OBJECT
    sig (*significand or float*) : Uint64;
    exp (*exponent*) : INTEGER; 
  OVERRIDES
    init := Init;
    f := GetSig;
    e := GetExp;
    setF := SetSig;
    setE := SetExp;
    minus := Minus;
    times := Times;
    normalize := Normalize;
  END;
  
PROCEDURE Init(<*UNUSED*>self : GFP; sig : Uint64; exp : INTEGER) : GFP =
  BEGIN
    RETURN NEW(GFP, sig := sig, exp := exp);
  END Init;
  
PROCEDURE GetSig(self : GFP) : Uint64 =
  BEGIN
    RETURN self.sig;
  END GetSig;

PROCEDURE GetExp(self : GFP) : INTEGER =
  BEGIN
    RETURN self.exp;
  END GetExp;
  
PROCEDURE SetSig(self : GFP; val : Uint64) =
  BEGIN
    self.sig := val;
  END SetSig;

PROCEDURE SetExp(self : GFP; val : INTEGER) =
  BEGIN
    self.exp := val;
  END SetExp;
  
PROCEDURE Minus(self : GFP; val :GFP) : GFP =
  VAR result := NEW(GFP).init(self.f(), self.e());
  BEGIN
    <*ASSERT self.exp = val.exp *>
    <*ASSERT Long.GE(self.sig, val.sig) *>
    result.sig := self.sig - val.sig;
    RETURN result;
  END Minus;

PROCEDURE Times(self : GFP; val :GFP) : GFP =
  (* Simply "emulates" a 128 bit multiplication.
     However: the resulting number only contains 64 bits. The least
     significant 64 bits are only used for rounding the most significant 64
     bits.*)
  CONST
    kM32 = 16_FFFFFFFFL;
  VAR
    a,b,c,d,ac,bc,ad,bd,tmp,f : Uint64;
    result := NEW(GFP).init(self.f(), self.e());    
  BEGIN
    a := Long.RightShift(self.sig, 32);
    b := Long.And(self.sig, kM32);
    c := Long.RightShift(val.sig, 32);
    d := Long.And(val.sig, kM32);
    ac := a * c; bc := b * c; ad := a * d; bd := b * d;
    tmp := Long.RightShift(bd, 32) + Long.And(ad, kM32) + Long.And(bc, kM32);
    (* By adding 1 << 31 to tmp we round the final result.
       Halfway cases will be rounded up.*)
    tmp := tmp + Long.LeftShift(1L, 31);
    f := ac + Long.RightShift(ad, 32) + Long.RightShift(bc, 32) + Long.RightShift(tmp, 32);
    result.exp := result.exp + val.exp + 64;
    result.sig := f;
    RETURN result;
  END Times;
  
PROCEDURE Normalize(self : GFP) : GFP =
  CONST
    k10MSBits = 16_FFC0000000000000L;
  VAR
    significand : Uint64;
    exponent : INTEGER;
    result := NEW(GFP);
  BEGIN
    <*ASSERT self.sig # 0L *>
    significand := self.sig;
    exponent := self.exp;

    (* This method is mainly called for normalizing boundaries. In general
       boundaries need to be shifted by 10 bits. We thus optimize for this case. *)
    WHILE Long.And(significand, k10MSBits) = 0L DO
      significand := Long.LeftShift(significand, 10);
      DEC(exponent, 10);
    END;

    WHILE Long.And(significand, kUint64MSB) = 0L DO
      significand := Long.LeftShift(significand, 1);
      DEC(exponent);
    END;
    result.sig := significand;
    result.exp := exponent;
    RETURN result;
  END Normalize;
  
BEGIN
END SimFP.
