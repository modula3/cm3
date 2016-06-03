(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE MODULE IEEE;

IMPORT Word,Long;
FROM SimFP IMPORT GFP,Uint64,Uint32,SignificandSize;

CONST
  (* Double constants *)
  dSignMask = 16_8000000000000000L;
  dExponentMask = 16_7FF0000000000000L;
  dSignificandMask = 16_000FFFFFFFFFFFFFL;
  dHiddenBit = 16_0010000000000000L;
  dPhysicalSignificandSize = 52;  (* Excludes the hidden bit. *)
  dSignificandSize = 53;

  dExponentBias = 16_3FF + dPhysicalSignificandSize;
  dDenormalExponent = -dExponentBias + 1;
<*NOWARN*>  dMaxExponent = 16_7FF - dExponentBias;
  dInfinity = 16_7FF0000000000000L;
  dNaN = 16_7FF8000000000000L;  
  
  (* Single constants *)  
  fSignMask = 16_80000000;
  fExponentMask = 16_7F800000;
  fSignificandMask = 16_007FFFFF;
  fHiddenBit = 16_00800000;
  fPhysicalSignificandSize = 23;  (* Excludes the hidden bit.*)
<*NOWARN*>  fSignificandSize = 24;
  
  fExponentBias = 16_7F + fPhysicalSignificandSize;
  fDenormalExponent = -fExponentBias + 1;
<*NOWARN*>  fMaxExponent = 16_FF - fExponentBias;
  fInfinity = 16_7F800000;
  fNaN = 16_7FC00000;
  
REVEAL
  Double = DoublePublic BRANDED OBJECT
    d64 : Uint64;
  METHODS
    asUint64() : Uint64 := AsUint64;
    nextDouble() : LONGREAL := NextDouble;
    previousDouble() : LONGREAL := PreviousDouble;
    upperBoundary() : GFP := UpperBoundary_D;
    significandSizeForOrderOfMagnitude(order : INTEGER) : INTEGER :=
      SignificandSizeForOrderOfMagnitude;    
    gfpToUint64(gfp : GFP) : Uint64 := GFPToUint64;
  OVERRIDES
    init := Init_D;
    value := Value64;
    asGFP := AsGFP_D;
    asNormalizedGFP := AsNormalizedGFP_D;
    exponent := Exponent_D;
    significand := Significand_D;
    isDenormal := IsDenormal_D;
    isSpecial := IsSpecial_D;
    isNan := IsNan_D;
    isInfinite := IsInfinite_D;
    sign := Sign_D;
    infinity := Infinity_D;
    nan := NaN_D;
    normalizedBoundaries := NormalizedBoundaries_D;
    lowerBoundaryIsCloser := LowerBoundaryIsCloser_D;
  END;

  Single = SinglePublic BRANDED OBJECT
    d32 : Uint32;
  METHODS
    asUint32() : Uint32 := AsUint32;
    upperBoundary() : GFP := UpperBoundary_S;
  OVERRIDES
    init := Init_S;
    value := Value_S;
    asGFP := AsGFP_S;
    exponent := Exponent_S;
    significand := Significand_S;
    isDenormal := IsDenormal_S;
    isSpecial := IsSpecial_S;
    isNan := IsNan_S;
    isInfinite := IsInfinite_S;
    sign := Sign_S;
    infinity := Infinity_S;
    nan := NaN_S;
    normalizedBoundaries := NormalizedBoundaries_S;
    lowerBoundaryIsCloser := LowerBoundaryIsCloser_S;    
  END;

(************************** Double methods **************************8*)
  
PROCEDURE Init_D(self : Double; v : LONGREAL) : Double =
  BEGIN
    self.d64 := LOOPHOLE(v,Uint64);
    RETURN self;
  END Init_D;
  
(* Returns the double's bit as uint64. *)
PROCEDURE AsUint64(self : Double) : Uint64 =
  BEGIN
    RETURN self.d64;
  END AsUint64;
  
PROCEDURE Value64(self : Double) : LONGREAL =
  BEGIN
    RETURN LOOPHOLE(self.d64,LONGREAL);
  END Value64;

(* Returns the next greater double. Returns +infinity on input +infinity. *)
PROCEDURE NextDouble(self : Double) : LONGREAL =
  BEGIN
    IF self.d64 = dInfinity THEN RETURN self.infinity(); END;
    IF self.sign() < 0 AND self.significand() = 0L THEN
      (* -0.0 *)
      RETURN 0.0D0;
    END;
    IF self.sign() < 0 THEN
      DEC(self.d64);
      RETURN self.value();
    ELSE
      INC(self.d64);
      RETURN self.value();
    END;
  END NextDouble;
  
PROCEDURE PreviousDouble(self : Double) : LONGREAL =
  BEGIN
    IF self.d64 = Long.Or(dInfinity, dSignMask) THEN RETURN -self.infinity(); END;
    IF self.sign() < 0 THEN
      INC(self.d64);
      RETURN self.value();
    ELSE
      IF self.significand() = 0L THEN
        RETURN -0.0D0;
      END;      
      DEC(self.d64);
      RETURN self.value();
    END;
  END PreviousDouble;
  
(* The value encoded by this Double must be greater or equal to +0.0.
   It must not be special (infinity, or NaN). *)
PROCEDURE AsGFP_D(self : Double) : GFP =
  BEGIN
    <*ASSERT self.sign() > 0 *> 
    <*ASSERT NOT self.isSpecial() *>
    RETURN NEW(GFP).init(self.significand(), self.exponent());
  END AsGFP_D;
    
(* The value encoded by this Double must be strictly greater than 0. *)
PROCEDURE AsNormalizedGFP_D(self : Double) : GFP =
  VAR
    f : Uint64;
    e : INTEGER;
  BEGIN
    <*ASSERT self.value() > 0.0D0 *> 
    f := self.significand();
    e := self.exponent();

    (* The current double could be a denormal. *)
    WHILE Long.And(f, dHiddenBit) = 0L DO
      f  := Long.LeftShift(f,1);
      DEC(e);
    END;
    (* Do the final shifts in one go. *)
    f := Long.LeftShift(f, SignificandSize - dSignificandSize);
    e := e - (SignificandSize - dSignificandSize);
    RETURN NEW(GFP).init(f, e);
  END AsNormalizedGFP_D;

PROCEDURE Exponent_D(self : Double) : INTEGER =
  VAR
    d64 : Uint64;
    biasedExp : INTEGER;
  BEGIN
    IF self.isDenormal() THEN RETURN dDenormalExponent; END;

    d64 := self.asUint64();
    biasedExp := VAL(Long.RightShift(Long.And(d64, dExponentMask), 
                                     dPhysicalSignificandSize),INTEGER);
    RETURN biasedExp - dExponentBias;
  END Exponent_D;

PROCEDURE Significand_D(self : Double) : Uint64 =
  VAR d64,significand : Uint64;
  BEGIN
    d64 := self.asUint64();
    significand := Long.And(d64, dSignificandMask);
    IF NOT self.isDenormal() THEN
      RETURN significand + dHiddenBit;
    ELSE
      RETURN significand;
    END;
  END Significand_D;

(* Returns true if the double is a denormal. *)
PROCEDURE IsDenormal_D(self : Double) : BOOLEAN =
  VAR d64 : Uint64;
  BEGIN
    d64 := self.asUint64();
    RETURN Long.And(d64, dExponentMask) = 0L;
  END IsDenormal_D;

(* We consider denormals not to be special.
  Hence only Infinity and NaN are special. *)
PROCEDURE IsSpecial_D(self : Double) : BOOLEAN =
  VAR d64 : Uint64;
  BEGIN
    d64 := self.asUint64();
    RETURN Long.And(d64, dExponentMask) = dExponentMask;
  END IsSpecial_D;
  
PROCEDURE IsNan_D(self : Double) : BOOLEAN =
  VAR d64 : Uint64;
  BEGIN
    d64 := self.asUint64();
    RETURN Long.And(d64, dExponentMask) = dExponentMask  AND
           Long.And(d64, dSignificandMask) # 0L;
  END IsNan_D;
  
PROCEDURE IsInfinite_D(self : Double) : BOOLEAN =
  VAR d64 := self.asUint64();
  BEGIN
    RETURN (Long.And(d64, dExponentMask) = dExponentMask) AND
           (Long.And(self.d64, dSignificandMask) = 0L);
  END IsInfinite_D;

PROCEDURE Sign_D(self : Double) : INTEGER =
  VAR d64 := self.asUint64();
  BEGIN
    IF Long.And(d64, dSignMask) = 0L THEN RETURN 1; ELSE RETURN -1; END;
  END Sign_D;

PROCEDURE Infinity_D(<*UNUSED*>self : Double) : LONGREAL =
  BEGIN
    RETURN LOOPHOLE(dInfinity,LONGREAL);
  END Infinity_D;

PROCEDURE NaN_D(<*UNUSED*>self : Double) : LONGREAL =
  BEGIN
    RETURN LOOPHOLE(dNaN,LONGREAL);
  END NaN_D;
  
(* Precondition: the value encoded by this Double must be greater or equal
  than +0.0. *)
PROCEDURE UpperBoundary_D(self : Double) : GFP =
  BEGIN
    <*ASSERT self.sign() > 0 *>
    RETURN NEW(GFP).init(self.significand() * 2L + 1L, self.exponent() - 1);
  END UpperBoundary_D;
  
(* Computes the two boundaries of this.
   The bigger boundary (m_plus) is normalized. The lower boundary has the same
   exponent as m_plus.
   Precondition: the value encoded by this Double must be greater than 0. *)  
PROCEDURE NormalizedBoundaries_D(self : Double; VAR outMminus,outMplus : GFP) =
  VAR v,mPlus,mMinus,plus : GFP;
      shift : Uint64;
  BEGIN
    <*ASSERT self.value() > 0.0D0 *> 
    v := self.asGFP();

    shift := Long.LeftShift(v.f(), 1) + 1L;
    plus := NEW(GFP).init(shift, v.e() - 1);
    mPlus := plus.normalize();
    
    IF self.lowerBoundaryIsCloser() THEN
      mMinus := NEW(GFP).init(Long.LeftShift(v.f(), 2) - 1L, v.e() - 2);
    ELSE
      mMinus := NEW(GFP).init(Long.LeftShift(v.f(), 1) - 1L, v.e() - 1);
    END;
    mMinus.setF(Long.LeftShift(mMinus.f(), (mMinus.e() - mPlus.e())));
    mMinus.setE(mPlus.e());
    outMplus := mPlus;
    outMminus := mMinus;
  END NormalizedBoundaries_D;

PROCEDURE LowerBoundaryIsCloser_D(self : Double) : BOOLEAN =
  VAR physSignificandIsZero : BOOLEAN;
  BEGIN
    (* The boundary is closer if the significand is of the form f == 2^p-1 then
       the lower boundary is closer.
       Think of v = 1000e10 and v- = 9999e9.
       Then the boundary (= (v - v-)/2) is not just at a distance of 1e9 but
       at a distance of 1e8.
       The only exception is for the smallest normal: the largest denormal is
       at the same distance as its successor.
       Note: denormals have the same exponent as the smallest normals. *)
    physSignificandIsZero := Long.And(self.asUint64(), dSignificandMask) = 0L;
    RETURN physSignificandIsZero AND (self.exponent() # dDenormalExponent);
  END LowerBoundaryIsCloser_D;

  (* Returns the significand size for a given order of magnitude.
     If v = f*2^e with 2^p-1 <= f <= 2^p then p+e is v's order of magnitude.
     This function returns the number of significant binary digits v will have
     once it's encoded into a double. In almost all cases this is equal to
     SignificandSize. The only exceptions are denormals. They start with
     leading zeroes and their effective significand-size is hence smaller.*)
     
PROCEDURE SignificandSizeForOrderOfMagnitude(<*UNUSED*>self : Double; order : INTEGER) : INTEGER =
  BEGIN
    IF (order >= (dDenormalExponent + SignificandSize)) THEN
      RETURN SignificandSize;
    END;
    IF (order <= dDenormalExponent) THEN RETURN 0; END;
    RETURN order - dDenormalExponent;
  END SignificandSizeForOrderOfMagnitude;
  
PROCEDURE GFPToUint64(<*UNUSED*>self : Double; gfp : GFP)  : Uint64 =
  VAR
    significand,biasedExponent : Uint64;
    exponent : INTEGER;
  BEGIN
    significand := gfp.f();
    exponent := gfp.e();
(*    WHILE significand > dHiddenBit + dSignificandMask DO*)
    WHILE Long.GT(significand, dHiddenBit + dSignificandMask) DO
      significand := Long.RightShift(significand, 1); 
      INC(exponent);
    END;
    IF (exponent >= dMaxExponent) THEN
      RETURN dInfinity;
    END;
    IF (exponent < dDenormalExponent) THEN
      RETURN 0L;
    END;
    WHILE (exponent > dDenormalExponent AND 
           Long.And(significand, dHiddenBit) = 0L) DO
      significand := Long.LeftShift(significand, 1);
      DEC(exponent);
    END;
    IF (exponent = dDenormalExponent AND 
        Long.And(significand, dHiddenBit) = 0L) THEN
      biasedExponent := 0L;
    ELSE
      biasedExponent := VAL(exponent + dExponentBias,Uint64);
    END;
    RETURN Long.Or(Long.And(significand, dSignificandMask),
        Long.LeftShift(biasedExponent, dPhysicalSignificandSize));    
  END GFPToUint64;
  
(*************************** Single methods **************************)

PROCEDURE Init_S(self : Single; v : REAL) : Single =
  BEGIN
    self.d32 := LOOPHOLE(v,Uint32);
    RETURN self;
  END Init_S;
  
(* Returns the single's bit as uint32. *)
PROCEDURE AsUint32(self : Single) : Uint32 =
  BEGIN
    RETURN self.d32;
  END AsUint32;
  
PROCEDURE Value_S(self : Single) : REAL =
  BEGIN
    RETURN FLOAT(self.d32,REAL);
  END Value_S;
  
(* The value encoded by this Single must be greater or equal to +0.0.
   It must not be special (infinity, or NaN). *)
PROCEDURE AsGFP_S(self : Single) : GFP =
  BEGIN
    <*ASSERT self.sign() > 0 *> 
    <*ASSERT NOT self.isSpecial() *>
    RETURN NEW(GFP).init(VAL(self.significand(),Uint64), self.exponent());
  END AsGFP_S;
  
PROCEDURE Exponent_S(self : Single) : INTEGER =
  VAR
    d32 : Uint32;
    biasedExp : INTEGER;
  BEGIN
    IF self.isDenormal() THEN RETURN dDenormalExponent; END;

    d32 := self.asUint32();
    biasedExp := Word.RightShift(Word.And(d32, fExponentMask), 
                                fPhysicalSignificandSize);
    RETURN biasedExp - fExponentBias;
  END Exponent_S;

PROCEDURE Significand_S(self : Single) : Uint32 =
  VAR d32,significand : Uint32;
  BEGIN
    d32 := self.asUint32();
    significand := Word.And(d32, fSignificandMask);
    IF NOT self.isDenormal() THEN
      RETURN significand + fHiddenBit;
    ELSE
      RETURN significand;
    END;
  END Significand_S;

(* Returns true if the single is a denormal. *)
PROCEDURE IsDenormal_S(self : Single) : BOOLEAN =
  VAR d32 : Uint32;
  BEGIN
    d32 := self.asUint32();
    RETURN Word.And(d32, fExponentMask) = 0;
  END IsDenormal_S;
  
(* We consider denormals not to be special.
  Hence only Infinity and NaN are special. *)
PROCEDURE IsSpecial_S(self : Single) : BOOLEAN =
  VAR d32 : Uint32;
  BEGIN
    d32 := self.asUint32();
    RETURN Word.And(d32, fExponentMask) = fExponentMask;
  END IsSpecial_S;
  
PROCEDURE IsNan_S(self : Single) : BOOLEAN =
  VAR d32 : Uint32;
  BEGIN
    d32 := self.asUint32();
    RETURN Word.And(d32, fExponentMask) = fExponentMask  AND
           Word.And(d32, fSignificandMask) # 0;
  END IsNan_S;
  
PROCEDURE IsInfinite_S(self : Single) : BOOLEAN =
  VAR d32 := self.asUint32();
  BEGIN
    RETURN (Word.And(d32, fExponentMask) = fExponentMask) AND
           (Word.And(self.d32, fSignificandMask) = 0);
  END IsInfinite_S;

PROCEDURE Sign_S(self : Single) : INTEGER =
  VAR d32 := self.asUint32();
  BEGIN
    IF Word.And(d32, fSignMask) = 0 THEN RETURN 1; ELSE RETURN -1; END;
  END Sign_S;
  
PROCEDURE Infinity_S(<*UNUSED*>self : Single) : REAL =
  BEGIN
    RETURN LOOPHOLE(VAL(fInfinity,Uint32),REAL);
  END Infinity_S;

PROCEDURE NaN_S(<*UNUSED*>self : Single) : REAL =
  BEGIN
    RETURN LOOPHOLE(VAL(fNaN,Uint32),REAL);
  END NaN_S;
  
(* Precondition: the value encoded by this Double must be greater or equal
  than +0.0. *)
PROCEDURE UpperBoundary_S(self : Single) : GFP =
  BEGIN
    <*ASSERT self.sign() > 0 *>
    RETURN NEW(GFP).init(VAL(self.significand(), Uint64) * 2L + 1L, self.exponent() - 1);
  END UpperBoundary_S;
  
(* Computes the two boundaries of self.
   The bigger boundary (m_plus) is normalized. The lower boundary has the same
   exponent as m_plus.
   Precondition: the value encoded by this Double must be greater than 0. *)
PROCEDURE NormalizedBoundaries_S(self : Single; VAR outMminus,outMplus : GFP) =
  VAR v,mPlus,mMinus,plus : GFP;
      shift : Uint64;
  BEGIN
    <*ASSERT self.value() > 0.0E0 *> 
    v := self.asGFP();

    shift := Long.LeftShift(v.f(), 1) + 1L;
    plus := NEW(GFP).init(shift, v.e() - 1);
    mPlus := plus.normalize();
    
    IF self.lowerBoundaryIsCloser() THEN
      mMinus := NEW(GFP).init(Long.LeftShift(v.f(), 2) - 1L, v.e() - 2);
    ELSE
      mMinus := NEW(GFP).init(Long.LeftShift(v.f(), 1) - 1L, v.e() - 1);
    END;
    mMinus.setF(Long.LeftShift(mMinus.f(), (mMinus.e() - mPlus.e())));
    mMinus.setE(mPlus.e());
    outMplus := mPlus;
    outMminus := mMinus;
  END NormalizedBoundaries_S;
  
PROCEDURE LowerBoundaryIsCloser_S(self : Single) : BOOLEAN =
  VAR physSignificandIsZero : BOOLEAN;
  BEGIN
    (* The boundary is closer if the significand is of the form f == 2^p-1 then
       the lower boundary is closer.
       Think of v = 1000e10 and v- = 9999e9.
       Then the boundary (= (v - v-)/2) is not just at a distance of 1e9 but
       at a distance of 1e8.
       The only exception is for the smallest normal: the largest denormal is
       at the same distance as its successor.
       Note: denormals have the same exponent as the smallest normals. *)
    physSignificandIsZero := Word.And(self.asUint32(), fSignificandMask) = 0;
    RETURN physSignificandIsZero AND (self.exponent() # fDenormalExponent);
  END LowerBoundaryIsCloser_S;
  
BEGIN
END IEEE.
