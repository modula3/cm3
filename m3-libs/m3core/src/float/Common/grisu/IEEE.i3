(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

INTERFACE IEEE;

FROM SimFP IMPORT Uint64,Uint32,GFP;

TYPE
  
  Double <: DoublePublic;
  
  DoublePublic = OBJECT
  METHODS
    init(v : LONGREAL) : Double;
    value() : LONGREAL;    
    asGFP() : GFP;
    asNormalizedGFP() : GFP;
    exponent() : INTEGER;
    significand() : Uint64;
    isDenormal() : BOOLEAN;
    isSpecial() : BOOLEAN;
    isNan() : BOOLEAN;
    isInfinite() : BOOLEAN;
    sign() : INTEGER;
    infinity() : LONGREAL;
    nan() : LONGREAL;
    normalizedBoundaries(VAR outMminus,outMplus : GFP);
    lowerBoundaryIsCloser() : BOOLEAN;
  END;

  Single <: SinglePublic;
  
  SinglePublic = OBJECT
  METHODS
    init(v : REAL) : Single;
    value() : REAL;    
    asGFP() : GFP;
    exponent() : INTEGER;
    significand() : Uint32;
    isDenormal() : BOOLEAN;
    isSpecial() : BOOLEAN;
    isNan() : BOOLEAN;
    isInfinite() : BOOLEAN;
    sign() : INTEGER;
    infinity() : REAL;
    nan() : REAL;    
    normalizedBoundaries(VAR outMminus,outMplus : GFP);
    lowerBoundaryIsCloser() : BOOLEAN;
  END;
  
END IEEE.