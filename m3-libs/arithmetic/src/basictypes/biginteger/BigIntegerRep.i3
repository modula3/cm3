INTERFACE BigIntegerRep;
(*Copyright (c) 1996, m3na project
  
Abstract: Integers of arbitrary size

Daniel Beer
*)

IMPORT Word AS W, BigIntegerBasic AS BI;
FROM xUtils IMPORT Error;

(*==========================*)

REVEAL
  BI.Value = BRANDED "BigIntegerValue" REF ARRAY OF W.T;

TYPE
  Value = BI.Value;
  T = BI.T;

PROCEDURE Clear ((*OUT*)v : Value);

PROCEDURE MinMax (VAR min, max : INTEGER; a, b : INTEGER);
PROCEDURE CorrectSize (VAR x : T; start : INTEGER);
PROCEDURE AddU (READONLY x, y : T) : T;
PROCEDURE SubU (READONLY x, y : T) : T;
PROCEDURE CompareU (READONLY x, y : T) : [-1..1];
PROCEDURE MulU (READONLY x, y : T) : T;
PROCEDURE DivModU (READONLY x, y : T; VAR r : T) : T RAISES {Error};

TYPE
  BitPos =
    RECORD
      word : INTEGER;
      bit  : [0..W.Size-1];
    END;

PROCEDURE AddBitPos (READONLY x,y : BitPos) : BitPos; (*only for testing*)
PROCEDURE SubBitPos (READONLY x,y : BitPos) : BitPos;
PROCEDURE CompareBitPos (READONLY x,y : BitPos) : [-1..1];
PROCEDURE BitPosEndToBegin (READONLY x : BitPos) : BitPos;
PROCEDURE GetMSBPos (READONLY x : T) : BitPos;
PROCEDURE GetSubword (READONLY x : T; sh : BitPos) : W.T;
PROCEDURE SubShiftedProd(VAR x : T; READONLY y : T; z : W.T; sh : BitPos);
PROCEDURE AddShifted(VAR x : T; y : W.T; sh : BitPos);

END BigIntegerRep.
