(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Aug  3 14:22:38 PDT 1993 by kalsow   *)
(*      modified on Thu Jul 15 15:45:50 PDT 1993 by swart    *)

UNSAFE MODULE Swap;

IMPORT Word;

FROM Word IMPORT Or, And, Not, LeftShift, RightShift, Extract;

EXCEPTION Failure;
<*FATAL Failure*>

CONST
  B0 = 16_FF;
  B1 = 16_FF00;
  B2 = 16_FF0000;
  B3 = 16_FF000000;


  (* These will all be zero on machines with BYTESIZE(INTEGER) = 32 *)
  B4 = LeftShift(B3, 8);
  B5 = LeftShift(B4, 8);
  B6 = LeftShift(B5, 8);
  B7 = LeftShift(B6, 8);

CONST SignExt32 = ARRAY [0..1] OF Word.T {0, Not(16_FFFFFFFF)};

(* Swaps the lower 4 bytes of i *)
PROCEDURE Swap4 (i: Int32): Int32 =
  BEGIN
    IF BYTESIZE(INTEGER) = 4 THEN
      RETURN Or(Or(RightShift(And(B3, i), 24), RightShift(And(B2, i), 8)),
                Or(LeftShift(And(B1, i), 8), LeftShift(And(B0, i), 24)));
    ELSIF BYTESIZE(INTEGER) = 8 THEN
      RETURN
        Or(SignExt32[Extract(i, 7, 1)],
           Or(Or(RightShift(And(B3, i), 24), RightShift(And(B2, i), 8)),
              Or(LeftShift(And(B1, i), 8), LeftShift(And(B0, i), 24))));
    ELSE
      RAISE Failure;
    END;
  END Swap4;

CONST SignExt16 = ARRAY [0..1] OF Word.T {0, Not(16_FFFF)};

(* Swaps the lower 2 bytes of i *)
PROCEDURE Swap2 (i: Int16): Int16 =
  BEGIN
    RETURN Or(SignExt16[Extract(i, 7, 1)],
              Or(RightShift(And(B1, i), 8), LeftShift(And(B0, i), 8)));
  END Swap2;

(* Swaps the lower 2 bytes of i *)
PROCEDURE Swap2U (i: UInt16): UInt16 =
  BEGIN
    RETURN Or(RightShift(And(B1, i), 8), LeftShift(And(B0, i), 8));
  END Swap2U;


PROCEDURE Swap8 (READONLY i: Int64On32): Int64On32 =
  BEGIN
    IF BYTESIZE(INTEGER) = 4 THEN
      RETURN Int64On32{a := Swap4(i.b), b := Swap4(i.a)}
    ELSIF BYTESIZE(INTEGER) = 8 THEN
      RETURN LOOPHOLE(Int64On64{v := SwapInt(LOOPHOLE(i, Int64On64).v)},
                      Int64On32);
    ELSE
      RAISE Failure;
    END;
  END Swap8;

PROCEDURE SwapInt (i: INTEGER): INTEGER =
  BEGIN
    IF BYTESIZE(INTEGER) = 4 THEN
      RETURN Or(Or(RightShift(And(B3, i), 24), RightShift(And(B2, i), 8)),
                Or(LeftShift(And(B1, i), 8), LeftShift(And(B0, i), 24)));
    ELSIF BYTESIZE(INTEGER) = 8 THEN
      RETURN <*NOWARN*>
        Or(
          Or(Or(Or(RightShift(And(i, B7), <*NOWARN*> 56),
                   RightShift(And(i, B6), <*NOWARN*> 40)),
                Or(RightShift(And(i, B5), 24), RightShift(And(i, B4), 8))),
             Or(LeftShift(And(i, B3), 8), LeftShift(And(i, B2), 24))),
          Or(LeftShift(And(i, B1), <*NOWARN*> 40),
             LeftShift(And(i, B0), <*NOWARN*> 56)));
    ELSE
      RAISE Failure;
    END;
  END SwapInt;

PROCEDURE FindByteOrder () =
  CONST
    i : Int32 = 16_01020304;
    a0        = Extract(i, 0, 8);
    a1        = Extract(i, 8, 8);
    a2        = Extract(i, 16, 8);
    a3        = Extract(i, 24, 8);
  VAR x := LOOPHOLE(i, RECORD b0, b1, b2, b3: BITS 8 FOR [0 .. 255] END);
  BEGIN
    <*ASSERT a0 = 4 AND a1 = 3 AND a2 = 2 AND a3 = 1 *>
    IF (4 = x.b0) AND (3 = x.b1) AND (2 = x.b2) AND (1 = x.b3) THEN
      endian := Endian.Little
    ELSIF (4 = x.b3) AND (3 = x.b2) AND (2 = x.b1) AND (1 = x.b0) THEN
      endian := Endian.Big;
    ELSE                         (* unsupported byte ordering ... *)
      RAISE Failure;
    END;
    IF NOT ((BITSIZE(INTEGER) = 32) OR (BITSIZE(INTEGER) = 64)) THEN
      RAISE Failure;
    END;
  END FindByteOrder;

BEGIN
  FindByteOrder();
END Swap.
