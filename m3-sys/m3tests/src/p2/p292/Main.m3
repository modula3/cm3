(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p292                              *)
(* Copyright 2022, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

UNSAFE MODULE Main;

(* Test for a compiler front-end bug that once incorrectly produced
   CG type Addr as type of record temporaries, while compiling MD5.m3 in
   libm3.  The correct CG type is struct.
*)

IMPORT Fmt, Long, TextWr, Stdio, Wr;

<*FATAL ANY*>

CONST
  FirstInt32 = -1 - 16_7FFFFFFF;
    (* This value is computed in such a way that it will have the same value
       on a 32 bit machine and on a 64 bit machine.  On a 32 bit
       2's complement machine this is FIRST(INTEGER). *)

TYPE
  Int32 = BITS 32 FOR [FirstInt32 .. 16_7FFFFFFF];
  (* The subrange of integer that can be represented in an INTEGER
     on a 32 bit 2's complement machine.  *)

TYPE
  Int64On32 = RECORD a, b: Int32;  END;

CONST A = 16_01234567;
CONST B = 16_0ECA8642;

TYPE
  Byte = [0..16_FF];

  Block8 = ARRAY[0..7] OF Byte;
  RefBlock = REF ARRAY OF Byte;

  MD5Control = RECORD
    bufLen : CARDINAL; (* current length of buffer *)
    length : CARDINAL; (* length of stream *)
  END;
  RefMD5Control = REF MD5Control;

  Endian = {Big, Little};

VAR 
  endian : Endian;

PROCEDURE GetEndian() : Endian =
  VAR  
    ref := NEW(UNTRACED REF INTEGER);
  BEGIN
    ref^ := 1;
    IF 1 = LOOPHOLE(ref, UNTRACED REF [0..255])^ THEN
      RETURN Endian.Little;
    ELSE
      RETURN Endian.Big;
    END;
  END GetEndian;

PROCEDURE Swap8(READONLY i: Int64On32): Int64On32 =
  BEGIN
    RETURN Int64On32 {i.b, i.a}
  END Swap8;

(* LengthBlock is the original failing code from MD5.m3. *)
PROCEDURE LengthBlock(ctrl : RefMD5Control) : RefBlock =
  VAR
    len64 : Int64On32;
    buf : RefBlock;
  BEGIN
    buf := NEW(RefBlock,8);
    len64 := LOOPHOLE(VAL(ctrl.length + ctrl.bufLen,LONGINT) * 8L,Int64On32);
    IF endian = Endian.Big THEN
      len64 := Swap8(len64);
    END;
    buf^ := LOOPHOLE(len64,Block8);
    RETURN buf;
  END LengthBlock;

(* A different way of computing the result, without using LOOPHOLE. *)
PROCEDURE To8Bytes (Val: LONGINT; VAR bytes8: Block8) =
  BEGIN
    IF endian = Endian.Big THEN
      bytes8 [7] := VAL(Long.And (Val, 16_FFL), Byte);
      bytes8 [6] := VAL(Long.And (Long.Shift (Val, -8), 16_FFL), Byte);
      bytes8 [5] := VAL(Long.And (Long.Shift (Val, -16), 16_FFL), Byte);
      bytes8 [4] := VAL(Long.And (Long.Shift (Val, -24), 16_FFL), Byte);
      bytes8 [3] := VAL(Long.And (Long.Shift (Val, -32), 16_FFL), Byte);
      bytes8 [2] := VAL(Long.And (Long.Shift (Val, -40), 16_FFL), Byte);
      bytes8 [1] := VAL(Long.And (Long.Shift (Val, -48), 16_FFL), Byte);
      bytes8 [0] := VAL(Long.And (Long.Shift (Val, -56), 16_FFL), Byte);
    ELSE
      bytes8 [0] := VAL(Long.And (Val, 16_FFL), Byte);
      bytes8 [1] := VAL(Long.And (Long.Shift (Val, -8), 16_FFL), Byte);
      bytes8 [2] := VAL(Long.And (Long.Shift (Val, -16), 16_FFL), Byte);
      bytes8 [3] := VAL(Long.And (Long.Shift (Val, -24), 16_FFL), Byte);
      bytes8 [4] := VAL(Long.And (Long.Shift (Val, -32), 16_FFL), Byte);
      bytes8 [5] := VAL(Long.And (Long.Shift (Val, -40), 16_FFL), Byte);
      bytes8 [6] := VAL(Long.And (Long.Shift (Val, -48), 16_FFL), Byte);
      bytes8 [7] := VAL(Long.And (Long.Shift (Val, -56), 16_FFL), Byte);
    END;
  END To8Bytes;

PROCEDURE Expect (A, B: Int32): Block8 =

  VAR Sum, Val8: LONGINT;
  VAR result: Block8;
  BEGIN
    Sum := VAL(A + B, LONGINT);
    Val8 := Sum * 8L;
    To8Bytes (Val8, result);
    RETURN result;
  END Expect; 

PROCEDURE Block8Image (Blk: Block8): TEXT =
  VAR result: Wr.T;
  BEGIN
    result := TextWr.New ();
    Wr.PutText(result, "16_");
    FOR RI := 7 TO 0 BY - 1 DO
      Wr.PutText(result, Fmt.Pad(Fmt.Int (Blk[RI],16),2,'0'));
    END;
    RETURN TextWr.ToText(result);
  END Block8Image;

PROCEDURE Work () =
  VAR inputRef: RefMD5Control;
  VAR resultRefBlock: RefBlock;
  VAR expectedBlock: Block8;
  BEGIN
    inputRef := NEW (RefMD5Control, bufLen := A, length := B);
    resultRefBlock := LengthBlock (inputRef);
    expectedBlock := Expect (A, B);
    Wr.PutText (Stdio.stdout, "Got result: ");
    Wr.PutText (Stdio.stdout, Block8Image (resultRefBlock^));
    IF resultRefBlock^ = expectedBlock THEN
      Wr.PutText (Stdio.stdout, ", as expected.");
    ELSE
      Wr.PutText (Stdio.stdout, ", but expected: ");
      Wr.PutText (Stdio.stdout, Block8Image (expectedBlock));
    END;
    Wr.PutText (Stdio.stdout, Wr.EOL);
  END Work;
  
BEGIN
  endian := GetEndian();
  Work ();
END Main.
