UNSAFE MODULE FastBinIO ;

IMPORT UnsafeWr, UnsafeRd, Thread, Rd, Wr, Swap ;

(* Start of stuff stolen from Swap.m3 *)
FROM Word IMPORT Or, And, LeftShift, RightShift ;

CONST
  B0 = 16_FF;
  B1 = 16_FF00;
  B2 = 16_FF0000;
  B3 = 16_FF000000;

PROCEDURE Swap4U(i: UInt32) : UInt32 =
  BEGIN
    RETURN Or(Or(RightShift(And(B3, i), 24), RightShift(And(B2, i), 8)),
              Or(LeftShift(And(B1, i), 8), LeftShift(And(B0, i), 24)));
  END Swap4U;
(* End Of Stuff Stolen from Swap.m3 *)

(* 
   This is really evil, but the only way to represent UInt32 in a semi
   portable fashion is to actually promote it to a Word.T, since you
   aren't allowed to do BITS 32 FOR [0..16_FFFFFFFF] on a 32 bit machine;
   at least the DEC SRC 3.6 compiler won't let you.  Well, on a
   64 bit machine, a Word.T is a 64 bit structure so we need to fake
   the fact that it's a 32 bit type, and make sure we write it out
   and read it in based on a 32 bit size.
*)
(* Begin of Evil Conversions *)
TYPE
  Int64Word = RECORD big_e, little_e : Int32 END ;

EXCEPTION Fatal ;
<*FATAL Fatal*>

PROCEDURE UInt32ToInt32(i : UInt32) : Int32 =
  VAR
    j : Int32 := 0 ;
  BEGIN
    IF BYTESIZE(UInt32) = 4 THEN
      j := LOOPHOLE(ADR(i), UNTRACED REF Int32)^ ;
    ELSIF BYTESIZE(UInt32) = 8 THEN
      IF native_endian = Swap.Endian.Little THEN
        j := LOOPHOLE(ADR(i), UNTRACED REF Int64Word)^.little_e ;
      ELSE (* BigEndian *)
        j := LOOPHOLE(ADR(i), UNTRACED REF Int64Word)^.big_e ;
      END ;
    ELSE
      RAISE Fatal ;
    END ;

    RETURN j ;
  END UInt32ToInt32;

PROCEDURE Int32ToUInt32(i : Int32) : UInt32 =
  VAR
    j : UInt32 := 0 ;
  BEGIN
    IF BYTESIZE(UInt32) = 4 THEN
      j := LOOPHOLE(ADR(i), UNTRACED REF UInt32)^ ;
    ELSIF BYTESIZE(UInt32) = 8 THEN
      IF native_endian = Swap.Endian.Little THEN
        LOOPHOLE(ADR(j), UNTRACED REF Int64Word)^.little_e := i;
      ELSE (* BigEndian *)
        LOOPHOLE(ADR(j), UNTRACED REF Int64Word)^.big_e := i;
      END ;
    ELSE
      RAISE Fatal ;
    END ;

    RETURN j ;
  END Int32ToUInt32;

(* End of Evil Conversions *)

TYPE
  CharInt32 = ARRAY [0..3] OF CHAR ;  (* 32 bits *)
  CharInt16 = ARRAY [0..1] OF CHAR ;  (* 16 bits *)
  CharByte  = CHAR ;  (* essentiall ARRAY [0..0] OF CHAR *) (* 8 bits *)

VAR
  native_endian := Swap.endian ; (* Endianess of this machine *)

PROCEDURE NeedsSwapping(endian: Endian) : BOOLEAN =
  BEGIN
    IF endian # Endian.Native THEN
      (* If explicit endian value not equal to native value then swap *)
      IF (endian = Endian.Little AND native_endian # Swap.Endian.Little) OR
         (endian = Endian.Big    AND native_endian # Swap.Endian.Big)
       THEN
        RETURN TRUE ; (* requested endianess not same as native endianess *)
      END ;
    END ;

    (* ELSE endian value is explicitly Native or explicit value is the
       same as the native endianess of the machine *)
    RETURN FALSE ;
  END NeedsSwapping;

PROCEDURE PutInt32(i: Int32; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF NeedsSwapping(endian) THEN
        i := Swap.Swap4(i) ;
    END ;
    UnsafeWr.FastPutString(wr, LOOPHOLE(ADR(i), UNTRACED REF CharInt32)^) ;
  END PutInt32;

PROCEDURE PutUInt32(i: UInt32; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    j : Int32 := 0;
  BEGIN
    IF NeedsSwapping(endian) THEN
        i := Swap4U(i) ;
    END ;
    j := UInt32ToInt32(i) ; (* See comments about procedure *)
    UnsafeWr.FastPutString(wr, LOOPHOLE(ADR(j), UNTRACED REF CharInt32)^) ;
  END PutUInt32;

PROCEDURE PutInt16(i: Int16; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF NeedsSwapping(endian) THEN
        i := Swap.Swap2(i) ;
    END ;
    UnsafeWr.FastPutString(wr, LOOPHOLE(ADR(i), UNTRACED REF CharInt16)^) ;
  END PutInt16;

PROCEDURE PutUInt16(i: UInt16; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF NeedsSwapping(endian) THEN
        i := Swap.Swap2U(i) ;
    END ;
    UnsafeWr.FastPutString(wr, LOOPHOLE(ADR(i), UNTRACED REF CharInt16)^) ;
  END PutUInt16;

PROCEDURE PutByte(i: Byte; wr: Wr.T; <*UNUSED*>endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    (* A Byte never needs to be swapped *)
    UnsafeWr.FastPutChar(wr, LOOPHOLE(ADR(i), UNTRACED REF CharByte)^) ;
  END PutByte;

PROCEDURE PutUByte(i: UByte; wr: Wr.T;
                   <*UNUSED*>endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    (* A Byte never needs to be swapped *)
    UnsafeWr.FastPutChar(wr, LOOPHOLE(ADR(i), UNTRACED REF CharByte)^) ;
  END PutUByte;

PROCEDURE GetInt32(rd: Rd.T; endian: Endian := Endian.Native) : Int32
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i : Int32 := 0 ;
  BEGIN
    WITH size = UnsafeRd.FastGetSub(rd, LOOPHOLE(ADR(i), 
                                                 UNTRACED REF CharInt32)^) DO
      IF size # BYTESIZE(CharInt32) THEN
        RAISE Rd.EndOfFile
      END;
    END ;
    IF NeedsSwapping(endian) THEN
        i := Swap.Swap4(i) ;
    END ;
    RETURN i ;
  END GetInt32;

PROCEDURE GetUInt32(rd: Rd.T; endian: Endian := Endian.Native) : UInt32
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i : Int32  := 0 ;
    j : UInt32 := 0 ;
  BEGIN
    WITH size = UnsafeRd.FastGetSub(rd, LOOPHOLE(ADR(i), 
                                                 UNTRACED REF CharInt32)^) DO
      IF size # BYTESIZE(CharInt32) THEN
        RAISE Rd.EndOfFile
      END;
    END ;
    j := Int32ToUInt32(i) ; (* see comments about procedure *)
    IF NeedsSwapping(endian) THEN
        j := Swap4U(j) ;
    END ;
    RETURN j ;
  END GetUInt32;

PROCEDURE GetInt16(rd: Rd.T; endian: Endian := Endian.Native) : Int16
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i : Int16 := 0 ;
  BEGIN
    WITH size = UnsafeRd.FastGetSub(rd, LOOPHOLE(ADR(i), 
                                                 UNTRACED REF CharInt16)^) DO
      IF size # BYTESIZE(CharInt16) THEN
        RAISE Rd.EndOfFile
      END;
    END ;
    IF NeedsSwapping(endian) THEN
        i := Swap.Swap2(i) ;
    END ;
    RETURN i ;
  END GetInt16;

PROCEDURE GetUInt16(rd: Rd.T; endian: Endian := Endian.Native) : UInt16
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i : UInt16 := 0 ;
  BEGIN
    WITH size = UnsafeRd.FastGetSub(rd, LOOPHOLE(ADR(i), 
                                                 UNTRACED REF CharInt16)^) DO
      IF size # BYTESIZE(CharInt16) THEN
        RAISE Rd.EndOfFile
      END;
    END ;
    IF NeedsSwapping(endian) THEN
        i := Swap.Swap2U(i) ;
    END ;
    RETURN i ;
  END GetUInt16;

PROCEDURE GetByte(rd: Rd.T; <*UNUSED*>endian: Endian := Endian.Native) : Byte
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i : Byte := 0 ;
  BEGIN
    LOOPHOLE(ADR(i), UNTRACED REF CharByte)^ := UnsafeRd.FastGetChar(rd) ;
    (* Bytes never need to be swapped *)
    RETURN i ;
  END GetByte;

PROCEDURE GetUByte(rd: Rd.T; <*UNUSED*>endian: Endian := Endian.Native) : UByte
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i : UByte := 0 ;
  BEGIN
    LOOPHOLE(ADR(i), UNTRACED REF CharByte)^ := UnsafeRd.FastGetChar(rd) ;
    (* Bytes never need to be swapped *)
    RETURN i ;
  END GetUByte;

BEGIN
END FastBinIO.
