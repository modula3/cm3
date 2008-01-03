MODULE BinIO ;

IMPORT Rd, Wr, FastBinIO, Thread, UnsafeRd, UnsafeWr ;
(* UnsafeRd and UnsafeWr are simply to know that Readers and Writers
   can be locked
*)

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted, Rd.EndOfFile *>

PROCEDURE PutInt32(i: Int32; wr: Wr.T; endian: Endian := Endian.Native) =
  BEGIN
    LOCK wr DO
      FastBinIO.PutInt32(i, wr, endian) ;
    END ;
    Wr.Flush(wr) ;
  END PutInt32;

PROCEDURE PutUInt32(i: UInt32; wr: Wr.T; endian: Endian := Endian.Native) =
  BEGIN
    LOCK wr DO
      FastBinIO.PutUInt32(i, wr, endian) ;
    END ;
    Wr.Flush(wr) ;
  END PutUInt32;

PROCEDURE PutInt16(i: Int16; wr: Wr.T; endian: Endian := Endian.Native) =
  BEGIN
    LOCK wr DO
      FastBinIO.PutInt16(i, wr, endian) ;
    END ;
    Wr.Flush(wr) ;
  END PutInt16;

PROCEDURE PutUInt16(i: UInt16; wr: Wr.T; endian: Endian := Endian.Native) =
  BEGIN
    LOCK wr DO
      FastBinIO.PutUInt16(i, wr, endian) ;
    END ;
    Wr.Flush(wr) ;
  END PutUInt16;

PROCEDURE PutByte(i: Byte; wr: Wr.T; endian: Endian := Endian.Native) =
  BEGIN
    LOCK wr DO
      FastBinIO.PutByte(i, wr, endian) ;
    END ;
    Wr.Flush(wr) ;
  END PutByte;

PROCEDURE PutUByte(i: UByte; wr: Wr.T; endian: Endian := Endian.Native) =
  BEGIN
    LOCK wr DO
      FastBinIO.PutUByte(i, wr, endian) ;
    END ;
    Wr.Flush(wr) ;
  END PutUByte;

PROCEDURE GetInt32(rd: Rd.T; endian: Endian := Endian.Native) : Int32 =
  BEGIN
    LOCK rd DO
      RETURN FastBinIO.GetInt32(rd, endian) ;
    END ;
  END GetInt32;

PROCEDURE GetUInt32(rd: Rd.T; endian: Endian := Endian.Native) : UInt32 =
  BEGIN
    LOCK rd DO
      RETURN FastBinIO.GetUInt32(rd, endian) ;
    END ;
  END GetUInt32;

PROCEDURE GetInt16(rd: Rd.T; endian: Endian := Endian.Native) : Int16 =
  BEGIN
    LOCK rd DO
      RETURN FastBinIO.GetInt16(rd, endian) ;
    END ;
  END GetInt16;

PROCEDURE GetUInt16(rd: Rd.T; endian: Endian := Endian.Native) : UInt16 =
  BEGIN
    LOCK rd DO
      RETURN FastBinIO.GetUInt16(rd, endian) ;
    END ;
  END GetUInt16;

PROCEDURE GetByte(rd: Rd.T; endian: Endian := Endian.Native) : Byte =
  BEGIN
    LOCK rd DO
      RETURN FastBinIO.GetByte(rd, endian) ;
    END ;
  END GetByte;

PROCEDURE GetUByte(rd: Rd.T; endian: Endian := Endian.Native) : UByte =
  BEGIN
    LOCK rd DO
      RETURN FastBinIO.GetUByte(rd, endian) ;
    END ;
  END GetUByte;

BEGIN
END BinIO.
