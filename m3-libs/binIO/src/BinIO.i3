INTERFACE BinIO ;

IMPORT Rd, Wr, FastBinIO ;

(*
  This interface is almost equivalent to FastBinIO, but no exceptions
  are thrown, the locking and unlocking of readers and writers
  (Rd.T and Wr.T) is automatic, and writer buffers are flushed after
  every operation.

  For many cases this is sufficient, but the FastBinIO interface
  exists for those instances where speed is of the utmost importance,
  or the handling of Rd, Wr, and/or Thread exceptions are necessary.
*)

(*
  These types are actually defined in FastBinIO, but have been included
  here in their full form to make it easier to understand the lower
  level details.

  Int32  = BITS 32 FOR [-16_7FFFFFFF-1 .. 16_7FFFFFFF] ;
  UInt32 = Word.T ;    [0              .. 16_FFFFFFFF] ;
  Int16  = BITS 16 FOR [-16_7FFF-1     .. 16_7FFF] ;
  UInt16 = BITS 16 FOR [0              .. 16_FFFF] ;
  Byte   = BITS 8  FOR [-16_7F         .. 16_7F] ;
  UByte  = BITS 8  FOR [0              .. 16_FF] ;

  Endian = {Native, Big, Little} ;

  Native => Read/Write data using native Endianess
  Big    => Read/Write data converting from/to Big    Endian if necessary
  Little => Read/Write data converting from/to Little Endian if necessary
*)

TYPE
  Int32  = FastBinIO.Int32 ;
  UInt32 = FastBinIO.UInt32 ;
  Int16  = FastBinIO.Int16 ;
  UInt16 = FastBinIO.UInt16 ;
  Byte   = FastBinIO.Byte ;
  UByte  = FastBinIO.UByte ;

  Endian = FastBinIO.Endian ;

PROCEDURE PutInt32(i: Int32; wr: Wr.T; endian: Endian := Endian.Native) ;

PROCEDURE PutUInt32(i: UInt32; wr: Wr.T; endian: Endian := Endian.Native) ;

PROCEDURE PutInt16(i: Int16; wr: Wr.T; endian: Endian := Endian.Native) ;

PROCEDURE PutUInt16(i: UInt16; wr: Wr.T; endian: Endian := Endian.Native) ;

PROCEDURE PutByte(i: Byte; wr: Wr.T; endian: Endian := Endian.Native) ;

PROCEDURE PutUByte(i: UByte; wr: Wr.T; endian: Endian := Endian.Native) ;

PROCEDURE GetInt32(rd: Rd.T; endian: Endian := Endian.Native) : Int32 ;

PROCEDURE GetUInt32(rd: Rd.T; endian: Endian := Endian.Native) : UInt32 ;

PROCEDURE GetInt16(rd: Rd.T; endian: Endian := Endian.Native) : Int16 ;

PROCEDURE GetUInt16(rd: Rd.T; endian: Endian := Endian.Native) : UInt16 ;

PROCEDURE GetByte(rd: Rd.T; endian: Endian := Endian.Native) : Byte ;

PROCEDURE GetUByte(rd: Rd.T; endian: Endian := Endian.Native) : UByte ;

END BinIO.
