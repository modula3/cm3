INTERFACE FastBinIO ;

IMPORT Rd, Wr, Thread, Word ;

TYPE
  Int32  = BITS 32 FOR [-16_7FFFFFFF-1 .. 16_7FFFFFFF] ;
  UInt32 = Word.T ; (* [0              .. 16_FFFFFFFF] ; *)
  Int16  = BITS 16 FOR [-16_7FFF-1     .. 16_7FFF] ;
  UInt16 = BITS 16 FOR [0              .. 16_FFFF] ;
  Byte   = BITS 8  FOR [-16_7F         .. 16_7F] ;
  UByte  = BITS 8  FOR [0              .. 16_FF] ; (* CHAR *)

  Endian = {Native, Big, Little} ;
  (*
    Native => Read/Write data using native Endianess
    Big    => Read/Write data converting from/to Big    Endian if necessary
    Little => Read/Write data converting from/to Little Endian if necessary
  *)

PROCEDURE PutInt32(i: Int32; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} ;

PROCEDURE PutUInt32(i: UInt32; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} ;

PROCEDURE PutInt16(i: Int16; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} ;

PROCEDURE PutUInt16(i: UInt16; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} ;

PROCEDURE PutByte(i: Byte; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} ;

PROCEDURE PutUByte(i: UByte; wr: Wr.T; endian: Endian := Endian.Native)
  RAISES {Wr.Failure, Thread.Alerted} ;

PROCEDURE GetInt32(rd: Rd.T; endian: Endian := Endian.Native) : Int32
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} ;

PROCEDURE GetUInt32(rd: Rd.T; endian: Endian := Endian.Native) : UInt32
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} ;

PROCEDURE GetInt16(rd: Rd.T; endian: Endian := Endian.Native) : Int16
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} ;

PROCEDURE GetUInt16(rd: Rd.T; endian: Endian := Endian.Native) : UInt16
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} ;

PROCEDURE GetByte(rd: Rd.T; endian: Endian := Endian.Native) : Byte
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} ;

PROCEDURE GetUByte(rd: Rd.T; endian: Endian := Endian.Native) : UByte
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} ;

END FastBinIO.
