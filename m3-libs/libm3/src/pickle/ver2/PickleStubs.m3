(* 
 * For more information on this program, contact Blair MacIntyre          
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 500 W 120th St, Room 450, New York, NY, 10027.                         
 *                                                                        
 * Copyright (C) Blair MacIntyre 1995, Columbia University 1995           
 * 
 *)

UNSAFE MODULE PickleStubs;
   (* unsafe because of marshalling code *)
   
IMPORT Pickle2 AS Pickle, PickleRd, RTPacking, RTType;
FROM PickleRd IMPORT myPacking;
FROM ConvertPacking IMPORT Kind;
FROM Swap IMPORT Int32, Int64On32, Int64On64;

IMPORT Rd, Wr, Text, TextF, Thread, RdClass, WrClass, UnsafeRd, UnsafeWr,
       Swap; 

REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;

(*---------marshalling/unmarshalling routines-----------*)

PROCEDURE OutRef (writer: Pickle.Writer; r: REFANY) 
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    writer.write(r);
  END OutRef;

PROCEDURE InRef (reader: Pickle.Reader; tc := -1): REFANY
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    WITH ref = reader.read() DO
      IF tc # -1 AND NOT RTType.IsSubtype(TYPECODE(ref), tc) THEN
        RaiseUnmarshalFailure();
      END;      
      RETURN ref;
    END;
  END InRef;

PROCEDURE InChars(reader: Pickle.Reader; VAR arr: ARRAY OF CHAR)
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    (* No longer an charSet infomation.
      IF reader.packing.charSet # myPacking.charSet THEN
        RaiseUnsupportedDataRep();
      END; *)
    IF reader.rd.getSub(arr) # NUMBER(arr) THEN
      RaiseUnmarshalFailure();
    END;
  END InChars;

PROCEDURE OutChars(writer: Pickle.Writer; READONLY arr: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    writer.wr.putString(arr);
  END OutChars;

PROCEDURE InBytes(reader: Pickle.Reader; VAR arr: ARRAY OF Byte8)
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR p := LOOPHOLE(ADR(arr[0]), UNTRACED REF ARRAY [0..65335] OF CHAR);
  BEGIN
    IF reader.rd.getSub(SUBARRAY(p^, 0, NUMBER(arr))) # NUMBER(arr) THEN
      RaiseUnmarshalFailure();
    END;
  END InBytes;

PROCEDURE OutBytes(writer: Pickle.Writer; READONLY arr: ARRAY OF Byte8)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR p := LOOPHOLE(ADR(arr[0]), UNTRACED REF ARRAY [0..65335] OF CHAR);
  BEGIN
    writer.wr.putString(SUBARRAY(p^, 0, NUMBER(arr)));
  END OutBytes;

(* this code is integer-length dependent *)
(* we also rely on the invariant that MsgRd/MsgWr will
   provide contiguous 8-byte chunks at proper alignment ..
   as long as there is no intervening flush *)

PROCEDURE InInteger(reader: Pickle.Reader; 
                    min := FIRST(INTEGER);
                    max := LAST(INTEGER)): INTEGER
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR i: INTEGER;
  BEGIN
    CASE reader.conversion OF
    | Kind.Copy, Kind.Swap =>
      VAR c := LOOPHOLE(ADR(i), 
                        UNTRACED REF ARRAY [1..BYTESIZE(INTEGER)] OF CHAR);
      BEGIN
        IF reader.rd.getSub(c^) # NUMBER(c^) THEN
          RaiseUnmarshalFailure();
        END;
        IF reader.conversion = Kind.Swap THEN
          CASE myPacking.word_size OF
          | 32 => i := Swap.Swap4(i);
          | 64 => 
            VAR ii: Int64On64;
            BEGIN
              ii.v := i;
              ii := LOOPHOLE(Swap.Swap8(LOOPHOLE(ii, Int64On32)), Int64On64);
              i := ii.v;
            END;
          ELSE
            RaiseUnsupportedDataRep();
          END;
        END;
      END;

    | Kind.Copy32to64, Kind.Swap32to64 =>
      VAR i32: Int32;
          c32 := LOOPHOLE(ADR(i32), UNTRACED REF ARRAY [1..4] OF CHAR);
      BEGIN
	IF reader.rd.getSub(c32^) # NUMBER(c32^) THEN
	  RaiseUnmarshalFailure();
	END;
	IF reader.conversion = Kind.Swap32to64 THEN
	  i32 := Swap.Swap4(i32);
	END;
	i := i32;
      END;

    | Kind.Copy64to32, Kind.Swap64to32 =>
      VAR i64: Int64On32;
          c64 := LOOPHOLE(ADR(i64), UNTRACED REF ARRAY [1..8] OF CHAR);
      BEGIN
	IF reader.rd.getSub(c64^) # NUMBER(c64^) THEN
	  RaiseUnmarshalFailure();
	END;
	IF reader.packing.little_endian THEN
	  i := i64.a;
	  IF i64.b # 0 AND i64.b # -1 THEN
	    RAISE Pickle.Error("Data value too big.");
	  END;
	ELSE
	  i := i64.b;
	  IF i64.a # 0 AND i64.a # -1 THEN
	    RAISE Pickle.Error("Data value too big.");
	  END;
	END;
  
	(* Now, swap it if need be. *)
	IF reader.conversion = Kind.Swap64to32 THEN
	  i := Swap.Swap4(i);
	END;
      END;
    END;

    IF i < min OR i > max THEN RaiseUnmarshalFailure(); END;
    RETURN i;
  END InInteger;

PROCEDURE InInt32(reader: Pickle.Reader; 
                    min := FIRST(Int32);
                    max := LAST(Int32)): Int32
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR i32: Int32;
      c32 := LOOPHOLE(ADR(i32), UNTRACED REF ARRAY [1..4] OF CHAR);
  BEGIN
    IF reader.rd.getSub(c32^) # NUMBER(c32^) THEN
      RaiseUnmarshalFailure();
    END;
    CASE reader.conversion OF
    | Kind.Swap, Kind.Swap64to32, Kind.Swap32to64 =>
      i32 := Swap.Swap4(i32);
    | Kind.Copy, Kind.Copy64to32, Kind.Copy32to64 =>
    END;
    IF i32 < min OR i32 > max THEN RaiseUnmarshalFailure(); END;
    RETURN i32;
  END InInt32;

PROCEDURE OutInteger(writer: Pickle.Writer; i: INTEGER)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR ip := LOOPHOLE(ADR(i), 
                     UNTRACED REF ARRAY [1..BYTESIZE(INTEGER)] OF CHAR);
  BEGIN
    writer.wr.putString(ip^);
  END OutInteger;

PROCEDURE OutInt32(writer: Pickle.Writer; i: Int32)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR ip := LOOPHOLE(ADR(i), UNTRACED REF ARRAY [1..BYTESIZE(Int32)] OF CHAR);
  BEGIN
    writer.wr.putString(ip^);
  END OutInt32;

PROCEDURE InByte(reader: Pickle.Reader; 
                 max := LAST(Byte8)): Byte8
     RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR b: Byte8;
  BEGIN
    TRY
      b := LOOPHOLE(UnsafeRd.FastGetChar(reader.rd), Byte8);
    EXCEPT
    | Rd.EndOfFile => RaiseUnmarshalFailure();
    END;
    IF b > max THEN
      RaiseUnmarshalFailure();
    END;
    RETURN b
  END InByte;

PROCEDURE OutByte(writer: Pickle.Writer; b: Byte8)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    UnsafeWr.FastPutChar(writer.wr, LOOPHOLE(b, CHAR));
  END OutByte;

PROCEDURE InCardinal(reader: Pickle.Reader;
     lim: CARDINAL := LAST(CARDINAL)): CARDINAL
     RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN InInteger(reader, 0, lim);
  END InCardinal;

PROCEDURE OutCardinal(writer: Pickle.Writer; card: CARDINAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    OutInteger(writer, card);
  END OutCardinal;

PROCEDURE InReal(reader: Pickle.Reader): REAL
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR i: REAL;
  BEGIN
    IF reader.packing.float # myPacking.float THEN
      RaiseUnsupportedDataRep();
    END;
    IF reader.rd.getSub(
        LOOPHOLE(i, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR)) # BYTESIZE(REAL) THEN
      RaiseUnmarshalFailure();
    END;
    IF NOT NativeEndian(reader.packing) THEN i := SwapReal(i); END;
    RETURN i;
  END InReal;

PROCEDURE OutReal(writer: Pickle.Writer; i: REAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    writer.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR));
  END OutReal;

PROCEDURE InLongreal(reader: Pickle.Reader): LONGREAL
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR i: LONGREAL;
  BEGIN
    IF reader.packing.float # myPacking.float THEN
      RaiseUnsupportedDataRep();
    END;
    IF reader.rd.getSub(
        LOOPHOLE(i, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR)) #
                       BYTESIZE(LONGREAL) THEN
      RaiseUnmarshalFailure();
    END;
    IF NOT NativeEndian(reader.packing) THEN i := SwapLongReal(i); END;
    RETURN i;
  END InLongreal;

PROCEDURE OutLongreal(writer: Pickle.Writer; i: LONGREAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    writer.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR));
  END OutLongreal;

PROCEDURE InExtended(reader: Pickle.Reader): EXTENDED
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN LOOPHOLE(InLongreal(reader), EXTENDED);
  END InExtended;

PROCEDURE OutExtended(writer: Pickle.Writer; i: EXTENDED)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    writer.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(EXTENDED)-1] OF CHAR));
  END OutExtended;

PROCEDURE InBoolean(reader: Pickle.Reader) : BOOLEAN
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR res: BOOLEAN;
  BEGIN
    TRY
      res := UnsafeRd.FastGetChar(reader.rd) # '\000';
    EXCEPT
    | Rd.EndOfFile => RaiseUnmarshalFailure();
    END;
    RETURN res;
  END InBoolean;

PROCEDURE OutBoolean(writer: Pickle.Writer; bool: BOOLEAN)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF bool THEN
      UnsafeWr.FastPutChar(writer.wr, '\001');
    ELSE
      UnsafeWr.FastPutChar(writer.wr, '\000');
    END;
  END OutBoolean;

PROCEDURE InText(reader: Pickle.Reader) : TEXT
   RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR len: INTEGER;
  VAR text: TEXT;
  BEGIN
    len := InInt32(reader);
    IF len = -1 THEN
      RETURN NIL;
    ELSIF len < 0 THEN
      RaiseUnmarshalFailure();
    ELSE
      text := NEW(TEXT, len+1);
      InChars(reader, SUBARRAY(text^, 0, len));
      text[len] := '\000';
    END;
    RETURN text;
  END InText;

PROCEDURE OutText(writer: Pickle.Writer; READONLY text: TEXT)
   RAISES {Wr.Failure, Thread.Alerted} =
  VAR len: INTEGER;
  BEGIN
    IF text # NIL THEN
      len := Text.Length(text);
    ELSE
      len := -1;
    END;
    OutInt32(writer, len);
    IF len > 0 THEN OutChars(writer, SUBARRAY(text^, 0, len)); END;
  END OutText;

PROCEDURE SwapReal(i: REAL) : REAL =
  BEGIN
    RETURN LOOPHOLE(Swap.Swap4(LOOPHOLE(i, Int32)), REAL);
  END SwapReal;

TYPE LR = RECORD a, b: Int32; END;

PROCEDURE SwapLongReal(i: LONGREAL) : LONGREAL =
  VAR res: LONGREAL;
  BEGIN
    WITH p = LOOPHOLE(ADR(i), UNTRACED REF LR) DO
      WITH r = LOOPHOLE(ADR(res), UNTRACED REF LR) DO
        r.a := Swap.Swap4(p.b);
        r.b := Swap.Swap4(p.a);
      END;
    END;
    RETURN res;
  END SwapLongReal;

PROCEDURE NativeEndian(packing: RTPacking.T) : BOOLEAN =
  BEGIN
    RETURN packing.little_endian = myPacking.little_endian;
  END NativeEndian;

PROCEDURE RaiseUnmarshalFailure() RAISES {Pickle.Error} =
  BEGIN
    RaiseError("UnmarshalFailure");
  END RaiseUnmarshalFailure;

PROCEDURE RaiseUnsupportedDataRep() RAISES {Pickle.Error} =
  BEGIN
    RaiseError("UnsupportedDataRep");
  END RaiseUnsupportedDataRep;

PROCEDURE RaiseError(t: TEXT) RAISES {Pickle.Error} =
  BEGIN
    RAISE Pickle.Error(t);
  END RaiseError;

BEGIN
END PickleStubs.
