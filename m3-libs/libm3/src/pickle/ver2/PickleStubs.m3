(* 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *
 * This file is released under the same conditions as Pickle.m3. See COPYRIGHT.
 *
 *)

UNSAFE MODULE PickleStubs;
   (* unsafe because of marshalling code *)
   
IMPORT Pickle2 AS Pickle, PickleRd, PickleWr, RTPacking, RTType;
IMPORT ConvertPacking; 
FROM PickleRd IMPORT myPacking;
FROM ConvertPacking IMPORT CPKind;
FROM Swap IMPORT Int32, Int64On32, Int64On64;

IMPORT Rd, Wr, Text, TextClass, Text8, Text16, Thread;
IMPORT RdClass, WrClass, UnsafeRd, UnsafeWr, Swap; 
FROM Word IMPORT And, Or, LeftShift, RightShift; 

REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;

TYPE
  CharPtr  = UNTRACED REF ARRAY [0..65535] OF CHAR;
  WCharPtr = UNTRACED REF ARRAY [0..65535] OF WIDECHAR;
  UInt8 = BITS 8 FOR [0 .. 16_FF];
  U16Aligned = RECORD forceAlign: INTEGER; u16: BITS 16 FOR [0..16_FFFF] END;

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

(* WC21 is a variable-length encoding of widechar values, used only in
   pickles and net objects.  The standard Unicode encodings explicitly disallow
   surrogate code points as unencoded values.  Programs may have a legitimate 
   need to store and/or manipulate surrogate values in memory, and these should
   be picklable too.  WC21 supports the entire code point range, which requires
   21 bits.

   The first byte has 7 (least significant) data bits and one bit (msb of the 
   byte) that, if set, indicates another byte follows.  If present, the second 
   byte is just like the first and supplies the next more significant 7 data 
   bits.  If it calls for a third byte, that contains the 7 most significant 
   data bits.  The bytes are always in least- to most-significant order in the 
   pickle, regardless of endianness of writing or reading machine.

   Note that this can encode values beyond the Unicode code point range.
   When writing, we prevent this by accepting a parameter of type WIDECHAR.
   When reading, we return an integer and let the caller apply a range
   check, which could be only 16_FFFF, when compiled with WIDECHAR of this
   size.      
*) 

PROCEDURE InWC21(rd: Rd.T): UInt32 
RAISES{Rd.EndOfFile, Rd.Failure, Thread.Alerted} = 
(* Unmarshal one WIDECHAR value in WC21 encoding and return in a 32-bit int,
   where caller can range check. *) 

  VAR B0, B1, B2: UInt8; 
  VAR intVal: INTEGER; 
  BEGIN 
    B0 := ORD(Rd.GetChar(rd)); 
    intVal := And(B0, 16_7F);
    IF And(B0, 16_80) # 0 THEN (* A second byte follows. *) 
      B1 := ORD(Rd.GetChar(rd)); 
      intVal := Or(intVal, LeftShift (And (B1, 16_7F), 7));
      IF And(B1, 16_80) # 0 THEN (* A third byte follows. *) 
        B2 := ORD(Rd.GetChar(rd)); 
        intVal := Or(intVal, LeftShift (And(B2, 16_7F), 14));
      END;  
    END;  
    RETURN intVal 
  END InWC21; 

PROCEDURE InWideChars(reader: Pickle.Reader; VAR arr: ARRAY OF WIDECHAR)
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR cnt: INTEGER := NUMBER(arr);  p: CharPtr;  n: INTEGER;
  VAR IntVal: UInt32; 
  VAR u16Al: U16Aligned; 
  BEGIN
    IF cnt <= 0 THEN RETURN; END;
    TRY 
      IF reader.packing.widechar_size = 32 THEN 
         (* Size 32 in the writing system => WC21 in the pickle. *) 
        CASE reader.widecharConvKind OF 
        | CPKind.Copy, CPKind.Swap => (* 32 on both systems. *) 
          FOR RI := 0 TO LAST(arr) DO
            IntVal := InWC21(reader.rd);
            IF IntVal > 16_10FFFF THEN 
              RaiseError("Malformed pickle: WIDECHAR out of range."); 
            END;
            arr[RI] := VAL(IntVal, WIDECHAR);  
          END; 
        | CPKind.Copy32to16 , CPKind.Swap32to16 => (* Remote 32, local 16. *) 
          FOR RI := 0 TO LAST(arr) DO
            IntVal := InWC21(reader.rd);
            IF IntVal > 16_FFFF THEN IntVal := ReplacementWt; END;
            arr[RI] := VAL(IntVal, WIDECHAR);  
          END; 
        ELSE <* ASSERT FALSE *> 
        END 
      ELSE (* size 16 in the pickle. *) 
        CASE reader.widecharConvKind OF 
        | CPKind.Copy => (* 16 on both systems, same endian. *)
          INC(cnt, cnt);  (* == # of 8-bit bytes *)
          p := LOOPHOLE(ADR(arr[0]), CharPtr);
          WHILE (cnt > 0) DO
            n := MIN(cnt, NUMBER(p^));
            IF reader.rd.getSub(SUBARRAY(p^, 0, n)) # n THEN
              RaiseUnmarshalFailure();
            END;
            INC(p, ADRSIZE(p^));  DEC(cnt, NUMBER(p^));
          END;
        | CPKind.Copy16to32 => (* Remote 16, local 32, same endian. *) 
          WITH u16arr = LOOPHOLE(u16Al.u16, ARRAY [0..1] OF CHAR) DO
            FOR RI := 0 TO LAST(arr) DO
              u16arr[0] := Rd.GetChar(reader.rd);
              u16arr[1] := Rd.GetChar(reader.rd);
              arr[RI] := VAL(u16Al.u16, WIDECHAR);  
            END; 
          END; 
        | CPKind.Swap, CPKind.Swap16to32 => (* Remote 16, opposite endian. *) 
          WITH u16arr = LOOPHOLE(u16Al.u16, ARRAY [0..1] OF CHAR) DO
            FOR RI := 0 TO LAST(arr) DO
              u16arr[1] := Rd.GetChar(reader.rd);
              u16arr[0] := Rd.GetChar(reader.rd);
              arr[RI] := VAL(u16Al.u16, WIDECHAR);  
            END; 
          END; 
        ELSE <* ASSERT FALSE *> 
        END 
      END; 
    EXCEPT Rd.EndOfFile => RaiseUnmarshalFailure();
    END; 
  END InWideChars;

PROCEDURE OutChars(writer: Pickle.Writer; READONLY arr: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    writer.wr.putString(arr);
  END OutChars;

PROCEDURE OutWC21(wr: Wr.T; wc: WIDECHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
(* Marshal one wide char in surrogate-tolerant WC21 incoding. *) 

  VAR intVal, B0, B1, B2: UInt32;
  BEGIN  
    intVal := ORD(wc); 
    B0 := And(intVal, 16_7F);
    IF And(intVal, 16_FFFFFF80) = 0 THEN (* No 2nd byte is needed. *)
      Wr.PutChar(wr, VAL(B0, CHAR));
    ELSE 
      B0 := Or (B0, 16_80); 
      Wr.PutChar(wr, VAL(B0, CHAR));
      B1 := RightShift(And(intVal, 16_3F80), 7); 
      IF And(intVal, 16_FFFFC000) = 0 THEN (* No 3rd byte is needed. *)
        Wr.PutChar(wr, VAL(B1, CHAR));
      ELSE 
        B1 := Or (B1, 16_80); 
        Wr.PutChar(wr, VAL(B1, CHAR));
        B2 := RightShift(And(intVal, 16_1FC000), 14);  
        Wr.PutChar(wr, VAL(B2, CHAR));
      END; 
    END; 
  END OutWC21; 

PROCEDURE OutWideChars(writer: Pickle.Writer; READONLY arr: ARRAY OF WIDECHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR cnt: INTEGER := NUMBER (arr);  p: CharPtr;
  BEGIN
    IF cnt <= 0 THEN RETURN; END;
    IF writer.packing.widechar_size = 16 THEN 
      INC(cnt, cnt);  (* == # of 8-bit bytes *)
      p := LOOPHOLE(ADR(arr[0]), CharPtr);
      WHILE (cnt > 0) DO
        writer.wr.putString(SUBARRAY(p^, 0, MIN (cnt, NUMBER(p^))));
        INC(p, ADRSIZE(p^)); DEC(cnt, NUMBER(p^));
      END;
    ELSE (* Writing on 32-bit WIDECHAR system. *) 
      FOR RI := 0 TO cnt-1 DO
        OutWC21(writer.wr, arr[RI]); 
      END; 
    END; 
  END OutWideChars;

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
    CASE reader.wordConvKind OF
    | CPKind.Copy, CPKind.Swap =>
      VAR c := LOOPHOLE(ADR(i), 
                        UNTRACED REF ARRAY [1..BYTESIZE(INTEGER)] OF CHAR);
      BEGIN
        IF reader.rd.getSub(c^) # NUMBER(c^) THEN
          RaiseUnmarshalFailure();
        END;
        IF reader.wordConvKind = CPKind.Swap THEN
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

    | CPKind.Copy32to64, CPKind.Swap32to64 =>
      VAR i32: Int32;
          c32 := LOOPHOLE(ADR(i32), UNTRACED REF ARRAY [1..4] OF CHAR);
      BEGIN
	IF reader.rd.getSub(c32^) # NUMBER(c32^) THEN
	  RaiseUnmarshalFailure();
	END;
	IF reader.wordConvKind = CPKind.Swap32to64 THEN
	  i32 := Swap.Swap4(i32);
	END;
	i := i32;
      END;

    | CPKind.Copy64to32, CPKind.Swap64to32 =>
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
	IF reader.wordConvKind = CPKind.Swap64to32 THEN
	  i := Swap.Swap4(i);
	END;
      END;
    ELSE <* ASSERT FALSE *>
    END;

    IF i < min OR i > max THEN RaiseUnmarshalFailure(); END;
    RETURN i;
  END InInteger;

PROCEDURE InLongint(reader: Pickle.Reader; 
                    min := FIRST(LONGINT);
                    max := LAST(LONGINT)): LONGINT
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR i: LONGINT;
  BEGIN
    CASE reader.longConvKind OF
    | CPKind.Copy, CPKind.Swap =>
      VAR c := LOOPHOLE(ADR(i), 
                        UNTRACED REF ARRAY [1..BYTESIZE(LONGINT)] OF CHAR);
      BEGIN
        IF reader.rd.getSub(c^) # NUMBER(c^) THEN
          RaiseUnmarshalFailure();
        END;
        IF reader.longConvKind = CPKind.Swap THEN
          CASE myPacking.long_size OF
          | 32 => i := VAL(Swap.Swap4(VAL(i, INTEGER)), LONGINT);
          | 64 => 
            VAR ii: Int64On64;
            BEGIN
              ii.v := VAL(i, INTEGER);
              ii := LOOPHOLE(Swap.Swap8(LOOPHOLE(ii, Int64On32)), Int64On64);
              i := VAL(ii.v, LONGINT);
            END;
          ELSE
            RaiseUnsupportedDataRep();
          END;
        END;
      END;

    | CPKind.Copy32to64, CPKind.Swap32to64 =>
      VAR i32: Int32;
          c32 := LOOPHOLE(ADR(i32), UNTRACED REF ARRAY [1..4] OF CHAR);
      BEGIN
	IF reader.rd.getSub(c32^) # NUMBER(c32^) THEN
	  RaiseUnmarshalFailure();
	END;
	IF reader.longConvKind = CPKind.Swap32to64 THEN
	  i32 := Swap.Swap4(i32);
	END;
	i := VAL(i32, LONGINT);
      END;

    | CPKind.Copy64to32, CPKind.Swap64to32 =>
      VAR i64: Int64On32;
          c64 := LOOPHOLE(ADR(i64), UNTRACED REF ARRAY [1..8] OF CHAR);
      BEGIN
	IF reader.rd.getSub(c64^) # NUMBER(c64^) THEN
	  RaiseUnmarshalFailure();
	END;
	IF reader.packing.little_endian THEN
	  i := VAL(i64.a, LONGINT);
	  IF i64.b # 0 AND i64.b # -1 THEN
	    RAISE Pickle.Error("Data value too big.");
	  END;
	ELSE
	  i := VAL(i64.b, LONGINT);
	  IF i64.a # 0 AND i64.a # -1 THEN
	    RAISE Pickle.Error("Data value too big.");
	  END;
	END;
  
	(* Now, swap it if need be. *)
	IF reader.longConvKind = CPKind.Swap64to32 THEN
	  i := VAL(Swap.Swap4(VAL(i, INTEGER)), LONGINT);
	END;
      END;
    ELSE <* ASSERT FALSE *>
    END;

    IF i < min OR i > max THEN RaiseUnmarshalFailure(); END;
    RETURN i;
  END InLongint;

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
    CASE reader.wordConvKind OF
    | CPKind.Swap, CPKind.Swap64to32, CPKind.Swap32to64 =>
      i32 := Swap.Swap4(i32);
    | CPKind.Copy, CPKind.Copy64to32, CPKind.Copy32to64 =>
    ELSE <* ASSERT FALSE *>
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

PROCEDURE OutLongint(writer: Pickle.Writer; i: LONGINT)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR ip := LOOPHOLE(ADR(i), 
                     UNTRACED REF ARRAY [1..BYTESIZE(LONGINT)] OF CHAR);
  BEGIN
    writer.wr.putString(ip^);
  END OutLongint;

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

PROCEDURE InLongcard(reader: Pickle.Reader;
     lim: LONGCARD := LAST(LONGCARD)): LONGCARD
     RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN InLongint(reader, 0L, lim);
  END InLongcard;

PROCEDURE OutLongcard(writer: Pickle.Writer; card: LONGCARD)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    OutLongint(writer, card);
  END OutLongcard;

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
  BEGIN
    len := InInt32(reader);
    IF len = -1 THEN
      RETURN NIL;
    ELSIF len = 0 THEN
      RETURN "";
    ELSIF len < 0 THEN
      RaiseUnmarshalFailure();
      RETURN NIL;
    ELSIF InByte(reader) # ORD(FALSE) THEN
      RETURN InText16(reader, len);
    ELSE
      RETURN InText8(reader, len);
    END;
  END InText;

PROCEDURE InText16(reader: Pickle.Reader;  len: INTEGER) : TEXT
   RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR buf: ARRAY [0..255] OF WIDECHAR;  txt16: Text16.T;
  BEGIN
    IF len <= NUMBER(buf) THEN
      WITH z = SUBARRAY(buf, 0, len) DO
        InWideChars(reader, z);
        RETURN Text.FromWideChars(z);
      END;
    ELSE
      txt16 := Text16.Create(len);
      InWideChars(reader, SUBARRAY(txt16.contents^, 0, len));
      RETURN txt16;
    END;
  END InText16;

PROCEDURE InText8(reader: Pickle.Reader;  len: INTEGER) : TEXT
   RAISES {Pickle.Error, Rd.Failure, Thread.Alerted} =
  VAR buf: ARRAY [0..255] OF CHAR;  txt8: Text8.T;
  BEGIN
    IF len <= NUMBER(buf) THEN
      WITH z = SUBARRAY(buf, 0, len) DO
        InChars(reader, z);
        RETURN Text.FromChars(z);
      END;
    ELSE
      txt8 := Text8.Create(len);
      InChars(reader, SUBARRAY(txt8.contents^, 0, len));
      RETURN txt8;
    END;
  END InText8;

PROCEDURE OutText(writer: Pickle.Writer; txt: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR info: TextClass.Info;
  BEGIN
    IF txt = NIL THEN
      OutInt32(writer, -1);
    ELSE
      txt.get_info (info);
      OutInt32(writer, info.length);
      IF info.length > 0 THEN
        OutByte(writer, ORD(info.wide));
        IF info.wide THEN
          IF info.start # NIL
            THEN OutString16(writer, info.start, info.length);
            ELSE OutText16(writer, txt, info.length);
          END;
        ELSE (* 8-bit characters only *)
          IF info.start # NIL
            THEN OutString8(writer, info.start, info.length);
            ELSE OutText8(writer, txt, info.length);
          END;
        END;
      END;
    END;
  END OutText;

PROCEDURE OutText16(writer: Pickle.Writer;  txt: TEXT;  len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR cnt := 0;  buf: ARRAY [0..511] OF WIDECHAR;
  BEGIN
    WHILE cnt < len DO
      Text.SetWideChars (buf, txt, start := cnt);
      OutWideChars(writer, SUBARRAY(buf, 0, MIN (len-cnt, NUMBER(buf))));
      INC(cnt, NUMBER(buf));
    END;
  END OutText16;

PROCEDURE OutString16(writer: Pickle.Writer;  start: ADDRESS;  len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR p: WCharPtr := start;
  BEGIN
    WHILE (len > 0) DO
      OutWideChars(writer, SUBARRAY(p^, 0, MIN(len, NUMBER(p^))));
      INC(p, ADRSIZE (p^));  DEC(len, NUMBER(p^));
    END;
  END OutString16;

PROCEDURE OutText8(writer: Pickle.Writer;  txt: TEXT;  len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR cnt := 0;  buf: ARRAY [0..511] OF CHAR;
  BEGIN
    WHILE cnt < len DO
      Text.SetChars (buf, txt, start := cnt);
      OutChars(writer, SUBARRAY(buf, 0, MIN (len-cnt, NUMBER(buf))));
      INC(cnt, NUMBER(buf));
    END;
  END OutText8;

PROCEDURE OutString8(writer: Pickle.Writer;  start: ADDRESS;  len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR p: CharPtr := start;
  BEGIN
    WHILE (len > 0) DO
      OutChars(writer, SUBARRAY(p^, 0, MIN(len, NUMBER(p^))));
      INC(p, ADRSIZE(p^));  DEC(len, NUMBER(p^));
    END;
  END OutString8;

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
