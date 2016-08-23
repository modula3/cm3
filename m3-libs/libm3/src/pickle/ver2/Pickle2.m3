(* Copyright 1992 Digital Equipment Corporation             *)
(* Distributed only by permission.                          *)
(* Last modified on Thu Jan 26 13:56:19 PST 1995 by kalsow  *)
(*      modified on Tue Mar  1 16:17:32 PST 1994 by wobber  *)
(*      modified on Mon Mar  8 12:43:59 PST 1993 by mjordan *)
(*      modified on Tue Jan 26 16:21:22 1993 by birrell *)

(* This module implements Pickles.  See the interface for
   documentation *)

UNSAFE MODULE Pickle2 EXPORTS Pickle2, PickleRd;

IMPORT Rd, RT0, RTAllocator, RTCollector, RTHeap, RTHeapRep, RTType, RTTypeFP,
       RTTypeMap, Thread, Word, Wr, Fingerprint, RTPacking, PklFpMap, 
       ConvertPacking, PklTipeMap, Swap, PickleRd, PickleWr, PickleStubs,
       BuiltinSpecials2, Fmt;

(* *)
(* Syntax of a pickle, and constants pertaining thereto *)
(* *)

(*
   Pickle::      Header Value Trailer
   Header::      Header1                - verify it's a pickle
                 Header2                - verify it's a pickle
                 Version                - version of this syntax
                 Representation         - information about the representation
   Representation (old)::
                 CharRep                - character encoding
                 Endian                 - byte order
                 IntSize                - size of an INTEGER
                 RealRep                - format of REAL and such
   Representation (new)::
                 4 bytes encoding RTPacking.T for local machine
   CharRep::     CharRepISOLatin1
   Endian::      LittleEndian | BigEndian
   IntSize::     IntSize16 | IntSize32 | IntSize64
   RealRep::     RealRepNative
   Value::       '0'                    - r=NIL
                 '1' 4-byte-integer     - pickle-relative index of "r" (0 based)
                 '2' 8-bytes              - COMPATIBILITY, old fingerprint
                 '3' scInt acInt Contents - COMPATIBILITY, old value
                 '4'                    - occurs only in Trailer as blunder check.
                 '5' sc Contents        - an actual value
                 '6' integer            - a pseudo pointer
   FingerPrint:: bytes                  - 8 bytes, details t.b.d.
   sc::          LocalCode              - typecode of Special
                                          writing the value
   Contents::    ( bytes | Value )*     - Special.write output
   Trailer::     Trailer1 Trailer2      - verify the parse was OK

   - Note that "LocalCode" may occur within the bytes written by a
     special. E.g. the root special does this to record the allocated
     type of the value. It is written by WriteType and parsed by ReadType.

   LocalCode::   0 8-bytes |            - first occurence of a fingerprint.
                 [1..254] |             - subsequent occurrence, n IN [1..254].
                 255 4-byte-integer     - subsequent occurence, other cases.
   *)

CONST
  Header1 = '&';
  Header2 = '%';
  (* BLAIR ---> *)
  Version = '3';                       (* new vastly improved version *)
  OldVersion = '2';                    (* old broken pickle version *)
  OlderVersion = '1';                  (* really old FP and ac syntax *)
  (* <--- BLAIR *)
  CharRepISOLatin1 = 'I';              (* 8-bit ISO Latin 1 *)
  LittleEndian = 'L';                  (* l.s. byte first *)
  BigEndian = 'B';                     (* m.s. byte first *)
  IntSize16 = '4';                     (* 16 bit INTEGER *)
  IntSize32 = '5';                     (* 32 bit INTEGER *)
  IntSize64 = '6';                     (* 64 bit INTEGER *)
  RealRepNative = 'N';                 (* whatever the writing
                                          host used *)
  Trailer1 = '4';                      (* # main cases *)
  Trailer2 = '\n';                     (* keeps editors happy *)


(* *)
(* Constants, types and revelations *)
(* *)

TYPE
  RefTable = REF ARRAY OF RECORD
      (* hash table keyed by REFANY, yields index in pickle *)
      r: REFANY := NIL;        (* the Ref *)
      index: INTEGER := 0;     (* pickle-relative index of this
                                  ref, 0-based *)
      nextUsed: INTEGER := 0;  (* index in this table of next
                                  used entry *)
    END;
  RefArray = REF ARRAY OF REFANY;
    (* array indexed by index in pickle, yields REFANY *)
  TypeTable = REF ARRAY OF INTEGER;
    (* indexed by RTType.TypeCode, yields pickle-relative
       typecode *)
    (* Or indexed by pickle-relative typecode, yields
       RTTypes.TypeCode *)

REVEAL
  Writer = PickleWr.Private BRANDED "Pickle.Writer 2.0" OBJECT
      level := 0;
      refCount: INTEGER;         (* count of refs written in this pickle *)
      firstUsed: INTEGER;        (* index in "refs" of first used entry *)
      refs: RefTable := NIL;     (* hash table of refs in this pickle *)
      tcCount: INTEGER;          (* count of typecodes in this pickle *)
      tcToPkl: TypeTable := NIL; (* process TC -> pickle TC *)
      pklToTC: TypeTable := NIL; (* pickle TC -> process TC, for erasing tcToPkl *)
      nextAddr: ADDRESS;         (* Used within RootSpecialWrite *)
      collisions: INTEGER := 0;  (* Performance measure *)
      visitor: WriteVisitor := NIL;
      (* BLAIR ---> *)
      firstAddr: ADDRESS;
      tipeVisitor: TipeWriteVisitor := NIL;
      (* <--- BLAIR *)
    OVERRIDES
      write := WriteRef;
      writeType := WriteType;
      writeInt := WriteInt;
    END;

  Reader = PickleRd.Private BRANDED "Pickle.Reader 2.0" OBJECT
      level := 0;
      acPending := 0;            (* COMPATIBILITY - OlderVersion *)
      refCount: INTEGER;         (* count of refs read in this pickle *)
      tcCount: INTEGER;          (* count of typecodes in this pickle *)
      refs: RefArray := NIL;     (* array of refs in this pickle *)
      pklToTC: TypeTable := NIL; (* pickle TC -> process TC *)
      (* BLAIR ---> *)
      nextAddr: ADDRESS;         (* COMPATIBILITY - Used in RootSpecialWrite *)
      version: CHAR;
      tipeVisitor: TipeReadVisitor := NIL;
      (* <--- BLAIR *)
      visitor: ReadVisitor := NIL; (* COMPATIBILITY *)
    OVERRIDES
      read := ReadRef;
      readType := ReadType;
      readInt := ReadInt;
      noteRef := NoteRef;
    END;

  Special = SpecialPublic BRANDED "Pickle.Special 2.0" OBJECT
    OVERRIDES
      write := RootSpecialWrite;
      read := RootSpecialRead;
    END;

TYPE
  WriteVisitor = RTTypeMap.Visitor OBJECT
      writer: Writer := NIL;
    OVERRIDES
      apply := VisitWrite;
    END;

  ReadVisitor = RTTypeMap.Visitor OBJECT
      reader: Reader := NIL;
    OVERRIDES
      apply := VisitRead;
    END;

(* BLAIR ---> *)
TYPE TipeReadVisitor = ConvertPacking.ReadVisitor OBJECT 
      reader: Reader := NIL;
    OVERRIDES
      readData := TipeReadData;
      skipData := TipeSkipReadData;
      readRef := TipeReadRef;
      readChar := TipeReadChar;
      getReader := TipeGetReader; 
    END;

TYPE TipeWriteVisitor = ConvertPacking.WriteVisitor OBJECT 
      writer: Writer := NIL;
    OVERRIDES
      writeData := TipeWriteData;
      skipData := TipeSkipWriteData;
      writeRef := TipeWriteRef;
      writeChar := TipeWriteChar; 
      getWriter := TipeGetWriter; 
    END;
(* <--- BLAIR*)

CONST
  RefFields = RTTypeMap.Mask { RTTypeMap.Kind.Ref,
                               RTTypeMap.Kind.UntracedRef,
                               RTTypeMap.Kind.Proc };

TYPE (* for binary I/O loopholes *)
  CharInt32 = ARRAY [0..3] OF CHAR;  (* 32 bits only *)
  CharFP    = ARRAY [0..BYTESIZE(Fingerprint.T)-1] OF CHAR;
  ToChars   = UNTRACED REF ARRAY [0..100000000] OF CHAR; (* for misc. data *)

(*BLAIR ---> *)
TYPE
  Int32  = [-16_7fffffff-1 .. 16_7fffffff];
(* <--- BLAIR*)

TYPE
  HC = { h1, h2, v, p0, p1, p2, p3 }; (* the chars in a pickle header *)
  Header = ARRAY HC OF CHAR;          (* a pickle header string *)
                                      (* COMPAT: p0=c, p1=e, p2=i, p3=r *)

  HT = { t1, t2 };                (* the chars in a pickle trailer *)
  Trailer = ARRAY HT OF CHAR;     (* a pickle trailer string *)

CONST
  InitRefCapacity = 99;           (* Init size of {Reader,Writer}.refs *)
  InitTypeCapacity = 99;          (* Init size of *.pklToTC *)

(* *)
(* Global variables (gasp!); initialized in main body *)
(* *)

VAR v1_header: Header;	          (* COMPAT. old header for pickles we write *)
VAR v2_header: Header;            (* header for the pickles we write *)
VAR myPackingCode: INTEGER;       (* our local packing. *)
VAR myTrailer: Trailer;           (* trailer for pickles we write *)
VAR nullReaderRef: REF INTEGER;   (* null value for reader.refs entries *)

PROCEDURE FPImage(READONLY fp: Fingerprint.T): TEXT = 
  (* Return a readable image of "fp". *) 

  BEGIN
    RETURN 
      "{"
      & Fmt.Int(fp.byte[0]) & "," 
      & Fmt.Int(fp.byte[1]) & "," 
      & Fmt.Int(fp.byte[2]) & "," 
      & Fmt.Int(fp.byte[3]) & "," 
      & Fmt.Int(fp.byte[4]) & "," 
      & Fmt.Int(fp.byte[5]) & "," 
      & Fmt.Int(fp.byte[6]) & "," 
      & Fmt.Int(fp.byte[7])  
      & "}"
  END FPImage;

(* *)
(* Top-level sugar: Write and Read *)
(* *)

PROCEDURE Write(wr: Wr.T; r: REFANY; write16BitWidechar := FALSE)
        RAISES { Error, Wr.Failure, Thread.Alerted } =
  BEGIN
    NEW (Writer, wr := wr, write16BitWidechar := write16BitWidechar).write(r);
  END Write;

PROCEDURE Read(rd: Rd.T): REFANY
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    RETURN NEW (Reader, rd := rd).read();
  END Read;


(* *)
(* Writer methods and subroutines *)
(* *)

PROCEDURE Hash(r: REFANY; table: RefTable): INTEGER =
  (* Considerations for the hash function:
        - use positive numbers
        - multiply to avoid collisions between equally
          distributed patterns
        - don't overflow 31-bit positive numbers
        - keep it efficient
        - result is in [0..LAST(table)]
  *)
  BEGIN
    RETURN ((Word.And(LOOPHOLE(r, Word.T), 16_7FFFFFFF) DIV
             65536) * (LOOPHOLE(r, Word.T) MOD 65536)
           ) MOD NUMBER(table^)
  END Hash;

PROCEDURE ExtendWriterTypes(writer: Writer) =
  (* Extend writer.pklToTC *)
    VAR old := writer.pklToTC;
  BEGIN
    writer.pklToTC := NEW(TypeTable, NUMBER(writer.pklToTC^) * 2);
    SUBARRAY(writer.pklToTC^, 0, NUMBER(old^)) := old^;
  END ExtendWriterTypes;

PROCEDURE ExtendWriterRefs(writer: Writer) =
    (* Make writer.refs bigger *)
    VAR old := writer.refs;
    VAR oldFirst := writer.firstUsed;
    VAR h: INTEGER;
  BEGIN
    writer.firstUsed := -1;
    writer.refs := NEW(RefTable, NUMBER(writer.refs^) * 2 - 1);
    WHILE oldFirst >= 0 DO
      (* for each entry in "old" *)
      WITH oldEntry = old[oldFirst] DO
        h := Hash(oldEntry.r, writer.refs);
        LOOP
          WITH entry = writer.refs[h] DO
            IF entry.r = NIL THEN
              entry.r := oldEntry.r;
              entry.index := oldEntry.index;
              entry.nextUsed := writer.firstUsed;
              writer.firstUsed := h;
              EXIT
            ELSE
              <* ASSERT(entry.r # oldEntry.r) *>
              INC(h);
              IF h >= NUMBER(writer.refs^) THEN h := 0 END;
            END;(*IF*)
          END;(*WITH*)
        END;(*LOOP*)
        oldFirst := oldEntry.nextUsed;
      END;(*WITH oldEntry*)
    END;(*WHILE*)
  END ExtendWriterRefs;

PROCEDURE WriteRef(writer: Writer; r: REFANY)
        RAISES { Error, Wr.Failure, Thread.Alerted } =
    VAR h: INTEGER;
    VAR sp: Special;
  BEGIN
    IF writer.level = 0 THEN
      (* Start of a pickle; do the overhead and call ourselves
         recursively *)
      IF writer.visitor = NIL THEN
        writer.visitor := NEW (WriteVisitor, writer := writer);
      END;
      writer.packing := RTPacking.Local();
      writer.widecharConvKind 
        := ConvertPacking.GetWidecharKind(writer.packing, writer.packing);

      (* BLAIR ---> *)
      IF writer.tipeVisitor = NIL THEN
        writer.tipeVisitor := NEW (TipeWriteVisitor, writer := writer);
      END;
      (* <--- BLAIR *)
      IF writer.refs = NIL THEN
        (* deferred allocation *)
        writer.refs := NEW(RefTable, InitRefCapacity * 2 - 1);
        WITH refs = writer.refs^ DO
          FOR i := 0 TO LAST(refs) DO refs[i].r := NIL END;
        END;
        writer.firstUsed := -1;
        writer.tcCount := 0; (* prevent excessive initialization *)
        writer.tcToPkl := NEW(TypeTable, RTType.MaxTypecode()+1);
        writer.pklToTC := NEW(TypeTable, InitTypeCapacity);
        WITH tcs = writer.tcToPkl^ DO
          FOR i := 0 TO LAST(tcs) DO tcs[i] := 0 END;
        END;
      END;
      (* (Re-)initialize the writer's state *)
      <*ASSERT (writer.firstUsed = -1)*>
          (* cleaned up last time *)
      writer.refCount := 0;
      FOR i := 1 TO writer.tcCount DO writer.tcToPkl[writer.pklToTC[i]] := 0 END;
      writer.tcCount := 0;
      writer.collisions := 0;
      StorePackingInHeader (v2_header,writer.write16BitWidechar); 
      Wr.PutString(writer.wr, v2_header);
      RTCollector.DisableMotion();
      INC(writer.level);
      TRY
        WriteRef(writer, r);
      FINALLY
        RTCollector.EnableMotion();
        DEC(writer.level);
        (* Flush the refs table now, to encourage the garbage
           collector. It must be flushed before re-use anyway *)
        h := writer.firstUsed;
        WHILE h >= 0 DO
          WITH entry = writer.refs[h] DO
            entry.r := NIL;
            h := entry.nextUsed;
          END;
        END;
        writer.firstUsed := -1;
      END;(*TRY*)
      Wr.PutString(writer.wr, myTrailer);
    ELSE
      (* Normal case: level#0 *)
      IF r = NIL THEN
        Wr.PutChar(writer.wr, '0');
      ELSIF Word.And (LOOPHOLE (r, INTEGER), 1) = 1 THEN (* Pseudopointer.*)
        Wr.PutChar(writer.wr, '6'); (* Always tag it as pseudopointer. *) 
        LOCK specialsMu DO sp := thePseudoSpecial; END;
        IF sp = NIL THEN  (* Pickle it as a Word.T. *)
          PickleStubs.OutInteger (writer, LOOPHOLE (r, INTEGER)); 
        ELSE (* Pickle it using the special's write method. *) 
          sp.write (r, writer); 
        END; 
      ELSE
        (* check refTable *)
        (* The following loop includes the entire hash table
           implementation. Considerations for the hash table
           algorithm:
             - mostly, we're adding new entries; looking up old
               entries is rare
             - we need to flush the table completely once per
               pickle
             - beware of relocating garbage collectors
           *)
        h := Hash(r, writer.refs);
        LOOP
          WITH entry = writer.refs[h] DO
            IF entry.r = NIL THEN
              (* virgin: insert in hash table *)
              entry.r := r;
              entry.index := writer.refCount;
              entry.nextUsed := writer.firstUsed;
                (* for fast flushing *)
              writer.firstUsed := h;
              INC(writer.refCount);
              IF writer.refCount * 2 > NUMBER(writer.refs^) THEN
                ExtendWriterRefs(writer);
              END;
              (* CAUTION: don't use "entry" after here, because
                 it might be invalidated by extendWriterRefs *)
              Wr.PutChar(writer.wr, '5');
              sp := GetSpecial(TYPECODE(r));
              writer.writeType(sp.sc);
              sp.write(r, writer);
              EXIT
            ELSIF entry.r = r THEN
              (* recycled *)
              Wr.PutChar(writer.wr, '1');
              writer.writeInt(entry.index);
              EXIT
            ELSE
              (* Hash collision *)
              INC(h);
              IF h >= NUMBER(writer.refs^) THEN h := 0 END;
              INC(writer.collisions);
            END;
          END;(*WITH*)
        END;(*LOOP*)
      END;(*r#NIL*)
    END;(*IF level=0 THEN ELSE*)
  END WriteRef;

PROCEDURE WriteType(writer: Writer; tc: INTEGER)
        RAISES { Wr.Failure, Thread.Alerted } =
    VAR fp: Fingerprint.T;
  BEGIN
    WITH pickleTC = writer.tcToPkl[tc] DO
      IF pickleTC = 0 THEN
        INC(writer.tcCount);
        IF writer.tcCount >= NUMBER(writer.pklToTC^) THEN
          ExtendWriterTypes(writer);
        END;
        pickleTC := writer.tcCount;
        writer.pklToTC[writer.tcCount] := tc;
        fp := RTTypeFP.ToFingerprint(tc);
        Wr.PutChar(writer.wr, VAL(0, CHAR));
        Wr.PutString(writer.wr, LOOPHOLE(fp, CharFP));
      ELSIF pickleTC < 255 THEN
        Wr.PutChar(writer.wr, VAL(pickleTC, CHAR));
      ELSE
        Wr.PutChar(writer.wr, VAL(255, CHAR));
        writer.writeInt(pickleTC);
      END;
    END;
  END WriteType;

PROCEDURE WriteInt(writer: Writer; i: INTEGER)
        RAISES { Wr.Failure, Thread.Alerted } =
    VAR int32: Int32 := i;
  BEGIN
    Wr.PutString(writer.wr, LOOPHOLE(ADR(int32), UNTRACED REF CharInt32)^);
  END WriteInt;

(* *)
(* Reader methods and subroutines *)
(* *)

PROCEDURE ExtendReaderTypes(reader: Reader) =
  (* Extend reader.pklToTC *)
    VAR old := reader.pklToTC;
  BEGIN
    reader.pklToTC := NEW(TypeTable, NUMBER(reader.pklToTC^) * 2);
    SUBARRAY(reader.pklToTC^, 0, NUMBER(old^)) := old^;
  END ExtendReaderTypes;

PROCEDURE GetBinaryInt(reader: Reader): INTEGER
        RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR i: Int32 := 0;
  BEGIN
    IF Rd.GetSub(reader.rd,
      LOOPHOLE (ADR(i), UNTRACED REF CharInt32)^) # BYTESIZE(CharInt32) THEN
        RAISE Rd.EndOfFile
    END;
    IF reader.version = Version THEN
      (* Handle different endians! *)
      IF reader.packing.little_endian # myPacking.little_endian THEN
        i := Swap.Swap4(i);
      END;
    END;
    RETURN i;
  END GetBinaryInt;

PROCEDURE ReadFP(reader: Reader): TypeCode
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    (* Read a fingerprint (8 bytes), store in reader.pklToTC,
       and return typecode *)
    VAR fp: Fingerprint.T; tc: TypeCode;
  BEGIN
    IF Rd.GetSub(reader.rd, LOOPHOLE(fp, CharFP)) # BYTESIZE(CharFP) THEN
      RAISE Rd.EndOfFile
    END;
    INC(reader.tcCount);
    IF reader.tcCount >= NUMBER(reader.pklToTC^) THEN
      ExtendReaderTypes(reader);
    END;
    tc := RTTypeFP.FromFingerprint(fp);
    IF tc = RTType.NoSuchType THEN
      tc := PklFpMap.FromFingerprint (fp); 
    END; 
    IF tc = RTType.NoSuchType THEN
      RAISE Error(
             "Can't read pickle, Fingerprint " 
             & FPImage(fp) & 
             " not known in this program")
    END;
    reader.pklToTC[reader.tcCount] := tc;
    RETURN tc
  END ReadFP;

PROCEDURE TCFromIndex(reader: Reader; index: INTEGER): TypeCode
      RAISES { Error } =
  BEGIN
    IF index > reader.tcCount THEN
      RAISE Error("Malformed pickle (TC index too large)")
    END;
    RETURN reader.pklToTC[index]
  END TCFromIndex;

PROCEDURE NewReadRefID (reader: Reader): RefID = 
(* allocate and return a new object reference ID. *) 
  VAR id: RefID;
  BEGIN 
    IF reader.refCount >= NUMBER(reader.refs^) THEN ExpandRefs (reader); END;
    id := reader.refCount;
    reader.refs[id] := nullReaderRef;
    INC(reader.refCount);
    RETURN id; 
  END NewReadRefID; 

PROCEDURE RefOfRefID (reader: Reader; ID : RefID) : REFANY = 
(* The reference indexed by ID *) 
  BEGIN 
    RETURN reader.refs [ ID ]
  END RefOfRefID; 

PROCEDURE InvokeSpecial(reader: Reader; sc: TypeCode): REFANY
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR sp: Special; r: REFANY; id: RefID;
  BEGIN
    sp := GetSpecial (sc);
    IF sp.sc # sc THEN
      RAISE Error("Can't read pickle (Special not defined)")
    END;
    id := NewReadRefID (reader); 
    INC(reader.level);
      r := sp.read(reader, id);
    DEC(reader.level);
    reader.noteRef(r, id);
    RETURN r
  END InvokeSpecial;

PROCEDURE ExpandRefs(reader: Reader) =
  VAR old := reader.refs;  n := NUMBER(old^);
  BEGIN
    reader.refs := NEW(RefArray, n + n);
    SUBARRAY(reader.refs^, 0, n) := old^;
  END ExpandRefs;

PROCEDURE ReadRef(reader: Reader): REFANY
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR r: REFANY; (* the result *)
    VAR repCase: CHAR;
    VAR sp: Special; 
  BEGIN
    IF reader.level = 0 THEN StartRead (reader); END;

    LOOP
      (* COMPATIBILITY: OlderVersion uses '2 fingerprint' *)
      repCase := Rd.GetChar(reader.rd);
      IF repCase # '2' THEN EXIT END;
      EVAL ReadFP(reader)
    END;

    IF repCase = '0' THEN
      r := NIL;
    ELSIF repCase = '1' THEN
      VAR refIndex := reader.readInt();
      BEGIN
        IF refIndex >= reader.refCount THEN
          RAISE Error("Malformed pickle (ref index too large)")
        END;
        r := reader.refs[refIndex];
      END;
    ELSIF repCase = '5' THEN
      r := InvokeSpecial(reader, reader.readType());
    ELSIF repCase = '3' THEN
      (* COMPATIBILITY: OlderVersion uses 3 sc ac contents *)
      VAR sc := GetBinaryInt(reader);
      BEGIN
        reader.acPending := GetBinaryInt(reader);
        r := InvokeSpecial(reader, TCFromIndex(reader, sc));
        reader.acPending := 0;
      END
    ELSIF repCase = '6' THEN (* Was pickled as a pseudopointer. *) 
      VAR pseudoInt : INTEGER; 
      BEGIN
        LOCK specialsMu DO sp := thePseudoSpecial; END;
        IF sp = NIL THEN (* Unpickle it as a Word.T. *) 
          pseudoInt := PickleStubs.InWord (reader);  
                    (* ^Which will do unsigned integer size and
                        endianness conversions. *) 
          <*ASSERT Word.And (pseudoInt, 1) = 1 *>
          r := LOOPHOLE(pseudoInt, NULL);
        ELSE (* Unpickle it using the special's read method. *) 
          r := sp.read(reader,RefIDNull);
        END; 
      END;
    ELSE
      RAISE Error("Malformed pickle (unknown switch)")
    END;

    IF reader.level = 0 THEN FinishRead (reader); END;
    RETURN r
  END ReadRef;

PROCEDURE StartRead (reader: Reader)
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR hdr: Header;
  BEGIN
    IF reader.refs = NIL THEN
      (* deferred allocation *)
      reader.refs := NEW(RefArray, InitRefCapacity);
      reader.pklToTC := NEW(TypeTable, InitTypeCapacity);
    END;

    IF reader.tipeVisitor = NIL THEN
      reader.tipeVisitor := NEW (TipeReadVisitor, reader := reader);
    END;

    (* (Re-)initialize the reader's state *)
    reader.refCount := 0;
    reader.tcCount := 0;

    IF Rd.GetSub(reader.rd, hdr) # NUMBER(HC) THEN
      RAISE Rd.EndOfFile
    ELSIF (hdr[HC.h1] # v2_header[HC.h1]) OR
            (hdr[HC.h2] # v2_header[HC.h2]) THEN
      RAISE Error("Malformed pickle (wrong signature)")
    END;

    reader.version := hdr[HC.v];
    IF hdr[HC.v] = Version THEN

      reader.packingCode := GetPacking(hdr);
      reader.packing := RTPacking.Decode(reader.packingCode);
      reader.wordConvKind 
        := ConvertPacking.GetWordKind(reader.packing, myPacking);
      reader.longConvKind 
        := ConvertPacking.GetLongintKind(reader.packing, myPacking);
      reader.widecharConvKind 
        := ConvertPacking.GetWidecharKind(reader.packing, myPacking);
      IF reader.packing.float # myPacking.float THEN
        RAISE Error("Can't read pickle (REAL rep)")
      END;

    ELSE

      (* It's an older Pickle, read it with the old routine! *)
      IF reader.visitor = NIL THEN
        reader.visitor := NEW (ReadVisitor, reader := reader);
      END;

      IF (hdr[HC.v] # v1_header[HC.v]) AND
        (hdr[HC.v] # OlderVersion) THEN
        RAISE Error("Can't read pickle (wrong version)")
      ELSIF hdr[HC.p0] # v1_header[HC.p0] THEN
        RAISE Error("Can't read pickle (char rep)")
      ELSIF hdr[HC.p1] # v1_header[HC.p1] THEN
        RAISE Error("Can't read pickle (endian)")
(*
      ELSIF hdr[HC.p2] # v1_header[HC.p2] THEN
        RAISE Error("Can't read pickle (INTEGER size)")
*)
      ELSIF hdr[HC.p3] # v1_header[HC.p3] THEN
        RAISE Error("Can't read pickle (REAL rep)")
      END;
    END;

  END StartRead;

PROCEDURE FinishRead (reader: Reader)
  RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    IF (Rd.GetChar(reader.rd) # Trailer1) OR
       (Rd.GetChar(reader.rd) # Trailer2) THEN
      RAISE Error("Malformed pickle (wrong trailer)")
    END;

    (* flush the ref table to encourage the garbage collector *)
    WITH refs = reader.refs^ DO
      FOR i := 0 TO reader.refCount-1 DO refs[i] := NIL END;
    END;
  END FinishRead;

PROCEDURE ReadType(reader: Reader): TypeCode
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR c: CHAR; ac: INTEGER;
  BEGIN
    IF reader.acPending # 0 THEN
      (* COMPATIBILITY *)
      ac := reader.acPending; reader.acPending := 0;
      RETURN TCFromIndex(reader, ac);
    ELSE
      c := Rd.GetChar(reader.rd);
      IF ORD(c) = 0 THEN
        RETURN ReadFP(reader)
      ELSIF ORD(c) < 255 THEN
        RETURN TCFromIndex(reader, ORD(c));
      ELSE
        RETURN TCFromIndex(reader, reader.readInt());
      END;
    END;
  END ReadType;

PROCEDURE ReadInt(reader: Reader): INTEGER
        RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN GetBinaryInt(reader)
  END ReadInt;

PROCEDURE NoteRef(reader: Reader; ref: REFANY; id: RefID) =
  BEGIN
    WITH rr = reader.refs[id] DO
      <* ASSERT (rr = nullReaderRef) OR (rr = ref) *>
      rr := ref;
    END;
  END NoteRef;

(*--------------------------------------------------------------- specials ---*)

TYPE
  SpecialTable = REF ARRAY OF Special;
    (* indexed by RTType.TypeCode, yields Special for nearest super-type *)

VAR
  specialsMu       := NEW(MUTEX);
  specials         : SpecialTable;            (* LL >= specialsMu *)
  theRootSpecial   : Special;                 (* LL >= specialsMu *)
  thePseudoSpecial : Special;                 (* LL >= specialsMu *)

<* INLINE *>
PROCEDURE GetSpecialLocked (tc: TypeCode): Special =
  (* LL >= specialsMu *)
  BEGIN
    IF (specials = NIL) THEN InitSpecials (); END;
    IF (tc >= NUMBER (specials^)) THEN ExpandSpecials (); END;
    RETURN specials[tc];
  END GetSpecialLocked;

PROCEDURE GetSpecial (tc: TypeCode): Special =
  (* LL = 0 *)
  BEGIN
    LOCK specialsMu DO
      IF (specials = NIL) THEN InitSpecials (); END;
      IF (tc >= NUMBER (specials^)) THEN ExpandSpecials (); END;
      RETURN specials[tc];
    END;
  END GetSpecial;

PROCEDURE InitSpecials() =
  (* LL >= specialsMu *)
  VAR max_type := RTType.MaxTypecode ();
  BEGIN
    theRootSpecial := NEW(Special, sc := RT0.NilTypecode);
    specials := NEW(SpecialTable, max_type+1);
    FOR i := 0 TO LAST(specials^) DO
      specials[i] := theRootSpecial;
    END;
    Thread.Release (specialsMu);
    TRY BuiltinSpecials2.Register ();
    FINALLY Thread.Acquire (specialsMu);
    END;
  END InitSpecials;

PROCEDURE ExpandSpecials() =
  (* LL >= specialsMu *)
  VAR max_type := RTType.MaxTypecode ();
  BEGIN
    IF (max_type > LAST (specials^)) THEN
      VAR new_sp := NEW (SpecialTable, max_type+1);  BEGIN
        SUBARRAY (new_sp^, 0, NUMBER (specials^)) := specials^;
        FOR i := NUMBER (specials^) TO LAST (new_sp^) DO
          new_sp[i] := FindBestSpecial (i);
        END;
	specials := new_sp;
      END;
    END;
  END ExpandSpecials;

PROCEDURE FindBestSpecial (tc: TypeCode): Special =
  (* LL >= specialsMu *)
  VAR sp: Special;
  BEGIN
    LOOP
      tc := RTType.Supertype (tc);
      IF (tc = RTType.NoSuchType) THEN
        RETURN theRootSpecial;
      END;
      IF (0 <= tc) AND (tc < NUMBER (specials^)) THEN
        sp := specials [tc];
        IF (sp # NIL) THEN RETURN sp; END;
      END;
    END;
  END FindBestSpecial;

EXCEPTION DuplicateSpecial;

PROCEDURE RegisterSpecial(sp: Special) =
  <* FATAL DuplicateSpecial *>
  VAR xp: Special;
  BEGIN
    LOCK specialsMu DO
      xp := GetSpecialLocked (sp.sc);
      IF xp.sc = sp.sc THEN RAISE DuplicateSpecial; END;
      FOR i := 0 TO LAST(specials^) DO
        IF (i # RT0.NilTypecode) AND RTType.IsSubtype(i,sp.sc) THEN
          (* i is a sub-type of this special *)
          IF (specials[i].sc = RT0.NilTypecode) OR
                      RTType.IsSubtype(sp.sc, specials[i].sc) THEN
            (* previous special for i isn't more specific than sp.sc *)
            specials[i] := sp;
          END;
        END;
      END;
    END;
  END RegisterSpecial;

PROCEDURE ReRegisterSpecial(sp: Special) =
  VAR xp: Special;
  BEGIN
    LOCK specialsMu DO
      xp := GetSpecialLocked (sp.sc);
      IF xp.sc = sp.sc THEN sp.prev := xp; END;
      FOR i := 0 TO LAST(specials^) DO
        IF (i # RT0.NilTypecode) AND RTType.IsSubtype(i,sp.sc) THEN
          (* i is a sub-type of this special *)
          IF (specials[i].sc = RT0.NilTypecode) OR
                      RTType.IsSubtype(sp.sc, specials[i].sc) THEN
            (* previous special for i isn't more specific than sp.sc *)
            specials[i] := sp;
          END;
        END;
      END;
    END;
  END ReRegisterSpecial;

PROCEDURE RegisterPseudoSpecial(sp: Special) =
  <* FATAL DuplicateSpecial *>
  BEGIN
    LOCK specialsMu DO
      IF thePseudoSpecial = NIL THEN 
         thePseudoSpecial := sp; 
      ELSE RAISE DuplicateSpecial;
      END;
    END;
  END RegisterPseudoSpecial;

PROCEDURE ReRegisterPseudoSpecial(sp: Special) =
  BEGIN
    LOCK specialsMu DO
      IF thePseudoSpecial # NIL THEN 
        sp.prev := thePseudoSpecial;
      END;
      thePseudoSpecial := sp; 
    END;
  END ReRegisterPseudoSpecial;

(* BLAIR ---> *)
PROCEDURE VisitWrite(v: WriteVisitor; field: ADDRESS; kind: RTTypeMap.Kind)
  RAISES ANY =
  (* Call-back from RTType.Visit for RootSpecialWrite *)
  VAR writer := v.writer;
  BEGIN
    (* write data fields preceding the ref *)
    IF field # writer.nextAddr THEN
      Wr.PutString(writer.wr,
                   SUBARRAY(LOOPHOLE(writer.nextAddr, ToChars)^,
                                     0, field - writer.nextAddr));
    END;
    IF kind = RTTypeMap.Kind.Ref THEN
      writer.write(LOOPHOLE(field, UNTRACED REF REFANY)^);
    ELSE
      (* Other REF fields, including procedures, are discarded on write *)
    END;
    writer.nextAddr := field + ADRSIZE(ADDRESS);
  END VisitWrite;

PROCEDURE TipeWriteData(v: TipeWriteVisitor; VAR data: ARRAY OF CHAR) 
      RAISES { Wr.Failure, Thread.Alerted } =
  VAR writer := v.writer;
  BEGIN
    Wr.PutString(writer.wr, data);
  END TipeWriteData;

PROCEDURE TipeSkipWriteData(v: TipeWriteVisitor; length: INTEGER) 
      RAISES { Wr.Failure, Thread.Alerted } =
  VAR writer := v.writer;
  VAR t := ARRAY [0..7] OF CHAR{'D', 'E', 'A', 'D', 'B', 'E', 'E', 'F'};
  BEGIN
    (* This should not be called with a length >= 8, so we'll optimize
       for length < 8.  We'll still handle the arbitrary case,
       although slightly slower. *)
    WHILE length > 0 DO
      IF length < 8 THEN
        Wr.PutString(writer.wr, SUBARRAY(t, 0, length));
        RETURN;
      END;
      Wr.PutString(writer.wr, t);
      DEC(length, 8);
    END;
  END TipeSkipWriteData;

PROCEDURE TipeWriteRef(v: TipeWriteVisitor;
                      refType: ConvertPacking.RefType; ref: REFANY)
      RAISES { ConvertPacking.Error, Wr.Failure, Thread.Alerted } =
  VAR writer := v.writer;
  BEGIN
    TRY
      IF refType = ConvertPacking.RefType.Ref THEN
        writer.write(ref);
      END;
    EXCEPT
    | Error(t) => RAISE ConvertPacking.Error("Pickle.Error: " & t);
    END;
  END TipeWriteRef;

PROCEDURE RootSpecialWrite(<*UNUSED*> sp: Special;
                           r: REFANY; writer: Writer)
        RAISES { Error <*NOWARN*>, Wr.Failure, Thread.Alerted } =
    VAR nDim: INTEGER;
    VAR shape: UNTRACED REF ARRAY [0..999] OF INTEGER;
(*    VAR limit: ADDRESS;
    VAR start: ADDRESS; *)
    VAR tmp: ARRAY [0..1] OF INTEGER;
  BEGIN
    writer.writeType(TYPECODE(r));
    RTHeapRep.UnsafeGetShape (r, nDim, shape);
    FOR i := 0 TO nDim-1 DO
      writer.writeInt(shape[i]);
    END;

    (*
    writer.nextAddr := RTHeap.GetDataAdr(r);
    start := writer.firstAddr;
    writer.firstAddr := writer.nextAddr;
    limit := writer.nextAddr + RTHeap.GetDataSize(r);
    <*FATAL ANY*> BEGIN
      RTTypeMap.WalkRef(r, RefFields, writer.visitor);
    END;
    *)
    (* Write remainder of the data fields *)
    (*
    IF limit # writer.nextAddr THEN
      Wr.PutString(writer.wr,
                   SUBARRAY(LOOPHOLE(writer.nextAddr, ToChars)^,
                                      0, limit-writer.nextAddr));
    END;
    writer.firstAddr := start;
    *)

    (* Use the RTTipe functions to fill in the data. *)
    TRY
      IF (nDim > 0) THEN
        PklTipeMap.Write(writer.tipeVisitor, r, TYPECODE(r), myPacking, 
                         SUBARRAY(shape^, 0, nDim), nDim); 
      ELSE
        PklTipeMap.Write(writer.tipeVisitor, r, TYPECODE(r), myPacking, 
                      tmp, 0);
      END;
    EXCEPT
    | PklTipeMap.Error(t) => RAISE Error("PklTipeMap.Write Error: " & t);
    END;
  END RootSpecialWrite;
(* <--- BLAIR *)

PROCEDURE VisitRead(v: ReadVisitor; field: ADDRESS; kind: RTTypeMap.Kind)
  RAISES ANY =
  (* Call-back from RTType.Visit for RootSpecialRead *)
  VAR reader := v.reader;
  BEGIN
    (* read data fields preceding the ref *)
    EVAL Rd.GetSub(reader.rd,
                   SUBARRAY(LOOPHOLE(reader.nextAddr, ToChars)^,
                                     0, field - reader.nextAddr));
    IF kind = RTTypeMap.Kind.Ref THEN
      LOOPHOLE(field, UNTRACED REF REFANY)^ := reader.read();
    ELSE
      (* Other REF fields, including procedures, are discarded on
         write *)
      LOOPHOLE(field, UNTRACED REF REFANY)^ := NIL;
    END;
    reader.nextAddr := field + ADRSIZE(ADDRESS);
  END VisitRead;

(* BLAIR ---> *)
PROCEDURE TipeReadData(v: TipeReadVisitor; VAR data: ARRAY OF CHAR) 
      RAISES { Rd.Failure, Thread.Alerted } =
  VAR reader := v.reader;
  BEGIN
    EVAL Rd.GetSub(reader.rd, data);
  END TipeReadData;

PROCEDURE TipeSkipReadData(v: TipeReadVisitor; length: INTEGER) 
      RAISES { Rd.Failure, Thread.Alerted } =
  VAR reader := v.reader;
  VAR t: ARRAY [0..7] OF CHAR;
  BEGIN
    (* This should not be called with a length >= 8, so we'll optimize
       for length < 8.  We'll still handle the arbitrary case,
       although slightly slower. *)
    WHILE length > 0 DO
      IF length < 8 THEN
        EVAL Rd.GetSub(reader.rd, SUBARRAY(t, 0, length));
        RETURN;
      END;
      EVAL Rd.GetSub(reader.rd, t);
      DEC(length, 8);
    END;
  END TipeSkipReadData;

PROCEDURE TipeReadRef(v: TipeReadVisitor; 
                      refType: ConvertPacking.RefType): REFANY
      RAISES { ConvertPacking.Error, Rd.EndOfFile, Rd.Failure, 
               Thread.Alerted } =
  VAR reader := v.reader;
  BEGIN
    TRY
      IF refType = ConvertPacking.RefType.Ref THEN
        RETURN ReadRef(reader);
      ELSE
        RETURN NIL;
      END;
    EXCEPT
    | Error(t) => RAISE ConvertPacking.Error("Pickle.Error: " & t);
    END;
  END TipeReadRef; 
(* <--- BLAIR *)

PROCEDURE TipeReadChar(v: TipeReadVisitor): CHAR 
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} = 

  BEGIN 
    RETURN Rd.GetChar(v.reader.rd); 
  END TipeReadChar; 

PROCEDURE TipeGetReader(v: TipeReadVisitor):Reader = 
  BEGIN 
    RETURN v.reader; 
  END TipeGetReader; 

PROCEDURE TipeWriteChar(v: TipeWriteVisitor; value: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} = 

  BEGIN 
    Wr.PutChar(v.writer.wr, value); 
  END TipeWriteChar; 

PROCEDURE TipeGetWriter(v: TipeWriteVisitor):Writer = 
  BEGIN 
    RETURN v.writer; 
  END TipeGetWriter; 

PROCEDURE RootSpecialRead(<*UNUSED*> sp: Special;
                          reader: Reader; id: RefID): REFANY
    RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR nDim: INTEGER;
    VAR shape: ARRAY [0..10] OF INTEGER;
    VAR limit: ADDRESS;
    VAR r: REFANY;
    VAR ac := reader.readType();
  BEGIN
    TRY
      nDim := RTType.GetNDimensions(ac);
      IF nDim > 0 THEN
        FOR i := 0 TO nDim-1 DO
          shape[i] := reader.readInt();
        END;
        r := RTAllocator.NewTracedArray(ac, SUBARRAY(shape, 0, nDim));
      ELSE
        r := RTAllocator.NewTraced(ac);
      END;
    EXCEPT RTAllocator.OutOfMemory =>
      RAISE Error("Can't read pickle (out of memory)")
    END;
    reader.noteRef(r, id);

    IF reader.version # Version THEN
      (* COMPATIBILITY: read old style pickles. *)
      reader.nextAddr := RTHeap.GetDataAdr(r);
      limit := reader.nextAddr + RTHeap.GetDataSize(r);
      <*FATAL ANY*> BEGIN
        RTTypeMap.WalkRef(r, RefFields, reader.visitor);
      END;
      IF limit > reader.nextAddr THEN
        (* Read remainder of the data fields *)
        EVAL Rd.GetSub(reader.rd,
                     SUBARRAY(LOOPHOLE(reader.nextAddr, ToChars)^,
                              0, limit-reader.nextAddr));
      END;
    ELSE
      (* Use the RTTipe functions to fill in the data. *)
      TRY
        PklTipeMap.Read(reader.tipeVisitor, r, ac, reader.packing, myPacking, 
                     SUBARRAY(shape, 0, nDim)); 
      EXCEPT
      | PklTipeMap.Error(t) => RAISE Error("PklTipeMap.Error: " & t);
      END;
    END;

    RETURN r;
  END RootSpecialRead;

(*---------------------------------------------------------- initialization ---*)

(* BLAIR ---> We are assuming that RTPacking.T's are encoded in the
   first 32 bits of INTEGER on any machine.  I think that's valid,
   since it should fit in an INTEGER on a 32bit machine! *)
PROCEDURE GetPacking (header: Header): INTEGER =
  VAR a, b, c, d: INTEGER;
      ret: INTEGER;
  BEGIN
    a := ORD(header[HC.p0]);
    b := ORD(header[HC.p1]);
    c := ORD(header[HC.p2]);
    d := ORD(header[HC.p3]);
    IF d > 127 THEN
      ret := Word.Not(0);
    ELSE
      ret := 0;
    END;
    RETURN Word.Insert (ret, Word.Or(
             Word.Or (Word.LeftShift (a,  0), Word.LeftShift (b, 8)),
             Word.Or (Word.LeftShift (c, 16), Word.LeftShift (d, 24))),
             0, 32);
  END GetPacking;

PROCEDURE StorePackingInHeader 
  (VAR header: Header; write16BitWidechar: BOOLEAN) =
  VAR Packing: RTPacking.T;
  VAR PackingCode: INTEGER; 
  BEGIN
    Packing := RTPacking.Local();
    IF write16BitWidechar THEN Packing.widechar_size := 16; END;
    PackingCode := RTPacking.Encode(Packing);
    header[HC.p0] := VAL(Word.Extract(PackingCode, 0, 8), CHAR);
    header[HC.p1] := VAL(Word.Extract(PackingCode, 8, 8), CHAR);
    header[HC.p2] := VAL(Word.Extract(PackingCode, 16, 8), CHAR);
    header[HC.p3] := VAL(Word.Extract(PackingCode, 24, 8), CHAR);
  END StorePackingInHeader;

PROCEDURE InitHeader() =
    VAR test: BITS 16 FOR [0..32767];
    TYPE EndianTest = ARRAY[0..1] OF BITS 8 FOR [0..255];
  BEGIN
    (* BLAIR ---> *)
    v1_header[HC.h1] := Header1;
    v1_header[HC.h2] := Header2;
    v1_header[HC.v] := OldVersion;
    v1_header[HC.p0] := CharRepISOLatin1;
    test := 1;
    IF LOOPHOLE(test, EndianTest)[0] = 1
      THEN v1_header[HC.p1] := LittleEndian
      ELSE v1_header[HC.p1] := BigEndian;
    END;
    IF    BITSIZE(INTEGER) = 16 THEN  v1_header[HC.p2] := IntSize16
    ELSIF BITSIZE(INTEGER) = 32 THEN  v1_header[HC.p2] := IntSize32
    ELSIF BITSIZE(INTEGER) = 64 THEN  v1_header[HC.p2] := IntSize64
    ELSE                              v1_header[HC.p2] := '?'
    END;
    v1_header[HC.p3] := RealRepNative;

    v2_header[HC.h1] := Header1;
    v2_header[HC.h2] := Header2;
    v2_header[HC.v] := Version;
    
    myPacking := RTPacking.Local();
    myPackingCode := RTPacking.Encode(myPacking);
    (* <--- BLAIR *)
    myTrailer[HT.t1] := Trailer1;
    myTrailer[HT.t2] := Trailer2;
  END InitHeader;

(* Adaptation to some builtin fingerprints that have different byte order in
   Pm3 and Cm3.
*) 

BEGIN (* Pickle2 *) 

  InitHeader();
  nullReaderRef := NEW(REF INTEGER);
  
END Pickle2.
