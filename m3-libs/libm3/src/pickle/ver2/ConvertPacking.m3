(*
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *)

UNSAFE MODULE ConvertPacking;

IMPORT RTTipe, RTPacking, PklAction, PklActionSeq, PickleStubs, 
       Thread, Wr, Rd, Swap, Word,
       IO, Fmt, PackingTbl, PackingTypeCode;
FROM Word IMPORT And, Or, LeftShift, RightShift; 

CONST WordBitsize = BITSIZE(Word.T); 

VAR packingCache: PackingTbl.T := NEW(PackingTbl.Default).init();

REVEAL ReadVisitor = RVPublic BRANDED "Packing Read Visitor 1.0" OBJECT END;
REVEAL WriteVisitor = WVPublic BRANDED "Packing Write Visitor 1.0" OBJECT END;

REVEAL T = Public BRANDED "ConvertPacking 1.0" OBJECT
      prog: PklActionSeq.T;
      wordKind: CPKind;
      longKind: CPKind;
      widecharKind: CPKind;
      fromOffset: INTEGER := 0;
      toOffset: INTEGER := 0;
      fromSize: INTEGER := 0;
      toSize: INTEGER := 0;
      fromTipe: RTTipe.T;
      toTipe : RTTipe.T;
      from: RTPacking.T;
      to: RTPacking.T;
      nDim, fromEltPack, toEltPack: INTEGER := 0;
      writeConv: T;
    METHODS
      extractSwap (x: Word.T; i, n: CARDINAL; 
                   size: INTEGER): Word.T := ExtractSwap;

      buildOne(fromTipe: RTTipe.T; toTipe: RTTipe.T) RAISES {Error} := 
        BuildOne; 

      addCopy(bitCt: INTEGER) := AddCopy;
      addCopy32to64(bitCt: INTEGER; signed: BOOLEAN) := AddCopy32to64;
      (* ^PRE: bitCt MOD 32 = 0 *) 
      addCopy64to32(bitCt: INTEGER; signed: BOOLEAN) := AddCopy64to32;
      (* ^PRE: bitCt MOD 64 = 0 *) 
      addCopy16to32(fromBitCt: INTEGER) := AddCopy16to32;
      (* ^PRE: fromBitCt MOD 16 = 0 *) 
      addCopy32to16(fromBitCt: INTEGER) := AddCopy32to16;
      (* ^PRE: fromBitCt MOD 32 = 0 *) 
      addCopyWC21to32(toBitCt: INTEGER) := AddCopyWC21to32;
      (* ^PRE: ToBitCt MOD 32 = 0 *) 
      addCopyWC21to16(toBitCt: INTEGER) := AddCopyWC21to16;
      (* ^PRE: ToBitCt MOD 16 = 0 *) 

      addPackedFirstField
        (fieldBitSize: INTEGER; 
         shortenWidechar: BOOLEAN; paKind:PklAction.PAKind) 
        := AddPackedFirstField;
      addPackedNextField
        (fieldBitSize: INTEGER; offset: INTEGER; 
          shortenWidechar: BOOLEAN; paKind:PklAction.PAKind) 
        := AddPackedNextField;
      addPackedArray
        (bitCt, numElts, fieldBitSize, packingWordBitSize: INTEGER;
         shortenWidechar: BOOLEAN; paKind:PklAction.PAKind) 
        := AddPackedArray; 

      addSkipFrom(bitCt: INTEGER) := AddSkipFrom;
      addSkipTo(bitCt: INTEGER) := AddSkipTo;
      addSkipOrCopy(bitCt: INTEGER) := AddSkipOrCopy;
      addSkip(fromDiff, toDiff: INTEGER) := AddSkip;
      addSwap16(bitCt: INTEGER) := AddSwap16;
      (* ^PRE: bitCt MOD 16 = 0 *) 
      addSwap32(bitCt: INTEGER) := AddSwap32;
      (* ^PRE: bitCt MOD 32 = 0 *) 
      addSwap64(bitCt: INTEGER) := AddSwap64;
      (* ^PRE: bitCt MOD 64 = 0 *) 
      addSwap32to64(bitCt: INTEGER; signed: BOOLEAN) := AddSwap32to64;
      (* ^PRE: bitCt MOD 32 = 0 *) 
      addSwap64to32(bitCt: INTEGER; signed: BOOLEAN) := AddSwap64to32;
      (* ^PRE: bitCt MOD 64 = 0 *) 
      addSwap16to32(fromBitCt: INTEGER) := AddSwap16to32;
      (* ^PRE: fromBitCt MOD 16 = 0 *) 
      addSwap32to16(fromBitCt: INTEGER) := AddSwap32to16;
      (* ^PRE: fromBitCt MOD 32 = 0 *) 
      addRef(type: RefType) := AddRef;
      addDone() := AddDone;

      buildSuper(typecode: INTEGER; 
                 VAR fromSize, fromAlign, toSize, toAlign: INTEGER) 
        RAISES {Error} := BuildSuper;
      buildFields(fromField: RTTipe.Field; fromSize: INTEGER;
                  toField: RTTipe.Field; toSize: INTEGER) 
        RAISES {Error} := BuildFields;

      appendProg(other: T) RAISES {Error} := AppendProg;
    OVERRIDES
      init := Init;
      convertRead := Convert;
      write := Write;
      print := Print;
      printProgram := PrintProgram;
      getDim := GetDim;
    END;

CONST SignExt32 = ARRAY [0..1] OF Swap.Int32 {0, -1};

TYPE
  UInt8 = BITS 8 FOR [0 .. 16_FF];

(* Similar to Word.Extract, extract a field of n bits, starting with bit
   number i, and return it right-justified in a word.  Differences are:
   1) Treat the bytes of x as if they were in little-endian order, i.e., byte
      0 is the least significant, regardless of the endianness of the 
      executing machine. 
   2) Extract only from the size least-significant bytes of x, which implies
      a checked runtime error if n+i >= 8*size.
   Like Word.Extract, interpret i as the number of the least-significant bit,
   in little-endian (i.e., right-to-left) bit ordering, within the size bytes.
*) 

PROCEDURE ExtractSwapLE (x: Word.T; i, n: CARDINAL; size: INTEGER): Word.T =

  VAR fromBytes := LOOPHOLE(x, ARRAY [0..BYTESIZE(Word.T)-1] OF UInt8);
      to: Word.T := 0;
      resBitNo: INTEGER := 0; (* In little-endian numbering. *) 
      fromByteNo: INTEGER; (* In little-endian numbering. *) 
      loBitNoInByte: CARDINAL; (* Low numbered, in little-endian numbering. *) 
      value: Word.T; 

  BEGIN
    IF n = 0 THEN RETURN 0; END; 
    loBitNoInByte := i MOD 8;
    fromByteNo := i DIV 8;
    resBitNo := 0; 
    (* Handle byte fragments of field least to most significant. *) 
    LOOP
      <* ASSERT fromByteNo < size *> 
      WITH len = MIN(n, 8 - loBitNoInByte) DO
        (* Word.Extract and Word.Insert always use LE bit numbering. *) 
        value := Word.Extract(fromBytes[fromByteNo], loBitNoInByte, len); 
        to := Word.Insert (to, value, resBitNo, len);
        DEC(n, len);
        IF n = 0 THEN RETURN to; END;
        INC(resBitNo, len);
        loBitNoInByte := 0;
        INC(fromByteNo); 
      END(*WITH*);
    END(*LOOP*);
  END ExtractSwapLE;

(* Similar to Word.Extract, extract a field of n bits, starting with bit
   number i, and return it right-justified in a word.  Differences are:
   1) Treat the bytes of x as if they were in big-endian order, i.e., byte
      0 is the most significant, regardless of the endianness of the 
      executing machine. 
   2) Extract only from the size most-significant bytes of x, which implies
      a checked runtime error if n+i >= 8*size.
   Unlike Word.Extract, interpret i as the number of the most-significant bit,
   in big-endian (i.e., left-to-right) bit ordering, within the size bytes.
*) 

PROCEDURE ExtractSwapBE (x: Word.T; i, n: CARDINAL; size: INTEGER): Word.T =

  VAR fromBytes := LOOPHOLE(x, ARRAY [0..BYTESIZE(Word.T)-1] OF UInt8);
      to: Word.T := 0;
      resBitNo: INTEGER := 0; (* Low numbered, in big-endian numbering. *) 
      fromByteNo: INTEGER; (* In little-endian numbering. *) 
      loBitNoInByte: CARDINAL; (* Low numbered, in big-endian numbering. *) 
      value: Word.T; 

  BEGIN
    IF n = 0 THEN RETURN 0; END; 
    loBitNoInByte := i MOD 8;
    fromByteNo := i DIV 8; 
    resBitNo := n;
    (* Handle byte fragments of field most to least significant. *) 
    LOOP
      <* ASSERT fromByteNo < size *> 
      WITH len = MIN(n, 8 - loBitNoInByte) DO
        DEC(resBitNo, len);
        (* Word.Extract and Word.Insert always use LE bit numbering. *) 
        value := Word.Extract (fromBytes[fromByteNo], 8-loBitNoInByte-len, len);
        to := Word.Insert (to, value, resBitNo, len);
        DEC(n, len);
        IF n = 0 THEN RETURN to; END;
        loBitNoInByte := 0;
        INC(fromByteNo); 
      END(*WITH*);
    END(*LOOP*);
  END ExtractSwapBE;

(* Based on Word.Extract, but extracts from the size lowest numbered bytes of x
   (as numbered on the from-endian sending machine), which has the 
   opposite endian to this machine.  Also, treats the starting bit number i
   as the lowest numbered bit in the (opposite) endian-bit-ordering of the 
   from machine.  Thus, i is the msb# if written on a big-endian machine, 
   otherwise, it's the lsb#. 

   Take n bits from x, with bit i as the lowest numbered bit, and return them
   as the least significant n bits of a Word.T whose other bits are 0. A checked
   runtime error if n + i > WordBitsize.  Treat bytes of x numbered >= size, in 
   executing-endian numbering as containing zeros. *) 
PROCEDURE ExtractSwap (self: T; x: Word.T; i, n: CARDINAL; 
                       size: INTEGER): Word.T =

  VAR fromBytes := LOOPHOLE(x, ARRAY [0..BYTESIZE(Word.T)-1] OF UInt8);
      to: Word.T := 0;
      bit: INTEGER := 0; (* Bit# within "to", in from-endian. *) 
      loBitNoInByte: CARDINAL;

  BEGIN
    IF n > 0 THEN 
      loBitNoInByte := i MOD 8;
      IF self.from.little_endian THEN
        FOR b := i DIV 8 TO size-1 DO
(* TODO: Assuming valid i and n, this FOR loop can just be a LOOP. *) 
          (* If we want some data from this *)
          WITH len = MIN(n, 8 - loBitNoInByte) DO
            (* Get the appropriate part of the byte *)

              to := Word.Insert
                      (to, Word.Extract(fromBytes[b], loBitNoInByte, len), bit, len);
              INC(bit, len);
            (* set loBitNoInByte to 0 after the first pass. *)
            loBitNoInByte := 0;

            (* keep track of how much we have left to get *)
            DEC(n, len);
            IF n = 0 THEN
              RETURN to;
            END;
          END;
        END;
      ELSE (* From machine was big-endian. *) 
        bit := n;
        FOR b := i DIV 8 TO size-1 DO
(* TODO: Assuming valid i and n, this FOR loop can just be a LOOP. *) 
          (* If we want some data from this *)
          WITH len = MIN(n, 8 - loBitNoInByte) DO
            (* Get the appropriate part of the byte *)
              DEC(bit, len);
              to 
                := Word.Insert
                    (to, Word.Extract(fromBytes[b], 8-loBitNoInByte-len, len), bit, len);
            (* set loBitNoInByte to 0 after the first pass. *)
            loBitNoInByte := 0;

            (* keep track of how much we have left to get *)
            DEC(n, len);
            IF n = 0 THEN
              RETURN to;
            END;
          END;
        END;
      END; 
    END; 
    RETURN to;
  END ExtractSwap;

CONST MAXLEN = 65536;
TYPE  BigBuf = ARRAY [0..MAXLEN-1] OF CHAR;
TYPE  BufPtr = UNTRACED REF BigBuf;

PROCEDURE ReadData(v: ReadVisitor;  dest: ADDRESS; len: INTEGER) 
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    WHILE len >= MAXLEN DO
      v.readData(LOOPHOLE(dest, BufPtr)^);
      INC(dest, MAXLEN);  DEC(len, MAXLEN);
    END;
    IF len > 0 THEN
      v.readData (SUBARRAY(LOOPHOLE (dest, BufPtr)^, 0, len));
    END;
  END ReadData;

TYPE Int32Rec = BITS 32 FOR RECORD v : Swap.Int32 END; 
(* We need v to be inside a record.  Otherwise, the language would allow
   a compiler to actually allocate more than the BITS 32 for a value of
   type Swap.Int32.
*) 
VAR Int32RecVar : Int32Rec; 
<*UNUSED*>
VAR CheckInt32 : [ 0 .. 0 ] := ADR(Int32RecVar) - ADR(Int32RecVar.v);

TYPE U16Rec = BITS 16 FOR RECORD v : Swap.UInt16 END; 
     (* Similarly to Int32Rec. *) 
VAR U16RecVar : U16Rec; 
<*UNUSED*>
VAR CheckU16 : [ 0 .. 0 ] := ADR(U16RecVar) - ADR(U16RecVar.v);

TYPE CharArrOverU16 = ARRAY [0..1] OF CHAR;
TYPE CharArrOverWord = ARRAY [0..BYTESIZE(Word.T)-1] OF CHAR;  

TYPE Int32onU16 = RECORD a, b: Swap.UInt16 END; 

PROCEDURE IsWidechar (Type: RTTipe.T): BOOLEAN =
  BEGIN 
    IF Type = NIL THEN RETURN FALSE; END; 
    CASE Type.kind 
    OF RTTipe.Kind.Enum => 
      WITH WEnum = NARROW(Type, RTTipe.Enum) DO 
        RETURN WEnum.n_elts = 16_10000 OR WEnum.n_elts = 16_110000 
(* FIXME: It would be quite difficult for a programmer to define an enumeration
         type with either of these numbers of elements, thus spoofing WIDECHAR
         here.  In case she did, BuildWidechar further checks that the size 
         has changed if the CPKind calls for WIDECHAR to do that.  Otherwise,  
         the consequence would be that the enumeration would be pickled in WC21.
         So, find a surer criterion than this.  RTTipe doesn't 
         appear to have sufficient information currently. *) 
(* Widechar Tipe. *) 
      END;
    | RTTipe.Kind.Packed =>
      WITH WPacked = NARROW(Type, RTTipe.Packed) DO 
        RETURN IsWidechar (WPacked.base); 
      END; 
    ELSE RETURN FALSE; 
    END; 
  END IsWidechar; 

PROCEDURE ReadWC21(rd: Rd.T): UInt32 
RAISES{Rd.EndOfFile, Rd.Failure, Thread.Alerted} = 
(* Read one WIDECHAR value in WC21 encoding and return in a 32-bit int. *) 

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
  END ReadWC21; 

PROCEDURE Convert
   (self: T; dest: ADDRESS; v: ReadVisitor; 
    progRepCt: INTEGER := 1 (* Repeat the program this many times. *) )
    : ADDRESS 
     RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR t: ARRAY [0..7] OF CHAR;
      insnRepCt: INTEGER := 1; (* Repeat each instruction this many times. *) 
      extract: Word.T;

  BEGIN
    IF self.prog.size() = 2 THEN
      (* We have a one step program (the step plus the DONE step), so
         lets just do it in one shot. *)
      insnRepCt := progRepCt;
      progRepCt := 1;
    END;
    (* Execute the program "progRepCt" times. *) 
    FOR n := 1 TO progRepCt DO 
      FOR i := 0 TO self.prog.size() - 1 DO
        WITH elem = self.prog.get(i), 
             insnUnitCt = elem.unitCt * insnRepCt DO
             (*  Repeat each instruction "insnRepCt" times. *)
          CASE elem.kind OF
          | PklAction.PAKind.Copy => 
            ReadData(v, dest, insnUnitCt);
            INC(dest, insnUnitCt);

          | PklAction.PAKind.CopyPackedLE => 
            (* Copy the packed fields of a contiguous sequence of fields, with 
               possibly a trailing pad field, that begins and ends on byte 
               boundaries.  Possibly narrow WIDECHAR fields. *)
            (* Was written and reading on little-endian. *) 
            WITH nelem = NARROW(elem, PklAction.Packed) DO

              VAR from: Word.T := 0;
                  to: Word.T := 0;
                  lsBitNoLE: INTEGER := 0; (* lsb# of field, little-endian. *)
                  fieldBitCt: INTEGER; 
                  fromBytesPtr 
                    := LOOPHOLE(ADR(from), UNTRACED REF CharArrOverWord);
              BEGIN
                FOR i := 1 TO insnUnitCt DO 
                  (* insnUnitCt chunks of nelem.size bytes each *)
                  v.readData(SUBARRAY(fromBytesPtr^, 0, nelem.size));

                  (** IO.Put
                        ( "CopyPacked, LE, extracting from: 2_" 
                           & Fmt.Pad(Fmt.Unsigned(from, 2), nelem.size*8, '0')
                           & "\n"); **)


                  lsBitNoLE := 0; 
                  FOR j := FIRST(nelem.field^) TO LAST(nelem.field^) DO
                    fieldBitCt := nelem.field^[j]; 
                    extract := Word.Extract (from, lsBitNoLE, fieldBitCt); 

                    (** IO.Put
                          ( " Extracted field: 2_" 
                            & Fmt.Pad
                                (Fmt.Unsigned(extract, 2), fieldBitCt, '0') 
                            & "\n"); **)

                    IF j IN nelem.widecharFieldSet THEN
                      IF extract > 16_FFFF THEN 
                         extract := PickleStubs . ReplacementWt  
                     (** IO.Put 
                           ( "WIDECHAR narrowed to: 2_" 
                            & Fmt.Pad
                                (Fmt.Unsigned(extract, 2), fieldBitCt, '0') 
                            & "\n"); **)
                      END; 
                    END; 

                    to := Word.Insert (to, extract, lsBitNoLE, fieldBitCt);

                    (** IO.Put
                          ( " Into result word: 2_" 
                            & Fmt.Pad(Fmt.Unsigned(to, 2), nelem.size*8, '0') 
                            & "\n"); **)

                    INC(lsBitNoLE, fieldBitCt);
                  END (*FOR fields.*) ;
                  (* Copy the bytes to destination. *)
                  SUBARRAY(LOOPHOLE(dest, BufPtr)^, 0, nelem.size) :=
                    SUBARRAY(LOOPHOLE(ADR(to), BufPtr)^, 0, nelem.size);
                  INC(dest, nelem.size);
                END(*FOR*);
              END (*Block*);
            END (*WITH*);

          | PklAction.PAKind.CopyPackedBE => 
            (* Copy the packed fields of a contiguous sequence of fields, with 
               possibly a trailing pad field, that begins and ends on byte 
               boundaries.  Possible narrow WIDECHAR fields. *)
            (* Was written and reading on big-endian. *) 
            WITH nelem = NARROW(elem, PklAction.Packed) DO

              VAR from: Word.T := 0;
                  to: Word.T := 0;
                  msBitNoBE: INTEGER := 0; (* msb# of field, big-endian. *) 
                  lsBitNoLE: INTEGER := 0; (* lsb# of field, little-endian. *)
                  fieldBitCt: INTEGER; 
                  fromBytesPtr 
                    := LOOPHOLE(ADR(from), UNTRACED REF CharArrOverWord);
              BEGIN
                FOR i := 1 TO insnUnitCt DO 
                  (* insnUnitCt chunks of nelem.size bytes each *)
                  v.readData(SUBARRAY(fromBytesPtr^, 0, nelem.size));

                  (** IO.Put
                        ( "CopyPacked, BE, extracting from: 2_" 
                          & Fmt.Pad
                              (Fmt.Unsigned
                                 (Word.Rightshift
                                    (from, WordBitsize-nelem.size*8), 
                               2), 
                               nelem.size*8, '0') 
                          & "\n"); **)

                  msBitNoBE := 0; 
                  FOR j := FIRST(nelem.field^) TO LAST(nelem.field^) DO
                    fieldBitCt := nelem.field^[j]; 
                    lsBitNoLE := WordBitsize-msBitNoBE-fieldBitCt;
                    extract := Word.Extract (from, lsBitNoLE, fieldBitCt); 

                    (** IO.Put
                         (" Extracted field: 2_" 
                          & Fmt.Pad(Fmt.Unsigned(extract, 2), fieldBitCt, '0')
                          & "\n"); **)

                    IF j IN nelem.widecharFieldSet THEN
                      IF extract > 16_FFFF THEN 
                         extract := PickleStubs . ReplacementWt  
                     (** IO.Put 
                           ( "WIDECHAR narrowed to: 2_" 
                            & Fmt.Pad
                                (Fmt.Unsigned(extract, 2), fieldBitCt, '0') 
                            & "\n"); **)
                      END; 
                    END; 
                    to := Word.Insert (to, extract, lsBitNoLE, fieldBitCt);

                    (** IO.Put
                          ( " Into result word: 2_" &
                            & Fmt.Pad(Fmt.Unsigned(to, 2), nelem.size*8, '0')
                            & "\n"); **)

                    INC(msBitNoBE, fieldBitCt);
                  END (*FOR fields.*);
                  (* Copy the bytes to destination. *)
                  SUBARRAY(LOOPHOLE(dest, BufPtr)^, 0, nelem.size) :=
                    SUBARRAY(LOOPHOLE(ADR(to), BufPtr)^, 0, nelem.size);
                  INC(dest, nelem.size);
                END(*FOR*);
              END(*Block*);
            END (*WITH*);

          | PklAction.PAKind.SwapPackedLEtoBE => 
            (* Copy the packed fields of a contiguous sequence of fields, with 
               possibly a trailing pad field, that begins and ends on byte 
               boundaries.  Possible narrow WIDECHAR fields. *)
            (* Was written on little-endian; executing on big-endian. *) 
            WITH nelem = NARROW(elem, PklAction.Packed) DO

              VAR from: Word.T := 0;
                  to: Word.T := 0;
                  bitNo: INTEGER := 0; (* of packed field. *) 
                  fieldBitCt: INTEGER; 
                  fromBytesPtr 
                    := LOOPHOLE(ADR(from), UNTRACED REF CharArrOverWord);
              BEGIN
                FOR i := 1 TO insnUnitCt DO 
                  (* insnUnitCt chunks of nelem.size bytes each *)
                  v.readData(SUBARRAY(fromBytesPtr^, 0, nelem.size));

                  (** IO.Put
                        ( "SwapPacked, from LE, extracting from: 2_" 
                           & Fmt.Pad
                               (Fmt.Unsigned
                                  (Word.Rightshift
                                     (from, WordBitsize-nelem.size*8), 
                                2), 
                                nelem.size*8, '0')
                           & "\n"); **)

                  bitNo := 0; 
                  FOR j := FIRST(nelem.field^) TO LAST(nelem.field^) DO
                    fieldBitCt := nelem.field^[j]; 
                    extract 
                      := ExtractSwapLE (from, bitNo, fieldBitCt, nelem.size); 
                         (* ^bitNo is LE lsb, on writing system and to
                              ExtractSwapLE. *) 

                    (** IO.Put
                          ( " Extracted field: 2_" 
                            & Fmt.Pad
                                (Fmt.Unsigned(extract, 2), fieldBitCt, '0') 
                            & "\n"); **)

                    IF j IN nelem.widecharFieldSet THEN
                      IF extract > 16_FFFF THEN 
                         extract := PickleStubs . ReplacementWt  
                     (** IO.Put 
                           ( "WIDECHAR narrowed to: 2_" 
                            & Fmt.Pad
                                (Fmt.Unsigned(extract, 2), fieldBitCt, '0') 
                            & "\n"); **)
                      END; 
                    END; 

                    to := Word.Insert 
                            (to, extract, WordBitsize-bitNo-fieldBitCt, 
                            (* bitNo means BE, msb on the to- system, but 
                               Insert takes a LE, lsb, so we convert it. *) 
                             fieldBitCt);

                    (** IO.Put
                          ( " Into result word: 2_" 
                            & Fmt.Pad
                                (Fmt.Unsigned
                                   (Word.Rightshift
                                      (to, WordBitsize-nelem.size*8), 
                                 2), 
                                 nelem.size*8, '0')
                            & "\n"); **)

                    INC(bitNo, fieldBitCt);
                  END (*FOR fields.*) ;
                  (* Copy the bytes to destination. *)
                  SUBARRAY(LOOPHOLE(dest, BufPtr)^, 0, nelem.size) :=
                    SUBARRAY(LOOPHOLE(ADR(to), BufPtr)^, 0, nelem.size);
                  INC(dest, nelem.size);
                END(*FOR*);
              END (*Block*);
            END (*WITH*);

          | PklAction.PAKind.SwapPackedBEtoLE => 
            (* Copy the packed fields of a contiguous sequence of fields, with 
               possibly a trailing pad field, that begins and ends on byte 
               boundaries.  Possibly narrow WIDECHAR fields. *)
            (* Was written on big-endian; executing on little-endian. *) 
            WITH nelem = NARROW(elem, PklAction.Packed) DO

              VAR from: Word.T := 0;
                  to: Word.T := 0;
                  bitNo: INTEGER := 0; (* of packed field. *)  
                  fieldBitCt: INTEGER; 
                  fromBytesPtr 
                    := LOOPHOLE(ADR(from), UNTRACED REF CharArrOverWord);
              BEGIN
                FOR i := 1 TO insnUnitCt DO 
                  (* insnUnitCt chunks of nelem.size bytes each *)
                  v.readData(SUBARRAY(fromBytesPtr^, 0, nelem.size));

                  (** IO.Put
                        ( "SwapPacked, from BE, extracting from: 2_" 
                          & Fmt.Pad
                              (Fmt.Unsigned (from, 2), nelem.size*8, '0') 
                          & "\n"); **)

                  bitNo := 0; 
                  FOR j := FIRST(nelem.field^) TO LAST(nelem.field^) DO
                    fieldBitCt := nelem.field^[j]; 
                    extract 
                      := ExtractSwapBE ( from, bitNo, fieldBitCt, nelem.size);
                         (* ^bitNo is BE msb, on writing system and to
                              ExtractSwapBE. *) 

                    (** IO.Put(" Extracted field: 2_" 
                               & Fmt.Pad(Fmt.Unsigned(extract, 2),
                               fieldBitCt, '0') & "\n"); **)

                    IF j IN nelem.widecharFieldSet THEN
                      IF extract > 16_FFFF THEN 
                         extract := PickleStubs . ReplacementWt  
                     (** IO.Put 
                           ( "WIDECHAR narrowed to: 2_" 
                            & Fmt.Pad
                                (Fmt.Unsigned(extract, 2), fieldBitCt, '0') 
                            & "\n"); **)
                      END; 
                    END; 
                    to := Word.Insert (to, extract, bitNo, fieldBitCt);
                          (* bitNo means LE, lsb on this system, and as
                             taken by Insert. *) 

                    (** IO.Put
                          ( " Into result word: 2_" &
                            & Fmt.Pad(Fmt.Unsigned(to, 2), nelem.size*8, '0')
                            & "\n"); **)

                    INC(bitNo, fieldBitCt);
                  END (*FOR fields.*);
                  (* Copy the bytes to destination. *)
                  SUBARRAY(LOOPHOLE(dest, BufPtr)^, 0, nelem.size) :=
                    SUBARRAY(LOOPHOLE(ADR(to), BufPtr)^, 0, nelem.size);
                  INC(dest, nelem.size);
                END(*FOR*);
              END(*Block*);
            END (*WITH*);

          | PklAction.PAKind.SkipFrom =>
            v.skipData(insnUnitCt);
          | PklAction.PAKind.SkipTo =>
            INC(dest, insnUnitCt);
          | PklAction.PAKind.Skip =>
            v.skipData(insnUnitCt);
            INC(dest, insnUnitCt);
          | PklAction.PAKind.Swap16 =>
            ReadData(v, dest, insnUnitCt*2);
            FOR i := 1 TO insnUnitCt DO
              WITH int16 = LOOPHOLE(dest, UNTRACED REF Swap.Int16) DO
                int16^ := Swap.Swap2(int16^);
              END;
              INC(dest, 2);
            END;
          | PklAction.PAKind.Swap32 =>
            ReadData(v, dest, insnUnitCt*4);
            FOR i := 1 TO insnUnitCt DO
              WITH int32 = LOOPHOLE(dest, UNTRACED REF Int32Rec)^.v DO
                int32 := Swap.Swap4(int32);
              END;
              INC(dest, 4);
            END;
          | PklAction.PAKind.Swap64 =>
            ReadData(v, dest, insnUnitCt*8);
            FOR i := 1 TO insnUnitCt DO
              WITH int64 = LOOPHOLE(dest, UNTRACED REF Swap.Int64On32) DO
                int64^ := Swap.Swap8(int64^);
              END;
              INC(dest, 8);
            END;
          | PklAction.PAKind.Copy32to64, PklAction.PAKind.Swap32to64 =>
            WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
              FOR i := 1 TO insnUnitCt DO
                v.readData(SUBARRAY(t, 0, 4));
                WITH int32 = LOOPHOLE(ADR(t[0]), UNTRACED REF Int32Rec)^.v,
                     int64 = LOOPHOLE(dest, UNTRACED REF Swap.Int64On32) DO
                  (* First, put it in a 64 bit word appropriate to the 
                     sending machine. *)
                  IF self.from.little_endian THEN
                    int64.a := int32;
                    IF nelem.signed THEN
                      int64.b := SignExt32[Word.Extract(ORD(t[3]), 7, 1)];
                    ELSE
                      int64.b := 0;
                    END;
                  ELSE
                    int64.b := int32;
                    IF nelem.signed THEN
                      int64.a := SignExt32[Word.Extract(ORD(t[0]), 7, 1)];
                    ELSE
                      int64.a := 0;
                    END;
                  END;
                  (* Now, swap it if need be. *)
                  IF elem.kind = PklAction.PAKind.Swap32to64 THEN
                    int64^ := Swap.Swap8(int64^);
                  END;
                END;
                INC(dest, 8);
              END;
            END;
          | PklAction.PAKind.Copy64to32, PklAction.PAKind.Swap64to32 =>
            WITH nelem = NARROW(elem, PklAction.Copy64to32) DO
              FOR i := 1 TO insnUnitCt DO
                v.readData(t);
                WITH int64 = LOOPHOLE(ADR(t[0]), UNTRACED REF Swap.Int64On32)^,
                     int32 = LOOPHOLE(dest, UNTRACED REF Int32Rec)^.v DO
                  (* First, get the lower 32 bits as perceived on the the 
                     sending machine. *)
                  IF self.from.little_endian THEN
                    int32 := int64.a;
                    IF int64.b # 0 AND (NOT nelem.signed OR int64.b # -1) THEN
                      RAISE Error("Data value too big.");
                    END;
                  ELSE
                    int32 := int64.b;
                    IF int64.a # 0 AND (NOT nelem.signed OR int64.a # -1) THEN
                      RAISE Error("Data value too big.");
                    END;
                  END;

                  (* Now, swap it if need be. *)
                  IF elem.kind = PklAction.PAKind.Swap64to32 THEN
                    int32 := Swap.Swap4(int32);
                  END;
                END;
                INC(dest, 4);
              END;
            END;

          | PklAction.PAKind.Copy16to32, PklAction.PAKind.Swap16to32 =>
              FOR i := 1 TO insnUnitCt DO
                v.readData(SUBARRAY(t, 0, 2));
                WITH int16p = LOOPHOLE(ADR(t[0]), UNTRACED REF Swap.UInt16),
                     int32onU16p = LOOPHOLE(dest, UNTRACED REF Int32onU16) DO
                  (* First, put it in a 32 bit word appropriate to the 
                     sending machine. *)
                  IF self.from.little_endian THEN
                    int32onU16p^.a := int16p^;
                    int32onU16p^.b := 0;
                  ELSE
                    int32onU16p^.b := int16p^;
                    int32onU16p^.a := 0;
                  END;
                  (* Now, swap it if need be. *)
                  IF elem.kind = PklAction.PAKind.Swap16to32 THEN
                    WITH int32p = LOOPHOLE(dest, UNTRACED REF Swap.Int32) DO
                      int32p^ := Swap.Swap4(int32p^);
                    END; 
                  END;
                END;
                INC(dest, 4);
              END;

          | PklAction.PAKind.Copy32to16, PklAction.PAKind.Swap32to16 =>
          (* Can this ever happen? *) 
              FOR i := 1 TO insnUnitCt DO
                v.readData(SUBARRAY(t, 0, 4));
                WITH int32onU16p = LOOPHOLE(ADR(t[0]), UNTRACED REF Int32onU16),
                     u16 = LOOPHOLE(dest, UNTRACED REF U16Rec)^.v DO
                  (* First, get the lower 16 bits as perceived on the the 
                     sending machine. *)

                  IF self.from.little_endian THEN
                    u16 := int32onU16p^.a;
                    IF int32onU16p^.b # 0 THEN 
                      u16 := PickleStubs . ReplacementWt; 
                    END;
                  ELSE
                    u16 := int32onU16p^.b;
                    IF int32onU16p^.a # 0 THEN
                      u16 := PickleStubs . ReplacementWt; 
                    END;
                  END;

                  (* Now, swap it if need be. *)
                  IF elem.kind = PklAction.PAKind.Swap32to16 THEN
                    u16 := Swap.Swap4(u16);
                  END;
                END;
                INC(dest, 2);
              END;

          | PklAction.PAKind.CopyWC21to32 => 
              VAR intVal: UInt32; 
              BEGIN 
                FOR i := 1 TO insnUnitCt (*32-bit words*) DO
                  intVal := PickleStubs.InWC21(v.getReader().rd); 
                  IF intVal > 16_10FFFF THEN 
                    RAISE Error("Malformed pickle: WIDECHAR out of range.");
                  END; 
                  WITH int32p = LOOPHOLE(dest, UNTRACED REF UInt32) DO
                    int32p^ := intVal; 
                  END;
                  INC(dest,4) 
                END; 
              END; 

          | PklAction.PAKind.CopyWC21to16 => 
              VAR intVal: UInt32; 
              BEGIN 
                FOR i := 1 TO insnUnitCt (*16-bit words*) DO
                  intVal := PickleStubs.InWC21(v.getReader().rd); 
                  IF intVal > 16_FFFF THEN 
                    intVal := PickleStubs . ReplacementWt  
                  END; 
                  WITH uint16p = LOOPHOLE(dest, UNTRACED REF Swap.UInt16) DO
                    uint16p^ := intVal; 
                  END;
                  INC(dest,2) 
                END; 
              END; 

          | PklAction.PAKind.ReadRef =>
            WITH nelem = NARROW(elem, PklAction.Ref) DO
              FOR i := 1 TO insnUnitCt DO
                WITH ref = LOOPHOLE(dest, UNTRACED REF REFANY) DO
                  ref^ := v.readRef(nelem.refType);
                END;
                INC(dest, self.to.word_size DIV 8);
              END;
            END;
          | PklAction.PAKind.Done =>
          END;
        END;
      END;
    END;
    RETURN dest;
  END Convert;

PROCEDURE WriteData(v: WriteVisitor;  src: ADDRESS;  len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    WHILE len >= MAXLEN DO
      v.writeData(LOOPHOLE(src, BufPtr)^);
      INC(src, MAXLEN);  DEC(len, MAXLEN);
    END;
    IF (len > 0) THEN
      v.writeData(SUBARRAY(LOOPHOLE(src, BufPtr)^, 0, len));
    END;
  END WriteData;

PROCEDURE WriteWC21(wr: Wr.T; intVal: UInt32)
  RAISES {Wr.Failure, Thread.Alerted} =

  VAR B0, B1, B2: UInt32;
  BEGIN  
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
  END WriteWC21; 

PROCEDURE Write 
   (self: T; src: ADDRESS; v: WriteVisitor; 
    progRepCt: INTEGER := 1 (* Repeat the program this many times. *) )
   : ADDRESS 
     RAISES {Error, Wr.Failure, Thread.Alerted} =
  VAR insnRepCt: INTEGER := 1; (* Repeat each instruction this many times. *) 

  BEGIN
    IF self.writeConv # NIL THEN 
      (* There is a different program for writing.  Use it instead. *) 
      RETURN self.writeConv.write(src, v, progRepCt);
    END;

    IF self.prog.size() = 2 THEN
      (* We have a one step program (the step plus the DONE step), so
         lets just do it in one shot. *)
      insnRepCt := progRepCt;
      progRepCt := 1;
    END;
    (* Execute the program "progRepCt" times. *) 
    FOR n := 1 TO progRepCt DO
      FOR i := 0 TO self.prog.size() - 1 DO (* All program steps. *) 
        WITH elem = self.prog.get(i),
             insnUnitCt = elem.unitCt * insnRepCt DO 
             (*  Repeat each instruction "insnRepCt" times. *)
          CASE elem.kind OF
          | PklAction.PAKind.Copy =>
            WriteData (v, src, insnUnitCt);
            INC(src, insnUnitCt); 

          | PklAction.PAKind.Skip =>
            v.skipData(insnUnitCt);
            INC(src, insnUnitCt);

          | PklAction.PAKind.ReadRef =>
            (* The name "ReadRef" is meant for when the program is executed 
               by the read/convert interpreter.  Here, we do the reverse, i.e., 
               write a Ref. *) 
            WITH nelem = NARROW(elem, PklAction.Ref) DO
              FOR i := 1 TO insnUnitCt DO
                WITH ref = LOOPHOLE(src, UNTRACED REF REFANY) DO
                  v.writeRef(nelem.refType, ref^);
                END;
                INC(src, self.to.word_size DIV 8);
              END;
            END;

          | PklAction.PAKind.CopyWC21to32,
            PklAction.PAKind.Copy16to32 =>
            (* The names "CopyWC21to32" and "Copy16to32" are meant for when 
               the program is executed by the read/convert interpreter.  Here, 
               the to-system, which we are executing/writing on, is a 32-bit 
               WIDECHAR system. The other system is irrelevant to writing.  
               So we write each 32-bit field in WC21, or maybe 16-bits, if 
               forced. *) 
              VAR outValR: U16Rec;
              VAR writer := v.getWriter(); 
              BEGIN 
                IF writer.write16BitWidechar THEN 
                  WITH u16arr = LOOPHOLE(outValR.v, CharArrOverU16) DO 
                    FOR i := 1 TO insnUnitCt (*32-bit words*) DO
                      WITH uint32p = LOOPHOLE(src, UNTRACED REF UInt32) DO
                        IF Word.GT(uint32p^, 16_FFFF) THEN
                          outValR.v := PickleStubs . ReplacementWt;
                        ELSE outValR.v := uint32p^; 
                        END;
                        Wr.PutChar(writer.wr, u16arr[0]);
                        Wr.PutChar(writer.wr, u16arr[1]);
                      END; 
                      INC(src, 4);  
                    END; 
                  END; 
                ELSE 
                  FOR i := 1 TO insnUnitCt (*32-bit words*) DO
                    WITH uint32p = LOOPHOLE(src, UNTRACED REF UInt32) DO
                      PickleStubs.OutWC21
                        (v.getWriter().wr,VAL(uint32p^,WIDECHAR))
                      (* WC21 codec will avoid endianness worries. *) 
                    END; 
                    INC(src, 4);  
                  END; 
                END;
              END; 

          | PklAction.PAKind.CopyWC21to16 =>
            (* The name "CopyWC21to16" is meant for when the program is executed
               by the read/convert interpreter.  Here, if this happens at all,
               the to-system, which we are executing/writing on, is a 16-bit 
               WIDECHAR system, so we just copy 16-16.  Here, "insnUnitCt" is 
               number of WIDECHARs. *)  
            WriteData (v, src, insnUnitCt*2);
            INC(src, insnUnitCt*2); 

          | PklAction.PAKind.Done =>
          | PklAction.PAKind.SwapPackedLEtoBE => 
            RAISE 
              Error("PklAction.PAKind.SwapPackedLEtoBE called during write?");
          | PklAction.PAKind.SwapPackedBEtoLE => 
            RAISE 
              Error("PklAction.PAKind.SwapPackedBEtoLE called during write?");
          | PklAction.PAKind.CopyPackedLE => 
            RAISE Error("PklAction.PAKind.CopyPackedLE called during write?");
          | PklAction.PAKind.CopyPackedBE => 
            RAISE Error("PklAction.PAKind.CopyPackedBE called during write?");
          | PklAction.PAKind.SkipFrom =>
            RAISE Error("PklAction.PAKind.SkipFrom called during write?");
          | PklAction.PAKind.SkipTo =>
            RAISE Error("PklAction.PAKind.SkipTo called during write?");
          | PklAction.PAKind.Swap16 =>
            RAISE Error("PklAction.PAKind.Swap16 called during write?");
          | PklAction.PAKind.Swap32 =>
            RAISE Error("PklAction.PAKind.Swap32 called during write?");
          | PklAction.PAKind.Swap64 =>
            RAISE Error("PklAction.PAKind.Swap64 called during write?");
          | PklAction.PAKind.Copy32to64 =>
            RAISE Error("PklAction.PAKind.Copy32to64 called during write?");
          | PklAction.PAKind.Swap32to64 =>
            RAISE Error("PklAction.PAKind.Swap32to64 called during write?");
          | PklAction.PAKind.Copy64to32 =>
            RAISE Error("PklAction.PAKind.Copy64to32 called during write?");
          | PklAction.PAKind.Swap64to32 =>
            RAISE Error("PklAction.PAKind.Swap64to32 called during write?");
          | PklAction.PAKind.Copy32to16 =>
            RAISE Error("PklAction.PAKind.Copy32to16 called during write?");
          | PklAction.PAKind.Swap32to16 =>
            RAISE Error("PklAction.PAKind.Swap32to16 called during write?");
          | PklAction.PAKind.Swap16to32 =>
            RAISE Error("PklAction.PAKind.Swap16to32 called during write?");
          END;
        END;
      END;
    END;
    RETURN src;
  END Write;
 
PROCEDURE AppendProg(self: T; other: T) RAISES {Error} =
  VAR nelem: PklAction.T;
  BEGIN
    FOR i := 0 TO other.prog.size() - 1 DO
      WITH elem = other.prog.get(i) DO
        CASE elem.kind OF
        | PklAction.PAKind.Copy => 
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*8);
          INC(self.toOffset, elem.unitCt*8);
        | PklAction.PAKind.SwapPackedLEtoBE, PklAction.PAKind.SwapPackedBEtoLE, 
          PklAction.PAKind.CopyPackedLE, PklAction.PAKind.CopyPackedBE=> 
          WITH t = NARROW(elem, PklAction.Packed),
               nt = NEW(PklAction.Packed, 
                        kind := t.kind, unitCt := t.unitCt, size := t.size,
                        field := NEW(REF ARRAY OF CARDINAL, 
                                     NUMBER(t.field^)),
                        widecharFieldSet:= t.widecharFieldSet
                       ) DO
            FOR i := FIRST(nt.field^) TO LAST(nt.field^) DO
              nt.field[i] := t.field[i];
            END;
            nelem := nt;
            INC(self.fromOffset, elem.unitCt*8*t.size);
            INC(self.toOffset, elem.unitCt*8*t.size);
          END;
        | PklAction.PAKind.SkipFrom =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*8);
        | PklAction.PAKind.SkipTo =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.toOffset, elem.unitCt*8);
        | PklAction.PAKind.Skip =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*8);
          INC(self.toOffset, elem.unitCt*8);
        | PklAction.PAKind.Swap16 =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*16);
          INC(self.toOffset, elem.unitCt*16);
        | PklAction.PAKind.Swap32, PklAction.PAKind.CopyWC21to32 =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*32);
          INC(self.toOffset, elem.unitCt*32);
        | PklAction.PAKind.CopyWC21to16 =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*32);
          INC(self.toOffset, elem.unitCt*16);
        | PklAction.PAKind.Swap64 =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*64);
          INC(self.toOffset, elem.unitCt*64);
        | PklAction.PAKind.Copy32to64, PklAction.PAKind.Swap32to64 =>
          WITH t = NARROW(elem, PklAction.Copy32to64) DO
            nelem := NEW(PklAction.Copy32to64, kind := t.kind, 
                         unitCt := t.unitCt, signed := t.signed);
          END;
          INC(self.fromOffset, elem.unitCt*32);
          INC(self.toOffset, elem.unitCt*64);
        | PklAction.PAKind.Copy64to32, PklAction.PAKind.Swap64to32 =>
          WITH t = NARROW(elem, PklAction.Copy32to64) DO
            nelem := NEW(PklAction.Copy64to32, kind := t.kind, 
                         unitCt := t.unitCt, signed := t.signed);
          END;
          INC(self.fromOffset, elem.unitCt*64);
          INC(self.toOffset, elem.unitCt*32);
        | PklAction.PAKind.Copy16to32, PklAction.PAKind.Swap16to32 =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*16);
          INC(self.toOffset, elem.unitCt*32);
        | PklAction.PAKind.Copy32to16, PklAction.PAKind.Swap32to16 =>
          nelem := NEW(PklAction.T, kind := elem.kind, unitCt := elem.unitCt);
          INC(self.fromOffset, elem.unitCt*32);
          INC(self.toOffset, elem.unitCt*16);
        | PklAction.PAKind.ReadRef =>
          WITH t = NARROW(elem, PklAction.Ref) DO
            nelem := NEW(PklAction.Ref, kind := t.kind, unitCt := t.unitCt,
                         refType := t.refType);
          END;
          INC(self.fromOffset, elem.unitCt*self.from.word_size);
          INC(self.toOffset, elem.unitCt*self.to.word_size);
        | PklAction.PAKind.Done =>
          RETURN;
        END;
        self.prog.addhi(nelem);
      END;
    END;
    RAISE Error("Invalid conversion program.");
  END AppendProg;
 
PROCEDURE GetHiKind(prog: PklActionSeq.T; kind: PklAction.PAKind;
                    VAR elem: PklAction.T) : BOOLEAN =
  BEGIN
    IF prog.size() > 0 THEN
      elem := prog.gethi();
      IF elem.kind = kind THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END GetHiKind;

PROCEDURE PackedPAKind (fromLE: BOOLEAN; DoSwap: BOOLEAN): PklAction.PAKind = 

  VAR Result: PklAction.PAKind; 
  BEGIN 
    IF DoSwap THEN
      IF fromLE
      THEN Result := PklAction.PAKind.SwapPackedLEtoBE;
      ELSE Result := PklAction.PAKind.SwapPackedBEtoLE;
      END; 
    ELSE 
      IF fromLE
      THEN Result := PklAction.PAKind.CopyPackedLE;
      ELSE Result := PklAction.PAKind.CopyPackedBE;
      END; 
    END; 
    RETURN Result; 
  END PackedPAKind; 

PROCEDURE AddCopy(self: T; bitCt: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.Copy, elem) THEN
      INC(elem.unitCt, bitCt DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Copy, 
                        unitCt := bitCt DIV 8));
  END AddCopy;

PROCEDURE AddPackedFirstField(self: T; fieldBitSize: INTEGER; 
                                  shortenWidechar: BOOLEAN;
                                  paKind:PklAction.PAKind) =
  (* We are starting on a byte boundary, i.e.: 
     PRE: self.fromOffset MOD 8 = 0.  
     PRE: self.toOffset MOD 8 = 0. *) 
  BEGIN
    (* We'll swap within a unit of the minimum number of whole bytes needed
       to hold the field. *)
    WITH wholeByteBitCt = RoundUp(fieldBitSize, 8) DO
      INC(self.fromOffset, wholeByteBitCt);
      INC(self.toOffset, wholeByteBitCt);
      WITH elem = NEW(PklAction.Packed, kind := paKind,
                      unitCt := 1, 
                      size := wholeByteBitCt DIV 8, (* Instruction unit is bytes. *) 
                      field := NEW(REF ARRAY OF CARDINAL, 1)) DO
        IF shortenWidechar THEN
          elem.widecharFieldSet 
            := elem.widecharFieldSet + PklAction.FieldNoSet {0};  
        END; 
        elem.field[0] := fieldBitSize;
        self.prog.addhi(elem);
      END;
    END;
  END AddPackedFirstField;

 PROCEDURE AddPackedNextField
   (self: T; fieldBitSize: INTEGER; 
    offset: INTEGER; shortenWidechar: BOOLEAN; paKind:PklAction.PAKind) =
  VAR elem: PklAction.T;
      totalBits: CARDINAL;
  BEGIN
    WITH ret = GetHiKind(self.prog, paKind, elem) DO
      (* The last entry must have paKind. *)
      <* ASSERT ret *>
      WITH nelem = NARROW(elem, PklAction.Packed) DO
        totalBits := 0;
        FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
          INC(totalBits, nelem.field[i]);
        END;
        (* They should be packed, so our offset should equal the 
           total bits thus far.  We can't check it, but we can at least
           check the bits line up in the current byte. *)
        <* ASSERT (totalBits MOD 8) = (offset MOD 8) *>

        INC(totalBits, fieldBitSize);

        (* Make sure the size in nelem is correct, and make sure fromOffset 
           and toOffset are updated to include any new bytes *)
        totalBits := RoundUp(totalBits, 8);
        (* The max total bit size of a packed set of fields is the word
           size!  This is guaranteed because a packed field cannot
           span a word boundary, and we only call this function when a
           packed field starts properly within a byte.  If a packed field
           starts on a byte boundary, we start a new set of packed
           fields using AddPackedFirstField. *)
        <* ASSERT totalBits <= self.from.word_size *>

        WITH AddlBytes = (totalBits DIV 8) - nelem.size DO
          IF AddlBytes > 0 THEN
            INC(self.fromOffset, AddlBytes*8);
            INC(self.toOffset, AddlBytes*8);
            INC(nelem.size, AddlBytes);
          END;
        END;
        IF shortenWidechar THEN
          nelem.widecharFieldSet 
            := nelem.widecharFieldSet 
               + PklAction.FieldNoSet {NUMBER(nelem.field^)};  
        END; 

        WITH new_field = NEW(REF ARRAY OF CARDINAL, 
                             NUMBER(nelem.field^) + 1) DO
          SUBARRAY(new_field^, 0, NUMBER(nelem.field^)) := nelem.field^;
          nelem.field := new_field;
        END;
        nelem.field[LAST(nelem.field^)] := fieldBitSize;
      END;
    END;
  END AddPackedNextField;

PROCEDURE AddPackedArray
  (self: T; bitCt(*Entire array in bits.*): INTEGER;
   numElts: INTEGER; fieldBitSize: INTEGER; packingWordBitSize: INTEGER;
   shortenWidechar: BOOLEAN; paKind:PklAction.PAKind) =
  (* packingWordBitSize is bits in the "word" we are converting within.  It will
     always fit within a Word.T on the machine executing this code. *)  
  VAR fieldsPerWord := packingWordBitSize DIV fieldBitSize;
  BEGIN
    (* We will build a packing for at most "fieldsPerWord" fields.  Packed
       arrays must not have elements that span word boundaries, so if 
       "numElts > fieldsPerWord" then 
       "fieldsPerWord * fieldBitSize = packingWordBitSize"
    *)
    <* ASSERT (numElts <= fieldsPerWord) 
              OR (fieldsPerWord * fieldBitSize = packingWordBitSize) 
    *>

    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt);

    WITH fullWordCt   = numElts DIV fieldsPerWord,
         bytesPerWord = packingWordBitSize DIV 8,
         extraEltCt   = numElts MOD fieldsPerWord,   
         extraBytes    = RoundUp(bitCt MOD packingWordBitSize, 8) DIV 8 DO

      (* First, we add a program action element to handle the bulk of the array
         element swapping when the array spans more than one word.
         It takes care of the first fullWordCt*fieldsPerWord elements, 
         which occupy exactly the first fullWordCt words. *)
      IF fullWordCt > 0 THEN
        WITH nelem = NEW(PklAction.Packed, 
                         kind := paKind,
                         unitCt := fullWordCt, 
                         size := bytesPerWord, 
                         field := NEW(REF ARRAY OF CARDINAL, fieldsPerWord)) DO
          FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
            nelem.field[i] := fieldBitSize;
          END;
          IF shortenWidechar THEN
            nelem.widecharFieldSet 
              := PklAction.FieldNoSet {0..fieldsPerWord-1};  
          ELSE nelem.widecharFieldSet := PklAction.FieldNoSet{ }; 
          END; 
          self.prog.addhi(nelem);
        END;
      END;

      (* Next, if the number of array elements does not fit exactly in
         full words, add one more program action element to the end which 
         handles the extra fields.  *)
      IF extraBytes > 0 THEN
        WITH nelem = NEW(PklAction.Packed, 
                         kind := paKind, 
                         unitCt := 1, 
                         size := extraBytes,
                         field := NEW(REF ARRAY OF CARDINAL, extraEltCt)) DO
          FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
            nelem.field[i] := fieldBitSize;
          END;
          IF shortenWidechar THEN
            nelem.widecharFieldSet 
              := PklAction.FieldNoSet {0..fieldsPerWord-1};  
          ELSE nelem.widecharFieldSet := PklAction.FieldNoSet{ }; 
          END; 
          self.prog.addhi(nelem);
        END;
      END;
    END;
  END AddPackedArray; 

PROCEDURE AddCopy32to64(self: T; bitCt: INTEGER; signed: BOOLEAN) =
(* PRE: bitCt MOD 32 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt*2);
    IF GetHiKind(self.prog, PklAction.PAKind.Copy32to64, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
        IF nelem.signed = signed THEN
          INC(elem.unitCt, bitCt DIV 32);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.Copy32to64, kind := PklAction.PAKind.Copy32to64, 
                        unitCt := bitCt DIV 32, signed := signed));
  END AddCopy32to64;

PROCEDURE AddCopy64to32(self: T; bitCt: INTEGER; signed: BOOLEAN) =
(* PRE: bitCt MOD 64 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    <* ASSERT bitCt MOD 2 = 0 *>
    INC(self.toOffset, bitCt DIV 2);
    IF GetHiKind(self.prog, PklAction.PAKind.Copy64to32, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Copy64to32) DO
        IF nelem.signed = signed THEN
          INC(elem.unitCt, bitCt DIV 64);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.Copy64to32, kind := PklAction.PAKind.Copy64to32,
                        unitCt := bitCt DIV 64, signed := signed));
  END AddCopy64to32;

PROCEDURE AddCopy16to32(self: T; fromBitCt: INTEGER) =
(* PRE: fromBitCt MOD 16 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, fromBitCt);
    INC(self.toOffset, fromBitCt*2);
    IF GetHiKind(self.prog, PklAction.PAKind.Copy16to32, elem) THEN
      INC(elem.unitCt, fromBitCt DIV 16);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T,
                        kind := PklAction.PAKind.Copy16to32, 
                        unitCt := fromBitCt DIV 16));
  END AddCopy16to32;

PROCEDURE AddCopy32to16(self: T; fromBitCt: INTEGER) =
(* PRE: fromBitCt MOD 32 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, fromBitCt);
    INC(self.toOffset, fromBitCt DIV 2);
    IF GetHiKind(self.prog, PklAction.PAKind.Copy32to16, elem) THEN
      INC(elem.unitCt, fromBitCt DIV 32);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T,
                        kind := PklAction.PAKind.Copy32to16, 
                        unitCt := fromBitCt DIV 32));
  END AddCopy32to16;

PROCEDURE AddCopyWC21to32(self: T; toBitCt: INTEGER) =
(* PRE: toBitCt MOD 32 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, toBitCt);
    (* Although this will read a variable amount from the stream when reading,
       fromOffset is duplicating the _in memory_ layout of the from-system. 
       If the pickle uses WC21, the from-system had to have 32-bit WIDECHAR. *)
    INC(self.toOffset, toBitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.CopyWC21to32, elem) THEN
      INC(elem.unitCt, toBitCt DIV 32);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T,
                        kind := PklAction.PAKind.CopyWC21to32, 
                        unitCt := toBitCt DIV 32));
  END AddCopyWC21to32;

PROCEDURE AddCopyWC21to16(self: T; toBitCt: INTEGER) =
(* PRE: toBitCt MOD 16 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, toBitCt*2);
    (* Although this will read a variable amount from the stream when reading,
       fromOffset is duplicating the _in memory_ layout of the from-system. 
       If the pickle uses WC21, the from-system had to have 32-bit WIDECHAR. *)
    INC(self.toOffset, toBitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.CopyWC21to16, elem) THEN
      INC(elem.unitCt, toBitCt DIV 16);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T,
                        kind := PklAction.PAKind.CopyWC21to16, 
                        unitCt := toBitCt DIV 16));
  END AddCopyWC21to16;

PROCEDURE AddSkipFrom(self: T; bitCt: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.SkipFrom, elem) THEN
      INC(elem.unitCt, bitCt DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.SkipFrom, 
                        unitCt := bitCt DIV 8));
  END AddSkipFrom;

PROCEDURE AddSkipTo(self: T; bitCt: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.toOffset, bitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.SkipTo, elem) THEN
      INC(elem.unitCt, bitCt DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.SkipTo, 
                        unitCt := bitCt DIV 8));
  END AddSkipTo;

PROCEDURE AddSkipOrCopy(self: T; bitCt: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.Copy, elem) THEN
      INC(elem.unitCt, bitCt DIV 8);
      RETURN;
    ELSIF GetHiKind(self.prog, PklAction.PAKind.Skip, elem) THEN
      INC(elem.unitCt, bitCt DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Skip, 
                        unitCt := bitCt DIV 8));
  END AddSkipOrCopy;

PROCEDURE AddSkip(self: T; fromDiff, toDiff: INTEGER) =
  BEGIN
    IF fromDiff = toDiff THEN
      IF fromDiff > 0 THEN
        self.addSkipOrCopy(fromDiff);
      END;
    ELSE
      WITH bothPad = MIN(toDiff, fromDiff) DO
        IF bothPad > 0 THEN
          self.addSkipOrCopy(bothPad);
        END;
        DEC(fromDiff, bothPad);
        DEC(toDiff, bothPad);
        IF fromDiff > 0 THEN
          self.addSkipFrom(fromDiff);
        END;
        IF toDiff > 0 THEN
          self.addSkipTo(toDiff);
        END;
      END;
    END;
  END AddSkip;

PROCEDURE AddSwap16(self: T; bitCt: INTEGER) =
(* PRE: bitCt MOD 16 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.Swap16, elem) THEN
      INC(elem.unitCt, bitCt DIV 16);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Swap16, 
                        unitCt := bitCt DIV 16));
  END AddSwap16;

PROCEDURE AddSwap32(self: T; bitCt: INTEGER) =
(* PRE: bitCt MOD 32 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.Swap32, elem) THEN
      INC(elem.unitCt, bitCt DIV 32);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Swap32, 
                        unitCt := bitCt DIV 32));
  END AddSwap32;

PROCEDURE AddSwap64(self: T; bitCt: INTEGER) =
(* PRE: bitCt MOD 64 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt);
    IF GetHiKind(self.prog, PklAction.PAKind.Swap64, elem) THEN
      INC(elem.unitCt, bitCt DIV 64);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Swap64, 
                        unitCt := bitCt DIV 64));
  END AddSwap64;

PROCEDURE AddSwap32to64(self: T; bitCt: INTEGER; signed: BOOLEAN) =
(* PRE: bitCt MOD 32 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    INC(self.toOffset, bitCt*2);
    IF GetHiKind(self.prog, PklAction.PAKind.Swap32to64, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
        IF nelem.signed = signed THEN
          INC(elem.unitCt, bitCt DIV 32);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.Copy32to64, kind := PklAction.PAKind.Swap32to64, 
                        unitCt := bitCt DIV 32, signed := signed));
  END AddSwap32to64;

PROCEDURE AddSwap64to32(self: T; bitCt: INTEGER; signed: BOOLEAN) =
(* PRE: bitCt MOD 64 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, bitCt);
    <* ASSERT bitCt MOD 64 = 0 *>
    INC(self.toOffset, bitCt DIV 2);
    IF GetHiKind(self.prog, PklAction.PAKind.Swap64to32, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
        IF nelem.signed = signed THEN
          INC(elem.unitCt, bitCt DIV 64);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Swap64to32, 
                        unitCt := bitCt DIV 64));
  END AddSwap64to32;

PROCEDURE AddSwap16to32(self: T; fromBitCt: INTEGER) =
(* PRE: fromBitCt MOD 16 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, fromBitCt);
    INC(self.toOffset, fromBitCt*2);
    IF GetHiKind(self.prog, PklAction.PAKind.Swap16to32, elem) THEN
      INC(elem.unitCt, fromBitCt DIV 16);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Swap16to32, 
                        unitCt := fromBitCt DIV 16));
  END AddSwap16to32;

PROCEDURE AddSwap32to16(self: T; fromBitCt: INTEGER) =
(* PRE: fromBitCt MOD 32 = 0 *) 
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, fromBitCt);
    INC(self.toOffset, fromBitCt DIV 2);
    IF GetHiKind(self.prog, PklAction.PAKind.Swap32to16, elem) THEN
      INC(elem.unitCt, fromBitCt DIV 32);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T,
                        kind := PklAction.PAKind.Swap32to16, 
                        unitCt := fromBitCt DIV 32));
  END AddSwap32to16;

PROCEDURE AddRef(self: T; type: RefType) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, self.from.word_size);
    INC(self.toOffset, self.to.word_size);
    IF GetHiKind(self.prog, PklAction.PAKind.ReadRef, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Ref) DO
        IF nelem.refType = type THEN
          INC(elem.unitCt);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.Ref, kind := PklAction.PAKind.ReadRef, 
                        unitCt := 1, refType := type));
  END AddRef;

PROCEDURE AddDone(self: T) =
  BEGIN
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.PAKind.Done));
  END AddDone;

PROCEDURE New(typecode: INTEGER; from: RTPacking.T; to: RTPacking.T; 
              VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER): T 
  RAISES {Error} =
  VAR key := PackingTypeCode.T{from := RTPacking.Encode(from), 
                               to := RTPacking.Encode(to),
                               tc := typecode};
      ref : REFANY; 
  BEGIN
    (* If we've already building this converter, return it. *)
    IF packingCache.get(key, ref) THEN
      WITH ret = NARROW(ref, T) DO
        ret.getDim(nDim, fromEltPack, toEltPack);
        RETURN ret;
      END;
    END;

    RETURN NEW(T).init(typecode, from, to, nDim, fromEltPack, toEltPack);
  END New;

PROCEDURE GetDim(self: T; VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER) =
  BEGIN
    nDim := self.nDim;
    fromEltPack := self.fromEltPack;
    toEltPack := self.toEltPack;
  END GetDim;

PROCEDURE RoundUp (size, alignment: INTEGER): INTEGER =
  BEGIN
    IF (alignment = 0) THEN RETURN size;
    ELSE RETURN ((size + alignment - 1) DIV alignment) * alignment END;
  END RoundUp;

PROCEDURE Init(self: T; typecode: INTEGER; from: RTPacking.T; 
               to: RTPacking.T; 
               VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER): T
  RAISES {Error} =
  VAR key := PackingTypeCode.T{from := RTPacking.Encode(from), 
                               to := RTPacking.Encode(to),
                               tc := typecode};
      ref : REFANY; 
  BEGIN
    (* If we've already built this converter, return it.  We've
       still wasted a NEW() to get here, but better than recomputing
       everything! *)
    IF packingCache.get(key, ref) THEN
      WITH ret = NARROW(ref, T) DO
        ret.getDim(nDim, fromEltPack, toEltPack);
        RETURN ret;
      END;
    END;

    (* Get the fromTipe data structure and check for a few errors. *)
    self.fromTipe := RTTipe.Get(typecode, from);
    IF self.fromTipe = NIL THEN
      RAISE Error("\"from\" RTTipe.T is NIL.");
    END;

    self.toTipe := RTTipe.Get(typecode, to);
    IF self.toTipe = NIL THEN
      RAISE Error("\"to\" RTTipe.T is NIL.");
    END;

    (* OpenArrays need to be handled outside of the convert routine.  A
       conversion is therefore built for the type of the array element,
       and that conversion should be applied the correct number of
       times when the array dimmensions are known. *) 
    TYPECASE self.fromTipe OF
    | RTTipe.OpenArray(oa) =>
      WITH oa2 = NARROW(self.toTipe, RTTipe.OpenArray) DO
        self.nDim := oa.n_dimensions;
        self.fromEltPack := oa.elt_pack; 
        (* ^Element size in bits, including alignment padding. *) 
        self.toEltPack := oa2.elt_pack;
        self.fromTipe := oa.element;
        self.toTipe := oa2.element;
      END;
    ELSE
      self.nDim := 0;
      self.fromEltPack := 0;
      self.toEltPack := 0;
    END;
    nDim := self.nDim;
    fromEltPack := self.fromEltPack;
    toEltPack := self.toEltPack;

    (* We don't know how to do floating point conversions. *)
    IF from.float # to.float THEN
      RAISE Error("Data format (float) not handled.");
    END;

    IF from.word_size < 32 OR to.word_size < 32 THEN
      RAISE Error("Data format (word size) not handled.");
    END;

    self.from := from;
    self.to := to;
    self.prog := NEW(PklActionSeq.T).init();

    self.wordKind := GetWordKind(from, to);
    self.longKind := GetLongintKind(from, to);
    self.widecharKind := GetWidecharKind(from, to);

    (* Build the tipe conversion for the top level  *)
    self.buildOne(self.fromTipe, self.toTipe);

    (* Figure out the allocated size. *)
    IF self.nDim > 0 THEN
      (* Open arrays will actually store the data for their element
         here, so we don't adjust that. *)
      self.fromSize := self.fromTipe.size;
      self.toSize := self.toTipe.size;
    ELSE
      (* If this is not an open array, we need to heed what it says in
         in RTType.FixSizes().  A comment points out that
         all the REF types dataSizes are a multiple of "header" words,
         where the size of "header" is the word size of the machine.
         So, we will ensure that any conversion copies a multiple of
         word_size if it is a reftype.  *)

      (* Get the size of the data of the referent.  If it's an object,
         we want the size of the fields.  Otherwise, the size is what we
         want. *)


      self.fromSize := self.fromOffset;
      self.toSize := self.toOffset;
(*
      TYPECASE self.fromTipe OF
      | RTTipe.Object(o) =>
        WITH o2 = NARROW(self.toTipe, RTTipe.Object) DO
          self.fromSize := o.field_size;
          self.toSize := o2.field_size;
        END;
      ELSE
        self.fromSize := self.fromTipe.size;
        self.toSize := self.toTipe.size;
      END;
*)
      WITH newFromSize = RoundUp(self.fromSize, self.from.word_size),
           newToSize = RoundUp(self.toSize, self.to.word_size),
           fromDiff = newFromSize - self.fromSize,
           toDiff = newToSize - self.toSize DO
	<* ASSERT fromDiff >= 0 *>
	<* ASSERT toDiff >= 0 *>
  
        self.addSkip(fromDiff, toDiff);
        self.fromSize := newFromSize; 
        self.toSize := newToSize; 
      END;
    END;
    self.addDone();

    (* the write method is only geared towards "converting" between
       exactly the same machine. *)
    IF self.from # self.to THEN
      VAR wnDim, wfromEltPack, wtoEltPack: INTEGER;
      BEGIN
        self.writeConv := New(typecode, to, to, wnDim, 
                           wfromEltPack, wtoEltPack);
      END;
    ELSE
      self.writeConv := NIL;
    END;

    EVAL packingCache.put(key, self);
(*
    BasicTipeToText(self.fromTipe);
    IO.Put(", total size: " & Fmt.Int(self.fromSize) & "\n");
    BasicTipeToText(self.toTipe);
    IO.Put(", total size: " & Fmt.Int(self.toSize) & "\n");
*)
    RETURN self;
  END Init;

PROCEDURE GetWordKind(from: RTPacking.T; to: RTPacking.T): CPKind =
(* The result is good for all ordinal types except LONGINT. *) 
  BEGIN
    IF from.word_size = to.word_size THEN
      IF from.little_endian = to.little_endian THEN
        RETURN CPKind.Copy;
      ELSE
        RETURN CPKind.Swap;
      END;
    ELSE
      IF from.little_endian = to.little_endian THEN
        IF from.word_size = 32 THEN
          RETURN CPKind.Copy32to64;
        ELSE
          RETURN CPKind.Copy64to32;
        END;
      ELSE
        IF from.word_size = 32 THEN
          RETURN CPKind.Swap32to64;
        ELSE
          RETURN CPKind.Swap64to32;
        END;
      END;
    END;
  END GetWordKind;

PROCEDURE GetLongintKind(from: RTPacking.T; to: RTPacking.T): CPKind =
(* The result is good only for LONGINT. *) 
  BEGIN
    IF from.long_size = to.long_size THEN
      IF from.little_endian = to.little_endian THEN
        RETURN CPKind.Copy;
      ELSE
        RETURN CPKind.Swap;
      END;
    ELSE
      IF from.little_endian = to.little_endian THEN
        IF from.long_size = 32 THEN
          RETURN CPKind.Copy32to64;
        ELSE
          RETURN CPKind.Copy64to32;
        END;
      ELSE
        IF from.long_size = 32 THEN
          RETURN CPKind.Swap32to64;
        ELSE
          RETURN CPKind.Swap64to32;
        END;
      END;
    END;
  END GetLongintKind;

PROCEDURE GetWidecharKind(from: RTPacking.T; to: RTPacking.T): CPKind =
(* The result is good only for WIDECHAR. *) 
  BEGIN
    IF from.widechar_size = to.widechar_size THEN
      IF from.little_endian = to.little_endian THEN
        RETURN CPKind.Copy;
      ELSE
        RETURN CPKind.Swap;
      END;
    ELSE (* WIDECHAR sizes differ. *) 
      IF from.little_endian = to.little_endian THEN
        IF from.widechar_size = 16 THEN
          RETURN CPKind.Copy16to32;
        ELSE 
          <* ASSERT from.widechar_size = 32 *> 
          RETURN CPKind.Copy32to16;
        END;
      ELSE
        IF from.widechar_size = 16 THEN
          RETURN CPKind.Swap16to32;
        ELSE
          RETURN CPKind.Swap32to16;
        END;
      END;
    END;
  END GetWidecharKind;

PROCEDURE BuildSuper(self: T; typecode: INTEGER; 
                     VAR fromSize, fromAlign, toSize, toAlign: INTEGER)
  RAISES {Error} =
  VAR nDim, fromEltPack, toEltPack: INTEGER;
      superConverter := New(typecode, self.from, self.to, nDim, 
                            fromEltPack, toEltPack);
      superFromTipe, superToTipe: RTTipe.Object;
  BEGIN
    (* Get the fromTipe data structure and check for a few errors. *)
    (*
    superFromTipe := RTTipe.Get(typecode, self.from);
    IF superFromTipe = NIL THEN
      RAISE Error("supertype \"from\" RTTipe.T is NIL.");
    END;

    superToTipe := RTTipe.Get(typecode, self.to);
    IF superToTipe = NIL THEN
      RAISE Error("supertype \"to\" RTTipe.T is NIL.");
    END;
    self.buildOne(superFromTipe, superToTipe);
    *)

    (* Add the supertype conversion program to ours. *)
    self.appendProg(superConverter);

    (* Return the proper allocated size. *)
    fromSize := superConverter.fromSize;
    toSize := superConverter.toSize;

    (* Coerce to RTTipe.Objects, which they must be. *)
    superFromTipe := superConverter.fromTipe;
    superToTipe := superConverter.toTipe;
    fromAlign := superFromTipe.field_align;
    toAlign := superToTipe.field_align;
  END BuildSuper; 

PROCEDURE BuildOrdinal(self: T; fromTipe: RTTipe.T; 
                       toTipe: RTTipe.T; convKind: CPKind; signed: BOOLEAN) 
          RAISES {Error} =
  BEGIN   
    CASE convKind OF
    | CPKind.Copy =>
      <* ASSERT fromTipe.size = toTipe.size *>
      self.addCopy(fromTipe.size);
    | CPKind.Swap =>
      <* ASSERT fromTipe.size = toTipe.size *>
      CASE fromTipe.size OF
      | 8 => self.addCopy(8);
      | 16 => self.addSwap16(16);
      | 32 => self.addSwap32(32);
      | 64 => self.addSwap64(64);
      ELSE
        RAISE Error("Should not get here: 2");
      END;
    | CPKind.Copy32to64 =>
      IF fromTipe.size = toTipe.size THEN
        self.addCopy(fromTipe.size);
      ELSE
        <* ASSERT fromTipe.size = 32 *> 
	<* ASSERT toTipe.size = 64 *> 
        self.addCopy32to64(32, signed);
      END;
    | CPKind.Copy64to32 =>
      IF fromTipe.size = toTipe.size THEN
        self.addCopy(fromTipe.size);
      ELSE
        <* ASSERT fromTipe.size = 64 *> 
	<* ASSERT toTipe.size = 32 *> 
        self.addCopy64to32(64, signed);
      END;
    | CPKind.Swap32to64 =>
      CASE fromTipe.size OF
      | 8 => <* ASSERT toTipe.size = 8 *> self.addCopy(8);
      | 16 => <* ASSERT toTipe.size = 16 *> self.addSwap16(16);
      | 32 => 
        IF toTipe.size = 32 THEN
          self.addSwap32(32);
        ELSE
          <* ASSERT toTipe.size = 64 *> 
          self.addSwap32to64(32, signed);
        END;
      | 64 => RAISE Error("Should not get here: 1");
      ELSE
        RAISE Error("Should not get here: 3");
      END;
    | CPKind.Swap64to32 =>
      CASE fromTipe.size OF
      | 8 => <* ASSERT toTipe.size = 8 *> self.addCopy(8);
      | 16 => <* ASSERT toTipe.size = 16 *> self.addSwap16(16);
      | 32 => <* ASSERT toTipe.size = 32 *> self.addSwap32(32);
      | 64 => <* ASSERT toTipe.size = 32 *> self.addSwap64to32(64, signed);
      ELSE
        RAISE Error("Should not get here: 4");
      END;
    ELSE <* ASSERT FALSE *> 
    END;
  END BuildOrdinal; 

PROCEDURE BuildWidechar(self: T; fromTipe: RTTipe.T; toTipe: RTTipe.T) 
          RAISES {Error} =
  BEGIN   
    CASE self.widecharKind OF
    | CPKind.Copy =>
      <* ASSERT fromTipe.size = toTipe.size *> 
      CASE fromTipe.size OF
      | 16 => self.addCopy(fromTipe.size);
      | 32 => self.addCopyWC21to32(toTipe.size);
        (* The Write interpreter will just write 32 in its own way for this. *)
      ELSE
        RAISE Error("Should not get here: 5");
      END; 
    | CPKind.Swap =>
      <* ASSERT fromTipe.size = toTipe.size *>
      CASE fromTipe.size OF
      | 16 => self.addSwap16(16);
      | 32 => self.addCopyWC21to32(toTipe.size); 
        (* The Write interpreter will just write 32 in its own way for this. *)
      ELSE
        RAISE Error("Should not get here: 6");
      END;

    (* Only the readConvert interpreter will get any of the following: *) 
    | CPKind.Copy16to32 =>
      IF fromTipe.size = toTipe.size THEN 
        (* It's an Enum with the right value count, but not really WIDECHAR. *)
(* Widechar Tipe. *) 
        self.addCopy(fromTipe.size);
      ELSE
        <* ASSERT fromTipe.size = 16 *> 
        <* ASSERT toTipe.size = 32 *> 
        self.addCopy16to32(16);
        (* The Write interpreter will just write 32 in its own way for this. *)
      END; 
    | CPKind.Swap16to32 =>
      IF fromTipe.size = toTipe.size THEN 
        (* It's an Enum with the right value count, but not really WIDECHAR. *)
(* Widechar Tipe. *) 
        CASE fromTipe.size OF 
        | 16 => self.addSwap16(16); 
        | 32 => self.addSwap32(32);
        ELSE <* ASSERT FALSE *>
        END; 
      ELSE
        <* ASSERT fromTipe.size = 16 *> 
        <* ASSERT toTipe.size = 32 *> 
        self.addSwap16to32(16);
      END; 
    | CPKind.Copy32to16 =>
      IF fromTipe.size = toTipe.size THEN 
        (* It's an Enum with the right value count, but not really WIDECHAR. *)
(* Widechar Tipe. *) 
        self.addCopy(fromTipe.size);
      ELSE
        <* ASSERT fromTipe.size = 32 *> 
        <* ASSERT toTipe.size = 16 *> 
        self.addCopyWC21to16(16); 
        (* The write interpreter will just write 16 in its own way for this. *)
      END; 
    | CPKind.Swap32to16 =>
      IF fromTipe.size = toTipe.size THEN 
        (* It's an Enum with the right value count, but not really WIDECHAR. *)
(* Widechar Tipe. *) 
        CASE fromTipe.size OF 
        | 16 => self.addSwap16(16); 
        | 32 => self.addSwap32(32);
        ELSE <* ASSERT FALSE *>
        END; 
      ELSE
        <* ASSERT fromTipe.size = 32 *> 
        <* ASSERT toTipe.size = 16 *> 
        self.addCopyWC21to16(16); 
        (* The write interpreter will just write 16 in its own way for this. *)
      END; 
    ELSE <* ASSERT FALSE *> 
    END;
  END BuildWidechar; 

PROCEDURE BuildOne(self: T; fromTipe: RTTipe.T; 
                   toTipe: RTTipe.T) RAISES {Error} =
  BEGIN
    (* When we start, we assume that the offsets are positioned
       correctly for these tipes in the data objects. *)
    
    (* Check some obvious conditions. *)
    <* ASSERT fromTipe.kind = toTipe.kind *>

    (* In fact, these next two aren't always correct because of the
       weird way objects are laid out. *)
    (* ASSERT self.fromOffset MOD fromTipe.align = 0 *)
    (* ASSERT self.toOffset MOD toTipe.align = 0 *)

    TYPECASE fromTipe OF
    | RTTipe.Builtin, RTTipe.Enum, RTTipe.Set, RTTipe.Subrange =>
      CASE fromTipe.kind OF
      | RTTipe.Kind.Address => self.addRef(RefType.UntracedRef);
      | RTTipe.Kind.Proc => self.addRef(RefType.Proc);
      | RTTipe.Kind.Refany => self.addRef(RefType.Ref);
      | RTTipe.Kind.Null => 
        (* No need to actually swap, since the only value of type NULL  is NIL,
            and it is always represented as zero.
        *) 
        CASE self.wordKind OF
	| CPKind.Copy, CPKind.Swap =>
	  <* ASSERT fromTipe.size = toTipe.size *>
	  self.addCopy(fromTipe.size);
	| CPKind.Copy32to64, CPKind.Swap32to64 =>
	  <* ASSERT fromTipe.size = 32 *> 
	  <* ASSERT toTipe.size = 64 *> 
	  self.addCopy32to64(32, signed := FALSE);
	| CPKind.Copy64to32, CPKind.Swap64to32 =>
	  <* ASSERT fromTipe.size = 64 *> 
	  <* ASSERT toTipe.size = 32 *> 
	  self.addCopy64to32(64, signed := FALSE);
        ELSE <* ASSERT FALSE *> 
	END;
      | RTTipe.Kind.Enum => 
        IF IsWidechar(fromTipe) THEN 
          <* ASSERT IsWidechar(toTipe) *> 
          (* ^But it could have different element count. *) 
          BuildWidechar (self, fromTipe, toTipe);  
(* Widechar Tipe. *) 
        ELSE 
          BuildOrdinal(self, fromTipe, toTipe, self.wordKind, signed:=FALSE);  
        END; 
      | RTTipe.Kind.Boolean, RTTipe.Kind.Char, 
        RTTipe.Kind.Set, RTTipe.Kind.Subrange, RTTipe.Kind.Cardinal => 
        BuildOrdinal(self, fromTipe, toTipe, self.wordKind, signed:=FALSE);  
      | RTTipe.Kind.Integer =>  
        BuildOrdinal(self, fromTipe, toTipe, self.wordKind, signed:=TRUE);  
      | RTTipe.Kind.Longcard =>
        BuildOrdinal(self, fromTipe, toTipe, self.longKind, signed:=FALSE);
      | RTTipe.Kind.Longint =>  
        BuildOrdinal(self, fromTipe, toTipe, self.longKind, signed:=TRUE);  
      | RTTipe.Kind.Extended, RTTipe.Kind.Longreal =>
        <* ASSERT toTipe.size = 64 *> 
        <* ASSERT fromTipe.size = 64 *> 
        CASE self.wordKind OF
	| CPKind.Copy, CPKind.Copy32to64, CPKind.Copy64to32 =>
	  self.addCopy(64);
	| CPKind.Swap, CPKind.Swap32to64, CPKind.Swap64to32 =>
	  self.addSwap64(64);
        ELSE <* ASSERT FALSE *> 
	END;
      | RTTipe.Kind.Real => 
        <* ASSERT toTipe.size = 32 *> 
        <* ASSERT fromTipe.size = 32 *> 
        CASE self.wordKind OF
	| CPKind.Copy, CPKind.Copy32to64, CPKind.Copy64to32 =>
	  self.addCopy(32);
	| CPKind.Swap, CPKind.Swap32to64, CPKind.Swap64to32 =>
	  self.addSwap32(32);
        ELSE <* ASSERT FALSE *> 
	END;
      ELSE
        RAISE Error("Builtin but not builtin.");
      END;

    | RTTipe.Packed(p) => 
      (* We should get here only if the packed value is an array element,
         since packed fields are handled in BuildFields.
         We can ignore the parent type and just create a single entry
         packedswap or a byte copy. *)
      <* ASSERT fromTipe.size = toTipe.size *> 
      CASE self.wordKind OF
      | CPKind.Copy, CPKind.Copy32to64, CPKind.Copy64to32 =>
        WITH bitsNeeded = RoundUp(p.size, 8) DO
          self.addCopy(bitsNeeded);
        END;
      | CPKind.Swap, CPKind.Swap32to64, CPKind.Swap64to32 =>
        self.addPackedFirstField
          (p.size, 
           IsWidechar(p) AND self.widecharKind = CPKind.Swap32to16,
           paKind := PackedPAKind (self.from.little_endian, TRUE) 
          );
      ELSE <* ASSERT FALSE *> 
      END;

    | RTTipe.Array(a) =>
      WITH a2 = NARROW(toTipe, RTTipe.Array) DO
        IF a.element.kind = RTTipe.Kind.Packed THEN
          IF a.element.size = 8 THEN
            (* An array of packed elements that fit exactly in a byte.  We can
               just copy this on any machine! *)
            self.addCopy(a.size);
          ELSE 
            
            CASE self.wordKind OF
            | CPKind.Copy =>
              IF IsWidechar(a.element) AND self.widecharKind=CPKind.Copy32to16
              THEN 
                self.addPackedArray
                  (a.size, a.n_elts, a.element.size, self.from.word_size,
                   TRUE, PackedPAKind(self.from.little_endian, FALSE));
              ELSE (* Same endian machines and not WIDECHAR elements,
                      => we can just copy these bytes. *)
                self.addCopy(a.size);
              END; 
            | CPKind.Copy32to64, CPKind.Copy64to32 =>
              IF IsWidechar(a.element) AND self.widecharKind=CPKind.Copy32to16
              THEN 
                self.addPackedArray
                  (a.size, a.n_elts, a.element.size,
                   32, (* Since at least one of the machines is 32-bit, and the
                          packed type is the same, the repacking will always 
                          work in 32-bit units. *) 
                   TRUE, PackedPAKind(self.from.little_endian, FALSE));
              ELSE (* Same endian machines and not WIDECHAR elements,
                      => we can just copy these bytes. *)
                self.addCopy(a.size);
              END; 
            | CPKind.Swap =>
              self.addPackedArray
                (a.size, a.n_elts, a.element.size, self.from.word_size,
                 IsWidechar(a.element) AND self.widecharKind=CPKind.Swap32to16,
                 PackedPAKind(self.from.little_endian,TRUE));
            | CPKind.Swap32to64, CPKind.Swap64to32 =>
              (* Must pack properly into the smaller 32 bit words *)
              self.addPackedArray
                (a.size, a.n_elts, a.element.size,
                 32, (* Since at least one of the machines is 32-bit, and the
                        packed type is the same, the repacking will always 
                        work in 32-bit units. *) 
                 IsWidechar(a.element) AND self.widecharKind=CPKind.Swap32to16,
                 PackedPAKind(self.from.little_endian,TRUE));
            ELSE <* ASSERT FALSE *> 
            END;
          END; 
        ELSE (* Array element is not packed. *) 
          (* Since array elements must fit in totally
             contiguous memory, we don't need to check the offsets
             when we return from buildOne *)
          FOR i := 1 TO a.n_elts DO
            self.buildOne(a.element, a2.element);
          END;
        END;
      END;

    | RTTipe.Object(o) =>
      WITH o2 = NARROW(toTipe, RTTipe.Object) DO
        <* ASSERT o.super # NIL *>
        <* ASSERT o2.super # NIL *>
        <* ASSERT o.super.typecode = o2.super.typecode *>
        (* Get the conversion program for the supertype, and append it
           onto the end of ours.  Note that we assume that we are
           operating on the data addresses of the objects, as returned
           by RTHeap.GetDataAdr().  Not the actual object address.
           This address is BYTESIZE(ADDRESS) after the actual start of
           the allocated space. *) 

        VAR fromSize, fromAlign, toSize, toAlign: INTEGER;
            fromPad, toPad: INTEGER := 0;
        BEGIN
          IF o.super.typecode = TYPECODE(ROOT) THEN
            fromSize := 0;
            toSize := 0;
            fromAlign := o.align;
            toAlign := o2.align;
          ELSIF o.super.typecode = TYPECODE(UNTRACED ROOT) THEN
            RAISE Error("UNTRACED ROOT passed to ConvertPacking");
          ELSE
            self.buildSuper(o.super.typecode, fromSize, fromAlign,
                            toSize, toAlign); 
          END;
          
          (* If the alignment increases from a supertype to a subtype,
             we may have to add some padding in the data fields.  Note
             that we have to take into account the real allocated
             space to figure the current alignment, so add that extra
             BITSIZE(ADDRESS) to the beginning. *)
          WITH fromDiff = o.field_align - fromAlign  DO
            IF fromDiff > 0 THEN
              fromPad := (fromSize + self.from.word_size) MOD o.field_align;
              IF fromPad > 0 THEN
                fromPad := o.field_align - fromPad;
              END;
            END;
          END;
          WITH toDiff = o2.field_align - toAlign DO
            IF toDiff > 0 THEN
              toPad := (toSize + self.to.word_size) MOD o2.field_align;
              IF toPad > 0 THEN
                toPad := o2.field_align - toPad;
              END;
            END;
          END;

          (* If we need to pad them, do it. *)
          self.addSkip(fromPad, toPad);
        END;
        (* Since a conversion program guarantees it reads and writes
           the proper number of bytes, we can now just continue with
           this objects fields. *)

        self.buildFields(o.fields, o.field_size, o2.fields, o2.field_size);
      END;

    | RTTipe.Record(r) =>
      WITH r2 = NARROW(toTipe, RTTipe.Record) DO
        self.buildFields(r.fields, r.size, r2.fields, r2.size);
      END;

    | RTTipe.Ref(r) =>
      IF r.traced THEN
        self.addRef(RefType.Ref);
      ELSE
        self.addRef(RefType.UntracedRef);
      END;
    | RTTipe.OpenArray => RAISE Error("OpenArray within BuildOne?");
    ELSE
    END;
  END BuildOne;

PROCEDURE PrescanPackedFields 
   ( self:T; 
     startField: RTTipe.Field;
     startOffset: INTEGER; 
   )
  : BOOLEAN = 
  (* PRE: startField is packed. 
     TRUE if startField or any following field prior to the first that begins 
     on a byte boundary needs WIDECHAR shortening. *) 

  VAR
    LField: RTTipe.Field; 
    LOffset: INTEGER; 

  BEGIN 
    IF self.widecharKind = CPKind.Swap32to16 
    THEN (* Probably won't get here in this case, but if it happens, we need 
            to extract regardless. *)
      RETURN TRUE
    ELSIF self.widecharKind = CPKind.Copy32to16 
    THEN (* See of there are any WIDECHAR fields in the group. *)  
      LField := startField; 
      LOOP
        TYPECASE LField.type OF
        | NULL => (* Shouldn't happen. *)
        | RTTipe.Packed(p) => 
          IF IsWidechar(p) 
          THEN RETURN TRUE; 
          END;
        ELSE (* Shouldn't happen. *)
        END; 
        LField := LField.next;
        IF LField = NIL 
        THEN RETURN FALSE; 
        END; 
        LOffset := startOffset + LField.offset;  
        IF LOffset MOD 8 = 0 THEN RETURN FALSE; END; 
      END; 
    ELSE RETURN FALSE;
    END; 
  END PrescanPackedFields;  

PROCEDURE BuildFields(self: T; 
                      fromField: RTTipe.Field; fromSize: INTEGER;
                      toField: RTTipe.Field; toSize: INTEGER) 
    RAISES {Error} =

  VAR fromOffset := self.fromOffset;
     toOffset := self.toOffset;
     LMustExtract : BOOLEAN := FALSE; 

  BEGIN
    WHILE fromField # NIL DO
      <* ASSERT fromField.type.kind = toField.type.kind *>
      (* Check that the last field advanced the data pointers
         correctly. *)
      WITH fromDiff = fromOffset + fromField.offset - self.fromOffset,
           toDiff = toOffset + toField.offset - self.toOffset DO
        <* ASSERT fromDiff > -8 *>
        <* ASSERT toDiff > -8 *>
        IF fromDiff < 0 THEN
          (* A data difference < 0 is ok if this field is packed. This happens
             when the previous field was packed (which advances [to|from]Offset
             to the next byte boundary) and this field is packed too (otherwise,
             its starting offset would lie at this byte boundary. *)
          <* ASSERT fromField.type.kind = RTTipe.Kind.Packed *>
          <* ASSERT toField.type.kind = RTTipe.Kind.Packed *>
          CASE self.wordKind OF
          | CPKind.Copy, CPKind.Copy32to64, CPKind.Copy64to32 =>
            (* If this field overflows the current packed byte, copy
               the next ones as required. *)
            IF LMustExtract THEN 
              self.addPackedNextField
                (fromField.type.size, 
                 fromField.offset,
                 IsWidechar(fromField.type) 
                   AND self.widecharKind = CPKind.Copy32to16,
                 paKind := PackedPAKind(self.from.little_endian,FALSE)); 
            ELSE (* Continue copying whole bytes to the next byte boundary. *)
              WITH bitsNeeded = fromField.type.size + fromDiff DO
                IF bitsNeeded > 0 THEN
                  WITH bytesNeeded = RoundUp(bitsNeeded, 8) DO
                       (* Which may copy extra bits, up to the next byte boundary. *) 
                    self.addCopy(bytesNeeded);
                  END;
                END;
              END;
            END;
          | CPKind.Swap, CPKind.Swap32to64, CPKind.Swap64to32 =>
            self.addPackedNextField
              (fromField.type.size,
               fromField.offset,
               IsWidechar(fromField.type)
                 AND self.widecharKind = CPKind.Swap32to16,
               paKind := PackedPAKind (self.from.little_endian, TRUE)); 
          ELSE <* ASSERT FALSE *> 
          END;
        ELSE (* We are already on a byte boundary. *) 
          (* If we need to add a skip, do it *)
          self.addSkip(fromDiff, toDiff);

          IF fromField.type.kind = RTTipe.Kind.Packed THEN
            (* We can ignore the word size here, size the size is
               explicit in the packing and will be the same on both
               machines, so no conversion is required. *)
            <* ASSERT fromField.type.size = toField.type.size *>
            <* ASSERT toField.type.kind = RTTipe.Kind.Packed *>
            CASE self.wordKind OF
            | CPKind.Copy, CPKind.Copy32to64, CPKind.Copy64to32 =>
              LMustExtract := PrescanPackedFields(self, fromField, fromOffset);
              IF LMustExtract THEN 
                self.addPackedFirstField
                  (fromField.type.size, 
                   IsWidechar(fromField.type) 
                     AND self.widecharKind = CPKind.Copy32to16,
                   paKind := PackedPAKind(self.from.little_endian,FALSE)); 
              ELSE (* Can copy whole bytes to the next byte boundary. *) 
                WITH bytesNeeded = RoundUp(fromField.type.size, 8) DO
                  (* Enough to get the entire packed field and up to the
                     next byte boundary. *) 
                  self.addCopy(bytesNeeded);
                END;
              END;
            | CPKind.Swap, CPKind.Swap32to64, CPKind.Swap64to32 =>
              <* ASSERT fromField.offset MOD 8 = 0 *>
              self.addPackedFirstField
                (fromField.type.size, 
                 IsWidechar(fromField.type) 
                   AND self.widecharKind = CPKind.Swap32to16,
                 paKind := PackedPAKind (self.from.little_endian, TRUE)); 
            ELSE <* ASSERT FALSE *> 
            END;
          ELSIF fromField.type.kind = RTTipe.Kind.Object THEN
            (* Treat objects, when in a field, as references.
               Otherwise we get an infinite loop trying to expand things. *)
            self.addRef(RefType.Ref);
          ELSE
            self.buildOne(fromField.type, toField.type);
          END;
        END;
      END;

      fromField := fromField.next;
      toField := toField.next;
    END;

    (* Now that the fields are done, check if there is any space after the last
       field that needs to be accounted for. *)
    WITH fromDiff = fromOffset + fromSize - self.fromOffset,
         toDiff = toOffset + toSize - self.toOffset DO
      <* ASSERT fromDiff >= 0 *>
      <* ASSERT toDiff >= 0 *>
      self.addSkip(fromDiff, toDiff);
    END;
  END BuildFields;

(***************************************************************
 * For printing 
 ***************************************************************)

PROCEDURE PrintPacking(packing: RTPacking.T) =
  BEGIN
    IO.Put(" word_size: " & Fmt.Unsigned(packing.word_size, 10) & "\n");
    IO.Put(" long_size: " & Fmt.Unsigned(packing.long_size, 10) & "\n");
    IO.Put(" widechar_size: " & Fmt.Unsigned(packing.widechar_size, 10) & "\n");
    IO.Put(" max_align: " & Fmt.Unsigned(packing.max_align, 10) & "\n");
    IO.Put(" struct_align: " & Fmt.Unsigned(packing.struct_align, 10) & "\n");
    CASE packing.float OF
    | RTPacking.FloatKind.IEEE => 
      IO.Put(" float_kind: IEEE\n");
    | RTPacking.FloatKind.VAX => 
      IO.Put(" float_kind: VAX\n");
    | RTPacking.FloatKind.other => 
      IO.Put(" float_kind: other\n");
    END;
    IO.Put(" little_endian: " & Fmt.Bool(packing.little_endian) & "\n");
  END PrintPacking;

PROCEDURE KindToText(kind: RTTipe.Kind) =
  BEGIN
    CASE kind OF
    | RTTipe.Kind.Address => IO.Put( "Address");
    | RTTipe.Kind.Array => IO.Put( "Array");
    | RTTipe.Kind.Boolean => IO.Put( "Boolean");
    | RTTipe.Kind.Cardinal => IO.Put( "Cardinal");
    | RTTipe.Kind.Char => IO.Put( "Char");
    | RTTipe.Kind.Enum => IO.Put( "Enum");
(* Widechar Tipe. *) 
    | RTTipe.Kind.Extended => IO.Put( "Extended");
    | RTTipe.Kind.Integer => IO.Put( "Integer");
    | RTTipe.Kind.Longcard => IO.Put( "Longcard");
    | RTTipe.Kind.Longint => IO.Put( "Longint");
    | RTTipe.Kind.Longreal => IO.Put( "Longreal");
    | RTTipe.Kind.Null => IO.Put( "Null");
    | RTTipe.Kind.Object => IO.Put( "Object");
    | RTTipe.Kind.OpenArray => IO.Put( "OpenArray");
    | RTTipe.Kind.Packed => IO.Put( "Packed");
    | RTTipe.Kind.Proc => IO.Put( "Proc");
    | RTTipe.Kind.Real => IO.Put( "Real");
    | RTTipe.Kind.Record => IO.Put( "Record");
    | RTTipe.Kind.Ref => IO.Put( "Ref");
    | RTTipe.Kind.Refany => IO.Put( "Refany");
    | RTTipe.Kind.Set => IO.Put( "Set");
    | RTTipe.Kind.Subrange => IO.Put( "Subrange");
    | RTTipe.Kind.UntracedRef => IO.Put( "UntracedRef");
    END;
  END KindToText; 

PROCEDURE BasicTipeToText(tipe: RTTipe.T) =
  BEGIN
    IO.Put( "kind: " );
    KindToText(tipe.kind);
    IO.Put( ", size: " & Fmt.Int(tipe.size DIV 8) & "+" & 
      Fmt.Int(tipe.size MOD 8) & ", align: " & Fmt.Int(tipe.align));
  END BasicTipeToText; 

PROCEDURE FieldsToText(f: RTTipe.Field; pre: TEXT; 
                       packing: RTPacking.T) = 
  BEGIN
    IF f = NIL THEN
      IO.Put( pre & "<NO FIELDS>");
      RETURN;
    END;
    IO.Put(pre & "offset: " & Fmt.Int(f.offset DIV 8) & "+" &
        Fmt.Int(f.offset MOD 8) & "\n");
    TipeToText(f.type, "  " & pre, packing);
    f := f.next;
    WHILE f # NIL DO
      IO.Put("\n" & pre & "offset: " & Fmt.Int(f.offset DIV 8) & "+" &
        Fmt.Int(f.offset MOD 8) & "\n");
      TipeToText(f.type, "  " & pre, packing);
      f := f.next;
    END;
  END FieldsToText;

CONST HexDigits = 2*ADRSIZE(INTEGER); 

PROCEDURE DoubleIntToText(Val: RTTipe.DoubleInt):TEXT = 
  BEGIN (* Just do it in hex--it's easier. *) 
    IF Val.Lo >= 0 AND Val.Hi # 0 OR Val.Lo < 0 AND Val.Hi # -1 
    THEN (* Hi is not just a sign extension. *) 
      RETURN "16_" 
             & Fmt.Pad(Fmt.Unsigned(Val.Hi,16),HexDigits,'0') 
             & Fmt.Pad(Fmt.Unsigned(Val.Lo,16),HexDigits,'0');
    ELSE 
      RETURN "16_" & Fmt.Pad(Fmt.Unsigned(Val.Lo,16),HexDigits,'0');
    END;  
  END DoubleIntToText; 
  
PROCEDURE TipeToText(tipe: RTTipe.T; pre: TEXT; packing: RTPacking.T) =
  BEGIN
    IF tipe = NIL THEN
      IO.Put(pre & "<NIL>");
      RETURN;
    END;
    IO.Put(pre);
    BasicTipeToText(tipe);

    TYPECASE tipe OF
    | RTTipe.Builtin =>
      IO.Put(" (Builtin)");
    | RTTipe.Array(a) =>
      IO.Put(" (packed elements " & Fmt.Int(a.elt_pack) & ") [1.." & 
             Fmt.Int(a.n_elts) & "] of \n");
      TipeToText(a.element, pre & "  ", packing);
    | RTTipe.Enum(e) =>
      IO.Put(" of " & Fmt.Int(e.n_elts) & " elements");
(* Widechar Tipe. *) 
    | RTTipe.Object(o) =>
      IO.Put(":\n" & pre & "  Super Type = ");
      IF o.super # NIL THEN
        IO.Put("typecode " & Fmt.Int(o.super.typecode) & "\n");
        TipeToText(RTTipe.Get(o.super.typecode, packing),
                   pre & "    ", packing);
      ELSE
        IO.Put(" <NIL> type?\n");
      END;
      IO.Put("\n" & pre & "  Fields.  size: " & 
        Fmt.Int(o.field_size DIV 8) & "+" & 
        Fmt.Int(o.field_size MOD 8) & ", align: " & 
        Fmt.Int(o.field_align) & "\n");
      FieldsToText(o.fields, "    " & pre, packing);
    | RTTipe.OpenArray(oa) =>
      IO.Put(" in " & Fmt.Int(oa.n_dimensions) & 
        " dimensions, packed " & Fmt.Int(oa.elt_pack) & " of \n"); 
      TipeToText(oa.element, pre & "  ", packing);
    | RTTipe.Packed(p) =>
      IO.Put(" in " & Fmt.Int(p.n_bits) & " bits of\n");
      TipeToText(p.base, "  " & pre, packing);
    | RTTipe.Record(r) =>
      IO.Put(" of:\n");
      FieldsToText(r.fields, "  " & pre, packing);
    | RTTipe.Ref(r) =>
      IO.Put(" (traced: " & Fmt.Bool(r.traced) & ", uid: " &
             Fmt.Int(r.uid) & ") to ");
      IF r.self # NIL THEN
        IO.Put("typecode " & Fmt.Int(r.self.typecode) & "\n");
      ELSE
        IO.Put("<NIL> type?\n");
      END;
    | RTTipe.Set(s) =>
      IO.Put(" of " & Fmt.Int(s.n_elts) & " elements");
    | RTTipe.Subrange(s) =>
      IO.Put(" from " & DoubleIntToText(s.min) 
             & " to " & DoubleIntToText(s.max)
            );
    ELSE
    END;
  END TipeToText; 

PROCEDURE Print(self: T) =
  BEGIN
    IO.Put("Converting from packing:\n");
    PrintPacking(self.from);
    IO.Put("to packing:\n");
    PrintPacking(self.to);
    IO.Put("Converting from RTTipe:\n");
    TipeToText(self.fromTipe, "  ", self.from);
    IO.Put("\nto RTTipe:\n");
    TipeToText(self.toTipe, "  ", self.to);
    self.printProgram();
  END Print;

PROCEDURE PackedOperation 
  (kind: [PklAction.PAKind.SwapPackedLEtoBE..PklAction.PAKind.CopyPackedBE])
  : TEXT = 

  BEGIN 
    CASE kind OF 
    | PklAction.PAKind.SwapPackedLEtoBE => 
      RETURN "swap packed, little- to big-endian ";
    | PklAction.PAKind.SwapPackedBEtoLE => 
      RETURN "swap packed, big- to little-endian ";
    | PklAction.PAKind.CopyPackedLE => 
      RETURN "copy packed, little-endian ";
    | PklAction.PAKind.CopyPackedBE => 
      RETURN "copy packed, big-endian ";
    END; 
  END PackedOperation;

PROCEDURE PrintProgram(self: T) =
  VAR fromByteCt := 0;
      toByteCt := 0;
      DoPrintFootnote: BOOLEAN; 
  BEGIN    
    IO.Put("\nDoing it in " & Fmt.Int(self.prog.size()) & " step(s):\n");
    FOR i := 0 TO self.prog.size() - 1 DO
      WITH elem = self.prog.get(i) DO
        CASE elem.kind OF
        | PklAction.PAKind.Copy => 
          IO.Put(" Copy " & Fmt.Int(elem.unitCt) & " byte(s).\n");
          INC(fromByteCt, elem.unitCt);
          INC(toByteCt, elem.unitCt);
        | PklAction.PAKind.SwapPackedLEtoBE, PklAction.PAKind.SwapPackedBEtoLE, 
          PklAction.PAKind.CopyPackedLE, PklAction.PAKind.CopyPackedBE => 
          WITH nelem = NARROW(elem, PklAction.Packed) DO
            IO.Put(PackedOperation(elem.kind)); 
            IO.Put(Fmt.Int(elem.unitCt) & 
              " unit(s) of " & Fmt.Int(nelem.size) & 
              " byte(s) with packed bitfields: ");
            DoPrintFootnote := FALSE; 
            FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
              IO.Put(Fmt.Int(nelem.field[i]));
              IF i IN nelem.widecharFieldSet 
              THEN
                IO.Put("* ");
                DoPrintFootnote := TRUE; 
              ELSE
                IO.Put(" ");
              END; 
            END;
            IO.Put("\n");
            IF DoPrintFootnote THEN
              IO.Put("( * Shorten this WIDECHAR packed field.)"); 
            END; 
            INC(fromByteCt, elem.unitCt*nelem.size);
            INC(toByteCt, elem.unitCt*nelem.size);
(* TODO: Print widecharFieldSet. *) 
          END;
        | PklAction.PAKind.SkipFrom =>
          IO.Put(" Skip " & Fmt.Int(elem.unitCt) & " src byte(s).\n");
          INC(fromByteCt, elem.unitCt);
        | PklAction.PAKind.SkipTo =>
          IO.Put(" Skip " & Fmt.Int(elem.unitCt) & " dst byte(s).\n");
          INC(toByteCt, elem.unitCt);
        | PklAction.PAKind.Skip =>
          IO.Put(" Skip " & Fmt.Int(elem.unitCt) & " src and dst byte(s).\n");
          INC(fromByteCt, elem.unitCt);
          INC(toByteCt, elem.unitCt);
        | PklAction.PAKind.Swap16 =>
          IO.Put(" Swap " & Fmt.Int(elem.unitCt) & " 16-bit word(s).\n");
          INC(fromByteCt, elem.unitCt*2);
          INC(toByteCt, elem.unitCt*2);
        | PklAction.PAKind.Swap32 =>
          IO.Put(" Swap " & Fmt.Int(elem.unitCt) & " 32-bit word(s).\n");
          INC(fromByteCt, elem.unitCt*4);
          INC(toByteCt, elem.unitCt*4);
        | PklAction.PAKind.Swap64 =>
          IO.Put(" Swap " & Fmt.Int(elem.unitCt) & " 64-bit word(s).\n");
          INC(fromByteCt, elem.unitCt*8);
          INC(toByteCt, elem.unitCt*8);
        | PklAction.PAKind.Copy32to64 =>
          IO.Put(" Copy " & Fmt.Int(elem.unitCt) & " 32 to 64-bit ");
          WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
            IF nelem.signed THEN
              IO.Put("signed ");
            ELSE
              IO.Put("unsigned ");
            END;
          END;
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*4);
          INC(toByteCt, elem.unitCt*8);
        | PklAction.PAKind.Copy64to32 =>
          IO.Put(" Copy " & Fmt.Int(elem.unitCt) & " 64 to 32-bit ");
          WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
            IF nelem.signed THEN
              IO.Put("signed ");
            ELSE
              IO.Put("unsigned ");
            END;
          END;
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*8);
          INC(toByteCt, elem.unitCt*4);
        | PklAction.PAKind.Copy16to32 =>
          IO.Put(" Copy " & Fmt.Int(elem.unitCt) & " 16 to 32-bit ");
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*2);
          INC(toByteCt, elem.unitCt*4);
        | PklAction.PAKind.Swap16to32 =>
          IO.Put(" Swap " & Fmt.Int(elem.unitCt) & " 16 to 32-bit ");
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*2);
          INC(toByteCt, elem.unitCt*4);
        | PklAction.PAKind.Copy32to16 =>
          IO.Put(" Copy " & Fmt.Int(elem.unitCt) & " 32 to 16-bit ");
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*4);
          INC(toByteCt, elem.unitCt*2);
        | PklAction.PAKind.Swap32to16 =>
          IO.Put(" Swap " & Fmt.Int(elem.unitCt) & " 32 to 16-bit ");
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*4);
          INC(toByteCt, elem.unitCt*2);
        | PklAction.PAKind.CopyWC21to32 =>
          IO.Put(" Copy " & Fmt.Int(elem.unitCt) & " WC21 to 32-bit ");
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*4);
          INC(toByteCt, elem.unitCt*4);
        | PklAction.PAKind.CopyWC21to16 =>
          IO.Put(" Copy " & Fmt.Int(elem.unitCt) & " WC21 to 16-bit ");
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*4);
          INC(toByteCt, elem.unitCt*2);
        | PklAction.PAKind.Swap32to64 =>
          IO.Put(" Copy and Swap " & Fmt.Int(elem.unitCt) & " 32 to 64-bit ");
          WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
            IF nelem.signed THEN
              IO.Put("signed ");
            ELSE
              IO.Put("unsigned ");
            END;
          END;
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*4);
          INC(toByteCt, elem.unitCt*8);
        | PklAction.PAKind.Swap64to32 =>
          IO.Put(" Copy and Swap " & Fmt.Int(elem.unitCt) & " 64 to 32-bit "); 
          WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
            IF nelem.signed THEN
              IO.Put("signed ");
            ELSE
              IO.Put("unsigned ");
            END;
          END;
          IO.Put("word(s).\n");
          INC(fromByteCt, elem.unitCt*8);
          INC(toByteCt, elem.unitCt*4);
        | PklAction.PAKind.ReadRef =>
          IO.Put(" Read " & Fmt.Int(elem.unitCt));
          WITH nelem = NARROW(elem, PklAction.Ref) DO
            CASE nelem.refType OF
            | PklAction.RefType.Ref => IO.Put(" traced");
            | PklAction.RefType.UntracedRef => IO.Put(" untraced");
            | PklAction.RefType.Proc => IO.Put(" procedure");
            END;
          END;
          IO.Put(" references(s).\n");
          INC(fromByteCt, elem.unitCt*(self.from.word_size DIV 8));
          INC(toByteCt, elem.unitCt*(self.to.word_size DIV 8));
        | PklAction.PAKind.Done =>
          IO.Put("Copied " & Fmt.Int(fromByteCt) & " bytes to " &
            Fmt.Int(toByteCt) & "\n");
        END;
      END;
    END;
  END PrintProgram;

BEGIN
  (* This will only work on machines with 8 bit chars right now. *)
  <* ASSERT BITSIZE(CHAR) = 8 *>
END ConvertPacking.
