(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Jun  2 16:28:54 PDT 1994 by mcjones    *)

UNSAFE MODULE DCRS;

IMPORT Atom, AtomList, Chf, Cstring, M3toC, Img, Irs, Rd, Rect, Text,
       TextF, TextList, Thread, Word;

(* IMPORT FileWr, Fmt, Wr;  *** Debugging only *)

FROM Ctypes IMPORT char_star, int, unsigned_char, unsigned_char_star,
                   unsigned_long, unsigned_long_star;

TYPE unsigned_byte = unsigned_char;

(* Counted string *)

PROCEDURE CopyBufferToText(buffer: char_star; buffer_size: unsigned_long)
  : TEXT =
  BEGIN
    WITH t = NEW(TEXT, buffer_size+1) DO
      EVAL Cstring.memcpy(ADR(t[0]), buffer, buffer_size);
      t[buffer_size] := '\000';
      RETURN t
    END
  END CopyBufferToText;

VAR mutex := NEW(MUTEX); (* protects all global variables, plus DCRS library *)

(* Condition handling facility *)

VAR
  error := FALSE;
  errorArgs: int;
  errorName: int;
  errorArg1: int;

(* Bracket each call to a procedure in the "Img" or "Irs" interfaces
   with the following boilerplate:

    EVAL Chf.Establish(Handler); error := FALSE;
      <call "Img" or "Irs" procedure>
    EVAL Chf.Revert();
    IF NOT error THEN <handle condition errorName(errorArg1, ...) END;

   Do NOT use Modula-3 exceptions or even TRY-FINALLY within such a
   sequence, because the stack is in a wierd state.
*)

PROCEDURE Handler(
  sig_args: Chf.SigArg_star;
  mch_args: Chf.MchArg_star): int =
  VAR depth := mch_args.depth;
  BEGIN
    error := TRUE;
    errorArgs := sig_args.args;
    errorName := sig_args.name;
    errorArg1 := sig_args.arg1;
    EVAL Chf.Unwind(ADR(depth), NIL);
    RETURN Chf.ActionUnwind
  END Handler;


(* DictionaryContext and global Irs state *)

REVEAL
  DictionaryContext = OBJECT METHODS
    init(
        primary := Language.English;
        userDictPathnames: TextList.T := NIL)
      : DictionaryContext
  END BRANDED OBJECT
    primary_dict: unsigned_long;
    user_dict_list: char_star_array
  OVERRIDES
    init := DCInit
  END;

TYPE char_star_array = UNTRACED REF ARRAY OF char_star;

VAR
  currentDictionary: DictionaryContext := NIL;
  currentDictContext: unsigned_long;

PROCEDURE DCInit(
    dc: DictionaryContext;
    primary := Language.English;
    userDictPathnames: TextList.T := NIL) : DictionaryContext =
  BEGIN
    <* ASSERT primary = Language.English *>
    dc.primary_dict := Irs.K_EnglishDict;
    WITH n = TextList.Length(userDictPathnames) DO
      dc.user_dict_list := NEW(char_star_array, n+1);
      FOR i := 0 TO n-1 DO
        dc.user_dict_list[i] :=
          M3toC.CopyTtoS(TextList.Nth(userDictPathnames, i))
      END;
      dc.user_dict_list[n] := NIL
    END;
    RETURN dc
  END DCInit;


(* Image page *)

REVEAL
  T = BRANDED REF RECORD
    state := State.New;
    allWhite: BOOLEAN := FALSE; (* if state<Closed *)
    resolution: Resolution;
    fid: unsigned_long; (* if state<Closed *)
    seg: unsigned_long; (* if state=Segmented *)
    recog: unsigned_long (* if state=Recognized AND NOT allWhite *)
  END;

VAR (*CONST*) DDIFFile := AtomList.List1(Atom.FromText("DCRS.DDIFFile"));

PROCEDURE FromDDIF(
    pathname: TEXT; resolution := Resolution.DPI300)
  : T RAISES {Error} =
  VAR
    i := NEW(T);
    ctx: unsigned_long;
    compressionType, retLen: unsigned_long;
  BEGIN
    LOCK mutex DO

      EVAL Chf.Establish(Handler); error := FALSE;
      ctx := Img.OpenFile(
        Img.K_ModeImport,
        Img.K_FtypeDDIF,
        Text.Length(pathname),
        M3toC.TtoS(pathname),
        NIL,
        0);
      EVAL Chf.Revert();
      IF error THEN RAISE Error(DDIFFile) END;

      EVAL Chf.Establish(Handler); error := FALSE;
      i.fid := Img.ImportFrame(ctx, 0);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      EVAL Chf.Establish(Handler); error := FALSE;
      EVAL Img.CloseFile(ctx, 0);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      i.resolution := resolution;

      EVAL Chf.Establish(Handler); error := FALSE;
      EVAL Img.Get(i.fid, Img.CompressionType, ADR(compressionType),
	BYTESIZE(compressionType), ADR(retLen), 0);
      EVAL Chf.Revert();
      <* ASSERT retLen = BYTESIZE(compressionType) *>
      <* ASSERT NOT error *>

      IF compressionType # Img.K_PcmCompression THEN
	EVAL Chf.Establish(Handler); error := FALSE;
	EVAL Img.DecompressFrame(i.fid, Img.M_InPlace, NIL);
	EVAL Chf.Revert();
	<* ASSERT NOT error *>
      END;

      RETURN i
    END
  END FromDDIF;

VAR (*CONST*) PBMFormat := AtomList.List1(Atom.FromText("DCRS.PBMFormat"));

PROCEDURE FromPBM(
    rd: Rd.T; resolution := Resolution.DPI300): T
  RAISES {Rd.Failure, Thread.Alerted, Error} =
  CONST NullFrame = 0;
  VAR
    i := NEW(T);
    height, width, widthInBytes: INTEGER;
    binary: BOOLEAN;
    itmlst: ARRAY [0..5] OF Img.ITMLST;
    alignment: unsigned_long;
    scanline_stride: unsigned_long;
    data_plane: unsigned_char_star;
    zero: unsigned_long := 0;
  BEGIN
    LOCK mutex DO

      (* Read header (see the pbm(5) manpage). *)
      VAR class, mode: CHAR;
      BEGIN
	TRY
	  class := Rd.GetChar(rd);
	  mode := Rd.GetChar(rd);
	EXCEPT Rd.EndOfFile => RAISE Error(PBMFormat)
	END;
	width := ScanInt(rd);
	widthInBytes := (width+7) DIV 8; (* round up *)
	height := ScanInt(rd);
	IF class = 'P' THEN
	  CASE mode OF
	  | '1' => binary := FALSE
	  | '4' => binary := TRUE
	  ELSE RAISE Error(PBMFormat)
	  END
	ELSE RAISE Error(PBMFormat)
	END
      END;

      WITH itm = itmlst[0] DO
	itm.Code := Img.PixelsPerLine;
	itm.Length := BYTESIZE(width);
	itm.Buffer := ADR(width);
	itm.Retlen := NIL;
	itm.Index := 0
      END;
      WITH itm = itmlst[1] DO
	itm.Code := Img.NumberOfLines;
	itm.Length := BYTESIZE(height);
	itm.Buffer := ADR(height);
	itm.Retlen := NIL;
	itm.Index := 0
      END;
      alignment := Img.K_AlignByte;
      WITH itm = itmlst[2] DO
	itm.Code := Img.ScanlineAlignment;
	itm.Length := BYTESIZE(alignment);
	itm.Buffer := ADR(alignment);
	itm.Retlen := NIL;
	itm.Index := 0
      END;
      scanline_stride := widthInBytes * 8; (* convert to bits *)
      WITH itm = itmlst[3] DO
	itm.Code := Img.ScanlineStride;
	itm.Length := BYTESIZE(scanline_stride);
	itm.Buffer := ADR(scanline_stride);
	itm.Retlen := NIL;
	itm.Index := 0
      END;
      WITH itm = itmlst[4] DO
	itm.Code := Img.DataOffset;
	itm.Length := BYTESIZE(zero);
	itm.Buffer := ADR(zero);
	itm.Retlen := NIL;
	itm.Index := 0
      END;
      WITH itm = itmlst[5] DO
	itm.Code := 0;
	itm.Length := 0;
	itm.Buffer := NIL;
	itm.Retlen := NIL;
	itm.Index := 0
      END;

      EVAL Chf.Establish(Handler); error := FALSE;
      i.fid := Img.AllocateFrame(
	Img.K_ClassBitonal,
	ADR(itmlst[0]),
	NullFrame,
	Img.M_NoDataPlaneAlloc);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      i.resolution := resolution;

      (* Allocate enough space for byte-aligned scanlines. *)
      EVAL Chf.Establish(Handler); error := FALSE;

      data_plane := Img.AllocDataPlane(height * widthInBytes, 0, 0);

      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      (* Read bits into "data_plane". *)
      i.allWhite := NOT GetPBMBits(rd, binary, height, width, data_plane);

      EVAL Chf.Establish(Handler); error := FALSE;
      EVAL Img.AttachDataPlane(i.fid, data_plane, 0);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      EVAL Chf.Establish(Handler); error := FALSE;
      Img.VerifyFrame(i.fid, 0);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      RETURN i
    END
  END FromPBM;

(* *** Debugging only
PROCEDURE CheckDataPlane(i: T) =
  VAR
    itmlst: ARRAY [0..3] OF Img.ITMLST;
    data_plane: unsigned_char_star;
    size, offset: unsigned_long;
  BEGIN
    WITH itm = itmlst[0] DO
      itm.Code := Img.DataPlaneBase;
      itm.Length := BYTESIZE(data_plane);
      itm.Buffer := ADR(data_plane);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[1] DO
      itm.Code := Img.DataPlaneSize;
      itm.Length := BYTESIZE(size);
      itm.Buffer := ADR(size);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[2] DO
      itm.Code := Img.DataOffset;
      itm.Length := BYTESIZE(offset);
      itm.Buffer := ADR(offset);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[3] DO
      itm.Code := 0;
      itm.Length := 0;
      itm.Buffer := NIL;
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    EVAL Chf.Establish(Handler); error := FALSE;
    EVAL Img.GetFrameAttributes(i.fid, ADR(itmlst[0]));
    EVAL Chf.Revert();
    <* ASSERT NOT error *>
    <*ASSERT data_plane = pDebug*>
    <*ASSERT offset = 0*>
    <*ASSERT size = sDebug*>
  END CheckDataPlane;

PROCEDURE DumpDataPlane(i: T; pn: TEXT) =
  VAR
    itmlst: ARRAY [0..5] OF Img.ITMLST;
    width, height: INTEGER;
    data_plane: unsigned_char_star;
    scanline_stride: INTEGER;
  BEGIN
    CheckDataPlane(i);
    WITH itm = itmlst[0] DO
      itm.Code := Img.PixelsPerLine;
      itm.Length := BYTESIZE(width);
      itm.Buffer := ADR(width);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[1] DO
      itm.Code := Img.NumberOfLines;
      itm.Length := BYTESIZE(height);
      itm.Buffer := ADR(height);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[2] DO
      itm.Code := Img.NumberOfLines;
      itm.Length := BYTESIZE(height);
      itm.Buffer := ADR(height);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[3] DO
      itm.Code := Img.DataPlaneBase;
      itm.Length := BYTESIZE(data_plane);
      itm.Buffer := ADR(data_plane);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[4] DO
      itm.Code := Img.ScanlineStride;
      itm.Length := BYTESIZE(scanline_stride);
      itm.Buffer := ADR(scanline_stride);
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    WITH itm = itmlst[5] DO
      itm.Code := 0;
      itm.Length := 0;
      itm.Buffer := NIL;
      itm.Retlen := NIL;
      itm.Index := 0
    END;
    EVAL Chf.Establish(Handler); error := FALSE;
    EVAL Img.GetFrameAttributes(i.fid, ADR(itmlst[0]));
    EVAL Chf.Revert();
    <* ASSERT NOT error *>
    <* ASSERT scanline_stride MOD 8 = 0*>

    WritePBM(pn, data_plane, width, height, scanline_stride)
  END DumpDataPlane;

PROCEDURE WritePBM(
    pn: TEXT;
    data_plane: unsigned_char_star;
    width, height, scanline_stride: INTEGER) =
  VAR wr: Wr.T; p: unsigned_char_star;
  BEGIN
    wr := FileWr.Open("/tmp/," & pn & ".pbm");
    Wr.PutText(wr, "P4\n" & Fmt.Int(width) & " " & Fmt.Int(height) & "\n");
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO (width + 7) DIV 8 - 1 DO
        p := LOOPHOLE(
          LOOPHOLE(data_plane, INTEGER)+v*(scanline_stride DIV 8) + h,
          unsigned_char_star);
        Wr.PutChar(wr, VAL(byteRev[p^], CHAR))
      END
    END;
    Wr.Close(wr)
  END WritePBM;
*)

VAR byteRev: ARRAY [0..255] OF unsigned_char;

PROCEDURE GetPBMBits(
    rd: Rd.T;
    binary: BOOLEAN; height, width: INTEGER; bits: unsigned_char_star)
  : BOOLEAN RAISES {Rd.Failure, Thread.Alerted, Error} =
(* Read body of pbm file into "bits", and return "TRUE" if at least one
   black bit was encountered. *)
  VAR
    scanLine: REF ARRAY OF CHAR;
    bits_i: unsigned_char_star;
    byte: unsigned_char;
    black: unsigned_char := 0;
  BEGIN
    TRY
      IF binary THEN 
        scanLine := NEW(REF ARRAY OF CHAR, (width+7) DIV 8);
        WHILE Rd.GetChar(rd) # '\n' DO END
          (* *** or just skip one (whitespace) character? *)
      END;
      bits_i := bits;
      FOR v := 0 TO height - 1 DO
	IF binary THEN
          WITH n = Rd.GetSub(rd, scanLine^) DO
             IF n < NUMBER(scanLine^) THEN
               RAISE Error(PBMFormat)
             END
          END;
	  FOR hdiv8 := 0 TO LAST(scanLine^) DO
	    byte := byteRev[ORD(scanLine[hdiv8])];
	    bits_i^ := byte;
            black := Word.Or(black, byte);
	    bits_i := LOOPHOLE(1+LOOPHOLE(bits_i, Word.T), unsigned_char_star)
	  END
	ELSE
          FOR hdiv8 := 0 TO width DIV 8 - 1 DO
            byte := 0;
            FOR hmod8 := 0 TO 7 DO
              byte := Word.Or(byte, Word.Shift(ScanBit(rd), hmod8))
            END;
            bits_i^ := byte;
            black := Word.Or(black, byte);
            bits_i := LOOPHOLE(1+LOOPHOLE(bits_i, Word.T), unsigned_char_star);
	  END;
          byte := 0;
          FOR hmod8 := 0 TO width MOD 8 - 1 DO
            byte := Word.Or(byte, Word.Shift(ScanBit(rd), hmod8))
          END;
          bits_i^ := byte;
          black := Word.Or(black, byte);
          bits_i := LOOPHOLE(1+LOOPHOLE(bits_i, Word.T), unsigned_char_star)
	END
      END;
    <* ASSERT bits_i - bits = height*((width+7) DIV 8) *>
    EXCEPT Rd.EndOfFile => RAISE Error(PBMFormat)
    END;
    RETURN black # 0
  END GetPBMBits;

PROCEDURE ScanInt(rd: Rd.T): INTEGER
  RAISES{Thread.Alerted, Rd.Failure, Error} =
  (* Return value of next integer in "rd"; skip any whitespace or
     comments before the first digit. *)
  (* Stolen from Steve Glassman's Image.m3 by way of Andrew Birrell's ImageRd. *)
  CONST
    Digits = SET OF CHAR{'0'.. '9'};
    Spaces  = SET OF CHAR{' ', '\t', '\n', '\r'};
    Comment = '#';
  VAR
    res: INTEGER;
    ch: CHAR;
  BEGIN
    TRY
      ch := Rd.GetChar(rd);
      WHILE (ch = Comment) OR (ch IN Spaces) DO
        IF ch = Comment THEN EVAL Rd.GetLine(rd) END;
        ch := Rd.GetChar(rd)
      END;
    EXCEPT Rd.EndOfFile => RAISE Error(PBMFormat)
    END;
    IF ch IN Digits THEN
      res := ORD(ch) - ORD('0')
    ELSE
      RAISE Error(PBMFormat)
    END;
    TRY
      LOOP
        ch := Rd.GetChar(rd);
        IF ch IN Digits THEN
          res := 10 * res + ORD(ch) - ORD('0')
        ELSE
          EXIT
        END
      END;
    EXCEPT Rd.EndOfFile => (*SKIP*)
    END;
    Rd.UnGetChar(rd);
    RETURN res
  END ScanInt;

PROCEDURE ScanBit(rd: Rd.T): [0..1]
  RAISES{Thread.Alerted, Rd.Failure, Error} =
  (* Return value of next integer, 0 or 1, in "rd"; skip any whitespace or
     comments before the significant digit. *)
  CONST
    Spaces = SET OF CHAR{' ', '\t', '\n', '\r'};
    Comment = '#';
  BEGIN
    TRY
      LOOP
        WITH ch = Rd.GetChar(rd) DO
          IF ch IN SET OF CHAR{'0'..'1'} THEN RETURN ORD(ch) - ORD('0')
          ELSIF ch IN Spaces THEN (*SKIP*)
          ELSIF ch = Comment THEN EVAL Rd.GetLine(rd)
          ELSE RAISE Error(PBMFormat)
          END
        END
      END
    EXCEPT Rd.EndOfFile => RAISE Error(PBMFormat)
    END
  END ScanBit;

PROCEDURE Segment(
    i: T;
    READONLY regionOfInterest: Rect.T := Rect.Full;
    bypassSegmentation := FALSE)
  RAISES {Error} =
  BEGIN
    LOCK mutex DO DoSegment(i, regionOfInterest, bypassSegmentation) END
  END Segment;

PROCEDURE DoSegment(
    i: T;
    READONLY regionOfInterest: Rect.T := Rect.Full;
    bypassSegmentation := FALSE)
  RAISES {Error} =
  CONST ResFromResolution = 
    ARRAY Resolution OF unsigned_long{
      Irs.K_Res200, Irs.K_Res300, Irs.K_Res400};
  VAR
    roi: unsigned_long;
    roi_rect: Img.ROI_RECT;
    itmlst: ARRAY [0..1] OF Img.ITMLST;
    resolution: unsigned_long;
    flags: unsigned_long;
  BEGIN
    <* ASSERT i.state = State.New *>

    roi := 0;
    IF regionOfInterest # Rect.Full THEN
      roi_rect.Ulx := regionOfInterest.west;
      roi_rect.Uly := regionOfInterest.north;
      roi_rect.Pxls := Rect.HorSize(regionOfInterest);
      roi_rect.Scnlns := Rect.VerSize(regionOfInterest);
      WITH itm = itmlst[0] DO
	itm.Code := Img.RoiRectangle;
	itm.Length := BYTESIZE(Img.ROI_RECT);
	itm.Buffer := ADR(roi_rect);
	itm.Retlen := NIL;
	itm.Index := 0
      END;
      WITH itm = itmlst[1] DO
	itm.Code := 0;
	itm.Length := 0;
	itm.Buffer := NIL;
	itm.Retlen := NIL;
	itm.Index := 0
      END;
      EVAL Chf.Establish(Handler); error := FALSE;
      roi := Img.CreateRoiDef(ADR(itmlst[0]), 0);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>
    END;
    resolution := ResFromResolution[i.resolution];
    flags := 0;
    IF bypassSegmentation THEN flags := Irs.K_BypassSegmentation END;
<* ASSERT roi = 0 *>
<* ASSERT resolution = Irs.K_Res300 *>
    EVAL Chf.Establish(Handler); error := FALSE;
    i.seg := Irs.SegmentRegion(
      i.fid, roi, resolution, flags);
    EVAL Chf.Revert();
    IF error THEN
      <* ASSERT errorName = Irs.X_LICNOTREG *>
      RAISE Error(NotLicensed)
    END;
    IF roi # 0 THEN
      EVAL Chf.Establish(Handler); error := FALSE;
      Img.DeleteRoiDef(roi);
      EVAL Chf.Revert();
      <*ASSERT NOT error *>
    END;

    IF i.seg = 0 THEN RAISE Error(NoSegmentsFound) END;
    i.state := State.Segmented
  END DoSegment;

VAR
  NoSegmentsFound := AtomList.List1(Atom.FromText("DCRS.NoSegmentsFound"));
  NotLicensed := AtomList.List1(Atom.FromText("DCRS.LicenseNotRegistered"));

PROCEDURE FilterSegments(i: T; s: SegmentFilter) =
  VAR r0, r: Irs.RegionList_star;
  BEGIN
    LOCK mutex DO
      <* ASSERT i.state = State.Segmented *> (* else client error *)
      <* ASSERT i.seg # 0 *>                 (* else impl error *)

      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.GetRegionList(i.seg, ADR(r0));
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      r := r0;
      WHILE r.RegType # 0 DO
	IF NOT s.keep(
	    r.RegNum,
	    r.BlockNum, 
	    Rect.FromEdges(
              r.BlockLLX, r.BlockURX+1, r.BlockURY, r.BlockLLY+1),
	    r.RegType = Irs.K_TextRegion) THEN
	  EVAL Chf.Establish(Handler); error := FALSE;
	  Irs.DeleteRegion(i.seg, r.RegNum);
	  EVAL Chf.Revert();
	  <* ASSERT NOT error *>
	END;
	r := LOOPHOLE(
	  LOOPHOLE(r, Word.T) + BYTESIZE(Irs.RegionList),
	  Irs.RegionList_star)
      END;

      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.DeleteBuffer(LOOPHOLE(r0, unsigned_long));
      EVAL Chf.Revert();
      <* ASSERT NOT error *>
    END
  END FilterSegments;

PROCEDURE Recognize(
    i: T;
    c: CharSet := NIL;
    d: DictionaryContext := NIL;
    forceMonospace := FALSE) 
  RAISES {Error} =
  BEGIN
    LOCK mutex DO DoRecognize(i, c, d, forceMonospace) END
  END Recognize;

PROCEDURE DoRecognize(
    i: T;
    c: CharSet := NIL;
    d: DictionaryContext := NIL;
    forceMonospace := FALSE) 
  RAISES {Error} =
  VAR
    language: unsigned_byte;
    user_charset: user_charset_type;
    flags: unsigned_long;
  BEGIN
    <* ASSERT i.state < State.Recognized *> (* else client error *)
    IF i.state < State.Segmented THEN DoSegment(i) END;
    <* ASSERT i.seg # 0 *>                  (* else impl error *)
    flags := 0;
    IF forceMonospace THEN flags := Irs.M_ForceMonospaceRecog END;
    language := Irs.K_IsoLatin1;
    user_charset := NIL;
    IF c # NIL THEN
      language := c.language;
      IF c.language = Irs.K_Custom THEN
	user_charset := c.user_charset
      END
    END;
    IF d # currentDictionary THEN
      IF currentDictionary # NIL THEN
	EVAL Chf.Establish(Handler); error := FALSE;
	Irs.DeleteStruct(currentDictContext);
	EVAL Chf.Revert();
	<* ASSERT NOT error *>
      END;

      <* ASSERT FALSE *> (* *** Consider GC issues *)

      EVAL Chf.Establish(Handler); error := FALSE;
      currentDictContext := Irs.CreateDictContext(
	d.primary_dict, ADR(d.user_dict_list[0]), 0);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      <* ASSERT currentDictContext # 0 *> (* or raise exception? *)

      currentDictionary := d
    END;

    EVAL Chf.Establish(Handler); error := FALSE;
    i.recog := Irs.RecognizeText(
		 i.seg,
		 language,
		 LOOPHOLE(user_charset, unsigned_long_star),
		 flags);
    EVAL Chf.Revert();
    i.seg := 0; (* *** only if NOT error? *)
    IF error THEN
      <* ASSERT errorName = Irs.X_IMGTOOCMPLX *>
      i.recog := 0;
      IF NOT i.allWhite THEN RAISE Error(TooComplex) END;
    ELSE
      IF i.recog = 0 THEN
        (* Maybe this should do the same as "allWhite=TRUE"? *)
        RAISE Error(NoWordsFound)
      END;
    END;

    i.state := State.Recognized
  END DoRecognize;


VAR (*CONST*)
  TooComplex := AtomList.List1(Atom.FromText("DCRS.ImageTooComplex"));
  NoWordsFound := AtomList.List1(Atom.FromText("DCRS.NoWordsFound"));

REVEAL
  CharSet = OBJECT METHODS
    init(language := Language.ISOLatin1): CharSet;
    custom(READONLY charset: ARRAY [0..255] OF BOOLEAN): CharSet
  END BRANDED OBJECT
    language: unsigned_byte;
    user_charset: user_charset_type
  OVERRIDES
    init := CSInit;
    custom := CSCustom
  END;

TYPE user_charset_type = UNTRACED REF ARRAY [0..255] OF BOOLEAN;

PROCEDURE CSInit(cs: CharSet; language := Language.ISOLatin1): CharSet =
  CONST LangFromLanguage = ARRAY Language OF unsigned_byte{
    Irs.K_IsoLatin1, Irs.K_CanadianFrench, Irs.K_Danish, Irs.K_Dutch,
    Irs.K_English, Irs.K_Finnish, Irs.K_French, Irs.K_German,
    Irs.K_Icelandic, Irs.K_Italian, Irs.K_Norwegian, Irs.K_Spanish,
    Irs.K_Swedish, Irs.K_Numeric};
  BEGIN
    cs.language := LangFromLanguage[language];
    cs.user_charset := NIL;
    RETURN cs
  END CSInit;

PROCEDURE CSCustom(cs: CharSet; READONLY charset: ARRAY [0..255] OF BOOLEAN)
  : CharSet =
  BEGIN
    <* ASSERT FALSE *> (* *** Need "close()" to deallocate user_charset *)
    cs.language := Irs.K_Custom;
    cs.user_charset := NEW(user_charset_type);
    cs.user_charset^ := charset;
    RETURN cs
  END CSCustom;

PROCEDURE GetWords(
    i: T; ws: WordSink; reject: CHAR; segment: CARDINAL := 0)
  RAISES ANY =
  VAR
    w0, w: Irs.WordList_star;
    f: Font;
  BEGIN
    LOCK mutex DO
      IF i.state < State.Recognized THEN DoRecognize(i) END;
      <* ASSERT i.state = State.Recognized *>

      IF i.recog = 0 AND i.allWhite THEN RETURN END;

      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.GetWordList(i.recog, ORD(reject), segment, ADR(w0));
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      w := w0;
      WHILE w.WordType # 0 DO
	CASE w.WordType OF
	  | Irs.K_PrimaryWord =>
	    ws.word(
	      w.RegNum,
	      w.BlockNum, 
	      CopyBufferToText(w.CharString, w.WordSize),
	      Rect.FromEdges(w.WordLLX, w.WordURX+1, w.WordURY, w.WordLLY+1))
	  | Irs.K_FontChange =>
	    IF Word.And(w.FontInfo, Irs.M_Courier)#0 THEN f := Font.Courier
	    ELSIF Word.And(w.FontInfo, Irs.M_Times)#0 THEN f := Font.Times
	    ELSIF Word.And(w.FontInfo, Irs.M_Helvetica)#0 THEN
	      f := Font.Helvetica
	    ELSE f := Font.Unknown
	    END;
	    ws.fontChange(
	      w.RegNum,
	      w.BlockNum,
	      f,
	      bold := Word.And(w.FontInfo, Irs.M_Bold) # 0,
	      italic := Word.And(w.FontInfo, Irs.M_Italic) # 0,
	      underline := Word.And(w.FontInfo, Irs.M_Underline) # 0,
	      pointSize := MAX(w.WordSize, 0) (* negative values have been observed! *))
	  | Irs.K_BeginNewLine =>
	    ws.newLine(w.RegNum, w.BlockNum)
	  ELSE <* ASSERT FALSE *>
	END;
	w := LOOPHOLE(
	  LOOPHOLE(w, Word.T) + BYTESIZE(Irs.WordList),
	  Irs.WordList_star)
      END;

      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.DeleteBuffer(LOOPHOLE(w0, unsigned_long));
      EVAL Chf.Revert();
      <* ASSERT NOT error *>
    END
  END GetWords;

PROCEDURE ExportText(
    i: T; reject: CHAR; wysiwyg: BOOLEAN := FALSE): TEXT RAISES {Error} =
  VAR
    flags: unsigned_long;
    buffer: char_star;
    buffer_size: unsigned_long;
    t: TEXT;
  BEGIN
    LOCK mutex DO
      IF i.state < State.Recognized THEN DoRecognize(i) END;
      <* ASSERT i.state = State.Recognized *>

      IF i.allWhite THEN RETURN "" END;

      flags := 0;
      IF wysiwyg THEN flags := Irs.M_FormatWYSIWYG END;
      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.ExportASCII(
	i.recog, ORD(reject), flags, ADR(buffer), ADR(buffer_size));
      EVAL Chf.Revert();
      IF error THEN
	<* ASSERT errorName = Irs.X_FMTFAIL *> (* *** Chf.MatchCondition??? *)
	t := "" (* "no text regions found" *)
      ELSE
	t := CopyBufferToText(buffer, buffer_size);

	EVAL Chf.Establish(Handler); error := FALSE;
	Irs.DeleteBuffer(LOOPHOLE(buffer, unsigned_long));
	EVAL Chf.Revert();
	<* ASSERT NOT error *>
      END;

      RETURN t
    END
  END ExportText;

PROCEDURE ExportDDIF(
    i: T; reject: CHAR; inclImages: BOOLEAN; pathname: TEXT) RAISES {Error} =
  VAR flags: unsigned_long;
  BEGIN
    LOCK mutex DO
      IF i.state < State.Recognized THEN DoRecognize(i) END;
      <* ASSERT i.state = State.Recognized *>

      <* ASSERT NOT i.allWhite *>

      flags := 0;
      IF inclImages THEN flags := Irs.M_IncludeImageRegions END;
      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.ExportDDIF(i.recog, ORD(reject), flags, M3toC.TtoS(pathname));
      EVAL Chf.Revert();
      <* ASSERT NOT error *>
    END
  END ExportDDIF;

PROCEDURE ExportPostScript(
    i: T;
    reject: CHAR;
    drawBorders, inclImages, relPositions: BOOLEAN): TEXT RAISES {Error} =
  VAR
    flags: unsigned_long;
    buffer: char_star;
    buffer_size: unsigned_long;
    t: TEXT;
  BEGIN
    LOCK mutex DO
      IF i.state < State.Recognized THEN DoRecognize(i) END;
      <* ASSERT i.state = State.Recognized *>

      <* ASSERT NOT i.allWhite *>

      flags := 0;
      IF drawBorders THEN flags := flags+Irs.M_RegionBorder END;
      IF inclImages THEN flags := flags+Irs.M_IncludeImageRegions END;
      IF relPositions THEN flags := flags+Irs.M_RelativeWordPosition END;
      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.ExportPS(
	i.recog, ORD(reject), flags, ADR(buffer), ADR(buffer_size));
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      t := CopyBufferToText(buffer, buffer_size);

      EVAL Chf.Establish(Handler); error := FALSE;
      Irs.DeleteBuffer(LOOPHOLE(buffer, unsigned_long));
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      RETURN t
    END
  END ExportPostScript;

PROCEDURE Close(i: T) =
  BEGIN
    LOCK mutex DO
      <* ASSERT i.state < State.Closed *>

      EVAL Chf.Establish(Handler); error := FALSE;
      Img.DeallocateFrame(i.fid);
      EVAL Chf.Revert();
      <* ASSERT NOT error *>

      IF i.state = State.Segmented THEN
        EVAL Chf.Establish(Handler); error := FALSE;
        Irs.DeleteStruct(i.seg);
        EVAL Chf.Revert();
        <* ASSERT NOT error *>
      END;

      IF i.state = State.Recognized THEN
        EVAL Chf.Establish(Handler); error := FALSE;
        Irs.DeleteStruct(i.recog);
        EVAL Chf.Revert();
        <* ASSERT NOT error *>
      END;

      i.state := State.Closed
    END
  END Close;

VAR b: unsigned_char;
BEGIN
  FOR i := 0 TO 255 DO
    b := 0;
    FOR j := 0 TO 7 DO
      b := Word.Insert(b, Word.Extract(i, j, 1), 7-j, 1)
    END;
    byteRev[i] := b
  END
END DCRS.
