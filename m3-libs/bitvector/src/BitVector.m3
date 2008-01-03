(* Copyright (C) 1997, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Created on Thu Mar 27 10:21:58 PST 1997 by heydon        *)
(* Last modified on Sat Nov 29 18:11:56 PST 1997 by heydon  *)

MODULE BitVector;

IMPORT Word, BitVectorRep;

REVEAL
  T = BitVectorRep.T BRANDED "BitVector.T" OBJECT METHODS
    initImpl(sizeHint: CARDINAL; doZero: BOOLEAN; freeMem := FALSE): T
      := InitImpl
  OVERRIDES
    init := Init;
    copy := Copy;
    empty := Empty;
    size := Size;
    cardinality := Cardinality;
    read := Read;
    write := Write;
    set := Set;
    reset := Reset;
    readSub := ReadSub;
    writeSub := WriteSub;
    writeInterval := WriteInterval;
    setInterval := SetInterval;
    resetInterval := ResetInterval;
    leastUnsetExcept := LeastUnsetExcept;
    leastUnset := LeastUnset;
    and := AndD;
    or := OrD;
    xor := XorD;
    minus := MinusD;
  END;

TYPE
  Words = REF ARRAY OF Word.T;

CONST
  AllOnes: Word.T = Word.Not(0);
  BitsPerWd = Word.Size;
  LowBitsMask: Word.T = BitsPerWd - 1;

(* "LowBitsMask" is a bit mask whose low-order "LogBitsPerWd" bits are set
   (with all other bits reset). Hence, for all non-negative integers "i", we
   have: 

|    i DIV BitsPerWd == Word.RightShift(i, LogBitsPerWd)
|    i MOD BitsPerWd == Word.And(i, LowBitsMask)
*)

VAR (* READONLY after initialization *)
  LogBitsPerWd: CARDINAL; (* "log_2" of "BitsPerWd" *)
  BitMask: ARRAY [0..BitsPerWd-1] OF Word.T;
  IntrvlMask: ARRAY [-(BitsPerWd-1)..(BitsPerWd-1)] OF Word.T;

(* "BitMask" is an array of "BitsPerWd" masks. There is one mask per
   bit; where "BitMask[i] = Word.LeftShift(1, i)" for "0 <= i < BitsPerWd".

   "IntrvlMask" is an array of "2 * BitsPerWd - 1" masks. For "i" in
   the interval "[-(BitsPerWd-1), (BitsPerWd-1)]", we have:

|    IntrvlMask[i] == Word.LeftShift(AllOnes, i)

   Notice that the index to the "IntrvlMask" array can be negative.
   A negative left shift is equivalent to a right shift of the
   negation of the shift amount, i.e., if "i < 0", then
   "IntrvlMask[i] == Word.RightShift(AllOnes, (-i))". *)

PROCEDURE WdIndex(n: CARDINAL): CARDINAL =
(* Return "n DIV BitsPerWd". This is the index of the word that contains
   the bit numbered "n". *)
  BEGIN
    RETURN Word.RightShift(n, LogBitsPerWd)
  END WdIndex;

PROCEDURE BitIndex(n: CARDINAL): CARDINAL =
(* Return "n MOD BitsPerWd". This is the index of bit "n" within its
   word. *)
  BEGIN
    RETURN Word.And(n, LowBitsMask);
  END BitIndex;

PROCEDURE WdCnt(n: CARDINAL): CARDINAL =
(* Return CEILING(FLOAT(n) / FLOAT(BitsPerWd)). This is the number of
   words required to hold a total of "n" bits. *)
  CONST Addend = BitsPerWd - 1; BEGIN
    RETURN Word.RightShift(n + Addend, LogBitsPerWd);
  END WdCnt;

PROCEDURE Init(self: T; sizeHint: CARDINAL; freeMem: BOOLEAN): T =
  BEGIN
    RETURN InitImpl(self, sizeHint, (* doZero := *) TRUE, freeMem)
  END Init;

PROCEDURE InitImpl(self: T; sizeHint: CARDINAL; doZero, freeMem: BOOLEAN): T =
(* Initialize the bit vector "self" to contain enough memory to hold
   "sizeHint" bits. If "self" already has at least that much space allocated
   for it and "freeMem" is false, then the larger array is left allocated;
   otherwise, the larger array is dropped and an array just large enough to
   hold "sizeHint" bits is allocated (if "sizeHint = 0", then no array is
   allocated in this latter case). Finally, if "doZero" is TRUE (the default),
   the entire array is zeroed according to I2; otherwise, the contents of the
   array are undefined. "doZero" should only be FALSE in those cases where
   the caller will be setting all of the bits. *)
  VAR
    wdCnt := WdCnt(sizeHint); (* number of words required *)
    zeroWds: CARDINAL;        (* number of words to zero out *)
  BEGIN
    (* allocate "word" array if necessary *)
    IF self.word = NIL THEN
      IF wdCnt > 0 THEN
        Extend(self, wdCnt, doPreserve := FALSE)
      END;
      zeroWds := wdCnt;
    ELSE
      IF freeMem OR wdCnt > NUMBER(self.word^) THEN
        (* re-allocate the "word" array *)
        self.word := NIL;
	IF wdCnt > 0 THEN
          Extend(self, wdCnt, doPreserve := FALSE)
        END;
        zeroWds := wdCnt;
      ELSE
        (* we can re-use the current "word" array *)
        zeroWds := WdCnt(self.sz); (* by I2, all remaining words are 0 *)
      END;
      self.sz := 0;
      self.firstAvailWd := 0;
    END;

    (* zero words if necessary *)
    IF doZero THEN
      FOR i := 0 TO zeroWds - 1 DO self.word[i] := 0 END
    END;
    RETURN self;
  END InitImpl;

PROCEDURE Extend(self: T; wordCnt: CARDINAL; doPreserve := TRUE) =
(* REQUIRES (self.word=NIL AND wordCnt > 0) OR wordCnt > NUMBER(self.word^) *)
(* Extend "self" to include at least a total of "wordCnt" words.
   If "doPreserve" is "TRUE", the contents of the bit vector are
   preserved, and all bits in the new words above the significant
   ones are guaranteed to be reset according to I2. Otherwise, the
   contents of the words are unspecified.

   This method may be called on a vector for which "self.word = NIL",
   in which case it does the necessary allocation. *)
  VAR numWords, newNumWords: CARDINAL; newWord: Words; BEGIN
    IF self.word = NIL
      THEN numWords := 0
      ELSE numWords := NUMBER(self.word^)
    END;
    <* ASSERT wordCnt > numWords *>
    newNumWords := MAX(wordCnt, 2 * numWords);
    newWord := NEW(Words, newNumWords);

    (* preserve bit vector if necessary *)
    IF doPreserve THEN
      (* copy old buffer to new if necessary *)
      IF self.word # NIL THEN
        SUBARRAY(newWord^, 0, numWords) := self.word^
      END;

      (* zero out new high words *)
      FOR i := numWords TO newNumWords - 1 DO
        newWord[i] := 0
      END
    END;

    (* switch to new buffer *)
    self.word := newWord
  END Extend;

PROCEDURE ExpandSz(self: T; i, wx: CARDINAL) =
(* Augment "self.sz" (and "self.word" if necessary) so as to contain
   bit "i"; "wx" must be the word containing that bit. *)
  BEGIN
    INC(i);
    IF self.word # NIL AND wx < NUMBER(self.word^) THEN
      (* fast path -- we already have enough words *)
      self.sz := MAX(self.sz, i)
    ELSE
      (* slow path -- extend "self.word" array *)
      Extend(self, wx + 1);
      self.sz := i
    END
  END ExpandSz;

PROCEDURE ReduceSz(self: T) =
(* Reduce "self.sz" if any of the high-order words of the "self.word"
   array are zero. It's wise to perform this operation after possibly
   resetting some of the high-order bits of a bit-vector. *)
  BEGIN
    IF self.word # NIL THEN
      VAR lastWd: INTEGER := WdCnt(self.sz) - 1; i := lastWd; BEGIN
        <* ASSERT lastWd < NUMBER(self.word^) *>
        WHILE i >= 0 AND self.word[i] = 0 DO DEC(i) END;
        self.sz := MIN(self.sz, (i + 1) * BitsPerWd);
        <* ASSERT self.firstAvailWd * BitsPerWd <= self.sz *>
      END
    END
  END ReduceSz;

PROCEDURE Copy(self: T): T =
(* NOTE: This method may have a "benign" side-effect on "self". *)
  VAR res: T; BEGIN
    ReduceSz(self);
    res := NEW(T, sz := self.sz, firstAvailWd := self.firstAvailWd);
    IF self.word = NIL THEN res.word := NIL ELSE
      res.word := NEW(REF ARRAY OF Word.T, NUMBER(self.word^));
      res.word^ := self.word^
    END;
    RETURN res
  END Copy;

PROCEDURE Empty(self: T): BOOLEAN =
(* NOTE: This method may have "benign" side-effects on "self". *)
  BEGIN
    (* do constant-time checks first *)
    IF self.sz = 0 THEN RETURN TRUE END;
    IF self.firstAvailWd > 0 THEN RETURN FALSE END;

    (* otherwise, iterate over all candidate words in the bit vector *)
    VAR i := WdCnt(self.sz) - 1; BEGIN
      <* ASSERT i >= 0 AND self.word # NIL *>
      WHILE i >= 0 AND self.word[i] = 0 DO DEC(i) END;
      IF i >= 0 THEN
        (* vector non-empty -- reduce "sz" if possible *)
        self.sz := MIN(self.sz, (i + 1) * BitsPerWd);
        <* ASSERT self.firstAvailWd * BitsPerWd <= self.sz *>
        RETURN FALSE
      END
    END;
    (* vector is empty *)
    self.sz := 0;
    self.word := NIL;
    RETURN TRUE
  END Empty;

(* "BitCnt[i]" is the number of bits set in the binary representation
   of the 8-bit value "i". *)

CONST BitCnt = ARRAY OF Word.T{
  (*   0 *)  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
  (*  16 *)  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  (*  32 *)  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  (*  48 *)  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  (*  64 *)  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  (*  80 *)  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  (*  96 *)  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  (* 112 *)  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  (* 128 *)  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  (* 144 *)  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  (* 160 *)  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  (* 176 *)  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  (* 192 *)  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  (* 208 *)  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  (* 224 *)  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  (* 240 *)  4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
};

PROCEDURE Size(self: T): CARDINAL =
  BEGIN RETURN self.sz END Size;

PROCEDURE Cardinality(self: T): CARDINAL =
(* NOTE: This method may have "benign" side-effects on "self". *)
  CONST ByteMask: Word.T = 16_ff; BEGIN
    (* do constant-time checks first *)
    IF self.sz = 0 THEN RETURN 0 END;

    (* iterate over "self.words" *)
    VAR
      inUseWds := WdCnt(self.sz);
      res := self.firstAvailWd * BitsPerWd;
      i: INTEGER := inUseWds - 1;
    BEGIN
      (* skip all-zero high-order words *)
      WHILE i >= self.firstAvailWd AND self.word[i] = 0 DO DEC(i) END;
      self.sz := MIN(self.sz, (i + 1) * BitsPerWd);

      (* count bits in remaining words *)
      WHILE i >= self.firstAvailWd DO
        VAR wd := self.word[i]; BEGIN
          WHILE wd # 0 DO
            INC(res, BitCnt[Word.And(wd, ByteMask)]);
            wd := Word.RightShift(wd, 8);
          END
        END;
        DEC(i)
      END;
      RETURN res
    END
  END Cardinality;

PROCEDURE Read(self: T; i: CARDINAL): BOOLEAN =
  BEGIN
    IF i >= self.sz THEN RETURN FALSE END;
    VAR wx := WdIndex(i); BEGIN
      IF wx < self.firstAvailWd THEN RETURN TRUE END;
      RETURN Word.And(self.word[wx], BitMask[BitIndex(i)]) # 0
    END
  END Read;

PROCEDURE Write(self: T; i: CARDINAL; val: BOOLEAN): BOOLEAN =
  BEGIN
    IF val
      THEN RETURN Set(self, i)
      ELSE RETURN Reset(self, i)
    END
  END Write;

PROCEDURE Set(self: T; i: CARDINAL): BOOLEAN =
  VAR wx := WdIndex(i); bx := BitIndex(i); res: BOOLEAN; BEGIN
    ExpandSz(self, i, wx);
    <* ASSERT wx < NUMBER(self.word^) *>
    res := Word.And(self.word[wx], BitMask[bx]) # 0;
    IF NOT res THEN
      (* set the bit if it was not already set *)
      WITH wd = self.word[wx] DO wd := Word.Or(wd, BitMask[bx]) END
    END;
    RETURN res
  END Set;

PROCEDURE Reset(self: T; i: CARDINAL): BOOLEAN =
  VAR wx := WdIndex(i); bx := BitIndex(i); res: BOOLEAN; BEGIN
    IF i < self.sz THEN
      <* ASSERT self.word # NIL AND wx < NUMBER(self.word^) *>
      res := Word.And(self.word[wx], BitMask[bx]) # 0;
      IF res THEN
        WITH wd = self.word[wx] DO
          wd := Word.And(wd, Word.Not(BitMask[bx]))
        END;
        self.firstAvailWd := MIN(self.firstAvailWd, wx)
      END
    ELSE
      res := FALSE
    END;
    RETURN res
  END Reset;

PROCEDURE ReadSub(self: T; start, len: CARDINAL): Word.T =
  VAR res: Word.T; BEGIN
    <* ASSERT len <= BitsPerWd *>
    IF start >= self.sz THEN RETURN 0 END;
    <* ASSERT self.word # NIL *>
    VAR
      startWd := WdIndex(start);
      startBit := BitIndex(start);
    BEGIN
      <* ASSERT startWd < NUMBER(self.word^) *>
      IF startBit + len <= BitsPerWd THEN
        (* fast path -- bits all in one word *)
        WITH mask = IntrvlMask[len - BitsPerWd] DO (* "len" 1s in low bits *)
          res := Word.And(mask, Word.RightShift(self.word[startWd], startBit))
        END
      ELSE
        (* slow path -- bits span word boundary *)
        VAR
          loLen := BitsPerWd - startBit;
          hiLen := len - loLen;
          loMask := IntrvlMask[loLen - BitsPerWd]; (* "loLen" 1s in low bits *)
        BEGIN
          res := Word.And(loMask,
            Word.RightShift(self.word[startWd], startBit));
          INC(startWd); (* move on to next word *)
          IF startWd < WdCnt(self.sz) THEN
            <* ASSERT startWd < NUMBER(self.word^) *>
            WITH hiMask = IntrvlMask[hiLen - BitsPerWd] DO
              res := Word.Or(res, Word.LeftShift(
                Word.And(self.word[startWd], hiMask), loLen))
            END
          END
        END
      END
    END;
    RETURN res
  END ReadSub;

PROCEDURE WriteSub(self: T; start, len: CARDINAL; val: Word.T) =
  VAR
    startWd := WdIndex(start);
    startBit := BitIndex(start);
    end := (start + len) - 1;    (* index of last bit being set *)
    endWd := WdIndex(end);       (* word index of "end" bit *)
  BEGIN
    <* ASSERT len <= BitsPerWd *>

    (* update "sz", "firstAvailWd" -- extend bit vector if necessary *)
    ExpandSz(self, end, endWd);
    <* ASSERT endWd < NUMBER(self.word^) *>
    self.firstAvailWd := MIN(self.firstAvailWd, start);

    (* set the bits *)
    IF startWd = endWd THEN
      (* fast path -- bits all in one word *)
      <* ASSERT startBit + len <= BitsPerWd *>
      VAR mask := IntrvlMask[len - BitsPerWd]; BEGIN
        val := Word.And(val, mask);
        mask := Word.LeftShift(mask, startBit);
        WITH wd = self.word[startWd] DO
          wd := Word.Or(Word.And(wd, Word.Not(mask)),
            Word.LeftShift(val, startBit));
        END
      END
    ELSE
      (* slow path -- bits span word boundary *)
      VAR
        loLen := BitsPerWd - startBit;
        hiLen := len - loLen;
        loMask := IntrvlMask[BitsPerWd - loLen];
        hiMask := IntrvlMask[hiLen - BitsPerWd];
        loVal := Word.LeftShift(val, startBit);
        hiVal := Word.And(Word.RightShift(val, loLen), hiMask);
      BEGIN
        (* lo word *)
        WITH wd = self.word[startWd] DO
          wd := Word.Or(Word.And(wd, Word.Not(loMask)), loVal)
        END;

        (* hi word *)
        INC(startWd);
        <* ASSERT startWd < NUMBER(self.word^) *>
        WITH wd = self.word[startWd] DO
          wd := Word.Or(Word.And(wd, Word.Not(hiMask)), hiVal)
        END
      END
    END
  END WriteSub;

PROCEDURE WriteInterval(self: T; lo, hi: CARDINAL; val: BOOLEAN) =
  BEGIN
    IF val
      THEN SetInterval(self, lo, hi)
      ELSE ResetInterval(self, lo, hi)
    END
  END WriteInterval;

PROCEDURE SetInterval(self: T; lo, hi: CARDINAL) =
  VAR
    wxLo := WdIndex(lo); bxLo := BitIndex(lo);
    wxHi := WdIndex(hi); bxHi := BitIndex(hi);
  BEGIN
    (* extend "self.sz" (and "self.word" if necessary) *)
    ExpandSz(self, hi, wxHi);
    <* ASSERT wxHi < NUMBER(self.word^) *>

    (* set the bits *)
    IF wxLo = wxHi THEN
      (* all bits in single word *)
      VAR mask := Word.LeftShift(IntrvlMask[(bxHi-bxLo+1) - BitsPerWd], bxLo);
      BEGIN WITH wd = self.word[wxLo] DO wd := Word.Or(wd, mask) END END
    ELSE
      (* bits span multiple words *)
      VAR i: CARDINAL; BEGIN
        (* partial low word *)
        IF wxLo < self.firstAvailWd OR (bxLo=0 AND wxLo=self.firstAvailWd) THEN
          (* fast case -- low bits are already set by I3 *)
          i := self.firstAvailWd;
          (* (The next statement anticipates the final result; it temporarily
             break invariant I3.) *)
          self.firstAvailWd := MAX(self.firstAvailWd, wxHi);
          <* ASSERT self.firstAvailWd * BitsPerWd <= self.sz *>
        ELSE
          (* normal case *)
          WITH wd = self.word[wxLo] DO
            wd := Word.Or(wd, IntrvlMask[bxLo])
          END;
          i := wxLo + 1
        END;

        (* middle full words *)
        WHILE i < wxHi DO
          self.word[i] := AllOnes;
          INC(i)
        END;

        (* partial high word *)
        IF i = wxHi THEN (* guard needed in case "self.firstAvailWd > wxHi" *)
          WITH wd = self.word[i] DO
            wd := Word.Or(wd, IntrvlMask[(bxHi+1) - BitsPerWd])
          END
        END
      END
    END
  END SetInterval;

PROCEDURE ResetInterval(self: T; lo, hi: CARDINAL) =
  BEGIN
    (* return quickly if there's no work to do *)
    IF self.sz = 0 THEN RETURN END;
    <* ASSERT self.word # NIL *>
    hi := MIN(hi, self.sz - 1); (* reduce "hi" if necessary by I2 *)
    IF lo > hi THEN RETURN END;

    VAR
      wxLo := WdIndex(lo); bxLo := BitIndex(lo);
      wxHi := WdIndex(hi); bxHi := BitIndex(hi);
    BEGIN
      (* reset the bits *)
      <* ASSERT wxHi < NUMBER(self.word^) *>
      IF wxLo = wxHi THEN
        (* all bits in single word *)
        WITH
          mask = Word.LeftShift(IntrvlMask[(bxHi-bxLo+1) - BitsPerWd], bxLo),
          wd = self.word[wxLo]
        DO
          wd := Word.And(wd, Word.Not(mask))
        END
      ELSE
        (* bits span multiple words *)
        VAR i: CARDINAL; BEGIN
          (* partial low word *)
          WITH wd = self.word[wxLo] DO
            wd := Word.And(wd, Word.Not(IntrvlMask[bxLo]))
          END;
          i := wxLo + 1;

          (* middle words *)
          WHILE i < wxHi DO
            self.word[i] := 0;
            INC(i)
          END;

          (* partial high word *)
          WITH
            mask = IntrvlMask[(bxHi+1) - BitsPerWd],
            wd = self.word[i]
          DO
            wd := Word.And(wd, Word.Not(mask))
          END
        END
      END;

      (* reduce "self.sz" if possible *)
      IF hi = self.sz - 1 THEN
        self.sz := MIN(self.sz, lo)
      END;

      (* reduce "self.firstAvailWd" if necessary *)
      self.firstAvailWd := MIN(self.firstAvailWd, wxLo)
    END
  END ResetInterval;

PROCEDURE LeastUnsetExcept(self, except: T; setIt: BOOLEAN): CARDINAL =
  VAR
    res: CARDINAL;
    maxWdExc: CARDINAL;      (* only valid if "except # NIL" *)
    wx := self.firstAvailWd; (* word in which result bit will be set *)
    maxWd := WdCnt(self.sz);
  BEGIN
    (* find first word containing any 0's *)
    WHILE wx < maxWd AND self.word[wx] = AllOnes DO INC(wx) END;
    self.firstAvailWd := wx; (* benevolent side-effect on "self" *)

    (* advance further if "except # NIL" *)
    IF except # NIL THEN
      (* advance "wx" *)
      wx := MAX(wx, except.firstAvailWd);
      maxWdExc := WdCnt(except.sz);

      (* advance in "except" *)
      WHILE wx < maxWdExc AND except.word[wx] = AllOnes DO INC(wx) END;
      except.firstAvailWd := wx; (* benevolent side-effect on "except" *)

      VAR minWd := MIN(maxWd, maxWdExc); BEGIN
        (* advance where both bit vectors are long enough *)
        WHILE wx < minWd AND Word.Or(self.word[wx], except.word[wx]) = AllOnes
          DO INC(wx)
        END;

        (* advance through longer of the two *)
        IF wx = minWd THEN
          IF maxWdExc > maxWd THEN
            WHILE wx < maxWdExc AND except.word[wx] = AllOnes DO INC(wx) END
          ELSIF maxWdExc < maxWd THEN
            WHILE wx < maxWd AND self.word[wx] = AllOnes DO INC(wx) END
          END
        END
      END (* VAR *)
    END; (* IF *)

    res := wx * BitsPerWd;
    VAR wd: Word.T; bx: CARDINAL; BEGIN
      (* compute word in which to find 0 bit *)
      IF wx < maxWd THEN wd := self.word[wx] ELSE wd := 0 END;
      IF except # NIL AND wx < maxWdExc THEN
        wd := Word.Or(wd, except.word[wx])
      END;
      <* ASSERT wd # AllOnes *>
    
      (* find the first 0 in "wd"; set "bx" to its index w/in the word *)
      bx := 0;
      IF wd # 0 THEN
        (* complement; now find a 1 *)
        wd := Word.Not(wd);
        <* ASSERT wd # 0 *>
    
        (* use binary search to find the 1 bit w/ smallest index*)
        VAR
          maskSz := BitsPerWd DIV 2;
          mask := IntrvlMask[-maskSz];
        BEGIN
          WHILE maskSz > 0 DO
            IF Word.And(wd, mask) = 0 THEN
              INC(bx, maskSz);
              wd := Word.RightShift(wd, maskSz)
            END;
            maskSz := maskSz DIV 2;
            mask := Word.RightShift(mask, maskSz)
          END;
          INC(res, bx)
        END
      END; (* IF *)
    
      (* set the bit if necessary *)
      IF setIt THEN
        ExpandSz(self, res, wx);
        WITH wd = self.word[wx] DO wd := Word.Or(wd, BitMask[bx]) END
      END
    END;
    RETURN res
  END LeastUnsetExcept;

PROCEDURE LeastUnset(self: T; setIt: BOOLEAN): CARDINAL =
  BEGIN RETURN LeastUnsetExcept(self, NIL, setIt) END LeastUnset;

PROCEDURE Equal(bv1, bv2: T): BOOLEAN =
  BEGIN
    (* check for special cases *)
    IF bv1.sz = 0 THEN
      RETURN Empty(bv2)
    ELSIF bv2.sz = 0 THEN
      RETURN Empty(bv1)
    END;
    <* ASSERT bv1.word # NIL AND bv2.word # NIL *>
    VAR
      wds1 := WdCnt(bv1.sz); wds2 := WdCnt(bv2.sz);
      minWds := MIN(wds1, wds2);
      firstWd := MIN(bv1.firstAvailWd, bv2.firstAvailWd);
      i := firstWd;
    BEGIN
      (* check that words agree where vectors have words in common *)
      WHILE i < minWds DO
        IF bv1.word[i] # bv2.word[i] THEN RETURN FALSE END;
        INC(i)
      END;

      (* check that the remainder of the longer word is all zero *)
      IF wds1 > wds2 THEN
        WHILE i < wds1 DO
          IF bv1.word[i] # 0 THEN RETURN FALSE END;
          INC(i)
        END
      ELSIF wds2 > wds1 THEN
        WHILE i < wds2 DO
          IF bv2.word[i] # 0 THEN RETURN FALSE END;
          INC(i)
        END
      END
    END;
    RETURN TRUE
  END Equal;

PROCEDURE Subset(bv1, bv2: T): BOOLEAN =
(* The implementation works by first determining how many words "bv1" and
   "bv2" have in common. For each pair of words in common, we return false
   immediately if the word of "bv1" has a bit set that is not set in the
   corresponding word of "bv2". If "bv1" has more words than "bv2", we must
   then also check that all extra words of "bv1" are zero. *)
  BEGIN
    (* check for special fast case *)
    IF bv1.sz = 0 THEN RETURN TRUE END;
    <* ASSERT bv1.word # NIL *>

    VAR
      wds1 := WdCnt(bv1.sz); wds2 := WdCnt(bv2.sz);
      minWds := MIN(wds1, wds2);
      i := bv2.firstAvailWd;
    BEGIN
      (* Check that "bv1"'s bits are a subset of "bv2"'s for all
         common words; we don't have to check the first "firstAvailWd"
         words of "bv2", since all their bits are set, and so the
         corresponding words of "bv1" are guaranteed to be a subset. *)
      WHILE i < minWds DO
        IF Word.And(bv1.word[i], Word.Not(bv2.word[i])) # 0 THEN
          RETURN FALSE
        END;
        INC(i)
      END;

      (* check that any extra words of "bv1" are zero *)
      WHILE i < wds1 DO
        IF bv1.word[i] # 0 THEN RETURN FALSE END;
        INC(i)
      END
    END;

    (* if both tests passed, return "TRUE" *)
    RETURN TRUE
  END Subset;

PROCEDURE ProperSubset(bv1, bv2: T): BOOLEAN =
  BEGIN
    (* check for special cases *)
    IF Empty(bv2) THEN RETURN FALSE END;
    IF Empty(bv1) THEN RETURN TRUE END;
    <* ASSERT bv1.word # NIL AND bv2.word # NIL *>

    (* First, check if there are any unset bits in the first
       "bv2.firstAvailWd" words of "bv1". The search can start
       at word "bv1.firstAvailWd" of "bv1" because all words
       before that are known to be all ones by I3. *)
    VAR i := bv1.firstAvailWd; BEGIN
      WHILE i < bv2.firstAvailWd DO
        IF bv1.word[i] # AllOnes THEN RETURN TRUE END
      END;

      VAR
        wds1 := WdCnt(bv1.sz); wds2 := WdCnt(bv2.sz);
        minWds := MIN(wds1, wds2);
      BEGIN
        (* next, look over words that agree *)
        WHILE i < minWds DO
          IF Word.And(bv1.word[i], Word.Not(bv2.word[i])) # 0 THEN
            RETURN FALSE
          END;
          (* every bit set in "bv1.word[i]" is also set in "bv2.word[i]" *)
          IF bv1.word[i] # bv2.word[i] THEN RETURN TRUE END;
          INC(i)
        END;

        (* check for set bits in "bv2" not set in "bv1". *)
        WHILE i < wds2 DO
          IF bv2.word[i] # 0 THEN RETURN TRUE END; INC(i)
        END;
        (* Note: if "wds1 > wds2", we should return "FALSE" even if none
           of the bits in the extra ``high'' words of "bv1" are set, since
           in that case "Equal(bv1, bv2)". *)
      END
    END;
    RETURN FALSE
  END ProperSubset;

PROCEDURE And(bv1, bv2: T): T =
  VAR
    newSz := MIN(bv1.sz, bv2.sz);
    newFAW := MIN(bv1.firstAvailWd, bv2.firstAvailWd);
    res := NEW(T, firstAvailWd := newFAW).initImpl(newSz, doZero := FALSE);
  BEGIN
    res.sz := newSz;
    <* ASSERT res.firstAvailWd * BitsPerWd <= res.sz *>
    IF newSz > 0 THEN (* work is only necessary if both args are non-empty *)
      VAR
        i: CARDINAL := 0;
        wdCnt1 := WdCnt(bv1.sz); wdCnt2 := WdCnt(bv2.sz);
        minWds := MIN(wdCnt1, wdCnt2);
      BEGIN
        (* set initial "newFAW" words *)
        <* ASSERT res.word # NIL AND res.firstAvailWd <= NUMBER(res.word^) *>
        WHILE i < res.firstAvailWd DO
          res.word[i] := AllOnes; INC(i)
        END;

        (* compute conjunction for words in common *)
        <* ASSERT minWds <= NUMBER(res.word^) *>
        WHILE i < minWds DO
          res.word[i] := Word.And(bv1.word[i], bv2.word[i]); INC(i)
        END;

        (* zero out the rest (in case "NUMBER(res.word^) > minWds") *)
        WHILE i < NUMBER(res.word^) DO
          res.word[i] := 0; INC(i)
        END
      END;

      (* reduce size of result if possible *)
      ReduceSz(res)
    ELSE
      (* both bit vectors are empty *)
      <* ASSERT res.word = NIL AND res.firstAvailWd = 0 *>
    END;
    RETURN res
  END And;

PROCEDURE AndD(self, bv: T): T =
  VAR wdCnt := WdCnt(self.sz); BEGIN
    IF wdCnt > 0 THEN (* work is only necessary if "self" is non-empty *)
      <* ASSERT self.word # NIL *>
      VAR
        wdCnt2 := WdCnt(bv.sz);
        minWds := MIN(wdCnt, wdCnt2);
        i: CARDINAL := self.firstAvailWd;
      BEGIN
        (* set sizes *)
        self.sz := MIN(self.sz, bv.sz);
        self.firstAvailWd := MIN(self.firstAvailWd, bv.firstAvailWd);

        (* compute conjunction for words in common *)
        WHILE i < minWds DO
          WITH wd = self.word[i] DO wd := Word.And(wd, bv.word[i]) END;
          INC(i)
        END;

        (* zero the rest in case "NUMBER(self.word^) > NUMBER(bv.word^)" *)
        WHILE i < wdCnt DO
          self.word[i] := 0; INC(i)
        END;

        (* reduce size of result if possible *)
        ReduceSz(self)
      END
    END;
    RETURN self
  END AndD;

PROCEDURE Or(bv1, bv2: T): T =
  VAR
    newSz := MAX(bv1.sz, bv2.sz);
    newFAW := MAX(bv1.firstAvailWd, bv2.firstAvailWd);
    res := NEW(T, firstAvailWd := newFAW).initImpl(newSz, doZero := FALSE);
  BEGIN
    res.sz := newSz;
    <* ASSERT res.firstAvailWd * BitsPerWd <= res.sz *>
    IF newSz > 0 THEN (* work is only necessary if either arg is non-empty *)
      VAR
        i: CARDINAL := 0;
        wdCnt1 := WdCnt(bv1.sz); wdCnt2 := WdCnt(bv2.sz);
        minWds := MIN(wdCnt1, wdCnt2); maxWds := MAX(wdCnt1, wdCnt2);
      BEGIN
        (* set initial "newFAW" words *)
        <* ASSERT res.word # NIL AND res.firstAvailWd <= NUMBER(res.word^) *>
        WHILE i < res.firstAvailWd DO
          res.word[i] := AllOnes; INC(i)
        END;

        (* compute disjunction for words in common *)
        <* ASSERT minWds <= NUMBER(res.word^) *>
        WHILE i < minWds DO
          res.word[i] := Word.Or(bv1.word[i], bv2.word[i]); INC(i)
        END;

        (* copy from longer bit vector *)
        <* ASSERT maxWds <= NUMBER(res.word^) *>
        VAR word: REF ARRAY OF Word.T; BEGIN
          IF wdCnt1 > wdCnt2
            THEN word := bv1.word
            ELSE word := bv2.word
          END;
          WHILE i < maxWds DO
            res.word[i] := word[i]; INC(i)
          END
        END;

        (* zero out rest (in case "NUMBER(res.word^) > minWds") *)
        WHILE i < NUMBER(res.word^) DO
          res.word[i] := 0; INC(i)
        END
      END
    ELSE
      <* ASSERT res.word = NIL AND res.firstAvailWd = 0 *>
    END;
    RETURN res
  END Or;

PROCEDURE OrD(self, bv: T): T =
  VAR wdCnt2 := WdCnt(bv.sz); BEGIN
    IF wdCnt2 > 0 THEN (* work only necessary if "bv" is non-empty *)
      (* extend "word" array if necessary *)
      IF self.word = NIL OR wdCnt2 > NUMBER(self.word^) THEN
        Extend(self, wdCnt2)
      END;
      <* ASSERT self.word # NIL AND wdCnt2 <= NUMBER(self.word^) *>

      VAR i := self.firstAvailWd; BEGIN
        (* update sizes *)
        self.sz := MAX(self.sz, bv.sz);
        self.firstAvailWd := MAX(self.firstAvailWd, bv.firstAvailWd);

        (* set more "firstAvailWd" words if necessary *)
        WHILE i < self.firstAvailWd DO
          self.word[i] := AllOnes; INC(i)
        END;

        (* compute disjunction of words in common *)
        VAR minWds := MIN(WdCnt(self.sz), wdCnt2); BEGIN
          WHILE i < minWds DO
            WITH wd = self.word[i] DO wd := Word.Or(wd, bv.word[i]) END;
            INC(i)
          END
        END;

        (* copy from "bv" if it is longer *)
        WHILE i < wdCnt2 DO
          self.word[i] := bv.word[i]; INC(i)
        END
      END
    END;
    RETURN self
  END OrD;

PROCEDURE Xor(bv1, bv2: T): T =
  VAR
    newSz := MAX(bv1.sz, bv2.sz);
    res := NEW(T).initImpl(newSz, doZero := FALSE);
  BEGIN
    res.sz := newSz;
    <* ASSERT res.firstAvailWd * BitsPerWd <= res.sz *>
    IF newSz > 0 THEN (* work is only necessary if both args are non-empty *)
      VAR
        wdCnt1 := WdCnt(bv1.sz); wdCnt2 := WdCnt(bv2.sz);
        minWds := MIN(wdCnt1, wdCnt2);
        i: CARDINAL := 0;
      BEGIN
        (* compute exclusive-OR over common words *)
        WHILE i < minWds DO
          res.word[i] := Word.Xor(bv1.word[i], bv2.word[i]); INC(i)
        END;

        (* copy remaining words of longer vector (if any) *)
        IF wdCnt1 > wdCnt2 THEN
          <* ASSERT wdCnt1 <= NUMBER(res.word^) *>
          WHILE i < wdCnt1 DO
            res.word[i] := bv1.word[i]; INC(i)
          END
        ELSIF wdCnt1 < wdCnt2 THEN
          <* ASSERT wdCnt2 <= NUMBER(res.word^) *>
          WHILE i < wdCnt2 DO
            res.word[i] := bv2.word[i]; INC(i)
          END
        END;

        (* zero out the rest (in case "NUMBER(res.word^) > i") *)
        WHILE i < NUMBER(res.word^) DO
          res.word[i] := 0; INC(i)
        END
      END
    ELSE
      <* ASSERT res.word = NIL AND res.firstAvailWd = 0 *>
    END;
    RETURN res
  END Xor;

PROCEDURE XorD(self, bv: T): T =
  VAR wdCnt2 := WdCnt(bv.sz); BEGIN
    IF wdCnt2 > 0 THEN (* work only necessary if "bv" is non-empty *)
      (* extend "word" array if necessary *)
      IF self.word = NIL OR wdCnt2 > NUMBER(self.word^) THEN
        Extend(self, wdCnt2)
      END;
      <* ASSERT self.word # NIL AND wdCnt2 <= NUMBER(self.word^) *>

      (* update sizes *)
      self.sz := MAX(self.sz, bv.sz);
      self.firstAvailWd := 0;

      VAR
        wdCnt := WdCnt(self.sz);
        minWds := MIN(wdCnt, wdCnt2);
        i: CARDINAL := 0;
      BEGIN
        (* compute exclusive-OR of words in common *)
        WHILE i < minWds DO
          WITH wd = self.word[i] DO wd := Word.Xor(wd, bv.word[i]) END;
          INC(i)
        END;

        (* if "bv" is longer, copy its words *)
        WHILE i < wdCnt2 DO
          self.word[i] := bv.word[i]; INC(i)
        END
      END
    END;
    RETURN self
  END XorD;

PROCEDURE Minus(bv1, bv2: T): T =
  VAR
    newSz := bv1.sz; (* the result is the same size as "bv1" *)
    res := NEW(T, firstAvailWd := bv1.firstAvailWd).initImpl(
      newSz, doZero := FALSE);
    wdCnt2 := WdCnt(bv2.sz);
  BEGIN
    res.sz := newSz;
    <* ASSERT res.firstAvailWd * BitsPerWd <= res.sz *>
    IF newSz > 0 THEN (* work only necessary if "bv1" is non-empty *)
      VAR
        wdCnt1 := WdCnt(bv1.sz);
        minWds := MIN(wdCnt1, wdCnt2);
        i: CARDINAL := 0;
      BEGIN
        IF wdCnt1 > 0 THEN (* work only necessary if "bv2" non-empty *)
          res.firstAvailWd := 0;

          (* subtract "bv2" from "bv1" where they have words in common *)
          <* ASSERT res.word # NIL AND wdCnt1 <= NUMBER(res.word^) *>
          WHILE i < minWds DO
            res.word[i] := Word.And(bv1.word[i], Word.Not(bv2.word[i])); INC(i)
          END
        END;

        (* copy the rest from "bv1" (if any) *)
        WHILE i < wdCnt1 DO
          res.word[i] := bv1.word[i]; INC(i)
        END;

        (* zero out the rest (in case "NUMBER(res.word^) > wdCnt1") *)
        WHILE i < NUMBER(res.word^) DO
          res.word[i] := 0; INC(i)
        END;

        (* reduce size if any high-order 0 words *)
        ReduceSz(res);
      END
    ELSE
      <* ASSERT res.word = NIL AND res.firstAvailWd = 0 *>
    END;
    RETURN res
  END Minus;

PROCEDURE MinusD(self, bv: T): T =
  VAR wdCnt2 := WdCnt(bv.sz); BEGIN
    IF self.sz > 0 AND wdCnt2 > 0 THEN (* work only nec. if both non-empty *)
      (* initialize sizes *)
      (* "self.sz" is unchanged *)
      self.firstAvailWd := 0;

      VAR
        wdCnt := WdCnt(self.sz);
        minWds := MIN(wdCnt, wdCnt2);
        i: CARDINAL := 0;
      BEGIN
        (* subtract where there are words in common *)
        WHILE i < minWds DO
          WITH wd = self.word[i] DO
            wd := Word.And(wd, Word.Not(bv.word[i]))
          END;
          INC(i)
        END;

        (* reduce size if any high-order 0 words *)
        ReduceSz(self);
      END
    END;
    RETURN self
  END MinusD;

PROCEDURE Hash(bv: T): Word.T =
  VAR res := 0; BEGIN
    IF bv.word # NIL THEN
      (* take parity of "bv.firstAvailWd" into account *)
      IF bv.firstAvailWd MOD 2 = 1 THEN res := AllOnes END;

      (* XOR in all the remaining words *)
      FOR i := bv.firstAvailWd TO WdCnt(bv.sz) - 1 DO
        res := Word.Xor(res, bv.word[i])
      END
    END;
    RETURN res
  END Hash;

REVEAL
  Iterator = BitVectorRep.Iterator BRANDED "BitVector.Iterator" OBJECT
  OVERRIDES
    init := IterInit;
    reset := IterReset;
    next := IterNext;
  END;

PROCEDURE IterInit(self: Iterator; bv: T): Iterator =
  BEGIN
    self.bv := bv;
    self.reset();
    RETURN self
  END IterInit;

PROCEDURE IterReset(self: Iterator) =
  BEGIN
    self.bitIndex := 0;
    self.wordIndex := 0;
    self.mask := 1;
  END IterReset;

PROCEDURE IterNext(self: Iterator; VAR (*OUT*) res: CARDINAL): BOOLEAN =
  BEGIN
    WHILE self.bitIndex < self.bv.sz DO
      WITH wd = self.bv.word[self.wordIndex] DO
        IF wd = 0 THEN
          INC(self.bitIndex, BitsPerWd)
        ELSE
      	  WHILE self.mask # 0 DO
            VAR bit := (Word.And(self.mask, wd) # 0); BEGIN
      	      self.mask := Word.LeftShift(self.mask, 1);
      	      INC(self.bitIndex);
              IF bit THEN
                res := self.bitIndex;
                INC(self.bitIndex);
                RETURN TRUE
              END;
      	      INC(self.bitIndex);
            END
      	  END
        END;
      	INC(self.wordIndex);
      	self.mask := 1
      END
    END;
    RETURN FALSE
  END IterNext;

BEGIN
  (* initialize "LogBitsPerWd" *)
  LogBitsPerWd := 0;
  VAR wd := 1; BEGIN
    WHILE wd < BitsPerWd DO
      INC(LogBitsPerWd);
      wd := Word.LeftShift(wd, 1)
    END
  END;

  (* initialize "BitMask" *)
  FOR i := 0 TO BitsPerWd - 1 DO
    BitMask[i] := Word.LeftShift(1, i)
  END;

  (* initialize "IntrvlMask" *)
  IntrvlMask[0] := AllOnes;
  FOR i := 1 TO BitsPerWd - 1 DO
    IntrvlMask[i] := Word.LeftShift(AllOnes, i);
    IntrvlMask[-i] := Word.RightShift(AllOnes, i);
  END;
END BitVector.
