MODULE SeekRdClass;
IMPORT Rd;
IMPORT RdClass;
IMPORT FileRd;
IMPORT File;
IMPORT OSError;
IMPORT Thread;

IMPORT Wr,Fmt,Stdio,Env;

PROCEDURE S(s: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout,"SeekRd: " &s&"\n");Wr.Flush(Stdio.stdout);
  END S;

VAR
  Debug := Env.Get("SeekRdDEBUG") # NIL;

CONST
  BufferSize = 256;
TYPE
  CharBuffer = REF ARRAY OF CHAR;
  BuffInfo = RECORD
    buff: CharBuffer;
    lo, hi: CARDINAL;
    linesBeforeLo: CARDINAL;
    tail: BuffList;
  END;
  BuffList = REF BuffInfo;
REVEAL
  T = Public BRANDED OBJECT
    past, future, free: BuffList := NIL;
    firstSeekablePos: CARDINAL;
    eofSeen: CARDINAL;
    linesBeforeLo: CARDINAL;
  OVERRIDES
    init := Init;
    lineNo := LineNo;
    discardPrevious := DiscardPrevious;
    seek := Seek;
    getSub := RdClass.GetSubDefault;
    (* FileRd.T.getSub is screwed up *)
  END;

PROCEDURE Check(rd: T) =
  BEGIN
    <* ASSERT rd.st = 0 *>
    <* ASSERT rd.cur >= rd.firstSeekablePos *>
    <* ASSERT rd.intermittent = TRUE *>
    <* ASSERT rd.seekable = TRUE *>
    IF (rd.cur >= rd.lo) AND (rd.cur <= rd.hi) THEN
      (* some invariants that we need *)
      <* ASSERT rd.cur - rd.lo >= FIRST(rd.buff^) *>
      <* ASSERT FIRST(rd.buff^) <= 0 *>
      <* ASSERT (rd.hi - rd.lo) <= LAST(rd.buff^) + 1 *>

      (* and even some invariants we don't really need *)
      <* ASSERT FIRST(rd.buff^) = 0 *>
      <* ASSERT rd.hi - rd.lo <= NUMBER(rd.buff^) *>
    END;
  END Check;

(* move current buffer to head of toList, and replace it with one from
   head of fromList. If fromList is empty and allocNew is true, then
   allocate new current buffer *)
PROCEDURE BuffSwap(self: T;
                   VAR toList, fromList: BuffList;
                   allocNew: BOOLEAN) RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    newCell: BuffList;
    newBuff: BuffInfo;
  BEGIN
    (* get newCell and newBuff, deleting from fromList *)
    IF fromList = NIL THEN
      <* ASSERT allocNew *>
      WITH new = fromList DO
        IF self.free = NIL THEN
          new := NEW(BuffList);
          new.buff := NEW(CharBuffer, BufferSize);
          IF Debug THEN S("Allocated new buffer"); END;
        ELSE
          new := self.free;
          self.free := self.free.tail;
        END;
        new.lo := self.hi;
        new.hi := self.hi;
        new.linesBeforeLo := CountLines(self, self.hi);
        new.tail := NIL;
      END;
    END;
    newCell := fromList;
    fromList := fromList.tail;
    newBuff := newCell^;
    
    (* move current buffer to newCell, and cons onto toList *)
    newCell.buff := self.buff;
    newCell.lo := self.lo;
    newCell.hi := self.hi;
    newCell.linesBeforeLo := self.linesBeforeLo;
    newCell.tail := toList;
    toList := newCell;

    (* make newBuff the current buffer *)
    self.buff := newBuff.buff;
    self.lo := newBuff.lo;
    self.hi := newBuff.hi;
    self.linesBeforeLo := newBuff.linesBeforeLo;
  END BuffSwap;

PROCEDURE CountLines(self: T; beforePos: CARDINAL): CARDINAL
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    originalPos: CARDINAL := self.cur;
    count: CARDINAL := self.linesBeforeLo;
    lastInd: CARDINAL;
  BEGIN
    IF beforePos > self.hi OR beforePos < self.lo THEN
      (* cur may have been adjusted without calling seek *)
      EVAL Seek(self, beforePos, FALSE);
      self.cur := originalPos;
    END;
    <* ASSERT beforePos <= self.hi *>
    <* ASSERT beforePos >= self.lo *>
    IF beforePos > self.lo THEN
      lastInd := beforePos-1-self.lo;
      <* ASSERT lastInd < NUMBER(self.buff^) *>
      FOR i := 0 TO lastInd DO
        IF self.buff[i] = '\n' THEN
          INC(count);
        END;
      END;
    END;
    RETURN count;
  END CountLines;

PROCEDURE LineNo(self: T): INTEGER = <* FATAL Rd.Failure, Thread.Alerted*>
  BEGIN RETURN CountLines(self, self.cur)+1; END LineNo;

(*
On intermittence.
Greg Nelson, p. 135: "An intermittent reader is never seekable."

Suppose I make it nonintermittent.
Then RdImpl.GetText wants to know the length of the whole thing.
I have to tell it something.
If I tell it anything other than infinity, then some GetText calls could
return the wrong length TEXT.
If I tell it infinity, then the semantics of GetText do not agree with the
Rd.i3 interface: If you ask it for more text than there is, it raises
Failure instead of giving you less text.

Suppose I make it intermittent.
Then it can't be seekable!?!?!

Discrimination against intermittent, seekable readers shall be ended!

*)

PROCEDURE Init(self: T; h: File.T): FileRd.T RAISES {OSError.E} =
  BEGIN
    self.buff := NEW(CharBuffer, BufferSize);
    EVAL FileRd.T.init(self, h);
    IF self.seekable THEN RETURN NEW(FileRd.T).init(h) END;
    self.past := NIL;
    self.future := NIL;
    self.seekable := TRUE;
    self.intermittent := TRUE; (* in defiance of the law *)
    self.firstSeekablePos := self.cur;
    self.eofSeen := LAST(CARDINAL);
    self.linesBeforeLo := 0;
    RETURN self;
  END Init;

PROCEDURE DiscardPrevious(self: T) =
  VAR
    next := self.past;
  BEGIN
    IF Debug THEN
      Check(self);
      S("forgetting before " & Fmt.Int(self.cur));
    END;
    self.firstSeekablePos := self.cur;
    WHILE next # NIL DO
      next := self.past.tail;
      self.past.tail := self.free;
      self.free := self.past;
      self.past := next;
    END;   Check(self);
  END DiscardPrevious;

PROCEDURE Seek (self: T; pos: CARDINAL; dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    originalPos := self.cur;
  PROCEDURE U(s: TEXT) =
    BEGIN
      IF Debug THEN
        S(s&" bufswap -> lo=" & Fmt.Int(self.lo)&", hi=" & Fmt.Int(self.hi));
      END;
    END U;
  BEGIN
    IF Debug THEN
      S("sr seek to " & Fmt.Int(pos));
      Check(self);
    END;
    <* ASSERT self.st = 0 *>
    <* ASSERT pos >= self.firstSeekablePos *>
    TRY
      IF pos < self.lo THEN
        REPEAT
          U("pre <");
          BuffSwap(self, self.future, self.past, FALSE);
          U("post <");
        UNTIL pos >= self.lo;
        <* ASSERT pos < self.hi *>
        self.cur := pos;   Check(self);
        RETURN RdClass.SeekResult.Ready;
      END;
      WHILE pos >= self.hi DO
        IF self.eofSeen = self.hi THEN
          RETURN RdClass.SeekResult.Eof;        
        END;
        IF self.hi # self.lo THEN
          U("pre >");
          BuffSwap(self, self.past, self.future, TRUE);
          U("post >");
        END;
        IF self.hi = self.lo THEN
          self.seekable := FALSE;
          IF Debug THEN S("fr seek to " & Fmt.Int(self.hi)); END;
          CASE FileRd.T.seek(self, self.hi, dontBlock) OF
          | RdClass.SeekResult.WouldBlock =>
            self.seekable := TRUE;
            RETURN RdClass.SeekResult.WouldBlock;
          | RdClass.SeekResult.Eof =>
            self.seekable := TRUE;
            self.eofSeen := self.hi;
            self.cur := pos;
            RETURN RdClass.SeekResult.Eof;
          ELSE
          END;
          self.seekable := TRUE;
        END;
      END;
      self.cur := pos;   (* Check(self); *)
      RETURN RdClass.SeekResult.Ready;
    EXCEPT
    | Rd.Failure(e) =>
      self.cur := originalPos;
      RAISE Rd.Failure(e);
    END;
  END Seek;

BEGIN
END SeekRdClass.
