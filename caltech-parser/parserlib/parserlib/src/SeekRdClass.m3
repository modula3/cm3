MODULE SeekRdClass;
IMPORT Rd;
IMPORT RdClass;
IMPORT FileRd;
IMPORT File;
IMPORT OSError;
IMPORT Thread;

(* IMPORT Wr,Fmt,Stdio; *)

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
  END;

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

PROCEDURE Init(self: T; h: File.T): FileRd.T RAISES {OSError.E} =
  BEGIN
    self.buff := NEW(CharBuffer, BufferSize);
    EVAL FileRd.T.init(self, h);
    IF self.seekable THEN RETURN NEW(FileRd.T).init(h) END;
    self.past := NIL;
    self.future := NIL;
    self.seekable := TRUE;
    self.intermittent := FALSE; (* actually it is, but Rd spec restricts *)
    self.firstSeekablePos := self.cur;
    self.eofSeen := LAST(CARDINAL);
    self.linesBeforeLo := 0;
    RETURN self;
  END Init;

PROCEDURE DiscardPrevious(self: T) =
  VAR
    next := self.past;
  BEGIN
    self.firstSeekablePos := self.cur;
    WHILE next # NIL DO
      next := self.past.tail;
      self.past.tail := self.free;
      self.free := self.past;
      self.past := next;
    END;
  END DiscardPrevious;

PROCEDURE Seek (self: T; pos: CARDINAL; dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    originalPos := self.cur;
  BEGIN
    <* ASSERT self.st = 0 *>
    <* ASSERT pos >= self.firstSeekablePos *>
    <* ASSERT NOT dontBlock *> (* they can't know we're intermittent *)
    TRY
      IF pos < self.lo THEN
        REPEAT
          BuffSwap(self, self.future, self.past, FALSE);
        UNTIL pos >= self.lo;
        <* ASSERT pos < self.hi *>
        self.cur := pos;
        RETURN RdClass.SeekResult.Ready;
      END;
      WHILE pos >= self.hi DO
        IF self.eofSeen = self.hi THEN
          RETURN RdClass.SeekResult.Eof;        
        END;
        IF self.hi # self.lo THEN
          BuffSwap(self, self.past, self.future, TRUE);
        END;
        IF self.hi = self.lo THEN
          self.seekable := FALSE;
          (* Wr.PutText(Stdio.stdout,"seeking to " & Fmt.Int(self.hi) & "\n");
             Wr.Flush(Stdio.stdout); *)
          CASE FileRd.T.seek(self, self.hi, dontBlock) OF
          | RdClass.SeekResult.WouldBlock => <* ASSERT FALSE *>
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
      self.cur := pos;
      RETURN RdClass.SeekResult.Ready;
    EXCEPT
    | Rd.Failure(e) =>
      self.cur := originalPos;
      RAISE Rd.Failure(e);
    END;
  END Seek;

BEGIN
END SeekRdClass.
