(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 26 13:58:35 PST 1995 by kalsow     *)
(*      modified on Fri Jun 18 18:12:46 PDT 1993 by wobber     *)
(*      modified on Tue Jun 15 13:41:05 1993 by gnelson        *)
(*      modified on Mon May 31 06:25:34 PDT 1993 by swart      *)
(*      modified on Mon Apr 26 17:22:58 PDT 1993 by mcjones    *)
(*      modified on Tue Apr 21 15:56:06 PDT 1992 by muller     *)

(* This module is very similar to the Wr module, so we will list
its code with only a few comments.  *)

MODULE Rd EXPORTS Rd, RdClass, UnsafeRd;
IMPORT Text, Text8, Thread, Word;
FROM Thread IMPORT Alerted;

REVEAL
  Private = Thread.Mutex BRANDED OBJECT END;

(* FastGetChar and GetChar are identical except that GetChar acquires
   and releases the lock while FastGetChar assumes it is already held. *)

(* It is invariant that for a closed reader "rd", "rd.buff = NIL" and
   "rd.lo = rd.hi".  Therefore the check that "rd" is ready need
   not inspect "rd.closed" on the fast path. *)

<*INLINE*>
PROCEDURE GetChar (rd: T): CHAR
  RAISES {EndOfFile, Failure, Alerted} =
  VAR res: CHAR;
  BEGIN
    LOCK rd DO
      IF rd.cur = rd.hi THEN DoSeek(rd) END;
      res := rd.buff[rd.st + (rd.cur - rd.lo)];
      INC(rd.cur);
      RETURN res
    END
  END GetChar;

<*INLINE*>
PROCEDURE FastGetChar(rd: T): CHAR
  RAISES {EndOfFile, Failure, Alerted} =
  (* rd is locked *)
  VAR res: CHAR; BEGIN
    IF rd.cur = rd.hi THEN DoSeek(rd) END;
    res := rd.buff[rd.st + (rd.cur - rd.lo)];
    INC(rd.cur);
    RETURN res
  END FastGetChar;

PROCEDURE DoSeek(rd: T)
  RAISES {EndOfFile, Failure, Alerted} =
  BEGIN
    (* rd.cur = rd.hi here *)
    IF rd.closed THEN Die() END;
    IF rd.seek(rd.cur, FALSE) = SeekResult.Eof THEN
      RAISE EndOfFile
    END
  END DoSeek;

<*INLINE*>
PROCEDURE GetWideChar (rd: T): WIDECHAR
  RAISES {EndOfFile, Failure, Alerted} =
  VAR ch: WIDECHAR;
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die() END;
      IF NOT GetWC (rd, ch) THEN RAISE EndOfFile; END;
      RETURN ch;
    END
  END GetWideChar;

<*INLINE*>
PROCEDURE FastGetWideChar(rd: T): WIDECHAR
  RAISES {EndOfFile, Failure, Alerted} =
  (* rd is locked *)
  VAR ch: WIDECHAR;
  BEGIN
    IF rd.closed THEN Die() END;
    IF NOT GetWC (rd, ch) THEN RAISE EndOfFile; END;
    RETURN ch;
  END FastGetWideChar;

<*INLINE*>
PROCEDURE GetWC(rd: T;  VAR(*OUT*) ch: WIDECHAR): BOOLEAN
  RAISES {Failure, Alerted} =
  (* rd is locked *)
  VAR c1, c2: CHAR;
  BEGIN

    IF rd.cur = rd.hi THEN
      IF rd.seek(rd.cur, FALSE) = SeekResult.Eof THEN
        RETURN FALSE;
      END;
    END;
    c1 := rd.buff[rd.st + (rd.cur - rd.lo)];
    INC(rd.cur);

    IF rd.cur # rd.hi THEN
      c2 := rd.buff[rd.st + (rd.cur - rd.lo)];
      INC(rd.cur);
    ELSIF rd.seek(rd.cur, FALSE) = SeekResult.Eof THEN
      c2 := '\000';
    ELSE
      c2 := rd.buff[rd.st + (rd.cur - rd.lo)];
      INC(rd.cur);
    END;

    ch := VAL (Word.LeftShift (ORD (c2), 8) + ORD (c1), WIDECHAR);
    RETURN TRUE;
  END GetWC;
  
PROCEDURE GetSub (rd: T; VAR (*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die() END;
      RETURN rd.getSub(str)
    END
  END GetSub;
  
PROCEDURE FastGetSub (rd: T; VAR (*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  BEGIN 
    IF rd.closed THEN Die() END;
    RETURN rd.getSub(str) 
  END FastGetSub;
  
PROCEDURE GetWideSub (rd: T; VAR (*out*) str: ARRAY OF WIDECHAR): CARDINAL
  RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO
      RETURN FastGetWideSub(rd, str);
    END;
  END GetWideSub;
  
PROCEDURE FastGetWideSub (rd: T; VAR (*out*) str: ARRAY OF WIDECHAR): CARDINAL
  RAISES {Failure, Alerted} =
  VAR len := 0;  ch: WIDECHAR;
  BEGIN
    IF rd.closed THEN Die() END;
    WHILE (len < NUMBER (str)) AND GetWC (rd, ch) DO
      str[len] := ch;  INC (len);
    END;
    RETURN len;
  END FastGetWideSub;

PROCEDURE GetSubDefault (rd: T; VAR (*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  VAR i := 0; BEGIN
    LOOP
      (* i chars have been read into str *)
      IF i = NUMBER(str) THEN EXIT END;
      IF rd.cur = rd.hi THEN
        IF rd.seek(rd.cur, FALSE) = SeekResult.Eof THEN EXIT END
      END;
      (* rd.lo <= rd.cur < rd.hi *)
      VAR n := MIN(rd.hi - rd.cur, NUMBER(str) - i); BEGIN
        SUBARRAY(str, i, n) :=
          SUBARRAY(rd.buff^, rd.cur - rd.lo + rd.st, n);
        INC(i, n);
        INC(rd.cur, n)
      END
    END;
    RETURN i
  END GetSubDefault;


(* EOF and FastEOF are identical except that EOF acquires and releases
   the reader lock while FastEOF assumes it is already held. *)

<*INLINE*> 
PROCEDURE EOF (rd: T): BOOLEAN
  RAISES {Failure, Alerted} =
  (* rd is unlocked *)
  BEGIN
    LOCK rd DO
      IF rd.cur # rd.hi THEN
        RETURN FALSE
      ELSE
        IF rd.closed THEN Die() END;
        RETURN rd.seek(rd.cur, FALSE) = SeekResult.Eof
      END
    END
  END EOF;
  
<*INLINE*> 
PROCEDURE FastEOF (rd: T): BOOLEAN
  RAISES {Failure, Alerted} =
  BEGIN
    (* rd is locked *)
    IF rd.cur # rd.hi THEN
      RETURN FALSE
    ELSE
      IF rd.closed THEN Die() END;
      RETURN rd.seek(rd.cur, FALSE) = SeekResult.Eof
    END
  END FastEOF;

PROCEDURE UnGetChar(rd: T) =
  BEGIN
    LOCK rd DO FastUnGetChar (rd) END;
  END UnGetChar;

PROCEDURE FastUnGetChar(rd: T) =
  BEGIN
    IF rd.closed OR rd.cur = rd.lo THEN Die() END;
    DEC(rd.cur)
  END FastUnGetChar;

PROCEDURE CharsReady(rd: T): CARDINAL
  RAISES {Failure} =
  <*FATAL Thread.Alerted*>
  BEGIN
    LOCK rd DO
      IF rd.cur = rd.hi THEN
        IF rd.closed THEN Die() END;
        IF rd.seek(rd.cur, TRUE) = SeekResult.Eof THEN RETURN 1 END
      END;
      RETURN rd.hi - rd.cur;
    END
  END CharsReady;

PROCEDURE Index(rd: T): CARDINAL =
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die() END;
      RETURN rd.cur
    END
  END Index;

PROCEDURE Length(rd: T): INTEGER
  RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die() END;
      RETURN rd.length()
    END
  END Length;

PROCEDURE Seek(rd: T; n: CARDINAL)
  RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO
      IF rd.closed OR NOT rd.seekable THEN Die() END;
      IF n < rd.lo OR n > rd.hi THEN
        EVAL rd.seek(n, FALSE);
      ELSE
        rd.cur := n;
      END
    END
  END Seek;

PROCEDURE Close(rd: T)
  RAISES {Failure, Alerted} =
  BEGIN
    LOCK rd DO FastClose (rd); END;
  END Close;

PROCEDURE FastClose(rd: T)
  RAISES {Failure, Alerted} =
  BEGIN
    IF NOT rd.closed THEN
      TRY
        rd.close()
      FINALLY
        rd.closed := TRUE;
        rd.cur := rd.hi;
        rd.lo := rd.hi;
        rd.buff := NIL
      END
    END
  END FastClose;

PROCEDURE GetSubLine (rd: T;  VAR(*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  VAR i: CARDINAL := 0;
  BEGIN
    LOCK rd DO
      LOOP
        (* i chars have been read into str *)
        IF i = NUMBER (str) THEN  RETURN i  END;
        IF rd.cur = rd.hi THEN
          IF rd.closed THEN Die () END;
          IF rd.seek (rd.cur, FALSE) = SeekResult.Eof THEN RETURN i END
        END;
        (* rd is ready *)
        VAR
          n := MIN (rd.hi, rd.cur + NUMBER (str) - i) - rd.lo + rd.st;
          j := rd.cur - rd.lo + rd.st;
        BEGIN
          WHILE (j # n) AND (rd.buff[j] # '\n') DO INC (j) END;
          VAR
            rd_cur := rd.cur - rd.lo + rd.st;
            k := j - rd_cur;
          BEGIN
            SUBARRAY (str, i, k) := SUBARRAY (rd.buff^, rd_cur, k);
            INC (i, k);
            INC (rd.cur, k);
          END;
          IF (j # n) THEN
            (* we found a newline *)
            str[i] := '\n';
            INC (i);
            INC (rd.cur);
            RETURN i;
          END;
        END;
      END;
    END;
  END GetSubLine;

PROCEDURE GetWideSubLine (rd: T;  VAR(*out*) str: ARRAY OF WIDECHAR): CARDINAL
  RAISES {Failure, Alerted} =
  VAR i: CARDINAL := 0;  ch: WIDECHAR;
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die () END;
      WHILE (i < NUMBER (str)) AND GetWC (rd, ch) DO
        str[i] := ch;  INC (i);
        IF ch = W'\n' THEN RETURN i; END;
      END;
      RETURN i;
    END;
  END GetWideSubLine;

PROCEDURE GetText (rd: T;  length: CARDINAL): TEXT
  RAISES { Failure, Alerted } =
  VAR txt: TEXT;
  BEGIN
    LOCK rd DO
      IF rd.closed THEN Die () END;

      IF (rd.lo <= rd.cur) AND (rd.hi - rd.cur >= length) THEN
        (* the bytes we need are already in the buffer *)
        txt := Text.FromChars (
                 SUBARRAY (rd.buff^, rd.cur - rd.lo + rd.st, length));
        INC (rd.cur, length);

      ELSIF (NOT rd.intermittent) THEN
        (* we know how long the reader is... *)
        VAR
          len := MIN (length, rd.length () - rd.cur);
          txt8 := Text8.Create (len);
        BEGIN
          txt := txt8;
          EVAL FastGetSub (rd, SUBARRAY (txt8.contents^, 0, len));
        END;

      ELSE (* general case *)
        txt := SlowGetText (rd, length);
      END;
    END;
    RETURN txt;
  END GetText;

TYPE Buffer = REF RECORD next: Buffer;  buf: ARRAY [0..2039] OF CHAR END;

PROCEDURE SlowGetText (rd: T;  length: CARDINAL): TEXT
  RAISES { Failure, Alerted } =
  VAR
    copied: CARDINAL := 0;
    head : Buffer := NIL;
    tail : Buffer := NIL;
  BEGIN

    (* build a list of buffers *)
    LOOP
      IF (copied = length) THEN EXIT END;
      VAR b := NEW (Buffer, next := NIL); BEGIN
        IF (head = NIL)
          THEN head      := b;
          ELSE tail.next := b;
        END;
        tail := b;
      END;
      VAR
        n := MIN (length - copied, NUMBER (tail.buf));
        i := FastGetSub (rd, SUBARRAY (tail.buf, 0, n));
      BEGIN
        INC (copied, i);
        IF (i < n) THEN EXIT END;
      END;
    END;

    (* assemble the result *)
    VAR
      txt := Text8.Create (copied);
      i := 0;
      n : INTEGER;
    BEGIN
      WHILE (head # NIL) DO
        n := MIN (copied - i, NUMBER (head.buf));
        SUBARRAY (txt.contents^, i, n) := SUBARRAY (head.buf, 0, n);
        head := head.next;
        INC (i, n);
      END;
      RETURN txt;
    END;
  END SlowGetText;

PROCEDURE GetWideText(rd: T; len: CARDINAL): TEXT
  RAISES {Failure, Alerted} =
  VAR
    res, tmp: TEXT;
    i, j, n_read: CARDINAL;
    buf: ARRAY [0..127] OF WIDECHAR;
  BEGIN
    IF (len <= NUMBER (buf)) THEN
      i := GetWideSub(rd, SUBARRAY(buf, 0, len));
      RETURN Text.FromWideChars (SUBARRAY(buf, 0, i));
    ELSE
      res := NIL;  n_read := 0;
      WHILE (n_read < len) DO
        i := MIN (NUMBER (buf), len - n_read);
        j := GetWideSub(rd, SUBARRAY (buf, 0, i));
        INC (n_read, j);
        IF (j > 0) THEN
          tmp := Text.FromWideChars (SUBARRAY (buf, 0, j));
          IF (res = NIL) THEN res := tmp; ELSE res := res & tmp; END;
        END;
        IF (j < i) THEN EXIT; END;
      END;
      IF (res = NIL) THEN res := ""; END;
      RETURN res;
    END;
  END GetWideText;

PROCEDURE GetLine (rd: T): TEXT
  RAISES {EndOfFile, Failure, Alerted} =
  VAR txt := "";   j, n: INTEGER;
  BEGIN
    LOCK rd DO
      LOOP (* INV: txt contains the partial result *)
        IF rd.cur = rd.hi THEN
          IF rd.closed THEN Die () END;
          IF rd.seek (rd.cur, FALSE) = SeekResult.Eof THEN
            IF (Text.Length (txt) > 0) THEN RETURN txt END;
            RAISE EndOfFile;
          END;
        END;
        (* rd is ready *)
        n := rd.hi - rd.lo + rd.st;
        j := rd.cur - rd.lo + rd.st;
        WHILE (j # n) AND rd.buff[j] # '\n' DO INC(j) END;
        VAR rd_cur := rd.cur - rd.lo + rd.st;
            len := j - rd_cur;
         BEGIN
          IF len >= 1 AND j # n  AND rd.buff[j-1] = '\r' THEN
            (* segment ends in \r\n *)
            txt := txt & Text.FromChars (SUBARRAY (rd.buff^, rd_cur, len-1));
            INC (rd.cur, len+1);
            RETURN txt;
          ELSIF j # n THEN
            (* segment ends in \n *)
            txt := txt & Text.FromChars (SUBARRAY (rd.buff^, rd_cur, len));
            INC (rd.cur, len+1);
            IF NOT Text.Empty(txt) AND
                 Text.GetChar(txt, Text.Length(txt)-1) = '\r' THEN
              txt := Text.Sub(txt, 0, Text.Length(txt)-1)
            END;
            RETURN txt;
          ELSE
            (* segment does not contain line break *)
            txt := txt & Text.FromChars (SUBARRAY (rd.buff^, rd_cur, len));
            INC (rd.cur, len);
          END;
        END;
      END; (* LOOP *)
    END;
  END GetLine;

PROCEDURE GetWideLine (rd: T): TEXT
  RAISES {EndOfFile, Failure, Alerted} =
  VAR
    txt, tmp : TEXT := NIL;
    len : CARDINAL := 0;
    last_ch, ch: WIDECHAR;
    buf : ARRAY [0..127] OF WIDECHAR;
  BEGIN
    LOCK rd DO
      last_ch := W' ';
      LOOP
        IF FastEOF(rd) THEN
          IF (txt = NIL) AND (len = 0) THEN RAISE EndOfFile; END;
          EXIT;
        END;
        ch := FastGetWideChar (rd);
        IF (ch = W'\n') THEN EXIT; END;
        IF len >= NUMBER (buf) THEN
          tmp := Text.FromWideChars (buf);
          IF (txt = NIL) THEN txt := tmp;  ELSE txt := txt & tmp; END;
          len := 0;
        END;
        buf[len] := ch;  INC (len);
        last_ch := ch;
      END;
    END;

    IF (ch = W'\n') AND (last_ch = W'\r') AND (len > 0) THEN
      (* remove the carriage return before allocating the text *)
      DEC (len);  last_ch := W' ';
    END;
    IF (len > 0) THEN
      tmp := Text.FromWideChars (SUBARRAY (buf, 0, len));
      IF (txt = NIL) THEN txt := tmp; ELSE txt := txt & tmp; END;
    END;
    IF (txt = NIL) THEN txt := ""; END;
    IF (ch = W'\n') AND (last_ch = W'\r') THEN
      txt := Text.Sub (txt, 0, Text.Length (txt) - 1);
    END;
    RETURN txt;
  END GetWideLine;

PROCEDURE Intermittent (rd: T): BOOLEAN =
  BEGIN
    RETURN (rd.intermittent);
  END Intermittent;

PROCEDURE Seekable (rd: T): BOOLEAN =
  BEGIN
    RETURN (rd.seekable);
  END Seekable;

PROCEDURE Closed (rd: T): BOOLEAN =
  BEGIN
    RETURN (rd.closed);
  END Closed;

PROCEDURE Lock (rd: T) =
  BEGIN
    Thread.Acquire (rd)
  END Lock;

PROCEDURE Unlock (rd: T) =
  BEGIN
    Thread.Release (rd)
  END Unlock;

PROCEDURE LengthDefault (<*UNUSED*> rd: T): INTEGER =
  BEGIN
    <*NOWARN*> Die()
  END LengthDefault;

PROCEDURE CloseDefault(<*UNUSED*> rd: T) =
  BEGIN
  END CloseDefault;

(*---------------------------------------------------------- internal ---*)

EXCEPTION FatalError;

PROCEDURE Die() =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Die;

BEGIN
END Rd.
