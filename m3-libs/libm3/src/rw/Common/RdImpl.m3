(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Jun 28 17:16:33 PDT 1993 by wobber     *)
(*      modified on Wed Jun  2 16:23:06 PDT 1993 by mjordan    *)
(*      modified on Wed May 26 18:15:31 PDT 1993 by swart      *)
(*      modified on Thu Apr 23 12:00:42 PDT 1992 by kalsow     *)
(*      modified on Tue Apr 21 15:56:06 PDT 1992 by muller     *)

(* This module implements the pieces of Rd that only require
   the facilities visible in RdClass and UnsafeRd. *)

MODULE RdImpl EXPORTS Rd;
IMPORT RdClass, Text, TextF;
FROM Thread IMPORT Alerted;
FROM UnsafeRd IMPORT FastGetSub;

PROCEDURE GetSubLine (rd: T;  VAR(*out*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted} =
  VAR i: CARDINAL := 0;
  BEGIN
    RdClass.Lock (rd);
    TRY
      LOOP
        (* i chars have been read into str *)
        IF i = NUMBER (str) THEN  RETURN i  END;
        IF rd.cur = rd.hi THEN
          IF rd.closed THEN Error () END;
          IF rd.seek (rd.cur, FALSE) = RdClass.SeekResult.Eof THEN RETURN i END
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
    FINALLY
      RdClass.Unlock (rd);
    END;
  END GetSubLine;

PROCEDURE GetText (rd: T;  length: CARDINAL): TEXT
  RAISES { Failure, Alerted } =
  VAR txt: TEXT;
  BEGIN
    RdClass.Lock (rd);
    TRY
      IF rd.closed THEN Error () END;

      IF (rd.lo <= rd.cur) AND (rd.hi - rd.cur >= length) THEN
        (* the bytes we need are already in the buffer *)
        txt := Text.FromChars (
                 SUBARRAY (rd.buff^, rd.cur - rd.lo + rd.st, length));
        INC (rd.cur, length);

      ELSIF (NOT rd.intermittent) THEN
        (* we know how long the reader is... *)
        VAR len := MIN (length, rd.length () - rd.cur); BEGIN
          txt := TextF.New (len);
          EVAL FastGetSub (rd, SUBARRAY (txt^, 0, len));
        END;

      ELSE (* general case *)
        txt := SlowGetText (rd, length);
      END;
    FINALLY
      RdClass.Unlock (rd);
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
      txt := TextF.New (copied);
      i := 0;
      n : INTEGER;
    BEGIN
      WHILE (head # NIL) DO
        n := MIN (copied - i, NUMBER (head.buf));
        SUBARRAY (txt^, i, n) := SUBARRAY (head.buf, 0, n);
        head := head.next;
        INC (i, n);
      END;
      RETURN txt;
    END;
  END SlowGetText;

PROCEDURE GetLine (rd: T): TEXT RAISES {EndOfFile, Failure, Alerted} =
  VAR txt := "";   j, n: INTEGER;
  BEGIN
    RdClass.Lock (rd);
    TRY
      LOOP (* INV: txt contains the partial result *)
        IF rd.cur = rd.hi THEN
          IF rd.closed THEN Error () END;
          IF rd.seek (rd.cur, FALSE) = RdClass.SeekResult.Eof THEN
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
    FINALLY
      RdClass.Unlock (rd);
    END;
  END GetLine;

PROCEDURE Intermittent (rd: T): BOOLEAN RAISES { } =
  BEGIN
    RETURN (rd.intermittent);
  END Intermittent;

PROCEDURE Seekable (rd: T): BOOLEAN RAISES { } =
  BEGIN
    RETURN (rd.seekable);
  END Seekable;

PROCEDURE Closed (rd: T): BOOLEAN RAISES { } =
  BEGIN
    RETURN (rd.closed);
  END Closed;

EXCEPTION FatalError;   <* FATAL FatalError *>  

PROCEDURE Error () RAISES {} =
  BEGIN
    RAISE FatalError;
  END Error;

BEGIN
END RdImpl.
