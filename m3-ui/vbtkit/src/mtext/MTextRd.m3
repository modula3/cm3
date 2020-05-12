(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul  9 10:59:17 PDT 1993 by wobber                   *)
(*      modified on Fri May 14 15:43:27 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 30 16:01:44 PDT 1992 by mhb                      *)
(*      modified on Tue Jun 16 13:16:22 PDT 1992 by muller                   *)
(*      modified on Sat Feb 29 11:46:47 PST 1992 by kalsow                   *)
(*      modified on Mon May 7 8:49:45 PDT 1990 by mcjones                    *)
(*      modified on Wed Nov 15 10:29:45 1989 by chan                         *)
(*      modified on Thu May 4 9:27:56 PDT 1989 by mbrown                     *)

MODULE MTextRd;

IMPORT RdClass, MText, MTextDs, MTextPrivate, Rd, Text, Thread;

FROM MTextPrivate IMPORT Node, NodeType;

CONST BufferSize = 512;

REVEAL
  T = Public BRANDED "MTextRd.T.2" OBJECT
        m                   : MText.T;
        rangeStart, rangeEnd: CARDINAL;
        len, first          : CARDINAL;
        last                : INTEGER;
        version             : INTEGER;
        reverse             : BOOLEAN;
        chars               : REF ARRAY OF CHAR := NIL
      OVERRIDES
        length := Length;
        seek   := Seek;
        init   := Init
      END;

PROCEDURE New (m         : MText.T  := NIL;
               start     : CARDINAL := 0;
               rangeStart: CARDINAL := 0;
               rangeEnd  : CARDINAL := LAST (CARDINAL);
               reverse   : BOOLEAN  := FALSE;           ): T
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN NEW (T).init (m, start, rangeStart, rangeEnd, reverse)
  END New;

PROCEDURE Init (rd        : T;
                m         : MText.T  := NIL;
                start     : CARDINAL := 0;
                rangeStart: CARDINAL := 0;
                rangeEnd  : CARDINAL := LAST (CARDINAL);
                reverse   : BOOLEAN  := FALSE            ): T
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    IF m # NIL THEN rd.m := m END;
    rd.first := rangeStart;
    rd.last := MIN (rangeEnd, rd.m.length) - 1;
    rd.len := MAX (rd.last - rd.first + 1, 0);
    rd.reverse := reverse;
    rd.closed := FALSE;
    rd.seekable := TRUE;
    rd.intermittent := FALSE;
    rd.st := 0;
    rd.lo := 0;
    rd.hi := 0;
    rd.Ungetlo := 0;
    rd.Ungethi := 0;
    rd.Waitinglo := 0;
    rd.Waitinghi := 0;
    IF rd.chars = NIL THEN
      rd.chars := NEW (REF ARRAY OF CHAR, BufferSize)
    END;
    rd.buff := rd.chars;
    IF reverse THEN
      IF rd.len = 0 THEN
        rd.cur := 0
      ELSIF start < rd.first THEN
        rd.cur := rd.len
      ELSIF start > rd.last THEN
        rd.cur := 0
      ELSE
        rd.cur := rd.last + 1 - start
      END
    ELSE
      IF rd.len = 0 THEN
        rd.cur := 0
      ELSIF start < rd.first THEN
        rd.cur := 0
      ELSIF start > rd.last THEN
        rd.cur := rd.len
      ELSE
        rd.cur := start - rd.first
      END
    END;
    Rd.Seek (rd, rd.cur);
    RETURN rd
  END Init;

PROCEDURE Seek (rd: T; n: CARDINAL; dontBlock: BOOLEAN): RdClass.SeekResult
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    IF n >= rd.len THEN
      rd.cur := rd.len;
      rd.lo := rd.cur;
      rd.hi := rd.cur;
      RETURN RdClass.SeekResult.Eof
    END;
    IF rd.reverse THEN
      RETURN RevSeek (rd, n, dontBlock)
    ELSE
      RETURN ForwardSeek (rd, n, dontBlock)
    END
  END Seek;

PROCEDURE ForwardSeek (rd: T; n: CARDINAL; <* UNUSED *> dontBlock: BOOLEAN):
  RdClass.SeekResult RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    beginN          : Node;
    beginI, count, i: CARDINAL;
    result                     := RdClass.SeekResult.Ready;
  BEGIN
    MTextDs.LocateB (rd.m, n + rd.first, beginN, beginI);
    count := MIN (MIN (beginN.length - beginI, BufferSize), rd.len - n);
    CASE beginN.type OF
    | NodeType.text =>
        Text.SetChars (SUBARRAY (rd.buff^, 0, count), beginN.text, beginI);
    | NodeType.buf =>
        SUBARRAY (rd.buff^, 0, count) :=
          SUBARRAY (beginN.buffer^, beginI, count);
    | NodeType.file =>
        Rd.Seek (beginN.file, beginI + beginN.start);
        i := 0;
        TRY
          WHILE i < count DO
            rd.buff [i] := Rd.GetChar (beginN.file);
            INC (i)
          END
        EXCEPT
          Rd.EndOfFile =>
        END;
        count := i;
    ELSE                         <* ASSERT FALSE *>
    END;
    rd.cur := n;
    rd.lo := n;
    rd.hi := rd.lo + count;
    RETURN result
  END ForwardSeek;

PROCEDURE RevSeek (rd: T; n: CARDINAL; <* UNUSED *> dontBlock: BOOLEAN):
  RdClass.SeekResult RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    beginN                 : Node;
    beginI, count, first, i: CARDINAL;
  PROCEDURE RevCopy (READONLY v: ARRAY OF CHAR) =
    BEGIN
      FOR i := 1 TO count DO rd.buff [count - i] := v [first + i - 1] END
    END RevCopy;
  PROCEDURE RevCopyText (READONLY t: TEXT) =
    BEGIN
      FOR i := 1 TO count DO rd.buff [count - i] :=
                   Text.GetChar(t,first + i - 1)
      END;
    END RevCopyText;
  BEGIN
    MTextDs.Locate (rd.m, rd.last + 1 - n, beginN, beginI);
    count := MIN (MIN (beginI, BufferSize), rd.len - n);
    first := beginI - count;
    CASE beginN.type OF
    | NodeType.text =>
        RevCopyText (beginN.text);
    | NodeType.file =>
        Rd.Seek (beginN.file, beginN.start + first);
        i := 0;
        TRY
          WHILE i < count DO
            rd.buff [count - 1 - i] := Rd.GetChar (beginN.file);
            INC (i)
          END
        EXCEPT
          Rd.EndOfFile =>
        END;
        count := i
    | NodeType.buf => RevCopy (beginN.buffer^)
    ELSE                         <* ASSERT FALSE *>
    END;
    rd.cur := n;
    rd.lo := n;
    rd.hi := rd.lo + count;
    RETURN RdClass.SeekResult.Ready
  END RevSeek;


PROCEDURE Length (rd: T): INTEGER =
  BEGIN
    RETURN rd.len
  END Length;

BEGIN
END MTextRd.



