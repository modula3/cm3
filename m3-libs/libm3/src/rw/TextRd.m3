(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Mar 21 13:15:19 PST 1994 by kalsow     *)
(*      modified on Fri Jun 18 11:34:44 PDT 1993 by wobber     *)
(*      modified on Thu May 20 15:22:41 PDT 1993 by swart      *)
(*      modified on Mon Apr 26 17:25:44 PDT 1993 by mcjones    *)
(*      modified on Thu Jul 11 20:58:41 1991 by muller         *)

MODULE TextRd;

IMPORT RdClass, Text;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        seek   := Seek;
        length := Length;
        init   := Init;
      END;

PROCEDURE Init (rd: T; t: TEXT): T =
  VAR len := Text.Length (t);
  BEGIN
    IF (rd.buff = NIL) OR (len > NUMBER (rd.buff^)) THEN
      rd.buff := NEW(REF ARRAY OF CHAR, len);
    END;
    Text.SetChars(rd.buff^, t);
    rd.st := 0;
    rd.lo := 0;
    rd.cur := 0;
    rd.hi := len;
    rd.closed := FALSE;
    rd.seekable := TRUE;
    rd.intermittent := FALSE;
    RETURN rd;
  END Init;

PROCEDURE New(t: TEXT): T = BEGIN RETURN NEW(T).init(t); END New;

PROCEDURE Seek (rd: T; pos: CARDINAL;
               <*UNUSED*> dontBlock: BOOLEAN): RdClass.SeekResult =
  BEGIN
    IF pos >= rd.hi THEN
      rd.cur := rd.hi;
      RETURN RdClass.SeekResult.Eof;
    ELSE
      rd.cur := pos;
      RETURN RdClass.SeekResult.Ready; END;
  END Seek;

PROCEDURE Length (rd: T): INTEGER =
  BEGIN
    RETURN rd.hi;
  END Length;

BEGIN
END TextRd.

