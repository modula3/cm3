(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 26 13:59:01 PST 1995 by kalsow     *)
(*      modified on Fri Jun 18 17:40:31 PDT 1993 by wobber     *)
(*      modified on Mon May 24 16:18:46 PDT 1993 by swart      *)
(*      modified on Tue Jun 25 10:54:37 1991 by muller         *)

MODULE NullWr;

IMPORT WrClass;

CONST
  BuffSize = 1024;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        seek:= Seek;
        putString := PutString;
        init := Init;
      END;

PROCEDURE Init (wr: T): T =
  BEGIN
    wr.st := 0;
    wr.lo := 0;
    wr.cur := 0;
    wr.hi := BuffSize;
    IF (wr.buff = NIL) THEN
      wr.buff := NEW(REF ARRAY OF CHAR, BuffSize);
    END;
    wr.closed := FALSE;
    wr.seekable := FALSE;
    wr.buffered := TRUE;
    RETURN wr;
  END Init;

PROCEDURE Seek (wr: T; n: CARDINAL) =
  BEGIN
    wr.lo := n;
    wr.cur := n;
    wr.hi := n + BuffSize;
  END Seek;

PROCEDURE PutString (wr: T; READONLY a: ARRAY OF CHAR) =
  BEGIN
    Seek(wr, wr.cur + NUMBER(a));
  END PutString;

BEGIN
END NullWr.
