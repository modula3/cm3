(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Jun 18 17:59:09 PDT 1993 by wobber         *)
(*      modified on Thu May 13 16:56:29 PDT 1993 by swart          *)
(*      modified on Sat Aug  3 01:25:40 1991 by kalsow         *)
(*      modified on Tue Jun 25 10:53:55 1991 by muller         *)

MODULE NullRd;

IMPORT RdClass;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        seek   := Seek;
        length := Length;
        init   := Init;
      END;

PROCEDURE Init (rd: T): T =
  BEGIN
    rd.st := 0;
    rd.lo := 0;
    rd.cur := 0;
    rd.hi := 0;
    rd.buff := NIL;
    rd.closed := FALSE;
    rd.seekable := TRUE;
    rd.intermittent := FALSE;
    RETURN rd;
  END Init;

PROCEDURE Seek (<*UNUSED*> rd: T; <*UNUSED*> pos: CARDINAL;
                                  <*UNUSED*> dontBlock: BOOLEAN):
  RdClass.SeekResult =
  BEGIN
    RETURN (RdClass.SeekResult.Eof);
  END Seek;

PROCEDURE Length(<*UNUSED*> rd: T): INTEGER =
  BEGIN RETURN 0; END Length;

BEGIN
END NullRd.


                
  
