(* Copyright 1994 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE XRd;

IMPORT RdClass;

REVEAL T = Public BRANDED OBJECT 
  len: CARDINAL
  OVERRIDES init := Init; seek := Seek; length := Length END;

CONST BuffSize = 8*1024;

PROCEDURE Init (xrd: T; n: CARDINAL): T = 
  BEGIN
    xrd.len := n;
    xrd.buff := NEW (REF ARRAY OF CHAR, BuffSize);
    xrd.st := 0;
    xrd.lo := 0;
    xrd.hi := BuffSize;
    xrd.cur := 0;
    xrd.closed := FALSE;
    xrd.seekable := TRUE;
    xrd.intermittent := FALSE;
    FOR i := 0 TO LAST (xrd.buff^) DO
      xrd.buff[i] := 'X'
    END;
    RETURN xrd                   
  END Init;
  
PROCEDURE Seek (xrd: T; pos: CARDINAL; <*UNUSED*> dontblock: BOOLEAN): 
	  RdClass.SeekResult =
  BEGIN
    xrd.cur := MIN (pos, xrd.len);
    xrd.lo := xrd.cur;
    xrd.hi := xrd.cur + NUMBER (xrd.buff^);
    RETURN RdClass.SeekResult.Ready
  END Seek;

PROCEDURE Length (xrd: T): INTEGER = 
  BEGIN RETURN xrd.len END Length;

BEGIN END XRd.
                  
