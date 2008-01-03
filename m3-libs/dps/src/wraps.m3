(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:53:53 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:09 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

UNSAFE MODULE wraps;

IMPORT DPS, Text;

PROCEDURE GetTransform ( ctxt: INTEGER; 
  VAR ctm, invctm: ARRAY [0..5] OF REAL;
  VAR xOffset, yOffset: INTEGER ) = 
  BEGIN
  GetTransformWrap ( ctxt, ADR(ctm[0]), ADR(invctm[0]), ADR(xOffset), ADR(yOffset) );
  END GetTransform;

PROCEDURE FetchInteger ( ctxt: INTEGER;
 u: TEXT; alreadyLocked: BOOLEAN := FALSE ): INTEGER = 
 VAR uu: UNTRACED REF ARRAY OF CHAR;
 VAR ret: INTEGER;
  BEGIN
  uu := MakeString (u);
  IF alreadyLocked THEN FetchIntegerWrap ( ctxt, ADR(uu[0]), ADR(ret) );
   ELSE
    TRY DPS.AcquireDPSMutex();
      FetchIntegerWrap ( ctxt, ADR(uu[0]), ADR(ret) );
     FINALLY DPS.ReleaseDPSMutex();
      END;
    END;
  DISPOSE (uu);
  RETURN ret;
  END FetchInteger;

PROCEDURE FetchNumber ( ctxt: INTEGER;
 u: TEXT; alreadyLocked: BOOLEAN := FALSE ): REAL = 
 VAR uu: UNTRACED REF ARRAY OF CHAR;
 VAR ret: REAL;
  BEGIN
  uu := MakeString (u);
  IF alreadyLocked THEN FetchNumberWrap ( ctxt, ADR(uu[0]), ADR(ret) );
   ELSE
    TRY DPS.AcquireDPSMutex();
      FetchNumberWrap ( ctxt, ADR(uu[0]), ADR(ret) );
     FINALLY DPS.ReleaseDPSMutex();
      END;
    END;
  DISPOSE (uu);
  RETURN ret;
  END FetchNumber;

PROCEDURE FetchString ( ctxt: INTEGER; 
 u: TEXT; alreadyLocked: BOOLEAN := FALSE ): TEXT = 
 VAR uu, vv: UNTRACED REF ARRAY OF CHAR;
 VAR ret: TEXT := "";
  BEGIN
  uu := MakeString (u);
  vv := NEW (UNTRACED REF ARRAY OF CHAR, 1000);
  IF alreadyLocked THEN FetchStringWrap ( ctxt, ADR(uu[0]), ADR(vv[0]) );
   ELSE
    TRY DPS.AcquireDPSMutex();
      FetchStringWrap ( ctxt, ADR(uu[0]), ADR(vv[0]) );
     FINALLY DPS.ReleaseDPSMutex();
      END;
    END;
  DISPOSE (uu);
  FOR j := 0 TO 999 DO
    IF ORD(vv^[j]) = 0 THEN RETURN ret; END;
    ret := ret & Text.FromChar (vv^[j]);
    END;
  RETURN ret;
  END FetchString;

PROCEDURE XYHit ( ctxt: INTEGER; x, y: REAL; u: TEXT ): BOOLEAN = 
 VAR uu: UNTRACED REF ARRAY OF CHAR;
 VAR ret: BOOLEAN;
  BEGIN
  uu := MakeString (u); 
  TRY DPS.AcquireDPSMutex();
    XYHitWrap ( ctxt, x, y, ADR(uu[0]), ADR(ret) );
   FINALLY DPS.ReleaseDPSMutex();
    END;
  DISPOSE (uu);
  RETURN ret;
  END XYHit;

PROCEDURE Stringwidth ( ctxt: INTEGER; f: TEXT; s: TEXT; VAR x, y: REAL ) =
 VAR ff, ss: UNTRACED REF ARRAY OF CHAR;
  BEGIN 
  ff := MakePSString (f);
  ss := MakePSString (s);
  TRY DPS.AcquireDPSMutex();
    StringwidthWrap ( ctxt, ADR(ff[0]), ADR(ss[0]), ADR(x), ADR(y) );
   FINALLY DPS.ReleaseDPSMutex();
    END;
  DISPOSE (ff); DISPOSE (ss);
  END Stringwidth;

PROCEDURE MakePSString (t: TEXT): UNTRACED REF ARRAY OF CHAR = 
 VAR s: UNTRACED REF ARRAY OF CHAR;
 VAR len, wax: INTEGER;
 VAR k: INTEGER;
 VAR c: CHAR;
  BEGIN
  len := Text.Length(t);
  wax := 0;
  FOR j := 0 TO len-1 DO 
    CASE Text.GetChar(t, j) OF
    | '(' => wax := wax + 1;
    | ')' => wax := wax + 1;
    | '\\' => wax := wax + 1;
      ELSE
      END; (* of CASE *)
    END; (* of FOR *)
  s := NEW (UNTRACED REF ARRAY OF CHAR, len+wax+1);
  k := 0;
  FOR j := 0 TO len-1 DO
    c := Text.GetChar(t, j);
    CASE c OF
    | '(' => s^[k] := '\\'; s^[k+1] := c; k := k + 2;
    | ')' => s^[k] := '\\'; s^[k+1] := c; k := k + 2;
    | '\\' => s^[k] := '\\'; s^[k+1] := c; k := k + 2;
     ELSE s^[k] := c; k := k + 1;
      END; (* of CASE *)
    END; (* of FOR *)
  s^[k] := '\000';
  RETURN s;
  END MakePSString;

PROCEDURE MakeString (t: TEXT): UNTRACED REF ARRAY OF CHAR = 
 VAR s: UNTRACED REF ARRAY OF CHAR;
 VAR len: INTEGER;
  BEGIN
  len := Text.Length(t);
  s := NEW (UNTRACED REF ARRAY OF CHAR, len+1);
  FOR j := 0 TO len-1 DO s^[j] := Text.GetChar(t, j); END; 
  s^[len] := '\000';
  RETURN s;
  END MakeString;

  BEGIN
  END wraps.


