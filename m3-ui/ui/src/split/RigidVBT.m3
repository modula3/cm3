(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Jan 31 09:45:13 PST 1995 by kalsow   *)
(*      modified on Mon Feb 24 13:54:21 PST 1992 by muller   *)
(*      modified on Sun Nov 10 19:44:31 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:33:23 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE RigidVBT;

IMPORT VBT, Filter, Axis, VBTClass;

REVEAL T = Public BRANDED OBJECT
    sh: Shape
  OVERRIDES
    shape := ShapeDefault;
    newShape := NewShape;
    init := Be
  END;

PROCEDURE Be(v: T; ch: VBT.T; sh: Shape): T RAISES {} =
  BEGIN
    EVAL Filter.T.init(v, ch);
    v.sh := sh;
    FOR i := FIRST(Axis.T) TO LAST(Axis.T) DO
      WITH s = sh[i] DO
        IF s.pref < s.lo OR s.pref > s.hi THEN Crash() END
      END
    END;
    RETURN v
  END Be;

PROCEDURE New(ch: VBT.T; sh: Shape): T =
  BEGIN RETURN Be(NEW(T), ch, sh) END New;

PROCEDURE FromHV(
  ch: VBT.T; 
  h, v: REAL; 
  hMax, vMax, hPref, vPref: REAL := -1.0)
  : T RAISES {} =
  BEGIN
    IF hMax = -1.0 THEN hMax := h END;
    IF vMax = -1.0 THEN vMax := v END;
    IF hPref = -1.0 THEN hPref := h END;
    IF vPref = -1.0 THEN vPref := v END;
    RETURN New(ch, Shape{SizeRange{lo := h, pref := hPref, hi := hMax},
      SizeRange{lo := v, pref := vPref, hi := vMax}})
  END FromHV;

PROCEDURE NewShape(<*UNUSED*>v: T; <*UNUSED*>ch: VBT.T) RAISES {} = 
  BEGIN END NewShape;

PROCEDURE ShapeDefault(v: T; ax: Axis.T; <*UNUSED*> n: CARDINAL): VBT.SizeRange =
  VAR
    s := v.sh[ax];
    lo := ROUND(VBT.MMToPixels(v, s.lo, ax)); 
    hi := ROUND(VBT.MMToPixels(v, s.hi, ax)) + 1;
    pref := ROUND(VBT.MMToPixels(v, s.pref, ax));
  BEGIN
    RETURN VBT.SizeRange{lo := lo, pref := pref, hi := hi}
  END ShapeDefault;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN END RigidVBT.
