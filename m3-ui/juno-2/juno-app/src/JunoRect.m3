(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Dec 18 09:04:49 PST 1995 by heydon                   *)

MODULE JunoRect;

IMPORT JunoValue, JunoPt, RTVal;

PROCEDURE Scale(READONLY r: T; s: JunoValue.Real): T =
  VAR res: T; BEGIN
    IF r = Empty THEN RETURN r END;
    res.west := r.west * s; res.south := r.south * s;
    res.east := r.east * s; res.north := r.north * s;
    RETURN res
  END Scale;

PROCEDURE Rotate90(READONLY r: T): T =
  BEGIN RETURN T{
    west := -r.north, east := -r.south,
    south := r.west, north := r.east};
  END Rotate90;

PROCEDURE Add(READONLY r: T; READONLY p: JunoPt.T): T =
  VAR res: T; BEGIN
    IF r = Empty THEN RETURN r END;
    res.south := r.south + p.y;
    res.north := r.north + p.y;
    res.east  := r.east  + p.x;
    res.west  := r.west  + p.x;
    RETURN res
  END Add;

PROCEDURE Join(READONLY r1, r2: T): T =
  VAR res: T; BEGIN
    IF r1 = Empty THEN RETURN r2 ELSIF r2 = Empty THEN RETURN r1 END;
    res.west  := MIN(r1.west,  r2.west);
    res.east  := MAX(r1.east,  r2.east);
    res.south := MIN(r1.south, r2.south);
    res.north := MAX(r1.north, r2.north);
    RETURN res
  END Join;

PROCEDURE ToRTVal(READONLY r: T): RTVal.T =
  BEGIN
    RETURN RTVal.FromPair(
      RTVal.FromPair(RTVal.FromReal(r.west), RTVal.FromReal(r.south)),
      RTVal.FromPair(RTVal.FromReal(r.east), RTVal.FromReal(r.north)))
  END ToRTVal;

BEGIN
END JunoRect.
