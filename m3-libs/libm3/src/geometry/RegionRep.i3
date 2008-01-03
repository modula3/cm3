(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Feb 11 16:24:34 PST 1992 by muller   *)
(*      modified on Wed Sep 11 15:19:39 PDT 1991 by msm      *)
<*PRAGMA LL*>

INTERFACE RegionRep;

IMPORT Interval, Region;

TYPE
  HList = REF ARRAY OF Interval.T;
  VEntry = RECORD v: Interval.T; h: HList END;
  VList = Region.P;

REVEAL Region.P = BRANDED REF ARRAY OF VEntry;

END RegionRep.
