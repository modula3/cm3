(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Jan 31 23:40:52 PST 1993 by mhb    *)
(*      modified on Mon Aug 10  0:31:46 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 13:07:55 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ZTilps.T" multi-split is like a "ZSplit", except that its
   children are stored from bottom to top.  For example,
   "MultiSplit.Nth(v,0)" returns the background child of the
   "ZTilps". *)

INTERFACE ZTilps;

IMPORT ZSplit;

TYPE
  <* SUBTYPE T <: MultiSplit.T *>
  T <: Public;
  Public = ZSplit.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (saveBits := FALSE; parlim := -1): T
           END;

(* The call "v.init(...)" initializes "v" as a "ZTilps" and
   returns "v". See the "ZSplit" interface for a description of
   "saveBits" and "parlim". *)

END ZTilps.



