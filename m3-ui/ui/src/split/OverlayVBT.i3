(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr  4 11:36:49 PDT 1995 by msm                      *)
<* PRAGMA LL *>

(* An "OverlayVBT" is a filter that can paint on top of its child. *)

INTERFACE OverlayVBT;

IMPORT VBT, Region, Filter;

TYPE
  T <: Public;
  Public =
    Filter.T OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      init (ch: VBT.T): T;
      <* LL = VBT.mu.v *>
      set (READONLY rgn: Region.T);
      (* client-supplied method: *)
      <* LL.sup = ch *>
      paint (READONLY rgn: Region.T);
    END;

(* The call "v.init(..)" initializes "v" as an "OverlayVBT"
   with child "ch" and an empty overlay region.  By "set"ting the
   region to be non-empty, the client's "paint" method will be called
   as needed to maintain the client overlay; the region supplied
   will be the subset of the region that needs to be repaired.  Clients
   may want to override the reshape method to set an appropriate region. *)

REVEAL VBT.Split <: VBT.Leaf; 
(* So that painting on a T is known to be legit. *)

END OverlayVBT.
