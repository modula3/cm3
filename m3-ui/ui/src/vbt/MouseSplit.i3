(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:16 PST 1992 by muller   *)
(*      modified on Wed Oct 23  0:34:14 PDT 1991 by gnelson  *)
(*      modified on Tue Jul 31 16:29:23 PDT 1990 by steveg   *)
<*PRAGMA LL*>

INTERFACE MouseSplit;

(* The procedures in this interface implement the semantics described 
   in the VBT interface for delivering mouseclicks and positions to 
   split children, and for setting the cursor and cage of a split 
   parent.  *) 

IMPORT VBT, ScrnCursor, VBTClass;

TYPE Public = VBTClass.Public OBJECT
  <* LL >= SELF *>
  effectiveCursor: ScrnCursor.T := NIL;
  <* LL >= {SELF, VBT.mu} *>
  mouseRef: MouseRef := NIL
END;

TYPE MouseRef <: REFANY;

REVEAL VBT.Split <: Public;

PROCEDURE Mouse(v: VBT.Split; READONLY cd: VBT.MouseRec);

PROCEDURE Position(v: VBT.Split; READONLY cd: VBT.PositionRec);

PROCEDURE Setcage(v: VBT.Split; ch: VBT.T);

PROCEDURE Setcursor(v: VBT.Split; ch: VBT.T);

PROCEDURE Getcursor(v: VBT.Split): ScrnCursor.T;

PROCEDURE InvalidateCache(v: VBT.Split);
(* Clear any cached results of the child locate method.  If v's methods 
   come from this interface, you must call this procedure whenever 
   the geometry of v changes.  *)

END MouseSplit.
