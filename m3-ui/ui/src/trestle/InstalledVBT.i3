(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Mar 23 11:09:42 PST 1993 by msm *)
(* modified on Tue Mar 10 19:01:48 1992 by steveg *)
(* modified on Mon Feb 24 13:53:34 PST 1992 by muller *)
(* modified on Tue Sep 3 21:06:58 PDT 1991 by gnelson *)

<*PRAGMA LL*>

(* A "InstalledVBT.T"" is a series of filters that are useful at the root
   of applications. *)

INTERFACE InstalledVBT;

IMPORT DpyFilter, VBT, JoinedVBT;

TYPE
  T <: DpyFilter.T;
  Join <: JoinedVBT.T;

PROCEDURE New (ch: VBT.T; p: DeleteProc := NIL): T;
(* Return a stack of filters over "ch" that implement et-agenting,
   teleportation, highlighting, palette initialization on rescreening, and
   calls "p(ch)" before forwarding a deleted or disconnected code to "ch",
   if "p # NIL".  In any case, after a deleted or disconnected code it
   removes the child from the filter. *)

PROCEDURE NewParent(ch: VBT.T): T;
(* If ch is attached, return a new parent for the Join above ch.  Otherwise,
   return NIL *)

PROCEDURE InitChild(j: Join; ch: VBT.T; p: DeleteProc := NIL);
(* Like New, except without the topmost VBT, and you allocate *)

PROCEDURE InitParent(p: T; ch: Join);
(* Like NewParent, except you pass the parent and the
   Join, and it doesn't check to see if ch is attached. *)

PROCEDURE Child (v: VBT.T): VBT.T; <* LL.sup = VBT.mu *>
(* Return the installed child of the tree containing "v", or the root
   of the tree containing "v" if it isn't installed. *)

TYPE DeleteProc = PROCEDURE(v: VBT.T) <* LL.sup = VBT.mu *>;

END InstalledVBT.
