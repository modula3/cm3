(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:54:22 PST 1992 by muller   *)
(*      modified on Wed Aug 14 17:15:48 PDT 1991 by gnelson  *)
(*      modified on Thu Sep  6 21:11:01 PDT 1990 by msm      *)
(*      modified on Fri Aug  3 10:13:01 PDT 1990 by steveg   *)
<*PRAGMA LL*>

(* A "RootVBT.T" provides the up methods for "VBTs" at the root of
   an applications, the methods that talk to an X server or talk
   RPC to a parent "VBT" in another address space.

   A "RootVBT.Child" provides a series of filters that are useful
   at the root of applications. *)

INTERFACE RootVBT;

IMPORT Filter, Split, VBT;

TYPE 
  T <: Split.T;
  Child <: Filter.T;

PROCEDURE NewChild(ch: VBT.T; p: DeleteProc := NIL): Child;
(* Return a stack of filters over "ch" that implement et-agenting,
   highlighting, palette initialization on rescreening, and
   calls "p(ch)" before forwarding a deleted or disconnected code
   to "ch", if "p # NIL".  In any case, after a deleted or disconnected code
   it removes the child from the filter. *)

TYPE DeleteProc = PROCEDURE(v: VBT.T) <* LL.sup = VBT.mu *>;

PROCEDURE Misc(v: Child; READONLY cd: VBT.MiscRec);
(* = Child.misc.  This will go away when the compiler bug is
   fixed. *)

END RootVBT.
