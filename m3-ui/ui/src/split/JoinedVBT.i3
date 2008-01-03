(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Nov 27 18:15:12 PST 1992 by msm    *)
(*      modified on Wed Aug 12 23:29:10 PDT 1992 by meehan *)
(*      modified on Wed Aug 12 17:12:45 PDT 1992 by guarino *)
(*      modified on Tue Jun 16 13:08:47 PDT 1992 by muller *)
(*      modified on Mon Jun 15 21:49:39 1992 by mhb    *)
(*      modified on Fri Mar 27 01:58:11 1992 by steveg *)
(* modified on Fri Feb 2 14:03:04 PST 1990 by glassman *)
(* modified on Sun Jun 5 23:37:29 PDT 1988 by gnelson *)
<*PRAGMA LL*>

(* A "JoinedVBT.T" allows many parent "VBT"s to view the screen
   of a single child "VBT".  One parent's view of the child's
   domain may overlap with another parent's view, and it might
   not cover the entire child domain.

   The domain of the child of a "JoinedVBT.T" is the union of the parent
   domains; you should wrap the child in a filter if you want to make the
   child domain have some other shape.  Since the domains of the parents may
   not always be in the most convenient coordinate system, you may
   want to use "TranslateVBT" or something similar to change domains.

   Event-time operations on the child are performed on the
   event-time parent (the parent that last received a mouse
   click, or as set by method calls on the VBT.  When the event-time parent
   changes, all selections held by the old event-time parent
   are released (if the parents are on different Trestles).  These
   event-time operations are handled by making the parent an
   "ETAgent" and inserting a filter between the parent and the
   child.  When a parent receives a "VBT.Deleted" code, it removes
   itself (and the filter).  Other miscellaneous codes are
   handled as an "ETAgent".

   The value of "v.parent" is "NIL" if the "JoinedVBT" "v" has
   no parents; otherwise it is an arbitrary parent (or rather
   the filter below the parent---these will not be explicitly
   mentioned hereafter).

   Those who are familiar with the rules for using concurrency
   while reformatting splits will note that two parents of a Joined
   may be reformatted concurrently.  These reformat procedures
   may wish to call "Reshape", or "Redisplay".
   Therefore, an internal lock is used by the JoinedVBT to
   serialize these operations; their locking level is "LL <= VBT.mu"
   instead of "LL = VBT.mu". *)

INTERFACE JoinedVBT;

IMPORT Filter, VBT;

TYPE
  T <: Public;
  Public = Filter.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (ch: VBT.T): T
           END;

(* The call "v.init(ch)" initializes "v" as a "JoinedVBT" with
     child "ch", and returns "v". *)

PROCEDURE New (ch: VBT.T): T;   
<* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

END JoinedVBT.

