(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Sep 25 10:42:08 EDT 1995 by dagenais *)
(*      modified on Fri Jan 29 15:15:13 PST 1993 by mhb    *)
(*      modified on Mon Aug 10  0:17:27 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 12:59:09 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* An "AnchorHelpSplit" is a multi-split version of "AnchorHelpVBT".
   The first child is the {\em anchor} that is displayed (such
   as a text string or an icon).  The second child is the {\em
   help window} that is displayed when the anchor is activated.  Attempts
   to give an anchor-split more than two children cause the extra
   children to be lost. *)

INTERFACE AnchorHelpSplit;

IMPORT AnchorHelpVBT, VBT;

TYPE
  <* SUBTYPE T <: MultiSplit.T *>
  T <: Public;
  Public = AnchorHelpVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (ch          : VBT.T;
                   help        : VBT.T;
                   n           : CARDINAL        := 0;
                   hfudge                        := 0.0;
                   vfudge                        := 1.0  ): T;
           END;

(* The call "v.init(...)" initializes "v" as an "AnchorHelpSplit".
   The parameters, "n", "hfudge", and "vfudge" are the same as in 
   "AnchorHelpVBT". *)


END AnchorHelpSplit.



