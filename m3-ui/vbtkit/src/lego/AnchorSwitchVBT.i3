(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 15:21:45 PST 1993 by mhb        *)
(*      modified on Tue Jun 16 13:09:05 PDT 1992 by muller     *)
(*      modified on Wed Nov 27 11:06:57 PST 1991 by meehan     *)

INTERFACE AnchorSwitchVBT;

(* A "AnchorSwitchVBT.T" is a switch version of AnchorBtnVBT.T.  *)

IMPORT AnchorBtnVBT, FeedbackVBT, SwitchVBT, VBT;

TYPE
  T <: Public;
  Public = AnchorBtnVBT.T OBJECT
           METHODS
             init (f             : FeedbackVBT.T;
                   menu          : VBT.T;
                   n             : CARDINAL        := 0;
                   anchorParent  : VBT.T           := NIL;
                   hfudge, vfudge                  := 0.0  ): T
           OVERRIDES
             pre    := SwitchVBT.Pre;
             post   := SwitchVBT.Post;
             cancel := SwitchVBT.Cancel;
           END;

  MC = SwitchVBT.MC;

END AnchorSwitchVBT.




