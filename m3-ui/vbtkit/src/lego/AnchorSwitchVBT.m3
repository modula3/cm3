(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 15:21:36 PST 1993 by mhb        *)
(*      modified on Tue Jun 16 13:09:05 PDT 1992 by muller     *)
(*      modified on Mon Dec  9 15:35:57 PST 1991 by meehan     *)

MODULE AnchorSwitchVBT;

IMPORT AnchorBtnVBT, FeedbackVBT, Multi, MultiClass, VBT;

REVEAL
  T = Public BRANDED "AnchorSwitchVBT.T" OBJECT OVERRIDES init := Init END;
            
PROCEDURE Init (v             : T;
                f             : FeedbackVBT.T;
                menu          : VBT.T;
                n             : CARDINAL     := 0;
                hfudge, vfudge               := 0.0  ): T =
                anchorParent  : VBT.T        := NIL;
  BEGIN
    EVAL AnchorBtnVBT.T.init (v, f, menu, n, anchorParent, hfudge, vfudge);
    MultiClass.Be (v, NEW (MC));
    MultiClass.BeChild (v, Multi.Child (f));
    RETURN v
  END Init;

BEGIN
END AnchorSwitchVBT.




