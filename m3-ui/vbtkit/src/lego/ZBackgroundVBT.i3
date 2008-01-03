(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 11 15:58:11 PDT 1993 by meehan *)
(*      modified on Mon Feb  1 23:13:13 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:02 PDT 1992 by muller *)
<* PRAGMA LL *>

(* A "ZBackgroundVBT" is a filter that should be put around the
   background child of a "ZSplit".  This filter will insulate
   highlighting that takes place within the background child from
   highlighting in the other children of the "ZSplit".  The
   implementation is merely a "HighlightVBT", but it's easier to
   remember the purpose of that highlighter by calling it a
   "ZBackgroundVBT" instead.

   In order for "ZChassisVBT" to display an outline of a subwindow
   that is visible against the background when it is moved or resized,
   you should use the "VBTColors" interface to associate the primary
   background and foreground colors of the contents of the
   "ZBackgroundVBT". *)

INTERFACE ZBackgroundVBT;

IMPORT HighlightVBT;

TYPE
  T = HighlightVBT.T BRANDED OBJECT END;

END ZBackgroundVBT.









