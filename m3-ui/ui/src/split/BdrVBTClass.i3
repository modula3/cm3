(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: BdrVBTClass.i3, by cgn, Tue Apr 21 22:00:25 1987 *)
<*PRAGMA LL*>

(* Last modified on Fri Sep 25 16:15:56 PDT 1992 by msm     *)
(*      modified on Tue Mar 10 01:31:52 1992 by steveg  *)

INTERFACE BdrVBTClass;

IMPORT BorderedVBT, Rect, Axis;

TYPE
  Private = BorderedVBT.Public OBJECT
              bSize: ARRAY Axis.T OF INTEGER;  (* borderSizes in pixels *)
            METHODS <* LL.sup = VBT.mu *>
              repaintBorder (READONLY clip: Rect.T)
            END;

(* The "repaint", "redisplay", and "reshape" methods invoke "repaintBorder"
   after adjusting the child.  The array "bSize" is automatically computed
   after a call to "SetSize" or to "rescreen". *)

REVEAL BorderedVBT.T <: Private;

END BdrVBTClass.
