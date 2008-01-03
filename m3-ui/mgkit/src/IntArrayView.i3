(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Thu Sep 23 11:17:28 PDT 1993 by mhb    *)
(*      modified on Fri Jul  9 13:33:25 PDT 1993 by steveg *)

INTERFACE IntArrayView;

<* PRAGMA LL *>

IMPORT Axis, Font, MG, PaintOp;

TYPE
  V <: PublicV;

  Elem = MG.Rectangle OBJECT i: INTEGER END;
  Elems = REF ARRAY OF Elem;

  PublicV = MG.V OBJECT
             <* LL = self.mu *>
             elems     : Elems;
             prefDimPts: ARRAY Axis.T OF REAL;
           METHODS
             init (size                 : CARDINAL;
                   widthPts, heightPts  : REAL;
                   font                 : Font.T     := Font.BuiltIn;
                   prefWidth, prefHeight             := 0.0           ): V
           END;
(* prefWidth = 0 => prefWidth = size * widthPts prefHeight = 0 =>
   prefHeight = heightPts *)

PROCEDURE SetColor (v: V; element: CARDINAL; color: PaintOp.ColorScheme); <* LL.sub < v *>
PROCEDURE SetValue (v: V; element: CARDINAL; i: INTEGER); <* LL.sup < v *>
PROCEDURE ClearValue(v: V; element: CARDINAL); <* LL.sup < v *>
PROCEDURE SetFont (v: V; element: CARDINAL; font: Font.T); <* LL.sup < v *>

END IntArrayView.

