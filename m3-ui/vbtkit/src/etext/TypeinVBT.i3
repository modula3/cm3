(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 14 18:36:06 PDT 1993 by meehan                   *)
<* PRAGMA LL *>

INTERFACE TypeinVBT;

IMPORT Font, PaintOp, TextPort, VBT;

TYPE
  T <: Public;
  Public = TextPort.T OBJECT
             <* LL.sup = VBT.mu *>
             tabNext: VBT.T := NIL
           METHODS
             init (expandOnDemand   := FALSE;
                   hMargin, vMargin := 0.5;
                   font             := Font.BuiltIn;
                   colorScheme: PaintOp.ColorScheme := NIL;
                   wrap                             := TRUE;
                   readOnly                         := FALSE;
                   turnMargin                       := 0.5;
                   model := TextPort.Model.Default): T;
           END;

END TypeinVBT. 

(* "TypeinVBT" overrides the "returnAction", "tabAction", "key", and
   "shape" methods.

   The default "returnAction" method is a no-op, but most clients will
   override this method.

   The "TextPort"'s height is initially set to the height of the
   tallest character in the current font.  Its default width is 30
   times the width of the widest character in the current font. The
   default height is one line, but if "expandOnDemand" is "TRUE", then
   "SELF" will expand (and contract) vertically as the text requires,
   so that the entire text is visible in the window.

   The default "tabAction" method tests whether "SELF.nextTab" is
   "NIL".  If so, it calls the parent-method, "TextPort.T.tabAction".
   If not, it sends a miscellaneous code of type "VBT.TakeSelection"
   with the "VBT.KBFocus" selection to "SELF.nextTab", i.e., it asks
   the "nextTab VBT" to take the keyboard focus. In addition, if that
   VBT is itself a TextPort, then it selects all the text in the
   TextPort in replace-mode.

*)

