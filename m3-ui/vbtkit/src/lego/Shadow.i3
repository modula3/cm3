(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 25 14:21:34 PST 1994 by mhb *)
(*      modified on Tue Jan 18 17:41:16 1994 by harrison *)
(*      modified on Mon Jun 14 21:47:37 PDT 1993 by meehan *)
(*      modified on Tue Jun 16 13:08:20 PDT 1992 by muller *)
<* PRAGMA LL *>

(* The "Shadow" interface contains the basic definitions for VBT
   classes that implement a Motif-like, 3-D look.  There are two
   basic primitives: a 3-D border, and a 3-D vertical or
   horizontal line.  The style, size, and colors of the shadow
   are specified by data structures defined in this interface.

   A 3-D border can give the visual illusion of ``raising'' an
   object above the background, ``lowering'' an object into the
   background, drawing a ``ridge'' above the background, or
   chiseling a ``groove'' into the background.  A 3-D line has
   the visual effect of being either a ``ridge'' above the
   background or a ``groove'' chiseled into the background (see
   Figure~\ref{fig:shadows}).

   These visual effects are actually quite simple to accomplish
   by drawing parts of the 3-D border or 3-D line using a dark
   variant of the background color, and by drawing other parts
   using a light variant of the background color.

   For example, to give the impression that an object is raised
   above its background, the north and west borders are drawn
   using a light color, whereas the south and east border are
   drawn in a dark color.  To draw a ``ridge,'' the north and
   west shadows start out in the light color, and, halfway,
   switch to the dark color.  Analogously, the south and east
   shadows start out dark and switch to a light color.

   The following chart summarizes the visual effects:


             \begin{center}
             \begin{tabular}{l|l|l}

             {\em Style}   & {\em North/West} & {\em South/East}\\
             \hline
                 Flat      &   Background  &  Background\\
                 Raised    &   Light       &  Dark\\
                 Lowered   &   Dark        &  Light\\
                 Ridged    &   Light/Dark  &  Dark/Light\\
                 Chiseled  &   Dark/Light  &  Light/Dark

             \end{tabular}
             \end{center}

   For maximum effectiveness, the child's background should be a
   color whose saturation level is about 50\%, and the light and
   dark shadows should be colors with the same hue and lightness,
   but with saturation levels of 25\% and 75\% respectively.

   On a monochrome display, the 3-D borders and lines appear flat and
   50\% of the size they'd be on non-monochrome displays. Also, those
   VBTkit widgets that use 3-D borders for feedback (say, a button
   that gives the effect of lowering its contents when depressed) are
   implemented in such a way as to give feedback in a non-3-D manner
   (e.g., the {\tt ShadowedFeedbackVBT} 
   interface in Section~\ref{ShadowedFeedbackVBTSection}). 

   You can force VBTkit widgets to use a non-3-D style of 
   feedback by specifying a shadow size that is negative. Such widgets
   will draw borders and lines with 50\% of the absoluate value of
   the shadow size. (You should also be sure to set the light and dark 
   shadow to be the same as the foreground color.) *)


INTERFACE Shadow;

IMPORT PaintOp, VBT;

TYPE
  T = PaintOp.ColorScheme OBJECT
        size: REAL;
        light, dark, both, reversed: PaintOp.T; 
      END;

TYPE 
  Style = {Flat, Raised, Lowered, Ridged, Chiseled};

PROCEDURE New (size : REAL      := 0.5;
               bg   : PaintOp.T := PaintOp.Bg;
               fg   : PaintOp.T := PaintOp.Fg;
               light: PaintOp.T := PaintOp.Fg;
               dark : PaintOp.T := PaintOp.Fg): T;
<* LL = arbitrary *>
(* Return a newly allocated "Shadow.T".  The "size", "light", and
   "dark" fields of the new "Shadow.T" are copies of the
   parameters, respectively.  The "both" field is computed from
   "PaintOp.Pair(light, dark)", and the "reversed" field is
   computed from "PaintOp.Pair(dark, light)". *)

(* The "size" is specified in millimeters.  All of the paint ops
   must be tints, arranged so that on a monochrome screen "bg"
   draws in background, while "fg", "light", and "dark" draw in
   foreground. *)

PROCEDURE Supported (shadow: T; v: VBT.T): BOOLEAN;
<* LL.sup < v *>
(* Return whether "shadow" should appear 3-D on "v".  Two conditions
   must hold: "v" must be on screen whose depth is greater than 1,
   and "shadow.size" must be positive. *)

(*  Finally, we have the definition for a ``default'' shadow: *)

VAR (* CONST *) None: T;

(* This variable is really a constant for

| New(0.0, PaintOp.Bg, PaintOp.Fg, PaintOp.Fg, PaintOp.Fg)

   Because "None" is not a constant, it cannot be the default
   value of a procedure argument.  Therefore, we adopt the
   following convention: when a parameter whose type is
   "Shadow.T" has a default value of "NIL", the procedure will
   use "Shadow.None" instead. *)

END Shadow.

