INTERFACE PLPlot;
(*Copyright (c) 1996, m3na project

   Abstract: High level interface to plplot library.

   'plplot' let you create function graphs directly from a program in X11
   window or in a postscript file etc.  The module provides wrapper
   routines which give you all of the comfort known from Modula 3.

   *)

IMPORT LongRealBasic AS R;
FROM NADefinitions IMPORT Error;

(*==========================*)

TYPE
  Map0Color = [0 .. 15];         (*the actual color is not determined*)
  Map1Color = R.T;

<*INLINE*>
PROCEDURE Init ();
(* Initializes PLplot, using preset or default options *)

<*INLINE*>
PROCEDURE Exit ();
(* End a plotting session for all open streams. *)


<*INLINE*>
PROCEDURE SubPlots (nx, ny: CARDINAL);
(* Set the number of subwindows in x and y *)

<*INLINE*>
PROCEDURE SetEnvironment (xmin, xmax, ymin, ymax: R.T;
                          just                  : CARDINAL := 0;
                          axis                  : CARDINAL := 0  );
(* Simple interface for defining viewport and window. *)

<*INLINE*>
PROCEDURE Clear ();
(* Clear current subpage. *)

PROCEDURE Advance (page: INTEGER);
(* pladv: "Advance to subpage \"page\", or to the next one if \"page\" =
   0." *)

<*INLINE*>
PROCEDURE SetXORMode (mode: BOOLEAN): BOOLEAN;
(* set xor mode; mode = TRUE-enter, FALSE-leave, return TRUE if interactive
   device *)


<*INLINE*>
PROCEDURE SetColor0 (icol0: Map0Color);
(* Set color, map 0. *)

<*INLINE*>
PROCEDURE SetColor1 (icol1: Map1Color);
(* Set color, map 1.  Argument is a float between 0.  and 1. *)

<*INLINE*>
PROCEDURE SetLabel (xlabel, ylabel, tlabel: TEXT);
(* Simple routine for labelling graphs. *)


<*INLINE*>
PROCEDURE PlotPoints (READONLY x, y: ARRAY OF R.T; code: CARDINAL)
  RAISES {Error};
(* Plots array y against x for n points using ASCII code "code".*)

<*INLINE*>
PROCEDURE PlotLines (READONLY x, y: ARRAY OF R.T) RAISES {Error};
(* Draws line segments connecting a series of points. *)

<*INLINE*>
PROCEDURE Histogram (READONLY data            : ARRAY OF R.T;
                              datamin, datamax: R.T;
                              numbin          : CARDINAL;
                              oldwin          : CARDINAL       := 0);
(* Draws a histogram of n values of a variable in array data[0..n-1] *)

(*==========================*)
END PLPlot.
