INTERFACE PLPlot;
(*Copyright (c) 1996, m3na project

Abstract: High level interface to plplot library.
          'plplot' let you create function graphs
          directly from a program
          in X11 window or in a postscript file etc.
          The module provides wrapper routines
          which give you all of the comfort known from Modula 3.

*)

IMPORT LongRealBasic AS R;

(*==========================*)

TYPE
  ColorMap = [0..15];

<*INLINE*>
PROCEDURE Init();
(* Initializes PLplot, using preset or default options *)

<*INLINE*>
PROCEDURE Exit();
(* End a plotting session for all open streams. *)

<*INLINE*>
PROCEDURE SetColorMap0(icol0:ColorMap);
(* Set color, map 0. *)

<*INLINE*>
PROCEDURE Histogram(READONLY data:ARRAY OF R.T;
                             datamin, datamax:R.T;
                             numbin:CARDINAL;
                             oldwin:CARDINAL:=0);
(* Draws a histogram of n values of a variable in array data[0..n-1] *)

<*INLINE*>
PROCEDURE SetLabel(xlabel, ylabel, tlabel : TEXT);
(* Simple routine for labelling graphs. *)

(*==========================*)
END PLPlot.
