(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtE.m3							*)
(* Last modified on Fri Apr 13 14:16:49 1990 by jerome		*)
(*      modified on Mon Feb 26 21:53:32 1990 by muller		*)


UNSAFE MODULE XtE;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	Representation Types  corresponding to:			*)
(*			X11 R4 Intrinsic			*)
(*			X11 R4 Athena Widget Set		*)
(*==============================================================*)


FROM M3toC IMPORT TtoS;

PROCEDURE ForceToLoadAnImplementation () =
BEGIN
END ForceToLoadAnImplementation;

BEGIN

  Ellipse                        := TtoS("Ellipse");
  Oval                           := TtoS("Oval");
  Rectangle                      := TtoS("Rectangle");
  RoundedRectangle               := TtoS("RoundedRectangle");
  always                         := TtoS("always");
  center                         := TtoS("center");
  default                        := TtoS("default");
  file                           := TtoS("file");
  left                           := TtoS("left");
  notUseful                      := TtoS("notUseful");
  right                          := TtoS("right");
  string                         := TtoS("string");
  textScrollNever                := TtoS("never");
  textScrollWhenNeeded           := TtoS("whenneeded");
  textScrollAlways               := TtoS("always");
  textWrapNever                  := TtoS("never");
  textWrapLine                   := TtoS("line");
  textWrapWord                   := TtoS("word");
  textResizeNever                := TtoS("never");
  textResizeWidth                := TtoS("width");
  textResizeHeight               := TtoS("height");
  textResizeBoth                 := TtoS("both");
  whenMapped                     := TtoS("whenMapped");

END XtE.
