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


FROM M3toC IMPORT FlatTtoS;

PROCEDURE ForceToLoadAnImplementation () =
BEGIN
END ForceToLoadAnImplementation;

BEGIN

  Ellipse                        := FlatTtoS("Ellipse");
  Oval                           := FlatTtoS("Oval");
  Rectangle                      := FlatTtoS("Rectangle");
  RoundedRectangle               := FlatTtoS("RoundedRectangle");
  always                         := FlatTtoS("always");
  center                         := FlatTtoS("center");
  default                        := FlatTtoS("default");
  file                           := FlatTtoS("file");
  left                           := FlatTtoS("left");
  notUseful                      := FlatTtoS("notUseful");
  right                          := FlatTtoS("right");
  string                         := FlatTtoS("string");
  textScrollNever                := FlatTtoS("never");
  textScrollWhenNeeded           := FlatTtoS("whenneeded");
  textScrollAlways               := FlatTtoS("always");
  textWrapNever                  := FlatTtoS("never");
  textWrapLine                   := FlatTtoS("line");
  textWrapWord                   := FlatTtoS("word");
  textResizeNever                := FlatTtoS("never");
  textResizeWidth                := FlatTtoS("width");
  textResizeHeight               := FlatTtoS("height");
  textResizeBoth                 := FlatTtoS("both");
  whenMapped                     := FlatTtoS("whenMapped");

END XtE.
