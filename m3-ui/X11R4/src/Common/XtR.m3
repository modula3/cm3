(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtR.m3							*)
(* Last modified on Fri Apr 13 14:17:17 1990 by jerome		*)
(*      modified on Mon Feb 26 22:03:22 1990 by muller		*)


UNSAFE MODULE XtR;

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

  AcceleratorTable               := TtoS("AcceleratorTable");
  AsciiType                      := TtoS("AsciiType");
  Atom                           := TtoS("Atom");
  BackingStore                   := TtoS("BackingStore");
  Bitmap                         := TtoS("Bitmap");
  Bool                           := TtoS("Bool");
  Boolean                        := TtoS("Boolean");
  CallProc                       := TtoS("CallProc");
  Callback                       := TtoS("Callback");
  Cardinal                       := TtoS("Cardinal");
  Color                          := TtoS("Color");
  Colormap                       := TtoS("Colormap");
  Cursor                         := TtoS("Cursor");
  Dimension                      := TtoS("Dimension");
  Display                        := TtoS("Display");
  EditMode                       := TtoS("EditMode");
  Enum                           := TtoS("Enum");
  File                           := TtoS("File");
  Float                          := TtoS("Float");
  Font                           := TtoS("Font");
  FontStruct                     := TtoS("FontStruct");
  Function                       := TtoS("Function");
  Geometry                       := TtoS("Geometry");
  Immediate                      := TtoS("Immediate");
  InitialState                   := TtoS("InitialState");
  Int                            := TtoS("Int");
  Justify                        := TtoS("Justify");
  Long                           := TtoS("Long");
  LongBoolean                    := Bool;
  Object                         := TtoS("Object");
  Orientation                    := TtoS("Orientation");
  Pixel                          := TtoS("Pixel");
  Pixmap                         := TtoS("Pixmap");
  Pointer                        := TtoS("Pointer");
  Position                       := TtoS("Position");
  Screen                         := TtoS("Screen");
  ShapeStyle                     := TtoS("ShapeStyle");
  Short                          := TtoS("Short");
  String                         := TtoS("String");
  StringArray                    := TtoS("StringArray");
  StringTable                    := TtoS("StringTable");
  TranslationTable               := TtoS("TranslationTable");
  UnsignedChar                   := TtoS("UnsignedChar");
  Visual                         := TtoS("Visual");
  Widget                         := TtoS("Widget");
  WidgetClass                    := TtoS("WidgetClass");
  WidgetList                     := TtoS("WidgetList");
  Window                         := TtoS("Window");

END XtR.
