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


FROM M3toC IMPORT FlatTtoS;

PROCEDURE ForceToLoadAnImplementation () =
BEGIN
END ForceToLoadAnImplementation;

BEGIN

  AcceleratorTable               := FlatTtoS("AcceleratorTable");
  AsciiType                      := FlatTtoS("AsciiType");
  Atom                           := FlatTtoS("Atom");
  BackingStore                   := FlatTtoS("BackingStore");
  Bitmap                         := FlatTtoS("Bitmap");
  Bool                           := FlatTtoS("Bool");
  Boolean                        := FlatTtoS("Boolean");
  CallProc                       := FlatTtoS("CallProc");
  Callback                       := FlatTtoS("Callback");
  Cardinal                       := FlatTtoS("Cardinal");
  Color                          := FlatTtoS("Color");
  Colormap                       := FlatTtoS("Colormap");
  Cursor                         := FlatTtoS("Cursor");
  Dimension                      := FlatTtoS("Dimension");
  Display                        := FlatTtoS("Display");
  EditMode                       := FlatTtoS("EditMode");
  Enum                           := FlatTtoS("Enum");
  File                           := FlatTtoS("File");
  Float                          := FlatTtoS("Float");
  Font                           := FlatTtoS("Font");
  FontStruct                     := FlatTtoS("FontStruct");
  Function                       := FlatTtoS("Function");
  Geometry                       := FlatTtoS("Geometry");
  Immediate                      := FlatTtoS("Immediate");
  InitialState                   := FlatTtoS("InitialState");
  Int                            := FlatTtoS("Int");
  Justify                        := FlatTtoS("Justify");
  Long                           := FlatTtoS("Long");
  LongBoolean                    := Bool;
  Object                         := FlatTtoS("Object");
  Orientation                    := FlatTtoS("Orientation");
  Pixel                          := FlatTtoS("Pixel");
  Pixmap                         := FlatTtoS("Pixmap");
  Pointer                        := FlatTtoS("Pointer");
  Position                       := FlatTtoS("Position");
  Screen                         := FlatTtoS("Screen");
  ShapeStyle                     := FlatTtoS("ShapeStyle");
  Short                          := FlatTtoS("Short");
  String                         := FlatTtoS("String");
  StringArray                    := FlatTtoS("StringArray");
  StringTable                    := FlatTtoS("StringTable");
  TranslationTable               := FlatTtoS("TranslationTable");
  UnsignedChar                   := FlatTtoS("UnsignedChar");
  Visual                         := FlatTtoS("Visual");
  Widget                         := FlatTtoS("Widget");
  WidgetClass                    := FlatTtoS("WidgetClass");
  WidgetList                     := FlatTtoS("WidgetList");
  Window                         := FlatTtoS("Window");

END XtR.
