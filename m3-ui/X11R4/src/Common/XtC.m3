(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtC.m3							*)
(* Last modified on Fri Apr 13 14:16:36 1990 by jerome		*)
(*      modified on Sat Feb 24 02:18:52 1990 by muller		*)


UNSAFE MODULE XtC;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	Class Types   corresponding to:				*)
(*			X11 R4 Intrinsic			*)
(*			X11 R4 Athena Widget Set		*)
(*==============================================================*)


FROM M3toC IMPORT FlatTtoS;

PROCEDURE ForceToLoadAnImplementation () =
BEGIN
END ForceToLoadAnImplementation;

BEGIN

  Accelerators                   := FlatTtoS("Accelerators");
  AllowShellResize               := FlatTtoS("AllowShellResize");
  Argc                           := FlatTtoS("Argc");
  Argv                           := FlatTtoS("Argv");
  AutoFill                       := FlatTtoS("AutoFill");
  Background                     := FlatTtoS("Background");
  BackingStore                   := FlatTtoS("BackingStore");
  BaseHeight                     := FlatTtoS("BaseHeight");
  BaseWidth                      := FlatTtoS("BaseWidth");
  Bitmap                         := FlatTtoS("Bitmap");
  Boolean                        := FlatTtoS("Boolean");
  BorderColor                    := FlatTtoS("BorderColor");
  BorderWidth                    := FlatTtoS("BorderWidth");
  Callback                       := FlatTtoS("Callback");
  CheckCommand                   := FlatTtoS("CheckCommand");
  Color                          := FlatTtoS("Color");
  Colormap                       := FlatTtoS("Colormap");
  ColumnWidth                    := FlatTtoS("ColumnWidth");
  Columns                        := FlatTtoS("Columns");
  CornerRoundPercent             := FlatTtoS("CornerRoundPercent");
  CreatePopupChildProc           := FlatTtoS("CreatePopupChildProc");
  Cursor                         := FlatTtoS("Cursor");
  DataCompression                := FlatTtoS("DataCompression");
  Depth                          := FlatTtoS("Depth");
  Edge                           := FlatTtoS("Edge");
  EditType                       := FlatTtoS("EditType");
  EventBindings                  := FlatTtoS("EventBindings");
  File                           := FlatTtoS("File");
  Flip                           := FlatTtoS("Flip");
  Font                           := FlatTtoS("Font");
  Foreground                     := FlatTtoS("Foreground");
  Fraction                       := FlatTtoS("Fraction");
  Function                       := FlatTtoS("Function");
  Geometry                       := FlatTtoS("Geometry");
  GripIndent                     := FlatTtoS("GripIndent");
  HSpace                         := FlatTtoS("HSpace");
  Height                         := FlatTtoS("Height");
  HeightInc                      := FlatTtoS("HeightInc");
  HorizontalMargins              := FlatTtoS("HorizontalMargins");
  Icon                           := FlatTtoS("Icon");
  IconMask                       := FlatTtoS("IconMask");
  IconName                       := FlatTtoS("IconName");
  IconNameEncoding               := FlatTtoS("IconNameEncoding");
  IconPixmap                     := FlatTtoS("IconPixmap");
  IconWindow                     := FlatTtoS("IconWindow");
  IconX                          := FlatTtoS("IconX");
  IconY                          := FlatTtoS("IconY");
  Iconic                         := FlatTtoS("Iconic");
  Index                          := FlatTtoS("Index");
  InitialResourcesPersistent     := FlatTtoS("InitialResourcesPersistent");
  InitialState                   := FlatTtoS("InitialState");
  Input                          := FlatTtoS("Input");
  Insensitive                    := FlatTtoS("Insensitive");
  InsertPosition                 := FlatTtoS("InsertPosition");
  Interval                       := FlatTtoS("Interval");
  JumpScroll                     := FlatTtoS("JumpScroll");
  Justify                        := FlatTtoS("Justify");
  KnobIndent                     := FlatTtoS("KnobIndent");
  KnobPixel                      := FlatTtoS("KnobPixel");
  Label                          := FlatTtoS("Label");
  LabelClass                     := FlatTtoS("LabelClass");
  LeftBitmap                     := FlatTtoS("LeftBitmap");
  Length                         := FlatTtoS("Length");
  LineWidth                      := FlatTtoS("LineWidth");
  List                           := FlatTtoS("List");
  Longest                        := FlatTtoS("Longest");
  MappedWhenManaged              := FlatTtoS("MappedWhenManaged");
  Margin                         := FlatTtoS("Margin");
  Max                            := FlatTtoS("Max");
  MaxAspectX                     := FlatTtoS("MaxAspectX");
  MaxAspectY                     := FlatTtoS("MaxAspectY");
  MaxHeight                      := FlatTtoS("MaxHeight");
  MaxWidth                       := FlatTtoS("MaxWidth");
  MenuEntry                      := FlatTtoS("MenuEntry");
  MenuName                       := FlatTtoS("MenuName");
  MenuOnScreen                   := FlatTtoS("MenuOnScreen");
  Min                            := FlatTtoS("Min");
  MinAspectX                     := FlatTtoS("MinAspectX");
  MinAspectY                     := FlatTtoS("MinAspectY");
  MinHeight                      := FlatTtoS("MinHeight");
  MinWidth                       := FlatTtoS("MinWidth");
  MinimumThumb                   := FlatTtoS("MinimumThumb");
  Notify                         := FlatTtoS("Notify");
  NumberStrings                  := FlatTtoS("NumberStrings");
  Orientation                    := FlatTtoS("Orientation");
  Output                         := FlatTtoS("Output");
  OverrideRedirect               := FlatTtoS("OverrideRedirect");
  Parameter                      := FlatTtoS("Parameter");
  PieceSize                      := FlatTtoS("PieceSize");
  Pixmap                         := FlatTtoS("Pixmap");
  PixmapMask                     := FlatTtoS("PixmapMask");
  PopupOnEntry                   := FlatTtoS("PopupOnEntry");
  Position                       := FlatTtoS("Position");
  PreferredPaneSize              := FlatTtoS("PreferredPaneSize");
  RadioData                      := FlatTtoS("RadioData");
  RadioGroup                     := FlatTtoS("RadioGroup");
  ReadOnly                       := FlatTtoS("ReadOnly");
  Resize                         := FlatTtoS("Resize");
  ReverseVideo                   := FlatTtoS("ReverseVideo");
  RightBitmap                    := FlatTtoS("RightBitmap");
  RowHeight                      := FlatTtoS("RowHeight");
  SaveUnder                      := FlatTtoS("SaveUnder");
  Scale                          := FlatTtoS("Scale");
  Screen                         := FlatTtoS("Screen");
  Scroll                         := FlatTtoS("Scroll");
  ScrollDCursor                  := FlatTtoS("ScrollDCursor");
  ScrollHCursor                  := FlatTtoS("ScrollHCursor");
  ScrollLCursor                  := FlatTtoS("ScrollLCursor");
  ScrollProc                     := FlatTtoS("ScrollProc");
  ScrollRCursor                  := FlatTtoS("ScrollRCursor");
  ScrollUCursor                  := FlatTtoS("ScrollUCursor");
  ScrollVCursor                  := FlatTtoS("ScrollVCursor");
  SelectTypes                    := FlatTtoS("SelectTypes");
  Selection                      := FlatTtoS("Selection");
  SelectionArray                 := FlatTtoS("SelectionArray");
  Sensitive                      := FlatTtoS("Sensitive");
  ShapeStyle                     := FlatTtoS("ShapeStyle");
  ShapeWindow                    := FlatTtoS("ShapeWindow");
  ShowGrip                       := FlatTtoS("ShowGrip");
  Shown                          := FlatTtoS("Shown");
  Space                          := FlatTtoS("Space");
  Spacing                        := FlatTtoS("Spacing");
  State                          := FlatTtoS("State");
  Stipple                        := FlatTtoS("Stipple");
  String                         := FlatTtoS("String");
  TemplateResource               := FlatTtoS("TemplateResource");
  TextOptions                    := FlatTtoS("TextOptions");
  TextPosition                   := FlatTtoS("TextPosition");
  TextSink                       := FlatTtoS("TextSink");
  TextSource                     := FlatTtoS("TextSource");
  Thickness                      := FlatTtoS("Thickness");
  Thumb                          := FlatTtoS("Thumb");
  Title                          := FlatTtoS("Title");
  TitleEncoding                  := FlatTtoS("TitleEncoding");
  TopOfThumb                     := FlatTtoS("TopOfThumb");
  Transient                      := FlatTtoS("Transient");
  TransientFor                   := FlatTtoS("TransientFor");
  Translations                   := FlatTtoS("Translations");
  Type                           := FlatTtoS("Type");
  UseStringInPlace               := FlatTtoS("UseStringInPlace");
  VSpace                         := FlatTtoS("VSpace");
  Value                          := FlatTtoS("Value");
  VertSpace                      := FlatTtoS("VertSpace");
  VerticalMargins                := FlatTtoS("VerticalMargins");
  Visual                         := FlatTtoS("Visual");
  Volume                         := FlatTtoS("Volume");
  WaitForWm                      := FlatTtoS("Waitforwm");
  Widget                         := FlatTtoS("Widget");
  Width                          := FlatTtoS("Width");
  WidthInc                       := FlatTtoS("WidthInc");
  WinGravity                     := FlatTtoS("WinGravity");
  Window                         := FlatTtoS("Window");
  WindowGroup                    := FlatTtoS("WindowGroup");
  WmTimeout                      := FlatTtoS("WmTimeout");
  Wrap                           := FlatTtoS("Wrap");
  X                              := FlatTtoS("X");
  Y                              := FlatTtoS("Y");

END XtC.

