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


FROM M3toC IMPORT TtoS;

PROCEDURE ForceToLoadAnImplementation () =
BEGIN
END ForceToLoadAnImplementation;

BEGIN

  Accelerators                   := TtoS("Accelerators");
  AllowShellResize               := TtoS("AllowShellResize");
  Argc                           := TtoS("Argc");
  Argv                           := TtoS("Argv");
  AutoFill                       := TtoS("AutoFill");
  Background                     := TtoS("Background");
  BackingStore                   := TtoS("BackingStore");
  BaseHeight                     := TtoS("BaseHeight");
  BaseWidth                      := TtoS("BaseWidth");
  Bitmap                         := TtoS("Bitmap");
  Boolean                        := TtoS("Boolean");
  BorderColor                    := TtoS("BorderColor");
  BorderWidth                    := TtoS("BorderWidth");
  Callback                       := TtoS("Callback");
  CheckCommand                   := TtoS("CheckCommand");
  Color                          := TtoS("Color");
  Colormap                       := TtoS("Colormap");
  ColumnWidth                    := TtoS("ColumnWidth");
  Columns                        := TtoS("Columns");
  CornerRoundPercent             := TtoS("CornerRoundPercent");
  CreatePopupChildProc           := TtoS("CreatePopupChildProc");
  Cursor                         := TtoS("Cursor");
  DataCompression                := TtoS("DataCompression");
  Depth                          := TtoS("Depth");
  Edge                           := TtoS("Edge");
  EditType                       := TtoS("EditType");
  EventBindings                  := TtoS("EventBindings");
  File                           := TtoS("File");
  Flip                           := TtoS("Flip");
  Font                           := TtoS("Font");
  Foreground                     := TtoS("Foreground");
  Fraction                       := TtoS("Fraction");
  Function                       := TtoS("Function");
  Geometry                       := TtoS("Geometry");
  GripIndent                     := TtoS("GripIndent");
  HSpace                         := TtoS("HSpace");
  Height                         := TtoS("Height");
  HeightInc                      := TtoS("HeightInc");
  HorizontalMargins              := TtoS("HorizontalMargins");
  Icon                           := TtoS("Icon");
  IconMask                       := TtoS("IconMask");
  IconName                       := TtoS("IconName");
  IconNameEncoding               := TtoS("IconNameEncoding");
  IconPixmap                     := TtoS("IconPixmap");
  IconWindow                     := TtoS("IconWindow");
  IconX                          := TtoS("IconX");
  IconY                          := TtoS("IconY");
  Iconic                         := TtoS("Iconic");
  Index                          := TtoS("Index");
  InitialResourcesPersistent     := TtoS("InitialResourcesPersistent");
  InitialState                   := TtoS("InitialState");
  Input                          := TtoS("Input");
  Insensitive                    := TtoS("Insensitive");
  InsertPosition                 := TtoS("InsertPosition");
  Interval                       := TtoS("Interval");
  JumpScroll                     := TtoS("JumpScroll");
  Justify                        := TtoS("Justify");
  KnobIndent                     := TtoS("KnobIndent");
  KnobPixel                      := TtoS("KnobPixel");
  Label                          := TtoS("Label");
  LabelClass                     := TtoS("LabelClass");
  LeftBitmap                     := TtoS("LeftBitmap");
  Length                         := TtoS("Length");
  LineWidth                      := TtoS("LineWidth");
  List                           := TtoS("List");
  Longest                        := TtoS("Longest");
  MappedWhenManaged              := TtoS("MappedWhenManaged");
  Margin                         := TtoS("Margin");
  Max                            := TtoS("Max");
  MaxAspectX                     := TtoS("MaxAspectX");
  MaxAspectY                     := TtoS("MaxAspectY");
  MaxHeight                      := TtoS("MaxHeight");
  MaxWidth                       := TtoS("MaxWidth");
  MenuEntry                      := TtoS("MenuEntry");
  MenuName                       := TtoS("MenuName");
  MenuOnScreen                   := TtoS("MenuOnScreen");
  Min                            := TtoS("Min");
  MinAspectX                     := TtoS("MinAspectX");
  MinAspectY                     := TtoS("MinAspectY");
  MinHeight                      := TtoS("MinHeight");
  MinWidth                       := TtoS("MinWidth");
  MinimumThumb                   := TtoS("MinimumThumb");
  Notify                         := TtoS("Notify");
  NumberStrings                  := TtoS("NumberStrings");
  Orientation                    := TtoS("Orientation");
  Output                         := TtoS("Output");
  OverrideRedirect               := TtoS("OverrideRedirect");
  Parameter                      := TtoS("Parameter");
  PieceSize                      := TtoS("PieceSize");
  Pixmap                         := TtoS("Pixmap");
  PixmapMask                     := TtoS("PixmapMask");
  PopupOnEntry                   := TtoS("PopupOnEntry");
  Position                       := TtoS("Position");
  PreferredPaneSize              := TtoS("PreferredPaneSize");
  RadioData                      := TtoS("RadioData");
  RadioGroup                     := TtoS("RadioGroup");
  ReadOnly                       := TtoS("ReadOnly");
  Resize                         := TtoS("Resize");
  ReverseVideo                   := TtoS("ReverseVideo");
  RightBitmap                    := TtoS("RightBitmap");
  RowHeight                      := TtoS("RowHeight");
  SaveUnder                      := TtoS("SaveUnder");
  Scale                          := TtoS("Scale");
  Screen                         := TtoS("Screen");
  Scroll                         := TtoS("Scroll");
  ScrollDCursor                  := TtoS("ScrollDCursor");
  ScrollHCursor                  := TtoS("ScrollHCursor");
  ScrollLCursor                  := TtoS("ScrollLCursor");
  ScrollProc                     := TtoS("ScrollProc");
  ScrollRCursor                  := TtoS("ScrollRCursor");
  ScrollUCursor                  := TtoS("ScrollUCursor");
  ScrollVCursor                  := TtoS("ScrollVCursor");
  SelectTypes                    := TtoS("SelectTypes");
  Selection                      := TtoS("Selection");
  SelectionArray                 := TtoS("SelectionArray");
  Sensitive                      := TtoS("Sensitive");
  ShapeStyle                     := TtoS("ShapeStyle");
  ShapeWindow                    := TtoS("ShapeWindow");
  ShowGrip                       := TtoS("ShowGrip");
  Shown                          := TtoS("Shown");
  Space                          := TtoS("Space");
  Spacing                        := TtoS("Spacing");
  State                          := TtoS("State");
  Stipple                        := TtoS("Stipple");
  String                         := TtoS("String");
  TemplateResource               := TtoS("TemplateResource");
  TextOptions                    := TtoS("TextOptions");
  TextPosition                   := TtoS("TextPosition");
  TextSink                       := TtoS("TextSink");
  TextSource                     := TtoS("TextSource");
  Thickness                      := TtoS("Thickness");
  Thumb                          := TtoS("Thumb");
  Title                          := TtoS("Title");
  TitleEncoding                  := TtoS("TitleEncoding");
  TopOfThumb                     := TtoS("TopOfThumb");
  Transient                      := TtoS("Transient");
  TransientFor                   := TtoS("TransientFor");
  Translations                   := TtoS("Translations");
  Type                           := TtoS("Type");
  UseStringInPlace               := TtoS("UseStringInPlace");
  VSpace                         := TtoS("VSpace");
  Value                          := TtoS("Value");
  VertSpace                      := TtoS("VertSpace");
  VerticalMargins                := TtoS("VerticalMargins");
  Visual                         := TtoS("Visual");
  Volume                         := TtoS("Volume");
  WaitForWm                      := TtoS("Waitforwm");
  Widget                         := TtoS("Widget");
  Width                          := TtoS("Width");
  WidthInc                       := TtoS("WidthInc");
  WinGravity                     := TtoS("WinGravity");
  Window                         := TtoS("Window");
  WindowGroup                    := TtoS("WindowGroup");
  WmTimeout                      := TtoS("WmTimeout");
  Wrap                           := TtoS("Wrap");
  X                              := TtoS("X");
  Y                              := TtoS("Y");

END XtC.

