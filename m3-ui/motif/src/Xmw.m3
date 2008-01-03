(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 07:57:10 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE Xmw;
(*
Abstract:  Macros for Xm widgets

8/4/94     H. George
           Initial version.
*)

IMPORT Xt;

(*=======================
  Template:

(*----------------*)
PROCEDURE IsXXX(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmXXXWidgetClass);
END IsXXX;

========================*)

 
(*----------------*)
PROCEDURE IsArrowButton(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmArrowButtonWidgetClass);
END IsArrowButton;

(*----------------*)
PROCEDURE IsBulletinBoard(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmBulletinBoardWidgetClass);
END IsBulletinBoard;

(*----------------*)
PROCEDURE IsCascadeButton(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmCascadeButtonWidgetClass);
END IsCascadeButton;

(*----------------*)
PROCEDURE IsDialogShell(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmDialogShellWidgetClass);
END IsDialogShell;

(*----------------*)
PROCEDURE IsDrawingArea(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmDrawingAreaWidgetClass);
END IsDrawingArea;

(*----------------*)
PROCEDURE IsDrawnButton(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmDrawnButtonWidgetClass);
END IsDrawnButton;

(*----------------*)
PROCEDURE IsForm(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmFormWidgetClass);
END IsForm;

(*----------------*)
PROCEDURE IsFrame(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmFrameWidgetClass);
END IsFrame;

(*----------------*)
PROCEDURE IsLabel(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmLabelWidgetClass);
END IsLabel;

(*----------------*)
PROCEDURE IsList(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmListWidgetClass);
END IsList;

(*----------------*)
PROCEDURE IsMainWindow(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmMainWindowWidgetClass);
END IsMainWindow;

(*----------------*)
PROCEDURE IsMenuShell(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmMenuShellWidgetClass);
END IsMenuShell;

(*----------------*)
PROCEDURE IsMessageBox(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmMessageBoxWidgetClass);
END IsMessageBox;

(*----------------*)
PROCEDURE IsPanedWindow(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmPanedWindowWidgetClass);
END IsPanedWindow;

(*----------------*)
PROCEDURE IsPushButton(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmPushButtonWidgetClass);
END IsPushButton;

(*----------------*)
PROCEDURE IsRowColumn(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmRowColumnWidgetClass);
END IsRowColumn;

(*----------------*)
PROCEDURE IsScale(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmScaleWidgetClass);
END IsScale;

(*----------------*)
PROCEDURE IsScrollBar(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmScrollBarWidgetClass);
END IsScrollBar;

(*----------------*)
PROCEDURE IsScrolledWindow(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmScrolledWindowWidgetClass);
END IsScrolledWindow;

(*----------------*)
PROCEDURE IsSelectionBox(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmSelectionBoxWidgetClass);
END IsSelectionBox;

(*----------------*)
PROCEDURE IsSeparator(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmSeparatorWidgetClass);
END IsSeparator;

(*----------------*)
PROCEDURE IsText(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmTextWidgetClass);
END IsText;

(*----------------*)
PROCEDURE IsTextField(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmTextFieldWidgetClass);
END IsTextField;

(*----------------*)
PROCEDURE IsToggleButton(w:Xt.Widget):Xt.Boolean =
BEGIN
  RETURN Xt.IsSubclass(w,xmToggleButtonWidgetClass);
END IsToggleButton;

(*----------------*)
BEGIN
END Xmw.
