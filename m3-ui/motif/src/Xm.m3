(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 07:56:20 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE Xm;
(*Generated from Xm.h by ctom3 on 7/26/93*)

FROM M3toC IMPORT TtoS;


PROCEDURE Version():INTEGER = 
BEGIN
  RETURN (VERSION * 1000 + REVISION);
END Version;


BEGIN
  STRING_ISO8859_1 := TtoS("ISO8859-1");
  STRING_OS_CHARSET := TtoS("ISO8859-1");
  FALLBACK_CHARSET := TtoS("ISO8859-1");
  STRING_DEFAULT_CHARSET := TtoS("");
  RAcceleratorTable := TtoS("AcceleratorTable");
  RAlignment := TtoS("Alignment");
  RXmBackgroundPixmap := TtoS("XmBackgroundPixmap");
  RBool := TtoS("Bool");
  RBoolean := TtoS("Boolean");
  RButtonType := TtoS("ButtonType");
  RCallback := TtoS("Callback");
  RCallbackProc := TtoS("CallbackProc");
  RCallProc := TtoS("CallProc");
  Rchar := TtoS("char");
  RcharSetTable := TtoS("charSetTable");
  RColor := TtoS("Color");
  RCommandWindowLocation := TtoS("CommandWindowLocation");
  RCompoundText := TtoS("CompoundText");
  RCursor := TtoS("Cursor");
  RDimension := TtoS("Dimension");
  RDisplay := TtoS("Display");
  REditMode := TtoS("EditMode");
  RFile := TtoS("File");
  RFont := TtoS("Font");
  RFontList := TtoS("FontList");
  RFontStruct := TtoS("FontStruct");
  RFunction := TtoS("Function");
  RGeometry := TtoS("Geometry");
  RHorizontalDimension := TtoS("HorizontalDimension");
  RHorizontalPosition := TtoS("HorizontalPosition");
  RImmediate := TtoS("Immediate");
  RIndicatorType := TtoS("IndicatorType");
  Rint := TtoS("int");
  RJustify := TtoS("Justify");
  RKeySym := TtoS("KeySym");
  RKeySymTable := TtoS("KeySymTable");
  RLabelType := TtoS("LabelType");
  RMenuWidget := TtoS("MenuWidget");
  RMnemonic := TtoS("Mnemonic");
  RNavigationType := TtoS("NavigationType");
  ROrientation := TtoS("Orientation");
  RPacking := TtoS("Packing");
  RPixel := TtoS("Pixel");
  RPixmap := TtoS("Pixmap");
  RGadgetPixmap := TtoS("GadgetPixmap");
  RPointer := TtoS("Pointer");
  RPosition := TtoS("Position");
  RProc := TtoS("Proc");
  RRowColumnType := TtoS("RowColumnType");
  Rshort := TtoS("short");
  RString := TtoS("String");
  RStringDirection := TtoS("StringDirection");
  RStringTable := TtoS("StringTable");
  RTextPosition := TtoS("TextPosition");
  Runsigned_char := TtoS("unsigned_char");
  RVerticalDimension := TtoS("VerticalDimension");
  RVerticalPosition := TtoS("VerticalPosition");
  RTranslationTable := TtoS("TranslationTable");
  RTraversalType := TtoS("TraversalType");
  RWhichButton := TtoS("WhichButton");
  RWidget := TtoS("Widget");
  RWidgetList := TtoS("WidgetList");
  RWidgetClass := TtoS("WidgetClass");
  RWindow := TtoS("Window");
  RXmString := TtoS("XmString");
  RXmStringTable := TtoS("XmStringTable");
  RXmStringcharSet := TtoS("XmStringcharSet");
  RPrimForegroundPixmap := TtoS("PrimForegroundPixmap");
  RManForegroundPixmap := TtoS("ManForegroundPixmap");
  RBackgroundPixmap := TtoS("BackgroundPixmap");
  RSizePolicy := TtoS("SizePolicy");
  RPrimHighlightPixmap := TtoS("HighlightPixmap");
  RPrimTopShadowPixmap := TtoS("TopShadowPixmap");
  RPrimBottomShadowPixmap := TtoS("BottomShadowPixmap");
  RUnitType := TtoS("UnitType");
  RManTopShadowPixmap := TtoS("ManTopShadowPixmap");
  RManBottomShadowPixmap := TtoS("ManBottomShadowPixmap");
  RManHighlightPixmap := TtoS("ManHighlightPixmap");
  RResizePolicy := TtoS("ResizePolicy");
  RAttachment := TtoS("Attachment");
  RBooleanDimension := TtoS("BooleanDimension");
  RMultiClick := TtoS("MultiClick");
  RShadowType := TtoS("ShadowType");
  RArrowDirection := TtoS("ArrowDirection");
  RMargin := TtoS("Margin");
  RSeparatorType := TtoS("SeparatorType");
  RProcessingDirection := TtoS("ProcessingDirection");
  RListSpacing := TtoS("ListSpacing");
  RListMarginWidth := TtoS("ListMarginWidth");
  RListMarginHeight := TtoS("ListMarginHeight");
  RItems := TtoS("Items");
  RItemCount := TtoS("ItemCount");
  RSelectedItems := TtoS("SelectedItems");
  RSelectedItemCount := TtoS("SelectedItemCount");
  RVisibleItemCount := TtoS("VisibleItemCount");
  RSelectionPolicy := TtoS("SelectionPolicy");
  RListSizePolicy := TtoS("ListSizePolicy");
  RDoubleClickinterval := TtoS("DoubleClickinterval");
  RScrollingPolicy := TtoS("ScrollingPolicy");
  RVisualPolicy := TtoS("VisualPolicy");
  RScrollBarDisplayPolicy := TtoS("ScrollBarDisplayPolicy");
  RScrollBarPlacement := TtoS("ScrollBarPlacement");
  RDialogStyle := TtoS("DialogStyle");
  RFileTypeMask := TtoS("FileTypeMask");
  RDefaultButtonType := TtoS("DefaultButtonType");
  RDialogType := TtoS("DialogType");
  VaPUSHBUTTON := TtoS("pushButton");
  VaTOGGLEBUTTON := TtoS("checkButton");
  VaCHECKBUTTON := TtoS("checkButton");
  VaRADIOBUTTON := TtoS("radioButton");
  VaCASCADEBUTTON := TtoS("cascadeButton");
  VaSEPARATOR := TtoS("separator");
  VaSINGLE_SEPARATOR := TtoS("singleSeparator");
  VaDOUBLE_SEPARATOR := TtoS("doubleSeparator");
  VaTITLE := TtoS("title");
END Xm.
