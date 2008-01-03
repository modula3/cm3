(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 07:56:20 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE MODULE Xm;
(*Generated from Xm.h by ctom3 on 7/26/93*)

FROM M3toC IMPORT FlatTtoS;


PROCEDURE Version():INTEGER = 
BEGIN
  RETURN (VERSION * 1000 + REVISION);
END Version;


BEGIN
  STRING_ISO8859_1 := FlatTtoS("ISO8859-1");
  STRING_OS_CHARSET := FlatTtoS("ISO8859-1");
  FALLBACK_CHARSET := FlatTtoS("ISO8859-1");
  STRING_DEFAULT_CHARSET := FlatTtoS("");
  RAcceleratorTable := FlatTtoS("AcceleratorTable");
  RAlignment := FlatTtoS("Alignment");
  RXmBackgroundPixmap := FlatTtoS("XmBackgroundPixmap");
  RBool := FlatTtoS("Bool");
  RBoolean := FlatTtoS("Boolean");
  RButtonType := FlatTtoS("ButtonType");
  RCallback := FlatTtoS("Callback");
  RCallbackProc := FlatTtoS("CallbackProc");
  RCallProc := FlatTtoS("CallProc");
  Rchar := FlatTtoS("char");
  RcharSetTable := FlatTtoS("charSetTable");
  RColor := FlatTtoS("Color");
  RCommandWindowLocation := FlatTtoS("CommandWindowLocation");
  RCompoundText := FlatTtoS("CompoundText");
  RCursor := FlatTtoS("Cursor");
  RDimension := FlatTtoS("Dimension");
  RDisplay := FlatTtoS("Display");
  REditMode := FlatTtoS("EditMode");
  RFile := FlatTtoS("File");
  RFont := FlatTtoS("Font");
  RFontList := FlatTtoS("FontList");
  RFontStruct := FlatTtoS("FontStruct");
  RFunction := FlatTtoS("Function");
  RGeometry := FlatTtoS("Geometry");
  RHorizontalDimension := FlatTtoS("HorizontalDimension");
  RHorizontalPosition := FlatTtoS("HorizontalPosition");
  RImmediate := FlatTtoS("Immediate");
  RIndicatorType := FlatTtoS("IndicatorType");
  Rint := FlatTtoS("int");
  RJustify := FlatTtoS("Justify");
  RKeySym := FlatTtoS("KeySym");
  RKeySymTable := FlatTtoS("KeySymTable");
  RLabelType := FlatTtoS("LabelType");
  RMenuWidget := FlatTtoS("MenuWidget");
  RMnemonic := FlatTtoS("Mnemonic");
  RNavigationType := FlatTtoS("NavigationType");
  ROrientation := FlatTtoS("Orientation");
  RPacking := FlatTtoS("Packing");
  RPixel := FlatTtoS("Pixel");
  RPixmap := FlatTtoS("Pixmap");
  RGadgetPixmap := FlatTtoS("GadgetPixmap");
  RPointer := FlatTtoS("Pointer");
  RPosition := FlatTtoS("Position");
  RProc := FlatTtoS("Proc");
  RRowColumnType := FlatTtoS("RowColumnType");
  Rshort := FlatTtoS("short");
  RString := FlatTtoS("String");
  RStringDirection := FlatTtoS("StringDirection");
  RStringTable := FlatTtoS("StringTable");
  RTextPosition := FlatTtoS("TextPosition");
  Runsigned_char := FlatTtoS("unsigned_char");
  RVerticalDimension := FlatTtoS("VerticalDimension");
  RVerticalPosition := FlatTtoS("VerticalPosition");
  RTranslationTable := FlatTtoS("TranslationTable");
  RTraversalType := FlatTtoS("TraversalType");
  RWhichButton := FlatTtoS("WhichButton");
  RWidget := FlatTtoS("Widget");
  RWidgetList := FlatTtoS("WidgetList");
  RWidgetClass := FlatTtoS("WidgetClass");
  RWindow := FlatTtoS("Window");
  RXmString := FlatTtoS("XmString");
  RXmStringTable := FlatTtoS("XmStringTable");
  RXmStringcharSet := FlatTtoS("XmStringcharSet");
  RPrimForegroundPixmap := FlatTtoS("PrimForegroundPixmap");
  RManForegroundPixmap := FlatTtoS("ManForegroundPixmap");
  RBackgroundPixmap := FlatTtoS("BackgroundPixmap");
  RSizePolicy := FlatTtoS("SizePolicy");
  RPrimHighlightPixmap := FlatTtoS("HighlightPixmap");
  RPrimTopShadowPixmap := FlatTtoS("TopShadowPixmap");
  RPrimBottomShadowPixmap := FlatTtoS("BottomShadowPixmap");
  RUnitType := FlatTtoS("UnitType");
  RManTopShadowPixmap := FlatTtoS("ManTopShadowPixmap");
  RManBottomShadowPixmap := FlatTtoS("ManBottomShadowPixmap");
  RManHighlightPixmap := FlatTtoS("ManHighlightPixmap");
  RResizePolicy := FlatTtoS("ResizePolicy");
  RAttachment := FlatTtoS("Attachment");
  RBooleanDimension := FlatTtoS("BooleanDimension");
  RMultiClick := FlatTtoS("MultiClick");
  RShadowType := FlatTtoS("ShadowType");
  RArrowDirection := FlatTtoS("ArrowDirection");
  RMargin := FlatTtoS("Margin");
  RSeparatorType := FlatTtoS("SeparatorType");
  RProcessingDirection := FlatTtoS("ProcessingDirection");
  RListSpacing := FlatTtoS("ListSpacing");
  RListMarginWidth := FlatTtoS("ListMarginWidth");
  RListMarginHeight := FlatTtoS("ListMarginHeight");
  RItems := FlatTtoS("Items");
  RItemCount := FlatTtoS("ItemCount");
  RSelectedItems := FlatTtoS("SelectedItems");
  RSelectedItemCount := FlatTtoS("SelectedItemCount");
  RVisibleItemCount := FlatTtoS("VisibleItemCount");
  RSelectionPolicy := FlatTtoS("SelectionPolicy");
  RListSizePolicy := FlatTtoS("ListSizePolicy");
  RDoubleClickinterval := FlatTtoS("DoubleClickinterval");
  RScrollingPolicy := FlatTtoS("ScrollingPolicy");
  RVisualPolicy := FlatTtoS("VisualPolicy");
  RScrollBarDisplayPolicy := FlatTtoS("ScrollBarDisplayPolicy");
  RScrollBarPlacement := FlatTtoS("ScrollBarPlacement");
  RDialogStyle := FlatTtoS("DialogStyle");
  RFileTypeMask := FlatTtoS("FileTypeMask");
  RDefaultButtonType := FlatTtoS("DefaultButtonType");
  RDialogType := FlatTtoS("DialogType");
  VaPUSHBUTTON := FlatTtoS("pushButton");
  VaTOGGLEBUTTON := FlatTtoS("checkButton");
  VaCHECKBUTTON := FlatTtoS("checkButton");
  VaRADIOBUTTON := FlatTtoS("radioButton");
  VaCASCADEBUTTON := FlatTtoS("cascadeButton");
  VaSEPARATOR := FlatTtoS("separator");
  VaSINGLE_SEPARATOR := FlatTtoS("singleSeparator");
  VaDOUBLE_SEPARATOR := FlatTtoS("doubleSeparator");
  VaTITLE := FlatTtoS("title");
END Xm.
