(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* Last modified on Fri May  7 16:20:32 PDT 1993 by mjordan     *)  
(*      modified on Tue Feb 11 17:33:16 PST 1992 by muller      *)
(*      modified on Tue Apr 24 17:41:54 1990 by jerome		*)

UNSAFE INTERFACE Xaw;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	The Athena Widgets Set					*)
(*	contains:						*)
(*								*)
(*			../include/Xt/Shell.h			*)
(*								*)
(*			../include/Xaw/AsciiSink.h		*)
(*			../include/Xaw/AsciiSrc.h		*)
(*			../include/Xaw/AsciiText.h		*)
(*			../include/Xaw/Box.h			*)
(*			../include/Xaw/Cardinals.h		*)
(*			../include/Xaw/Clock.h			*)
(*			../include/Xaw/Command.h		*)
(*			../include/Xaw/Dialog.h			*)
(*			../include/Xaw/Form.h			*)
(*			../include/Xaw/Grip.h			*)
(*			../include/Xaw/Label.h			*)
(*			../include/Xaw/List.h			*)
(*			../include/Xaw/Logo.h			*)
(*			../include/Xaw/Mailbox.h		*)
(*			../include/Xaw/MenuButton.h		*)
(*			../include/Xaw/Paned.h			*)
(*			../include/Xaw/Scrollbar.h		*)
(*			../include/Xaw/Simple.h			*)
(*			../include/Xaw/SimpleMenu.h		*)
(*			../include/Xaw/Sme.h			*)
(*			../include/Xaw/SmeBSB.h			*)
(*			../include/Xaw/SmeLine.h		*)
(*			../include/Xaw/StripChart.h		*)
(*			../include/Xaw/Template.h		*)
(*			../include/Xaw/Text.h			*)
(*			../include/Xaw/TextSink.h		*)
(*			../include/Xaw/TextSrc.h		*)
(*			../include/Xaw/Toggle.h			*)
(*			../include/Xaw/Viewport.h		*)
(*			../include/Xaw/XawInit.h		*)
(*==============================================================*)



FROM Ctypes  	IMPORT char_star_star, int_star, long, unsigned_long_star;
FROM X  	IMPORT Atom, AtomStar, Enumeration, 
                       XEventStar, XRectangleStar;
FROM Xt 	IMPORT AppContext, Boolean, CallbackProc, Cardinal, 
                       Dimension,  Position, String, StringStar, 
                       Widget, WidgetClass;
IMPORT Xmu;

(* 
    Size of a "regular" List of objects
*)

CONST
  MaxSizeList  = 63;


(***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************)


(*======================================================================
 * $XConsortium: Shell.h,v 1.21 89/12/13 08:57:08 swick Exp $
 * $oHeader: Shell.h,v 1.2 88/08/18 15:56:14 asente Exp $
 *======================================================================*)

(***********************************************************************
 *
 * Shell Widget
 *
 ***********************************************************************)

(*
 * Shell specific atoms
 *)

(* #define XtNiconName "iconName"                              in XtN.i3 *)
(* #define XtCIconName "IconName"                              in XtC.i3 *)
(* #define XtNiconPixmap "iconPixmap"                          in XtN.i3 *)
(* #define XtCIconPixmap "IconPixmap"                          in XtC.i3 *)
(* #define XtNiconWindow "iconWindow"                          in XtN.i3 *)
(* #define XtCIconWindow "IconWindow"                          in XtC.i3 *)
(* #define XtNiconMask "iconMask"                              in XtN.i3 *)
(* #define XtCIconMask "IconMask"                              in XtC.i3 *)
(* #define XtNwindowGroup "windowGroup"                        in XtN.i3 *)
(* #define XtCWindowGroup "WindowGroup"                        in XtC.i3 *)
(* #define XtNvisual "visual"                                  in XtN.i3 *)
(* #define XtCVisual "Visual"                                  in XtC.i3 *)
(* #define XtNtitleEncoding "titleEncoding"                    in XtN.i3 *)
(* #define XtCTitleEncoding "TitleEncoding"                    in XtC.i3 *)
 
(* #define XtNsaveUnder "saveUnder"                            in XtN.i3 *)
(* #define XtCSaveUnder "SaveUnder"                            in XtC.i3 *)
(* #define XtNtransient "transient"                            in XtN.i3 *)
(* #define XtCTransient "Transient"                            in XtC.i3 *)
(* #define XtNoverrideRedirect "overrideRedirect"              in XtN.i3 *)
(* #define XtCOverrideRedirect "OverrideRedirect"              in XtC.i3 *)
(* #define XtNtransientFor "transientFor"                      in XtN.i3 *)
(* #define XtCTransientFor "TransientFor"                      in XtC.i3 *)

(* #define XtNiconNameEncoding "iconNameEncoding"              in XtN.i3 *)
(* #define XtCIconNameEncoding "IconNameEncoding"              in XtC.i3 *)

(* #define XtNallowShellResize "allowShellResize"              in XtN.i3 *)
(* #define XtCAllowShellResize "AllowShellResize"              in XtC.i3 *)
(* #define XtNcreatePopupChildProc "createPopupChildProc"      in XtN.i3 *)
(* #define XtCCreatePopupChildProc "CreatePopupChildProc"      in XtC.i3 *)

(* #define XtNtitle "title"                                    in XtN.i3 *)
(* #define XtCTitle "Title"                                    in XtC.i3 *)

(* #define XtRAtom "Atom"                                      in XtR.i3 *)

(* 
 * The following are only used at creation and can not be changed via 
 * SetValues.
 *)

(* #define XtNargc "argc"                                      in XtN.i3 *)
(* #define XtCArgc "Argc"                                      in XtC.i3 *)
(* #define XtNargv "argv"                                      in XtN.i3 *)
(* #define XtCArgv "Argv"                                      in XtC.i3 *)
(* #define XtNiconX "iconX"                                    in XtN.i3 *)
(* #define XtCIconX "IconX"                                    in XtC.i3 *)
(* #define XtNiconY "iconY"                                    in XtN.i3 *)
(* #define XtCIconY "IconY"                                    in XtC.i3 *)
(* #define XtNinput "input"                                    in XtN.i3 *)
(* #define XtCInput "Input"                                    in XtC.i3 *)
(* #define XtNiconic "iconic"                                  in XtN.i3 *)
(* #define XtCIconic "Iconic"                                  in XtC.i3 *)
(* #define XtNinitialState "initialState"                      in XtN.i3 *)
(* #define XtCInitialState "InitialState"                      in XtC.i3 *)
(* #define XtNgeometry "geometry"                              in XtN.i3 *)
(* #define XtCGeometry "Geometry"                              in XtC.i3 *)
(* #define XtNbaseWidth "baseWidth"                            in XtN.i3 *)
(* #define XtCBaseWidth "BaseWidth"                            in XtC.i3 *)
(* #define XtNbaseHeight "baseHeight"                          in XtN.i3 *)
(* #define XtCBaseHeight "BaseHeight"                          in XtC.i3 *)
(* #define XtNwinGravity "winGravity"                          in XtN.i3 *)
(* #define XtCWinGravity "WinGravity"                          in XtC.i3 *)
(* #define XtNminWidth "minWidth"                              in XtN.i3 *)
(* #define XtCMinWidth "MinWidth"                              in XtC.i3 *)
(* #define XtNminHeight "minHeight"                            in XtN.i3 *)
(* #define XtCMinHeight "MinHeight"                            in XtC.i3 *)
(* #define XtNmaxWidth "maxWidth"                              in XtN.i3 *)
(* #define XtCMaxWidth "MaxWidth"                              in XtC.i3 *)
(* #define XtNmaxHeight "maxHeight"                            in XtN.i3 *)
(* #define XtCMaxHeight "MaxHeight"                            in XtC.i3 *)
(* #define XtNwidthInc "widthInc"                              in XtN.i3 *)
(* #define XtCWidthInc "WidthInc"                              in XtC.i3 *)
(* #define XtNheightInc "heightInc"                            in XtN.i3 *)
(* #define XtCHeightInc "HeightInc"                            in XtC.i3 *)
(* #define XtNminAspectY "minAspectY"                          in XtN.i3 *)
(* #define XtCMinAspectY "MinAspectY"                          in XtC.i3 *)
(* #define XtNmaxAspectY "maxAspectY"                          in XtN.i3 *)
(* #define XtCMaxAspectY "MaxAspectY"                          in XtC.i3 *)
(* #define XtNminAspectX "minAspectX"                          in XtN.i3 *)
(* #define XtCMinAspectX "MinAspectX"                          in XtC.i3 *)
(* #define XtNmaxAspectX "maxAspectX"                          in XtN.i3 *)
(* #define XtCMaxAspectX "MaxAspectX"                          in XtC.i3 *)
(* #define XtNwmTimeout "wmTimeout"                            in XtN.i3 *)
(* #define XtCWmTimeout "WmTimeout"                            in XtC.i3 *)
(* #define XtNwaitForWm "waitforwm"                            in XtN.i3 *)
(* #define XtCWaitForWm "Waitforwm"                            in XtC.i3 *)

(* Class record constants *)

<* EXTERNAL *>  VAR
   shellWidgetClass:  WidgetClass;
   overrideShellWidgetClass:  WidgetClass;
   wmShellWidgetClass:  WidgetClass;
   transientShellWidgetClass:  WidgetClass;
   topLevelShellWidgetClass:  WidgetClass;
   applicationShellWidgetClass:  WidgetClass;


(*======================================================================
 * $XConsortium: AsciiSink.h,v 1.3 89/11/01 17:33:17 kit Exp $
 *======================================================================*)

(***********************************************************************
 *
 * AsciiSink Object
 *
 ***********************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 echo                Output             Boolean         True
 displayNonprinting  Output             Boolean         True

*)

(* #define XtCOutput "Output"                                  in XtC.i3 *)

(* #define XtNdisplayNonprinting "displayNonprinting"          in XtN.i3 *)
(* #define XtNecho "echo"                                      in XtN.i3 *)

(* Class record constants *)

<* EXTERNAL *>
   VAR asciiSinkObjectClass: WidgetClass;

(*==============================================================
 * $XConsortium: AsciiSrc.h,v 1.5 89/10/05 13:17:30 kit Exp $
 *==============================================================*)

(* Class record constants *)

<* EXTERNAL *>
   VAR asciiSrcObjectClass: WidgetClass;

(*
 * Resource Definitions.
 *)

(* #define XtCDataCompression "DataCompression"                in XtC.i3 *)
(* #define XtCPieceSize "PieceSize"                            in XtC.i3 *)
(* #define XtCType "Type"                                      in XtC.i3 *)
(* #define XtCUseStringInPlace "UseStringInPlace"              in XtC.i3 *)

(* #define XtNdataCompression "dataCompression"                in XtN.i3 *)
(* #define XtNpieceSize "pieceSize"                            in XtN.i3 *)
(* #define XtNtype "type"                                      in XtN.i3 *)
(* #define XtNuseStringInPlace "useStringInPlace"              in XtN.i3 *)

(* #define XtRAsciiType "AsciiType"                            in XtR.i3 *)

(* #define XtEstring "string"                                  in XtE.i3 *)
(* #define XtEfile "file"                                      in XtE.i3 *)

TYPE 
  AsciiType = Enumeration;
CONST
  AsciiFile	= 0;
  AsciiString	= 1;

(************************************************************
 *
 * Public routines 
 *
 ************************************************************)

(*	Function Name: Xaw.AsciiSourceFreeString
 *	Description: Frees the string returned by a get values call
 *                   on the string when the source is of type string.
 *	Arguments: w - the AsciiSrc object.
 *	Returns: none.
 *)

<* EXTERNAL XawAsciiSourceFreeString *> 
   PROCEDURE AsciiSourceFreeString(w: Widget);

(*	Function Name: Xaw.AsciiSave
 *	Description: Saves all the pieces into a file or string as required.
 *	Arguments: w - the asciiSrc Object.
 *	Returns: TRUE if the save was successful.
 *)

<* EXTERNAL XawAsciiSave *>
   PROCEDURE AsciiSave (w: Widget): Boolean;

(*	Function Name: Xaw.AsciiSaveAsFile
 *	Description: Save the current buffer as a file.
 *	Arguments: w - the asciiSrc object.
 *                 name - name of the file to save this file into.
 *	Returns: True if the save was sucessful.
 *)

<* EXTERNAL XawAsciiSaveAsFile *>
   PROCEDURE AsciiSaveAsFile (w: Widget; name: String): Boolean;

(*	Function Name: Xaw.AsciiSourceChanged
 *	Description: Returns true if the source has changed since last saved.
 *	Arguments: w - the asciiSource object.
 *	Returns: a Boolean (see description).
 *)

<* EXTERNAL XawAsciiSourceChanged *>
   PROCEDURE AsciiSourceChanged (w: Widget): Boolean;


(*================================================================
 * $XConsortium: AsciiText.h,v 1.15 89/07/06 16:00:35 kit Exp $ 
 *================================================================*)

(****************************************************************
 *
 * AsciiText widgets
 *
 ****************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 displayPosition     TextPosition	int		0
 editType	     EditType		XawTextEditType	XawtextRead
 font		     Font		XFontStruct*	Fixed
 foreground	     Foreground		Pixel		Black
 height		     Height		Dimension	font height
 insertPosition	     TextPosition	int		0
 leftMargin	     Margin		Dimension	2
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 selectTypes	     SelectTypes	Pointer		(internal)
 selection	     Selection		Pointer		empty selection
 sensitive	     Sensitive		Boolean		True
 string		     String		String		NULL
 textOptions	     TextOptions	int		0
 width		     Width		Dimension	100
 x		     Position		Position	0
 y		     Position		Position	0

*)

(*
 * Everything else we need is in StringDefs.h or Text.h
 *)

<* EXTERNAL *> VAR
   asciiDiskWidgetClass: WidgetClass;
   asciiTextWidgetClass: WidgetClass;
   asciiStringWidgetClass: WidgetClass;

(*=============================================================
 * $XConsortium: Box.h,v 1.19 89/11/06 10:51:21 swick Exp $
 *=============================================================*)

(***********************************************************************
 *
 * Box Widget (subclass of CompositeClass)
 *
 ***********************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 hSpace 	     HSpace		Dimension	4
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 orientation	     Orientation	XtOrientation	vertical
 vSpace 	     VSpace		Dimension	4
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*)


(* New fields *)

(* #define XtNhSpace "hSpace"                                  in XtN.i3 *)
(* #define XtNvSpace "vSpace"                                  in XtN.i3 *)

(* Class record constants *)

<* EXTERNAL *> VAR
   boxWidgetClass: WidgetClass;


(*===============================================================
 * $XConsortium: Cardinals.h,v 1.5 89/03/30 16:05:19 jim Exp $
 *===============================================================*)

CONST
  ZERO       = 0;
  ONE        = 1;
  TWO        = 2;
  THREE      = 3;
  FOUR       = 4;
  FIVE       = 5;
  SIX        = 6;
  SEVEN      = 7;
  EIGHT      = 8;
  NINE       = 9;
  TEN        = 10;


(*===========================================================
 * $XConsortium: Clock.h,v 1.28 89/07/20 14:54:38 jim Exp $
 *===========================================================*)

(***********************************************************************
 *
 * Clock Widget
 *
 ***********************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 analog		     Boolean		Boolean		True
 background	     Background		Pixel		white
 backingStore	     BackingStore	BackingStore	default
 border		     BorderColor	Pixel		Black
 borderWidth	     BorderWidth	Dimension	1
 chime		     Boolean		Boolean		False
 destroyCallback     Callback		Pointer		NULL
 font		     Font		XFontStruct*	fixed
 foreground	     Foreground		Pixel		black
 hand		     Foreground		Pixel		black
 height		     Height		Dimension	164
 highlight	     Foreground		Pixel		black
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 padding	     Margin		int		8
 reverseVideo	     ReverseVideo	Boolean		False
 update		     Interval		int		60 (seconds)
 width		     Width		Dimension	164
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* Resource names used to the clock widget *)

		(* color of hands *)
(* #define XtNhand "hands"                                     in XtN.i3 *)

		(* Boolean: digital if FALSE *)
(* #define XtNanalog "analog"                                  in XtN.i3 *)

		(* Boolean:  *)
(* #define XtNchime "chime"                                    in XtN.i3 *)

		(* Int: amount of space around outside of clock *)
(* #define XtNpadding "padding"                                in XtN.i3 *)

<* EXTERNAL *> VAR
   clockWidgetClass: WidgetClass;


(*================================================================
 * $XConsortium: Command.h,v 1.26 89/10/03 14:51:22 kit Exp $
 *================================================================*)

(***********************************************************************
 *
 * Command Widget
 *
 ***********************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 bitmap		     Pixmap		Pixmap		None
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 callback	     Callback		Pointer		NULL
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		Pointer		NULL
 font		     Font		XFontStruct*	XtDefaultFont
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	text height
 highlightThickness  Thickness		Dimension	2
 insensitiveBorder   Insensitive	Pixmap		Gray
 internalHeight	     Height		Dimension	2
 internalWidth	     Width		Dimension	4
 justify	     Justify		XtJustify	XtJustifyCenter
 label		     Label		String		NULL
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 resize		     Resize		Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	text width
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* #define XtNhighlightThickness "highlightThickness"          in XtN.i3 *)
(* #define XtNshapeStyle "shapeStyle"                          in XtN.i3 *)
(* #define XtCShapeStyle "ShapeStyle"                          in XtC.i3 *)
(* #define XtRShapeStyle "ShapeStyle"                          in XtR.i3 *)
(* #define XtNcornerRoundPercent "cornerRoundPercent"          in XtN.i3 *)
(* #define XtCCornerRoundPercent "CornerRoundPercent"          in XtC.i3 *)

CONST
   ShapeRectangle		= Xmu.ShapeRectangle;
   ShapeOval			= Xmu.ShapeOval;
   ShapeEllipse 		= Xmu.ShapeEllipse;
   ShapeRoundedRectangle	= Xmu.ShapeRoundedRectangle;

<* EXTERNAL *> VAR
   commandWidgetClass: WidgetClass;


(*==================================================================
 * $XConsortium: Dialog.h,v 1.20 89/11/12 14:02:57 kit Exp $ 
 *==================================================================*)

(***********************************************************************
 *
 * Dialog Widget
 *
 ***********************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	computed at create
 icon		     Icon		Pixmap		0
 label		     Label		String		NULL
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 value		     Value		String		NULL
 width		     Width		Dimension	computed at create
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* #define XtCIcon "Icon"                                      in XtC.i3 *)

(* #define XtNicon "icon"                                      in XtN.i3 *)

<* EXTERNAL *> VAR
   dialogWidgetClass: WidgetClass;

<* EXTERNAL XawDialogAddButton *>
   PROCEDURE DialogAddButton (parent: Widget; name: String; 
                              function: CallbackProc; param: ADDRESS);

<* EXTERNAL XawDialogGetValueString *>
   PROCEDURE DialogGetValueString (w: Widget): String;


(*============================================================
 * $XConsortium: Form.h,v 1.22 89/07/21 01:51:26 kit Exp $ 
 *============================================================*)

(***********************************************************************
 *
 * Form Widget
 *
 ***********************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 defaultDistance     Thickness		int		4
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	computed at realize
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	computed at realize
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* Constraint parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 bottom		     Edge		XtEdgeType	XtRubber
 fromHoriz	     Widget		Widget		(left edge of form)
 fromVert	     Widget		Widget		(top of form)
 horizDistance	     Thickness		int		defaultDistance
 left		     Edge		XtEdgeType	XtRubber
 resizable	     Boolean		Boolean		False
 right		     Edge		XtEdgeType	XtRubber
 top		     Edge		XtEdgeType	XtRubber
 vertDistance	     Thickness		int		defaultDistance

*)

(* #define XtNdefaultDistance "defaultDistance"                in XtN.i3 *)
(* #define XtNtop "top"                                        in XtN.i3 *)
(* #define XtNbottom "bottom"                                  in XtN.i3 *)
(* #define XtNleft "left"                                      in XtN.i3 *)
(* #define XtNright "right"                                    in XtN.i3 *)
(* #define XtNfromHoriz "fromHoriz"                            in XtN.i3 *)
(* #define XtNfromVert "fromVert"                              in XtN.i3 *)
(* #define XtNhorizDistance "horizDistance"                    in XtN.i3 *)
(* #define XtNvertDistance "vertDistance"                      in XtN.i3 *)
(* #define XtNresizable "resizable"                            in XtN.i3 *)

(* #define XtCEdge "Edge"                                      in XtC.i3 *)
(* #define XtCWidget "Widget"                                  in XtC.i3 *)

(* #define XtRWidget "Widget"                                  in XtR.i3 *)

TYPE
  EdgeType		= Enumeration;
CONST
    ChainTop		= 0;	(* Keep this edge a constant distance from
				   the top of the form *)
    ChainBottom		= 1;	(* Keep this edge a constant distance from
				   the bottom of the form *)
    ChainLeft		= 2;	(* Keep this edge a constant distance from
				   the left of the form *)
    ChainRight		= 3;	(* Keep this edge a constant distance from
				   the right of the form *)
    Rubber		= 4;	(* Keep this edge a proportional distance
				   from the edges of the form*)

<* EXTERNAL *> VAR
   formWidgetClass: WidgetClass;

<* EXTERNAL XawFormDoLayout *>
   PROCEDURE FormDoLayout (widget: Widget; doit: Boolean);


(*==========================================================
 * $XConsortium: Grip.h,v 1.15 89/07/21 01:51:29 kit Exp $
 *==========================================================*)

(*
 *  Grip.h - Public Definitions for Grip widget (used by VPane Widget)
 *
 *)

(***************************************************************************
 *
 * Grip Widget 
 *
 **************************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 foreground	     Foreground		Pixel		XtDefaultForeground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	0
 callback	     Callback		Pointer		GripAction
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	8
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	8
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* #define XtNgripTranslations "gripTranslations"              in XtN.i3 *)

TYPE
  GripCallDataRec =  RECORD
      event: XEventStar;		(* the event causing the GripAction *)
      params: StringStar;		(* the TranslationTable params *)
      num_params: Cardinal		(* count of params *)
                     END;
  GripCallDataStar = UNTRACED REF GripCallDataRec;

(* Class Record Constant *)

<* EXTERNAL *> VAR
   gripWidgetClass: WidgetClass;


(*=============================================================
 * $XConsortium: Label.h,v 1.24 89/07/21 01:48:51 kit Exp $
 *=============================================================*)

(***********************************************************************
 *
 * Label Widget
 *
 ***********************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 bitmap		     Pixmap		Pixmap		None
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		XtCallbackList	NULL
 font		     Font		XFontStruct*	XtDefaultFont
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	text height
 insensitiveBorder   Insensitive	Pixmap		Gray
 internalHeight	     Height		Dimension	2
 internalWidth	     Width		Dimension	4
 justify	     Justify		XtJustify	XtJustifyCenter
 label		     Label		String		NULL
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 resize		     Resize		Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	text width
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* #define XtNbitmap "bitmap"                                  in XtN.i3 *)
(* #define XtNforeground "foreground"                          in XtN.i3 *)
(* #define XtNlabel "label"                                    in XtN.i3 *)
(* #define XtNfont "font"                                      in XtN.i3 *)
(* #define XtNinternalWidth "internalWidth"                    in XtN.i3 *)
(* #define XtNinternalHeight "internalHeight"                  in XtN.i3 *)
(* #define XtNresize "resize"                                  in XtN.i3 *)
(* #define XtCResize "Resize"                                  in XtC.i3 *)
 
(* #define XtCBitmap "Bitmap"                                  in XtC.i3 *)

(* Class record constants *)

<* EXTERNAL *> VAR
  labelWidgetClass: WidgetClass;


(*============================================================
 * $XConsortium: List.h,v 1.15 89/12/11 15:08:59 kit Exp $
 *============================================================*)

(*  This is the List widget, it is useful to display a list, without the
 *  overhead of having a widget for each item in the list.  It allows 
 *  the user to select an item in a list and notifies the application through
 *  a callback function.
 *
 *	Created: 	8/13/88
 *	By:		Chris D. Peterson
 *                      MIT X Consortium
 *)

(***********************************************************************
 *
 * List Widget
 *
 ***********************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 callback            Callback           XtCallbackList  NULL       **6
 columnSpacing       Spacing            Dimension       6
 cursor		     Cursor		Cursor		left_ptr
 defaultColumns      Columns            int             2          **5
 destroyCallback     Callback		Pointer		NULL 
 font		     Font		XFontStruct*	XtDefaultFont
 forceColumns        Columns            Boolean         False      **5
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	0          **1
 insensitiveBorder   Insensitive	Pixmap		Gray
 internalHeight	     Height		Dimension	2
 internalWidth	     Width		Dimension	4
 list                List               String *        NULL       **2
 longest             Longest            int             0          **3  **4
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 numberStrings       NumberStrings      int             0          **4
 pasteBuffer         Boolean            Boolean         False
 rowSpacing          Spacing            Dimension       4
 sensitive	     Sensitive		Boolean		True
 verticalList        Boolean            Boolean         False
 width		     Width		Dimension	0          **1
 x		     Position		Position	0
 y		     Position		Position	0

 **1 - If the Width or Height of the list widget is zero (0) then the value
       is set to the minimum size necessay to fit the entire list.

       If both Width and Height are zero then they are adjusted to fit the
       entire list that is created width the number of default columns 
       specified in the defaultColumns resource.

 **2 - This is an array of strings the specify elements of the list.
       This resource must be specified. 
       (What good is a list widget without a list??  :-)

 **3 - Longest is the length of the widest string in pixels.

 **4 - If either of these values are zero (0) then the list widget calculates
       the correct value. 

       (This allows you to make startup faster if you already have 
        this information calculated)

       NOTE: If the numberStrings value is zero the list must 
             be NULL terminated.

 **5 - By setting the List.Columns resource you can force the application to
       have a given number of columns.	     
        
 **6 - This returns the name and index of the item selected in an 
       XawListReturnStruct that is pointed to by the client_data
       in the CallbackProc.

*)

(*
 * Value returned when there are no highlighted objects. 
 *)

CONST
  XAW_LIST_NONE = -1;

(* #define XtCList "List"                                      in XtC.i3 *)
(* #define XtCSpacing "Spacing"                                in XtC.i3 *)
(* #define XtCColumns "Columns"                                in XtC.i3 *)
(* #define XtCLongest "Longest"                                in XtC.i3 *)
(* #define XtCNumberStrings "NumberStrings"                    in XtC.i3 *)

(* #define XtNcursor "cursor"                                  in XtN.i3 *)
(* #define XtNcolumnSpacing "columnSpacing"                    in XtN.i3 *)
(* #define XtNdefaultColumns "defaultColumns"                  in XtN.i3 *)
(* #define XtNforceColumns "forceColumns"                      in XtN.i3 *)
(* #define XtNlist "list"                                      in XtN.i3 *)
(* #define XtNlongest "longest"                                in XtN.i3 *)
(* #define XtNnumberStrings "numberStrings"                    in XtN.i3 *)
(* #define XtNpasteBuffer "pasteBuffer"                        in XtN.i3 *)
(* #define XtNrowSpacing "rowSpacing"                          in XtN.i3 *)
(* #define XtNverticalList "verticalList"                      in XtN.i3 *)
 
(* Class record constants *)

<* EXTERNAL *> VAR
   listWidgetClass: WidgetClass;

(* The list return structure. *)

TYPE
  ListReturnStruct = RECORD  string: String; list_index: INTEGER; END;
  ListReturnStructStar = UNTRACED REF ListReturnStruct;

(******************************************************************
 *
 * Exported Functions
 *
 *****************************************************************)

(*	Function Name: Xaw.ListChange.
 *	Description: Changes the list being used and shown.
 *	Arguments: w - the list widget.
 *                 list - the new list.
 *                 nitems - the number of items in the list.
 *                 longest - the length (in Pixels) of the longest element
 *                           in the list.
 *                 resize - if TRUE the the list widget will
 *                          try to resize itself.
 *	Returns: none.
 *      NOTE:      If nitems of longest are <= 0 then they will be caluculated.
 *                 If nitems is <= 0 then the list needs to be NULL terminated.
 *)

<* EXTERNAL XawListChange *>
   PROCEDURE ListChange (w: Widget; list: char_star_star; 
                         nitems, longest: INTEGER; resize: Boolean);

(*	Function Name: Xaw.ListUnhighlight
 *	Description: unlights the current highlighted element.
 *	Arguments: w - the widget.
 *	Returns: none.
 *)

<* EXTERNAL XawListUnhighlight *>
   PROCEDURE ListUnhighlight (w: Widget);

(*	Function Name: Xaw.ListHighlight
 *	Description: Highlights the given item.
 *	Arguments: w - the list widget.
 *                 item - the item to hightlight.
 *	Returns: none.
 *)

<* EXTERNAL XawListHighlight *>
   PROCEDURE ListHighlight (w: Widget; item: INTEGER);

(*	Function Name: Xaw.ListShowCurrent
 *	Description: returns the currently highlighted object.
 *	Arguments: w - the list widget.
 *	Returns: the info about the currently highlighted object.
 *)

<* EXTERNAL XawListShowCurrent *>
   PROCEDURE ListShowCurrent (w: Widget): ListReturnStructStar;


(*==========================================================
 * $XConsortium: Logo.h,v 1.9 89/05/11 01:05:50 kit Exp $
 *==========================================================*)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		White
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 foreground	     Foreground		Pixel		Black
 height		     Height		Dimension	100
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 reverseVideo	     ReverseVideo	Boolean		False
 width		     Width		Dimension	100
 x		     Position		Position	0
 y		     Position		Position	0

*)

<* EXTERNAL *> VAR
   logoWidgetClass: WidgetClass ;


(*===============================================================
 * $XConsortium: Mailbox.h,v 1.19 89/07/21 01:55:46 kit Exp $
 *===============================================================*)

(*
 * Mailbox widget; looks a lot like the clock widget, don't it...
 *)

(* resource names used by mailbox widget that aren't defined in StringDefs.h *)

(* #define XtNupdate "update"                                  in XtN.i3 *)

(* command to exec *)
(* #define XtNcheckCommand "checkCommand"                      in XtN.i3 *)
(* #define XtNonceOnly "onceOnly"                              in XtN.i3 *)

(* Int: volume for bell *)
(* #define XtNvolume "volume"                                  in XtN.i3 *)
(* #define XtNfullPixmap "fullPixmap"                          in XtN.i3 *)
(* #define XtNfullPixmapMask "fullPixmapMask"                  in XtN.i3 *)
(* #define XtNemptyPixmap "emptyPixmap"                        in XtN.i3 *)
(* #define XtNemptyPixmapMask "emptyPixmapMask"                in XtN.i3 *)
(* #define XtNflip "flip"                                      in XtN.i3 *)
(* #define XtNshapeWindow "shapeWindow"                        in XtN.i3 *)

(* #define XtCCheckCommand "CheckCommand"                      in XtC.i3 *)
(* #define XtCVolume "Volume"                                  in XtC.i3 *)
(* #define XtCPixmapMask "PixmapMask"                          in XtC.i3 *)
(* #define XtCFlip "Flip"                                      in XtC.i3 *)
(* #define XtCShapeWindow "ShapeWindow"                        in XtC.i3 *)

(* structures *)

<* EXTERNAL *> VAR
   mailboxWidgetClass: WidgetClass;


(*================================================================
 * $XConsortium: MenuButton.h,v 1.7 89/12/11 14:57:44 kit Exp $
 *================================================================*)

(***********************************************************************
 *
 * MenuButton Widget
 *
 ***********************************************************************)

(*
 * MenuButton.h - Public Header file for MenuButton widget.
 *
 * This is the public header file for the Athena MenuButton widget.
 * It is intended to provide an easy method of activating pulldown menus.
 *
 * Date:    May 2, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 *)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 bitmap		     Pixmap		Pixmap		None
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 callback	     Callback		Pointer		NULL
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		Pointer		NULL
 font		     Font		XFontStruct*	XtDefaultFont
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	text height
 highlightThickness  Thickness		Dimension	2
 insensitiveBorder   Insensitive	Pixmap		Gray
 internalHeight	     Height		Dimension	2
 internalWidth	     Width		Dimension	4
 justify	     Justify		XtJustify	XtJustifyCenter
 label		     Label		String		NULL
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 menuName            MenuName           String          "menu"
 resize		     Resize		Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	text width
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* #define XtNmenuName "menuName"                              in XtN.i3 *)
(* #define XtCMenuName "MenuName"                              in XtC.i3 *)

<* EXTERNAL *> VAR
   menuButtonWidgetClass: WidgetClass;


(*=============================================================
 * $XConsortium: Paned.h,v 1.8 89/10/04 19:35:43 kit Exp $
 *=============================================================*)

(*
 * Paned.h - Paned Composite Widget's public header file.
 *
 * Updated and significantly modifided from the Athena VPaned Widget.
 *
 * Date:    March 1, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium
 *          kit@expo.lcs.mit.edu
 *)

(****************************************************************
 *
 * Vertical Paned Widget (SubClass of CompositeClass)
 *
 ****************************************************************)

(* RESOURCES:

 Name		         Class		   RepType	    Default Value
 ----		         -----		   -------	    -------------
 background	         Background	   Pixel	    XtDefaultBackground
 betweenCursor	         Cursor	           Cursor	    **
 border		         BorderColor       Pixel	    XtDefaultForeground
 borderWidth	         BorderWidth       Dimension	    1
 cursor		         Cursor	           Cursor	    None
 destroyCallback         Callback	   Pointer	    NULL
 height		         Height	           Dimension	    0
 gripIndent	         GripIndent	   Position	    16
 gripCursor	         Cursor	           Cursor	    **
 horizontalGripCursol    Cursor	           Cursor	    sb_h_double_arrow
 horizontalBetweencursor Cursor	           Cursor	    sb_up_arrow
 internalBorderColor     BorderColor	   Pixel	    XtDefaultForeground
 internalBorderWidth     BorderWidth	   Position	    1
 leftCursor	         Cursor	           Cursor	    sb_left_arrow
 lowerCursor	         Cursor	           Cursor	    sb_down_arrow
 mappedWhenManaged       MappedWhenManaged Boolean	    True
 orientation             Orientation       XtOrientation    XtorientVertical
 refigureMode	         Boolean	   Boolean	    On
 rightCursor	         Cursor	           Cursor           sb_right_arrow
 sensitive	         Sensitive	   Boolean	    True
 upperCursor	         Cursor	           Cursor	    sb_up_arrow
 verticalBetweenCursor   Cursor	           Cursor           sb_left_arrow
 verticalGripCursor      Cursor	           Cursor           sb_v_double_arrow
 width		         Width	           Dimension	    0
 x		         Position	   Position	    0
 y		         Position	   Position    	    0

** These resources now are set to the vertical or horizontal cursor
   depending upon orientation, by default.  If a value is specified here
   then that cursor will be used reguardless of orientation.

CONSTRAINT RESOURCES:

 Name		      Class		RepType		Default Value
 ----		      -----		-------		-------------
 allowResize	      Boolean	        Boolean         False
 max		      Max	        Dimension	unlimited
 min		      Min		Dimension	Grip Size
 preferredPaneSize    PerferredPaneSize Dimension	PANED_ASK_CHILD
 resizeToPreferred    Boolean		Boolean	 	False
 showGrip	      ShowGrip		Boolean		True
 skipAdjust	      Boolean	        Boolean         False

*)

CONST
  PANED_ASK_CHILD = 0;
  PANED_GRIP_SIZE = 0;

(* New Fields *)

(* #define XtNallowResize "allowResize"                        in XtN.i3 *)
(* #define XtNbetweenCursor "betweenCursor"                    in XtN.i3 *)
(* #define XtNverticalBetweenCursor "verticalBetweenCursor"    in XtN.i3 *)
(* #define XtNhorizontalBetweenCursor "horizontalBetweenCursor"in XtN.i3 *)
(* #define XtNgripCursor "gripCursor"                          in XtN.i3 *)
(* #define XtNgripIndent "gripIndent"                          in XtN.i3 *)
(* #define XtNhorizontalGripCursor "horizontalGripCursor"      in XtN.i3 *)
(* #define XtNinternalBorderColor "internalBorderColor"        in XtN.i3 *)
(* #define XtNinternalBorderWidth "internalBorderWidth"        in XtN.i3 *)
(* #define XtNleftCursor "leftCursor"                          in XtN.i3 *)
(* #define XtNlowerCursor "lowerCursor"                        in XtN.i3 *)
(* #define XtNrefigureMode "refigureMode"                      in XtN.i3 *)
(* #define XtNposition "position"                              in XtN.i3 *)
(* #define XtNmin "min"                                        in XtN.i3 *)
(* #define XtNmax "max"                                        in XtN.i3 *)
(* #define XtNpreferredPaneSize "preferredPaneSize"            in XtN.i3 *)
(* #define XtNresizeToPreferred "resizeToPreferred"            in XtN.i3 *)
(* #define XtNrightCursor "rightCursor"                        in XtN.i3 *)
(* #define XtNshowGrip "showGrip"                              in XtN.i3 *)
(* #define XtNskipAdjust "skipAdjust"                          in XtN.i3 *)
(* #define XtNupperCursor "upperCursor"                        in XtN.i3 *)
(* #define XtNverticalGripCursor "verticalGripCursor"          in XtN.i3 *)

(* #define XtCGripIndent "GripIndent"                          in XtC.i3 *)
(* #define XtCMin "Min"                                        in XtC.i3 *)
(* #define XtCMax "Max"                                        in XtC.i3 *)
(* #define XtCPreferredPaneSize "PreferredPaneSize"            in XtC.i3 *)
(* #define XtCShowGrip "ShowGrip"                              in XtC.i3 *)

(* Class record constant *)

<* EXTERNAL *> VAR
   panedWidgetClass: WidgetClass;

(************************************************************
 *
 *  Public Procedures 
 *
 ************************************************************)

(*	Function Name: Xaw.PanedSetMinMax
 *	Description: Sets the min and max size for a pane.
 *	Arguments: widget - the widget that is a child of the Paned widget.
 *                 min, max - the new min and max size for the pane.
 *	Returns: none.
 *)

<* EXTERNAL XawPanedSetMinMax *>
   PROCEDURE PanedSetMinMax (widget: Widget; min, max: INTEGER);

(*	Function Name: Xaw.PanedGetMinMax
 *	Description: Gets the min and max size for a pane.
 *	Arguments: widget - the widget that is a child of the Paned widget.
 ** RETURNED **    min, max - the current min and max size for the pane.
 *	Returns: none.
 *)

<* EXTERNAL XawPanedGetMinMax *>
   PROCEDURE PanedGetMinMax (widget: Widget; min, max: int_star);

(*	Function Name: Xaw.PanedSetRefigureMode
 *	Description: Allows a flag to be set the will inhibit 
 *                   the paned widgets relayout routine.
 *	Arguments: w - the paned widget.
 *                 mode - if FALSE then inhibit refigure.
 *	Returns: none.
 *)

<* EXTERNAL XawPanedSetRefigureMode *>
   PROCEDURE PanedSetRefigureMode (w: Widget; mode: Boolean);

(*	Function Name: Xaw.PanedGetNumSub
 *	Description: Returns the number of panes in the paned widget.
 *	Arguments: w - the paned widget.
 *	Returns: the number of panes in the paned widget.
 *)

<* EXTERNAL XawPanedGetNumSub *>
   PROCEDURE PanedGetNumSub (w: Widget): INTEGER;

(*	Function Name: Xaw.PanedAllowResize
 *	Description: Allows a flag to be set that determines if the paned
 *                   widget will allow geometry requests from this child
 *	Arguments: widget - a child of the paned widget.
 *	Returns: none.
 *)

<* EXTERNAL XawPanedAllowResize *>
   PROCEDURE PanedAllowResize (widget: Widget; allow_resize: Boolean);


(*===============================================================
 * $XConsortium: Scrollbar.h,v 1.1 89/12/15 11:40:43 kit Exp $
 *===============================================================*)

(****************************************************************
 *
 * Scrollbar Widget
 *
 ****************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		White
 border		     BorderColor	Pixel		Black
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Function		NULL
 foreground	     Color		Pixel		Black
 height		     Height		Dimension	length or thickness
 jumpProc	     Callback		Function	NULL
 length		     Length		Dimension	1
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 orientation	     Orientation	XtOrientation	XtorientVertical
 reverseVideo	     ReverseVideo	Boolean		False
 scrollDCursor	     Cursor		Cursor		XC_sb_down_arrow
 scrollHCursor	     Cursor		Cursor		XC_sb_h_double_arrow
 scrollLCursor	     Cursor		Cursor		XC_sb_left_arrow
 scrollProc	     Callback		Function	NULL
 scrollRCursor	     Cursor		Cursor		XC_sb_right_arrow
 scrollUCursor	     Cursor		Cursor		XC_sb_up_arrow
 scrollVCursor	     Cursor		Cursor		XC_sb_v_double_arrow
 sensitive	     Sensitive		Boolean		True
 shown		     Shown		float		0.0
 thickness	     Thickness		Dimension	14
 thumb		     Thumb		Pixmap		Grey
 topOfThumb	     TopOfThumb		float		0.0
 width		     Width		Dimension	thickness or length
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* 
 * Most things we need are in StringDefs.h 
 *)

(* #define XtCMinimumThumb "MinimumThumb"                      in XtC.i3 *)
(* #define XtCShown "Shown"                                    in XtC.i3 *)
(* #define XtCTopOfThumb "TopOfThumb"                          in XtC.i3 *)

(* #define XtNminimumThumb "minimumThumb"                      in XtN.i3 *)
(* #define XtNtopOfThumb "topOfThumb"                          in XtN.i3 *)

<* EXTERNAL *> VAR
   scrollbarWidgetClass: WidgetClass;

<* EXTERNAL XawScrollbarSetThumb *>
   PROCEDURE ScrollbarSetThumb (scrollBar: Widget; top, shown: REAL);


(*===============================================================
 * $XConsortium: Simple.h,v 1.9 89/07/21 01:44:53 kit Exp $
 *===============================================================*)

(****************************************************************
 *
 * Simple widgets
 *
 ****************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 insensitiveBorder   Insensitive	Pixmap		Gray
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* #define XtNcursor "cursor"                                  in XtN.i3 *)
(* #define XtNinsensitiveBorder "insensitiveBorder"            in XtN.i3 *)
(* #define XtCInsensitive "Insensitive"                        in XtC.i3 *)

<* EXTERNAL *> VAR
   simpleWidgetClass: WidgetClass;


(*==================================================================
 * $XConsortium: SimpleMenu.h,v 1.17 89/12/11 15:01:55 kit Exp $
 *==================================================================*)

(*
 * SimpleMenu.h - Public Header file for SimpleMenu widget.
 *
 * This is the public header file for the Athena SimpleMenu widget.
 * It is intended to provide one pane pulldown and popup menus within
 * the framework of the X Toolkit.  As the name implies it is a first and
 * by no means complete implementation of menu code. It does not attempt to
 * fill the needs of all applications, but does allow a resource oriented
 * interface to menus.
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 *)

(****************************************************************
 *
 * SimpleMenu widget
 *
 ****************************************************************)

(* SimpleMenu Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 backgroundPixmap    BackgroundPixmap	Pixmap          None
 borderColor	     BorderColor	Pixel		XtDefaultForeground
 borderPixmap	     BorderPixmap	Pixmap		None
 borderWidth	     BorderWidth	Dimension	1
 bottomMargin        VerticalMargins    Dimension       VerticalSpace
 columnWidth         ColumnWidth        Dimension       Width of widest text
 cursor              Cursor             Cursor          None
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 label               Label              String          NULL (No label)
 labelClass          LabelClass         Pointer         smeBSBObjectClass
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 rowHeight           RowHeight          Dimension       Height of Font
 sensitive	     Sensitive		Boolean		True
 topMargin           VerticalMargins    Dimension       VerticalSpace
 width		     Width		Dimension	0
 x		     Position		Position	0n
 y		     Position		Position	0

*)

<* EXTERNAL *> VAR
   simpleMenuWidgetClass: WidgetClass;

(* #define XtNcursor "cursor"                                  in XtN.i3 *)
(* #define XtNbottomMargin "bottomMargin"                      in XtN.i3 *)
(* #define XtNcolumnWidth "columnWidth"                        in XtN.i3 *)
(* #define XtNlabelClass "labelClass"                          in XtN.i3 *)
(* #define XtNmenuOnScreen "menuOnScreen"                      in XtN.i3 *)
(* #define XtNpopupOnEntry "popupOnEntry"                      in XtN.i3 *)
(* #define XtNrowHeight "rowHeight"                            in XtN.i3 *)
(* #define XtNtopMargin "topMargin"                            in XtN.i3 *)

(* #define XtCColumnWidth "ColumnWidth"                        in XtC.i3 *)
(* #define XtCLabelClass "LabelClass"                          in XtC.i3 *)
(* #define XtCMenuOnScreen "MenuOnScreen"                      in XtC.i3 *)
(* #define XtCPopupOnEntry "PopupOnEntry"                      in XtC.i3 *)
(* #define XtCRowHeight "RowHeight"                            in XtC.i3 *)
(* #define XtCVerticalMargins "VerticalMargins"                in XtC.i3 *)

(************************************************************
 *
 * Public Functions.
 *
 ************************************************************)

(*	Function Name: Xaw.SimpleMenuAddGlobalActions
 *	Description: adds the global actions to the simple menu widget.
 *	Arguments: app_con - the appcontext.
 *	Returns: none.
 *)

<* EXTERNAL XawSimpleMenuAddGlobalActions *>
   PROCEDURE SimpleMenuAddGlobalActions (app_con: AppContext);
 
(*	Function Name: Xaw.SimpleMenuGetActiveEntry
 *	Description: Gets the currently active (set) entry.
 *	Arguments: w - the smw widget.
 *	Returns: the currently set entry or NULL if none is set.
 *)

<* EXTERNAL XawSimpleMenuGetActiveEntry *>
   PROCEDURE SimpleMenuGetActiveEntry (w: Widget): Widget;

(*	Function Name: Xaw.SimpleMenuClearActiveEntry
 *	Description: Unsets the currently active (set) entry.
 *	Arguments: w - the smw widget.
 *	Returns: none.
 *)

<* EXTERNAL XawSimpleMenuClearActiveEntry *>
   PROCEDURE SimpleMenuClearActiveEntry (w: Widget);


(*============================================================
 * $XConsortium: Sme.h,v 1.4 89/12/11 15:20:09 kit Exp $
 *============================================================*)

(*
 * Sme.h - Public Header file for Sme object.
 *
 * This is the public header file for the Athena Sme object.
 * It is intended to be used with the simple menu widget.  
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 *)

(****************************************************************
 *
 * Sme Object
 *
 ****************************************************************)

(* Simple Menu Entry Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 callback            Callback		Pointer		NULL
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0n
 y		     Position		Position	0

*)

<* EXTERNAL *> VAR
   smeObjectClass: WidgetClass;


(*============================================================
 * $XConsortium: SmeBSB.h,v 1.5 89/12/11 15:20:14 kit Exp $
 *============================================================*)
(*
 * SmeBSB.h - Public Header file for SmeBSB object.
 *
 * This is the public header file for the Athena BSB Sme object.
 * It is intended to be used with the simple menu widget.  This object
 * provides bitmap - string - bitmap style entries.
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 *)

(****************************************************************
 *
 * SmeBSB object
 *
 ****************************************************************)

(* BSB Menu Entry Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 callback            Callback           Callback        NULL
 destroyCallback     Callback		Pointer		NULL
 font                Font               XFontStruct *   XtDefaultFont
 foreground          Foreground         Pixel           XtDefaultForeground
 height		     Height		Dimension	0
 label               Label              String          Name of entry
 leftBitmap          LeftBitmap         Pixmap          None
 leftMargin          HorizontalMargins  Dimension       4
 rightBitmap         RightBitmap        Pixmap          None
 rightMargin         HorizontalMargins  Dimension       4
 sensitive	     Sensitive		Boolean		True
 vertSpace           VertSpace          int             25
 width		     Width		Dimension	0
 x		     Position		Position	0n
 y		     Position		Position	0

*)

<* EXTERNAL *> VAR
   smeBSBObjectClass: WidgetClass;


(*================================================================
 * $XConsortium: SmeLine.h,v 1.3 89/12/11 15:20:19 kit Exp $
 *================================================================*)
(*
 * SmeLine.h - Public Header file for SmeLine object.
 *
 * This is the public header file for the Athena SmeLine object.
 * It is intended to be used with the simple menu widget.  
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 *)

(****************************************************************
 *
 * SmeLine Object
 *
 ****************************************************************)

(* Menu Entry Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 callback            Callback		Pointer		NULL
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0n
 y		     Position		Position	0

*)

(* #define XtCLineWidth "LineWidth"                            in XtC.i3 *)
(* #define XtCStipple "Stipple"                                in XtC.i3 *)

(* #define XtNlineWidth "lineWidth"                            in XtN.i3 *)
(* #define XtNstipple "stipple"                                in XtN.i3 *)

<* EXTERNAL *> VAR
   smeLineObjectClass: WidgetClass;


(*=================================================================
 * $XConsortium: StripChart.h,v 1.2 89/08/24 11:52:00 kit Exp $
 *=================================================================*)

(***********************************************************************
 *
 * StripChart Widget
 *
 ***********************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 foreground	     Foreground		Pixel		XtDefaultForeground
 getValue	     Callback		Callback	NULL
 height		     Height		Dimension	120
 highlight	     Foreground		Pixel		Black
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 minScale	     Scale		int		1
 reverseVideo	     ReverseVideo	Boolean		False
 update		     Interval		int		5 (seconds)
 width		     Width		Dimension	120
 x		     Position		Position	0
 y		     Position		Position	0

*)

CONST
  DEFAULT_JUMP = -1;

(* #define XtCJumpScroll "JumpScroll"                          in XtC.i3 *)
(* #define XtCScale "Scale"                                    in XtC.i3 *)

(* #define XtNgetValue "getValue"                              in XtN.i3 *)
(* #define XtNhighlight "highlight"                            in XtN.i3 *)
(* #define XtNjumpScroll "jumpScroll"                          in XtN.i3 *)
(* #define XtNminScale "minScale"                              in XtN.i3 *)
(* #define XtNscale "scale"                                    in XtN.i3 *)
(* #define XtNupdate "update"                                  in XtN.i3 *)
(* #define XtNvmunix "vmunix"                                  in XtN.i3 *)
 
<* EXTERNAL *> VAR
   stripChartWidgetClass: WidgetClass;


(*===================================================================
 * $XConsortium: Template.h,v 1.4 89/07/21 01:41:49 kit Exp $ 
 *===================================================================*)

(****************************************************************
 *
 * Template widget
 *
 ****************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* define any special resource names here that are not in <X11/StringDefs.h> *)

(* #define XtNtemplateResource "templateResource"              in XtN.i3 *)

(* #define XtCTemplateResource "TemplateResource"              in XtC.i3 *)

(* declare the class constant *)

<* EXTERNAL *> VAR
   templateWidgetClass: WidgetClass;


(*=============================================================
 * $XConsortium: Text.h,v 1.32 89/10/19 15:01:11 kit Exp $
 *=============================================================*)

(****************************************************************
 *
 * Text widget
 *
 ****************************************************************)

(* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 dialogHOffset	     Margin		int		10
 dialogVOffset	     Margin		int		10
 displayCaret	     Output		Boolean		True
 displayPosition     TextPosition	int		0
 editType	     EditType		XtTextEditType	XttextRead
 height		     Height		Dimension	font height
 insertPosition	     TextPosition	int		0
 leftMargin	     Margin		Dimension	2
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 selectTypes	     SelectTypes	Pointer		(internal)
 selection	     Selection		Pointer		empty selection
 sensitive	     Sensitive		Boolean		True
 textSink	     TextSink		Pointer		(none)
 textSource	     TextSource		Pointer		(none)
 width		     Width		Dimension	100
 x		     Position		int		0
 y		     Position		int		0

*)

(* #define XtEtextScrollNever "never"                          in XtE.i3 *)
(* #define XtEtextScrollWhenNeeded "whenneeded"                in XtE.i3 *)
(* #define XtEtextScrollAlways "always"                        in XtE.i3 *)

(* #define XtEtextWrapNever "never"                            in XtE.i3 *)
(* #define XtEtextWrapLine "line"                              in XtE.i3 *)
(* #define XtEtextWrapWord "word"                              in XtE.i3 *)

(* #define XtEtextResizeNever "never"                          in XtE.i3 *)
(* #define XtEtextResizeWidth "width"                          in XtE.i3 *)
(* #define XtEtextResizeHeight "height"                        in XtE.i3 *)
(* #define XtEtextResizeBoth "both"                            in XtE.i3 *)

(* #define XtNautoFill "autoFill"                              in XtN.i3 *)
(* #define XtNbottomMargin "bottomMargin"                      in XtN.i3 *)
(* #define XtNdialogHOffset "dialogHOffset"                    in XtN.i3 *)
(* #define XtNdialogVOffset "dialogVOffset"                    in XtN.i3 *)
(* #define XtNdisplayCaret "displayCaret"                      in XtN.i3 *)
(* #define XtNdisplayPosition "displayPosition"                in XtN.i3 *)
(* #define XtNinsertPosition "insertPosition"                  in XtN.i3 *)
(* #define XtNleftMargin "leftMargin"                          in XtN.i3 *)
(* #define XtNresize "resize"                                  in XtN.i3 *)
(* #define XtNrightMargin "rightMargin"                        in XtN.i3 *)
(* #define XtNscrollVertical "scrollVertical"                  in XtN.i3 *)
(* #define XtNscrollHorizontal "scrollHorizontal"              in XtN.i3 *)
(* #define XtNselectTypes "selectTypes"                        in XtN.i3 *)
(* #define XtNselection "selection"                            in XtN.i3 *)
(* #define XtNtopMargin "topMargin"                            in XtN.i3 *)
(* #define XtNwrap "wrap"                                      in XtN.i3 *)

(* #define XtCAutoFill "AutoFill"                              in XtC.i3 *)
(* #define XtCResize "Resize"                                  in XtC.i3 *)
(* #define XtCScroll "Scroll"                                  in XtC.i3 *)
(* #define XtCSelectTypes "SelectTypes"                        in XtC.i3 *)
(* #define XtCWrap "Wrap"                                      in XtC.i3 *)

(* Return Error code for XawTextSearch *)

VAR
  TextSearchError     := -12345;

(* Return codes from XawTextReplace *)

CONST
  EditDone           = 0;
  EditError          = 1;
  PositionError      = 2;

<* EXTERNAL *> 
   VAR FMT8BIT: Atom;

(* Class record constants *)

<* EXTERNAL *> 
   VAR textWidgetClass: WidgetClass;

(* other stuff *)

TYPE
  TextScrollMode	= Enumeration;
CONST
  textScrollNever	= 0;
  textScrollWhenNeeded	= 1;
  textScrollAlways	= 3;

TYPE
  TextWrapMode		= Enumeration;
CONST
  textWrapNever		= 0;
  textWrapLine		= 1;
  textWrapWord		= 2;

TYPE
  TextResizeMode	= Enumeration;
CONST
  textResizeNever	= 0;
  textResizeWidth	= 1;
  textResizeHeight	= 3;
  textResizeBoth	= 4;

TYPE
  TextScanDirection	= Enumeration;
CONST
  sdLeft		= 0;
  sdRight		= 1;

TYPE
  TextEditType		= Enumeration;
CONST
  textRead		= 0;
  textAppend		= 1;
  textEdit		= 2;

TYPE
  TextSelectType	= Enumeration;
CONST
  selectNull		= 0;
  selectPosition	= 1;
  selectChar		= 2;
  selectWord		= 3;
  selectLine		= 4;
  selectParagraph	= 5;
  selectAll		= 6;

TYPE
  TextSelectTypeStar =  UNTRACED REF TextSelectType;

  TextBlock          =  RECORD
				firstPos: INTEGER;
				length: INTEGER;
				ptr: String;
				format: Atom;
			END;
  TextBlockStar      =  UNTRACED REF TextBlock;

<* EXTERNAL XawTextDisplay *>
   PROCEDURE TextDisplay (w: Widget);

<* EXTERNAL XawTextEnableRedisplay *>
   PROCEDURE TextEnableRedisplay (w: Widget);

<* EXTERNAL XawTextDisableRedisplay *>
   PROCEDURE TextDisableRedisplay (w: Widget);

<* EXTERNAL XawTextSetSelectionArray *>
   PROCEDURE TextSetSelectionArray (w: Widget; sarray: TextSelectTypeStar);

<* EXTERNAL XawTextGetSelectionPos *>
   PROCEDURE TextGetSelectionPos (w: Widget; left, right: TextPositionStar);

<* EXTERNAL XawTextSetSource *>
   PROCEDURE TextSetSource (w, source: Widget; startPos: TextPosition);

<* EXTERNAL XawTextReplace *>
   PROCEDURE TextReplace (w: Widget; startPos, endPos: TextPosition; text: TextBlockStar): INTEGER;

<* EXTERNAL XawTextTopPosition *>
   PROCEDURE TextTopPosition (w: Widget): TextPosition;

<* EXTERNAL XawTextSetInsertionPoint *>
   PROCEDURE TextSetInsertionPoint (w: Widget; position: TextPosition);

<* EXTERNAL XawTextGetInsertionPoint *>
   PROCEDURE TextGetInsertionPoint (w: Widget): TextPosition;

<* EXTERNAL XawTextUnsetSelection *>
   PROCEDURE TextUnsetSelection (w: Widget);

<* EXTERNAL XawTextSetSelection *>
   PROCEDURE TextSetSelection (w: Widget; left, right: TextPosition);

<* EXTERNAL XawTextInvalidate *>
   PROCEDURE TextInvalidate (w: Widget; from, to: TextPosition);

<* EXTERNAL XawTextGetSource *>
   PROCEDURE TextGetSource (w: Widget): Widget;

<* EXTERNAL XawTextSearch *>
   PROCEDURE TextSearch (w: Widget; dir: TextScanDirection; 
                         text: TextBlockStar): TextPosition;


(*==============================================================
 * $XConsortium: TextSink.h,v 1.5 89/11/01 17:28:26 kit Exp $
 *==============================================================*)

(***********************************************************************
 *
 * TextSink Object
 *
 ***********************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 font                Font            XFontStruct *      XtDefaultFont
 foreground          Foreground      Pixel              XtDefaultForeground
 background          Background      Pixel              XtDefaultBackground

*)
 
(* Class record constants *)

<* EXTERNAL *> VAR
   textSinkObjectClass: WidgetClass;

TYPE
  TextInsertState = Enumeration;
CONST
  isOn		= 0;
  isOff		= 1;

(************************************************************
 *
 * Public Functions.
 *
 ************************************************************)

(*	Function Name: Xaw.TextSinkDisplayText
 *	Description: Stub function that in subclasses will display text. 
 *	Arguments: w - the TextSink Object.
 *                 x, y - location to start drawing text.
 *                 pos1, pos2 - location of starting and ending points
 *                              in the text buffer.
 *                 highlight - hightlight this text?
 *	Returns: none.
 *
 * This function doesn't actually display anything, it is only a place
 * holder.
 *)

<* EXTERNAL XawTextSinkDisplayText *>
   PROCEDURE TextSinkDisplayText (w: Widget; 
                                  x, y: Position; 
                                  pos1, pos2: TextPosition; 
                                  highlight: Boolean);

(*	Function Name: Xaw.TextSinkInsertCursor
 *	Description: Places the InsertCursor.
 *	Arguments: w - the TextSink Object.
 *                 x, y - location for the cursor.
 *                 staye - whether to turn the cursor on, or off.
 *	Returns: none.
 *
 * This function doesn't actually display anything, it is only a place
 * holder.
 *)

<* EXTERNAL XawTextSinkInsertCursor *>
   PROCEDURE TextSinkInsertCursor (w: Widget; 
                                   x, y: Position; 
                                   state: TextInsertState);

(*	Function Name: Xaw.TextSinkClearToBackground
 *	Description: Clears a region of the sink to the background color.
 *	Arguments: w - the TextSink Object.
 *                 x, y  - location of area to clear.
 *                 width, height - size of area to clear
 *	Returns: void.
 *
 * This function doesn't actually display anything, it is only a place
 * holder.
 *)

<* EXTERNAL XawTextSinkClearToBackground  *>
   PROCEDURE TextSinkClearToBackground  (w: Widget; 
                                         x, y: Position; 
                                         width, height: Dimension);

(*	Function Name: Xaw.TextSinkFindPosition
 *	Description: Finds a position in the text.
 *	Arguments: w - the TextSink Object.
 *                 fromPos - reference position.
 *                 fromX   - reference location.
 *                 width,  - width of section to paint text.
 *                 stopAtWordBreak - returned position is a word break?
 *                 resPos - Position to return.      *** RETURNED ***
 *                 resWidth - Width actually used.   *** RETURNED ***
 *                 resHeight - Height actually used. *** RETURNED ***
 *	Returns: none (see above).
 *)

<* EXTERNAL XawTextSinkFindPosition *>
   PROCEDURE TextSinkFindPosition (w: Widget; 
                                   fromPos: TextPosition; 
                                   fromx, width: INTEGER;
                                   stopAtWordBreak: Boolean; 
                                   resPos: TextPositionStar;
                                   resWidth, resHeight: int_star);

(*	Function Name: Xaw.TextSinkFindDistance
 *	Description: Find the Pixel Distance between two text Positions.
 *	Arguments: w - the TextSink Object.
 *                 fromPos - starting Position.
 *                 fromX   - x location of starting Position.
 *                 toPos   - end Position.
 *                 resWidth - Distance between fromPos and toPos.
 *                 resPos   - Acutal toPos used.
 *                 resHeight - Height required by this text.
 *	Returns: none.
 *)

<* EXTERNAL XawTextSinkFindDistance  *>
   PROCEDURE TextSinkFindDistance  (w: Widget; 
                                    fromPos: TextPosition; 
                                    fromx: INTEGER;
				    toPos: TextPosition; 
                                    resPos: TextPositionStar; 
                                    resHeight: int_star);

(*	Function Name: Xaw.TextSinkResolve
 *	Description: Resloves a location to a position.
 *	Arguments: w - the TextSink Object.
 *                 pos - a reference Position.
 *                 fromx - a reference Location.
 *                 width - width to move.
 *                 resPos - the resulting position.
 *	Returns: none
 *)

<* EXTERNAL XawTextSinkResolve *>
   PROCEDURE TextSinkResolve (w: Widget; 
                              pos: TextPosition; 
                              fromx, width: INTEGER;
                              resPos: TextPositionStar);

(*	Function Name: Xaw.TextSinkMaxLines
 *	Description: Finds the Maximum number of lines that will fit in
 *                   a given height.
 *	Arguments: w - the TextSink Object.
 *                 height - height to fit lines into.
 *	Returns: the number of lines that will fit.
 *)

<* EXTERNAL XawTextSinkMaxLines *>
   PROCEDURE TextSinkMaxLines (w: Widget; height: Dimension): INTEGER;

(*	Function Name: Xaw.TextSinkMaxHeight
 *	Description: Finds the Minium height that will contain a given number 
 *                   lines.
 *	Arguments: w - the TextSink Object.
 *                 lines - the number of lines.
 *	Returns: the height.
 *)

<* EXTERNAL XawTextSinkMaxHeight *>
   PROCEDURE TextSinkMaxHeight (w: Widget; lines: INTEGER): INTEGER;

(*	Function Name: Xaw.TextSinkSetTabs
 *	Description: Sets the Tab stops.
 *	Arguments: w - the TextSink Object.
 *                 tab_count - the number of tabs in the list.
 *                 tabs - the text positions of the tabs.
 *	Returns: none
 *)

<* EXTERNAL XawTextSinkSetTabs *>
   PROCEDURE TextSinkSetTabs (w: Widget; tab_count: INTEGER; tabs: int_star);
						  
(*	Function Name: Xaw.TextSinkGetCursorBounds
 *	Description: Finds the bounding box for the insert curor (caret).
 *	Arguments: w - the TextSinkObject.
 *                 rect - an X rectance containing the cursor bounds.
 *	Returns: none (fills in rect).
 *)

<* EXTERNAL XawTextSinkGetCursorBounds *>
   PROCEDURE TextSinkGetCursorBounds (w: Widget; rect: XRectangleStar);


(*=================================================================
 * $XConsortium: TextSrc.h,v 1.4 89/10/31 17:12:42 kit Exp $
 *=================================================================*)

(***********************************************************************
 *
 * TextSrc Object
 *
 ***********************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 editType	     EditType		XawTextEditType	XawtextRead

*)
 
(* Class record constants *)

<* EXTERNAL *> VAR
   textSrcObjectClass: WidgetClass;

TYPE
  TextScanType		= Enumeration;
CONST
  stPositions		= 0;
  stWhiteSpace		= 1;
  stEOL			= 2;
  stParagraph		= 3;
  stAll			= 4;

TYPE
  highlightType		= Enumeration;
CONST
  Normal		= 0;
  Selected		= 1;

TYPE
  TextSelectionMode	= Enumeration;
CONST
  smTextSelect		= 0;
  smTextExtend		= 1;

TYPE
  TextSelectionAction	= Enumeration;
CONST
  actionStart		= 0;
  actionAdjust		= 1;
  actionEnd		= 2;

TYPE
  TextPosition        = long;
  TextPositionStar    = UNTRACED REF TextPosition;

(*
 * Error Conditions:
 *)

CONST
  TextReadError = -1;
  TextScanError = -1;

(************************************************************
 *
 * Public Functions.
 *
 ************************************************************)

(*	Function Name: Xaw.TextSourceRead
 *	Description: This function reads the source.
 *	Arguments: w - the TextSrc Object.
 *                 pos - position of the text to retreive.
 * RETURNED        text - text block that will contain returned text.
 *                 length - maximum number of characters to read.
 *	Returns: The number of characters read into the buffer.
 *)

<* EXTERNAL XawTextSourceRead *>
   PROCEDURE TextSourceRead (w: Widget; pos: TextPosition; 
                             text: TextBlockStar; 
                             length: INTEGER): TextPosition;

(*	Function Name: Xaw.TextSourceReplace.
 *	Description: Replaces a block of text with new text.
 *	Arguments: src - the Text Source Object.
 *                 startPos, endPos - ends of text that will be removed.
 *                 text - new text to be inserted into buffer at startPos.
 *	Returns: XawEditError or XawEditDone.
 *)

<* EXTERNAL XawTextSourceReplace  *>
   PROCEDURE TextSourceReplace  (w: Widget; startPos, endPos: TextPosition;
                                 text: TextBlockStar): INTEGER;

(*	Function Name: Xaw.TextSourceScan
 *	Description: Scans the text source for the number and type
 *                   of item specified.
 *	Arguments: w - the TextSrc Object.
 *                 position - the position to start scanning.
 *                 type - type of thing to scan for.
 *                 dir - direction to scan.
 *                 count - which occurance if this thing to search for.
 *                 include - whether or not to include the character found in
 *                           the position that is returned. 
 *	Returns: The position of the text.
 *
 *)

<* EXTERNAL XawTextSourceScan *>
   PROCEDURE TextSourceScan (w: Widget; position: TextPosition; 
                             type: TextScanType;
                             dir: TextScanDirection; count: INTEGER; 
                             include: Boolean): TextPosition;

(*	Function Name: Xaw.TextSourceSearch
 *	Description: Searchs the text source for the text block passed
 *	Arguments: w - the TextSource Object.
 *                 position - the position to start scanning.
 *                 dir - direction to scan.
 *                 text - the text block to search for.
 *	Returns: The position of the text we are searching for or
 *               XawTextSearchError.
 *)

<* EXTERNAL XawTextSourceSearch *>
   PROCEDURE TextSourceSearch (w: Widget;  position: TextPosition; 
                               dir: TextScanDirection; 
                               text: TextBlockStar): TextPosition;

(*	Function Name: Xaw.TextSourceConvertSelection
 *	Description: Dummy selection converter.
 *	Arguments: w - the TextSrc object.
 *                 selection - the current selection atom.
 *                 target    - the current target atom.
 *                 type      - the type to conver the selection to.
 * RETURNED        value, length - the return value that has been converted.
 * RETURNED        format    - the format of the returned value.
 *	Returns: TRUE if the selection has been converted.
 *
 *)

<* EXTERNAL XawTextSourceConvertSelection *>
   PROCEDURE TextSourceConvertSelection (w: Widget; 
                                         selection, target, type: AtomStar;
                                         value: ADDRESS; 
                                         length: unsigned_long_star;
                                         format: int_star): Boolean;

(*	Function Name: Xaw.TextSourceSetSelection
 *	Description: allows special setting of the selection.
 *	Arguments: w - the TextSrc object.
 *                 left, right - bounds of the selection.
 *                 selection - the selection atom.
 *	Returns: none
 *)

<* EXTERNAL XawTextSourceSetSelection *>
   PROCEDURE TextSourceSetSelection (w: Widget; 
                                     left, right: TextPosition; 
                                     selection: Atom);


(*==============================================================
 * $XConsortium: Toggle.h,v 1.7 89/12/11 15:23:02 kit Exp $
 *==============================================================*)

(***********************************************************************
 *
 * Toggle Widget
 *
 ***********************************************************************)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 radioGroup          RadioGroup         Widget          NULL              +
 radioData           RadioData          Pointer         (caddr_t) Widget  ++
 state               State              Boolean         Off

 background	     Background		Pixel		XtDefaultBackground
 bitmap		     Pixmap		Pixmap		None
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 callback	     Callback		Pointer		NULL
 cursor		     Cursor		Cursor		None
 destroyCallback     Callback		Pointer		NULL
 font		     Font		XFontStructx*	XtDefaultFont
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	text height
 highlightThickness  Thickness		Dimension	2
 insensitiveBorder   Insensitive	Pixmap		Gray
 internalHeight	     Height		Dimension	2
 internalWidth	     Width		Dimension	4
 justify	     Justify		XtJustify	XtJustifyCenter
 label		     Label		String		NULL
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 resize		     Resize		Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	text width
 x		     Position		Position	0
 y		     Position		Position	0

+ To use the toggle as a radio toggle button, set this resource to point to
  any other widget in the radio group.

++ This is the data returned from a call to XtToggleGetCurrent, by default
   this is set to the name of toggle widget.

*)

(*
 * These should be in StringDefs.h but aren't so we will define
 * them here if they are needed.
 *)

(* #define XtCWidget "Widget"                                  in XtC.i3 *)
(* #define XtCState "State"                                    in XtC.i3 *)
(* #define XtCRadioGroup "RadioGroup"                          in XtC.i3 *)
(* #define XtCRadioData "RadioData"                            in XtC.i3 *)

(* #define XtRWidget "Widget"                                  in XtR.i3 *)

(* #define XtNstate "state"                                    in XtN.i3 *)
(* #define XtNradioGroup "radioGroup"                          in XtN.i3 *)
(* #define XtNradioData "radioData"                            in XtN.i3 *)

<* EXTERNAL *> VAR
   toggleWidgetClass: WidgetClass;

(************************************************************
 * 
 * Public Functions
 *
 ************************************************************)
   
(*	Function Name: Xaw.ToggleChangeRadioGroup
 *	Description: Allows a toggle widget to change radio lists.
 *	Arguments: w - The toggle widget to change lists.
 *                 radio_group - any widget in the new list.
 *	Returns: none.
 *)

<* EXTERNAL XawToggleChangeRadioGroup *>
   PROCEDURE ToggleChangeRadioGroup (w: Widget; radio_group: Widget);

(*	Function Name: Xaw.ToggleGetCurrent
 *	Description: Returns the RadioData associated with the toggle
 *                   widget that is currently active in a toggle list.
 *	Arguments: radio_group - any toggle widget in the toggle list.
 *	Returns: The XtNradioData associated with the toggle widget.
 *)

<* EXTERNAL XawToggleGetCurrent *>
   PROCEDURE ToggleGetCurrent (radio_group: Widget): ADDRESS;

(*	Function Name: Xaw.ToggleSetCurrent
 *	Description: Sets the Toggle widget associated with the
 *                   radio_data specified.
 *	Arguments: radio_group - any toggle widget in the toggle list.
 *                 radio_data - radio data of the toggle widget to set.
 *	Returns: none.
 *)

<* EXTERNAL XawToggleSetCurrent *>
   PROCEDURE ToggleSetCurrent (radio_group: Widget; radio_data: ADDRESS);
 
(*	Function Name: Xaw.ToggleUnsetCurrent
 *	Description: Unsets all Toggles in the radio_group specified.
 *	Arguments: radio_group - any toggle widget in the toggle list.
 *	Returns: none.
 *)

<* EXTERNAL XawToggleUnsetCurrent *>
   PROCEDURE ToggleUnsetCurrent (radio_group: Widget);


(*==================================================================
 * $XConsortium: Viewport.h,v 1.11 89/07/21 01:55:47 kit Exp $
 *==================================================================*)

(* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 allowHoriz	     Boolean		Boolean		False
 allowVert	     Boolean		Boolean		False
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 foreceBars	     Boolean		Boolean		False
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 useBottom	     Boolean		Boolean		False
 useRight	     Boolean		Boolean		False
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*)

(* fields added to Form *)

(* #define XtNforceBars "forceBars"                            in XtN.i3 *)
(* #define XtNallowHoriz "allowHoriz"                          in XtN.i3 *)
(* #define XtNallowVert "allowVert"                            in XtN.i3 *)
(* #define XtNuseBottom "useBottom"                            in XtN.i3 *)
(* #define XtNuseRight "useRight"                              in XtN.i3 *)

<* EXTERNAL *> VAR
   viewportWidgetClass: WidgetClass;


(*===============================================================
 * $XConsortium: XawInit.h,v 1.1 89/10/09 14:59:48 jim Exp $
 *===============================================================*)

<* EXTERNAL XawInitializeWidgetSet *>
   PROCEDURE InitializeWidgetSet ();



END Xaw.
