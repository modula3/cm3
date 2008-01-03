(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 08:31:25 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE INTERFACE Xm;
(*Generated from Xm.h by ctom3 on 7/26/93*)
(*then finished by hand*)

FROM Ctypes IMPORT char, char_star, int_star, long, unsigned_char;

IMPORT Word,X,Xt,Xatom,Xrm;

(*
IMPORT X11/intrinsic;
IMPORT X11/Vendor;
IMPORT Xm/VirtKeys;
*)


VAR   SCCSID:= "OSF/Motif: Xm.h	3.67 91/01/10"; 
(******************************************************************************
*******************************************************************************
*
*  (c) Copyright 1989, 1990, 1991 OPEN SOFTWARE FOUNDATION, INC.
*  (c) Copyright 1987, 1988, 1989, 1990, HEWLETT-PACKARD COMPANY
*  ALL RIGHTS RESERVED
*  
*  	THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED
*  AND COPIED ONLY IN ACCORDANCE WITH THE TERMS OF SUCH LICENSE AND
*  WITH THE INCLUSION OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE OR
*  ANY OTHER COPIES THEREOF MAY NOT BE PROVIDED OR OTHERWISE MADE
*  AVAILABLE TO ANY OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF THE
*  SOFTWARE IS HEREBY TRANSFERRED.
*  
*  	THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT
*  NOTICE AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY OPEN SOFTWARE
*  FOUNDATION, INC. OR ITS THIRD PARTY SUPPLIERS  
*  
*  	OPEN SOFTWARE FOUNDATION, INC. AND ITS THIRD PARTY SUPPLIERS,
*  ASSUME NO RESPONSIBILITY FOR THE USE OR INABILITY TO USE ANY OF ITS
*  SOFTWARE .   OSF SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
*  KIND, AND OSF EXPRESSLY DISCLAIMS ALL IMPLIED WARRANTIES, INCLUDING
*  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE.
*  
*  Notice:  Notwithstanding any other lease or license that may pertain to,
*  or accompany the delivery of, this computer software, the rights of the
*  Government regarding its use, reproduction and disclosure are as set
*  forth in Section 52.227-19 of the FARS Computer Software-Restricted
*  Rights clause.
*  
*  (c) Copyright 1989, 1990, 1991 Open Software Foundation, Inc.  Unpublished - all
*  rights reserved under the Copyright laws of the United States.
*  
*  RESTRICTED RIGHTS NOTICE:  Use, duplication, or disclosure by the
*  Government is subject to the restrictions as set forth in subparagraph
*  (c)(1)(ii) of the Rights in Technical Data and Computer Software clause
*  at DFARS 52.227-7013.
*  
*  Open Software Foundation, Inc.
*  11 Cambridge Center
*  Cambridge, MA   02142
*  (617)621-8700
*  
*  RESTRICTED RIGHTS LEGEND:  This computer software is submitted with
*  "restricted rights."  Use, duplication or disclosure is subject to the
*  restrictions as set forth in NASA FAR SUP 18-52.227-79 (April 1985)
*  "Commercial Computer Software- Restricted Rights (April 1985)."  Open
*  Software Foundation, Inc., 11 Cambridge Center, Cambridge, MA  02142.  If
*  the contract contains the Clause at 18-52.227-74 "Rights in Data General"
*  then the "Alternate III" clause applies.
*  
*  (c) Copyright 1989, 1990, 1991 Open Software Foundation, Inc.
*  ALL RIGHTS RESERVED 
*  
*  
* Open Software Foundation is a trademark of The Open Software Foundation, Inc.
* OSF is a trademark of Open Software Foundation, Inc.
* OSF/Motif is a trademark of Open Software Foundation, Inc.
* Motif is a trademark of Open Software Foundation, Inc.
* DEC is a registered trademark of Digital Equipment Corporation
* DIGITAL is a registered trademark of Digital Equipment Corporation
* X. Window System is a trademark of the Massachusetts Institute of Technology
*
*******************************************************************************
******************************************************************************)


CONST VERSION = 1;
CONST REVISION = 1;


PROCEDURE Version():INTEGER; 


VAR   xmUseVersion : INTEGER ; 

TYPE Cardinal = INTEGER;

(* define used to denote an unspecified pixmap  *)

CONST UNSPECIFIED_PIXMAP = 2;

(* define charset constants *)
VAR   STRING_ISO8859_1:char_star;
VAR   STRING_OS_CHARSET:char_star;
VAR   FALLBACK_CHARSET:char_star;

(****************
 *
 * XmString structure defines. These must be here (at the start of the file) 
 * becaused they are used later on.
 *
 ****************)

CONST STRING_DIRECTION_L_TO_R = 0;
CONST STRING_DIRECTION_R_TO_L = 1;
CONST STRING_DIRECTION_DEFAULT = 255;

TYPE String = char_star; 		(* opaque to outside *)
TYPE StringTable = UNTRACED REF  String; 		(* opaque to outside *)
TYPE StringcharSet = UNTRACED REF  char; 	(* Null term string *)
TYPE StringComponentType =  unsigned_char; 	(* component tags *)
TYPE StringDirection =  unsigned_char; 

TYPE FontList = UNTRACED ROOT;        (* opaque to outside *)
TYPE uStringContext = UNTRACED ROOT;  (* opaque to outside *)
TYPE uString = UNTRACED ROOT;         (* opaque to outside *)
TYPE StringContext = UNTRACED ROOT;   (* opaque to outside *)
TYPE FontContext = UNTRACED ROOT;     (* opaque to outside *)

CONST STRING_COMPONENT_UNKNOWN = 0;
CONST STRING_COMPONENT_CHARSET = 1;
CONST STRING_COMPONENT_TEXT = 2;
CONST STRING_COMPONENT_DIRECTION = 3;
CONST STRING_COMPONENT_SEPARATOR = 4	(* 5-125 reserved *);

CONST STRING_COMPONENT_END = 126	(* no more comp in string *);

CONST STRING_COMPONENT_USER_BEGIN = 128	(* 128-255 are user tags *);
CONST STRING_COMPONENT_USER_END = 255;
VAR   STRING_DEFAULT_CHARSET:char_star;


(************************************************************************
 *  Resource names
 *	Taken from StringDefs, with Xt. replaced by Xm.
 ************************************************************************)


(* Class types *) 



(* Representation types *)

VAR   RAcceleratorTable:char_star;
VAR   RAlignment:char_star;
VAR   RXmBackgroundPixmap:char_star;
VAR   RBool:char_star;
VAR   RBoolean:char_star;
VAR   RButtonType:char_star;
VAR   RCallback:char_star;
VAR   RCallbackProc:char_star;
VAR   RCallProc:char_star;
VAR   Rchar:char_star;
VAR   RcharSetTable:char_star;
VAR   RColor:char_star;
VAR   RCommandWindowLocation:char_star;
VAR   RCompoundText:char_star;
VAR   RCursor:char_star;
VAR   RDimension:char_star;
VAR   RDisplay:char_star;
VAR   REditMode:char_star;
VAR   RFile:char_star;
VAR   RFont:char_star;
VAR   RFontList:char_star;
VAR   RFontStruct:char_star;
VAR   RFunction:char_star;
VAR   RGeometry:char_star;
VAR   RHorizontalDimension:char_star;
VAR   RHorizontalPosition:char_star;
VAR   RImmediate:char_star;
VAR   RIndicatorType:char_star;
VAR   Rint:char_star;
VAR   RJustify:char_star;
VAR   RKeySym:char_star;
VAR   RKeySymTable:char_star;
VAR   RLabelType:char_star;
VAR   RMenuWidget:char_star;
VAR   RMnemonic:char_star;
VAR   RNavigationType:char_star;
VAR   ROrientation:char_star;
VAR   RPacking:char_star;
VAR   RPixel:char_star;
VAR   RPixmap:char_star;
VAR   RGadgetPixmap:char_star;
VAR   RPointer:char_star;
VAR   RPosition:char_star;
VAR   RProc:char_star;
VAR   RRowColumnType:char_star;
VAR   Rshort:char_star;
VAR   RString:char_star;
VAR   RStringDirection:char_star;
VAR   RStringTable:char_star;
VAR   RTextPosition:char_star;
VAR   Runsigned_char:char_star;
VAR   RVerticalDimension:char_star;
VAR   RVerticalPosition:char_star;
VAR   RTranslationTable:char_star;
VAR   RTraversalType:char_star;
VAR   RWhichButton:char_star;
VAR   RWidget:char_star;
VAR   RWidgetList:char_star;
VAR   RWidgetClass:char_star;
VAR   RWindow:char_star;
VAR   RXmString:char_star;
VAR   RXmStringTable:char_star;
VAR   RXmStringcharSet:char_star;

(************************************************************************
 *  Resource names
 *	Taken from Shell.h, Xt. replaced by Xm
 ************************************************************************)

(****************
 *
 * New R4 psudo defines
 *
 ****************)

(************************************************************************
 *  Include VendorE.h in order to get it's resources, so a caller
 *  doesn't have to include it explicitly
 ************************************************************************)

CONST EXPLICIT = 0;
CONST POINTER = 1;


(************************************************************************
 *
 *  Base widget class and record definitions.
 *	Included are the definitions for XmPrimitive, XmManager,
 *      and XmGadget.
 *
 ************************************************************************)

(*  Primitive widget class and record definitions  *)

VAR   xmPrimitiveWidgetClass : Xt.WidgetClass ; 

TYPE PrimitiveWidgetClass = UNTRACED ROOT; 
TYPE PrimitiveWidget = UNTRACED ROOT; 


(*  Gadget widget class and record definitions  *)

VAR   xmGadgetClass : Xt.WidgetClass ; 

TYPE GadgetClass = UNTRACED ROOT; 
TYPE Gadget = UNTRACED ROOT; 


(*  Manager widger class and record definitions  *)


VAR   xmManagerWidgetClass : Xt.WidgetClass ; 

TYPE ManagerWidgetClass = UNTRACED ROOT; 
TYPE ManagerWidget = UNTRACED ROOT; 


(************************************************************************
 *  Fast subclassing -- just do Xt.IsSubclass now.  To be replaced.
 ************************************************************************)

CONST 
  XmIsPrimitive=TRUE;
  XmIsGadget =TRUE;
  XmIsManager = TRUE;


(************************************************************************
 *  Primitive Resources and define values
 ************************************************************************)


VAR   RPrimForegroundPixmap:char_star;
VAR   RManForegroundPixmap:char_star;

VAR   RBackgroundPixmap:char_star;



VAR   RSizePolicy:char_star;



VAR   RPrimHighlightPixmap:char_star;



VAR   RPrimTopShadowPixmap:char_star;


VAR   RPrimBottomShadowPixmap:char_star;

VAR   RUnitType:char_star;



(* size policy values  *)

CONST CHANGE_ALL = 0;
CONST CHANGE_NONE = 1;
CONST CHANGE_WIDTH = 2;
CONST CHANGE_HEIGHT = 3;


(*  unit type values  *)
(*NOTE: Used leading "u" to make valid symbols in m3*)
CONST PIXELS = 0;
CONST u100TH_MILLIMETERS = 1;
CONST u1000TH_INCHES = 2;
CONST u100TH_POINTS = 3;
CONST u100TH_FONT_UNITS = 4;


(************************************************************************
 *  Navigation defines 
 ************************************************************************)
CONST NONE = 0;
CONST TAB_GROUP = 1;
CONST STICKY_TAB_GROUP = 2;
CONST EXCLUSIVE_TAB_GROUP = 3;
CONST DYNAMIC_DEFAULT_TAB_GROUP = 255;

(************************************************************************
 *  Manager Resources and define values
 ************************************************************************)

VAR   RManTopShadowPixmap:char_star;
VAR   RManBottomShadowPixmap:char_star;
VAR   RManHighlightPixmap:char_star;




(************************************************************************
 *  Menu defines
 ************************************************************************)

CONST NO_ORIENTATION = 0;
CONST VERTICAL = 1;
CONST HORIZONTAL = 2;

CONST WORK_AREA = 0;
CONST MENU_BAR = 1;
CONST MENU_PULLDOWN = 2;
CONST MENU_POPUP = 3;
CONST MENU_OPTION = 4;

CONST NO_PACKING = 0;
CONST PACK_TIGHT = 1;
CONST PACK_COLUMN = 2;
CONST PACK_NONE = 3;


(************************************************************************
 *  Label defines
 ************************************************************************)

CONST ALIGNMENT_BEGINNING = 0;
CONST ALIGNMENT_CENTER = 1;
CONST ALIGNMENT_END = 2;


(************************************************************************
 *  ToggleButton  defines
 ************************************************************************)

CONST N_OF_MANY = 1;
CONST ONE_OF_MANY = 2;


(************************************************************************
 *  Form resources and defines
 ************************************************************************)

(*  Form resources  *)




VAR   RResizePolicy:char_star;


(*  Form constraint resources  *)

VAR   RAttachment:char_star;





(*  Form defines  *)

CONST ATTACH_NONE = 0;
CONST ATTACH_FORM = 1;
CONST ATTACH_OPPOSITE_FORM = 2;
CONST ATTACH_WIDGET = 3;
CONST ATTACH_OPPOSITE_WIDGET = 4;
CONST ATTACH_POSITION = 5;
CONST ATTACH_SELF = 6;

CONST RESIZE_NONE = 0;
CONST RESIZE_GROW = 1;
CONST RESIZE_ANY = 2	(*  for BulletinBoard, DrawingArea  *) ;



(****************************************************************************
 *  Callback reasons 
 ****************************************************************************)

CONST CR_NONE = 0;
CONST CR_HELP = 1;
CONST CR_VALUE_CHANGED = 2;
CONST CR_INCREMENT = 3;
CONST CR_DECREMENT = 4;
CONST CR_PAGE_INCREMENT = 5;
CONST CR_PAGE_DECREMENT = 6;
CONST CR_TO_TOP = 7;
CONST CR_TO_BOTTOM = 8;
CONST CR_DRAG = 9;
CONST CR_ACTIVATE = 10;
CONST CR_ARM = 11;
CONST CR_DISARM = 12;
CONST CR_MAP = 16;
CONST CR_UNMAP = 17;
CONST CR_FOCUS = 18;
CONST CR_LOSING_FOCUS = 19;
CONST CR_MODIFYING_TEXT_VALUE = 20;
CONST CR_MOVING_INSERT_CURSOR = 21;
CONST CR_EXECUTE = 22;
CONST CR_SINGLE_SELECT = 23;
CONST CR_MULTIPLE_SELECT = 24;
CONST CR_EXTENDED_SELECT = 25;
CONST CR_BROWSE_SELECT = 26;
CONST CR_DEFAULT_ACTION = 27;
CONST CR_CLIPBOARD_DATA_REQUEST = 28;
CONST CR_CLIPBOARD_DATA_DELETE = 29;
CONST CR_CASCADING = 30;
CONST CR_OK = 31;
CONST CR_CANCEL = 32;
CONST CR_APPLY = 34;
CONST CR_NO_MATCH = 35;
CONST CR_COMMAND_ENTERED = 36;
CONST CR_COMMAND_CHANGED = 37;
CONST CR_EXPOSE = 38;
CONST CR_RESIZE = 39;
CONST CR_INPUT = 40;
CONST CR_GAIN_PRIMARY = 41;
CONST CR_LOSE_PRIMARY = 42;


(************************************************************************
 *  Callback structures 
 ************************************************************************)

TYPE  AnyCallbackStruct = 
  RECORD
         reason : INTEGER ; 
         event : X.XEventStar ; 
  END; (* AnyCallbackStruct*)

TYPE  ArrowButtonCallbackStruct = 
  RECORD
         reason : INTEGER ; 
         event : X.XEventStar ; 
    	 click_count : INTEGER ; 
  END; (* ArrowButtonCallbackStruct*)

TYPE  DrawingAreaCallbackStruct = 
  RECORD
         reason : INTEGER ; 
         event : X.XEventStar ; 
         window : X.Window ; 
  END; (* DrawingAreaCallbackStruct*)

TYPE  DrawnButtonCallbackStruct = 
  RECORD
         reason : INTEGER ; 
         event : X.XEventStar ; 
         window : X.Window ; 
    	 click_count : INTEGER ; 
  END; (* DrawnButtonCallbackStruct*)

TYPE  PushButtonCallbackStruct = 
  RECORD
         reason : INTEGER ; 
         event : X.XEventStar ; 
    	 click_count : INTEGER ; 
  END; (* PushButtonCallbackStruct*)

TYPE  RowColumnCallbackStruct = 
  RECORD
         reason : INTEGER ; 
         event : X.XEventStar ; 
         widget : Xt.Widget ; 
         data : char_star ; 
         callbackstruct : char_star ; 
  END; (* RowColumnCallbackStruct*)

TYPE  ScrollBarCallbackStruct = 
  RECORD
     reason : INTEGER ; 
     event : X.XEventStar ; 
     value : INTEGER ; 
     pixel : INTEGER ; 
  END; (* ScrollBarCallbackStruct*)

TYPE  ToggleButtonCallbackStruct = 
  RECORD
     reason : INTEGER ; 
     event : X.XEventStar ; 
     set : INTEGER ; 
  END; (* ToggleButtonCallbackStruct*)

TYPE  ListCallbackStruct = 
  RECORD
     	reason : INTEGER ; 
        event : X.XEventStar ; 
        item : String ; 
        item_length : INTEGER ; 
        item_position : INTEGER ; 
        selected_items : Xt.StringStar ; 
        selected_item_count : INTEGER ; 
        selected_item_positions : int_star; 
        selection_type : char ; 
  END; (* ListCallbackStruct*)

TYPE  SelectionBoxCallbackStruct = 
  RECORD
     reason : INTEGER ; 
     event : X.XEventStar ; 
     value : String ; 
     length : INTEGER ; 
  END; (* SelectionBoxCallbackStruct*)

TYPE  CommandCallbackStruct = 
  RECORD
     reason : INTEGER ; 
     event : X.XEventStar ; 
     value : String ; 
     length : INTEGER ; 
  END; (* CommandCallbackStruct*)

TYPE   FileSelectionBoxCallbackStruct = 
  RECORD
     	reason : INTEGER ; 
    	event : X.XEventStar ; 
    	value : String ; 
    	length : INTEGER ; 
    	mask : String ; 
    	mask_length : INTEGER ; 
    	dir  : String ; 
    	dir_length  : INTEGER ; 
        pattern  : String ; 
    	pattern_length  : INTEGER ; 
  END; (* FileSelectionBoxCallbackStruct*)


TYPE  ScaleCallbackStruct = 
  RECORD
     reason : INTEGER ; 
     event : X.XEventStar ; 
     value : INTEGER ; 
  END; (* ScaleCallbackStruct*)


(************************************************************************
 *  PushButton defines
 ************************************************************************)
VAR   RBooleanDimension:char_star;

VAR   RMultiClick:char_star;

CONST MULTICLICK_DISCARD = 0;
CONST MULTICLICK_KEEP = 1;

(************************************************************************
 *  DrawnButton defines
 ************************************************************************)
VAR   RShadowType:char_star;

CONST SHADOW_IN = 7;
CONST SHADOW_OUT = 8;

(************************************************************************
 *  Arrow defines
 ************************************************************************)
VAR   RArrowDirection:char_star;

CONST ARROW_UP = 0;
CONST ARROW_DOWN = 1;
CONST ARROW_LEFT = 2;
CONST ARROW_RIGHT = 3;

(************************************************************************
 *  Separator defines
 ************************************************************************)

VAR   RMargin:char_star;

VAR   RSeparatorType:char_star;

CONST NO_LINE = 0;
CONST SINGLE_LINE = 1;
CONST DOUBLE_LINE = 2;
CONST SINGLE_DASHED_LINE = 3;
CONST DOUBLE_DASHED_LINE = 4;
CONST SHADOW_ETCHED_IN = 5;
CONST SHADOW_ETCHED_OUT = 6;

CONST PIXMAP = 1;
CONST STRING = 2;



(************************************************************************
 *  ScrollBar resource names and #defines
 ************************************************************************)

VAR   RProcessingDirection:char_star;

(*  Defined values for scrollbar  *)

CONST MAX_ON_TOP = 0;
CONST MAX_ON_BOTTOM = 1;
CONST MAX_ON_LEFT = 2;
CONST MAX_ON_RIGHT = 3;


(************************************************************************
 *									*
 * List Widget defines							*
 *									*
 ************************************************************************)


VAR   RListSpacing:char_star;

VAR   RListMarginWidth:char_star;

VAR   RListMarginHeight:char_star;

VAR   RItems:char_star;

VAR   RItemCount:char_star;

VAR   RSelectedItems:char_star;

VAR   RSelectedItemCount:char_star;

VAR   RVisibleItemCount:char_star;

VAR   RSelectionPolicy:char_star;

VAR   RListSizePolicy:char_star;

VAR   RDoubleClickinterval:char_star;

(****************
 *
 * Selection types
 *
 ****************)
CONST SINGLE_SELECT = 0;
CONST MULTIPLE_SELECT = 1;
CONST EXTENDED_SELECT = 2;
CONST BROWSE_SELECT = 3;

CONST STATIC = 0;
CONST DYNAMIC = 1;

(************************************************************************
 *									*
 * Scrolled Window defines.						*
 *									*
 ************************************************************************)
VAR   RScrollingPolicy:char_star;

VAR   RVisualPolicy:char_star;

VAR   RScrollBarDisplayPolicy:char_star;

VAR   RScrollBarPlacement:char_star;

CONST VARIABLE = 0;
CONST CONSTANT = 1;
CONST RESIZE_IF_POSSIBLE = 2;

CONST AUTOMATIC = 0;
CONST APPLICATION_DEFINED = 1;

(* #define	XmSTATIC	0     ** This is already defined by List ***)
CONST AS_NEEDED = 1;

CONST SW_TOP = 1;
CONST SW_BOTTOM = 0;
CONST SW_LEFT = 2;
CONST SW_RIGHT = 0;

CONST TOP_LEFT = SW_TOP + SW_LEFT;
CONST BOTTOM_LEFT = SW_BOTTOM + SW_LEFT;
CONST TOP_RIGHT = SW_TOP + SW_RIGHT;
CONST BOTTOM_RIGHT = SW_BOTTOM + SW_RIGHT;


(************************************************************************
 *									*
 * MainWindow Resources                                                 *
 *									*
 ************************************************************************)

CONST COMMAND_ABOVE_WORKSPACE = 0;
CONST COMMAND_BELOW_WORKSPACE = 1;

(************************************************************************
 *									*
 * Text Widget defines							*
 *									*
 ************************************************************************)
CONST MULTI_LINE_EDIT = 0;
CONST SINGLE_LINE_EDIT = 1;

TYPE TextPosition =  long; 
TYPE TextFormat =  X.Atom; 

CONST FMT8BIT = Xatom.XA_STRING;
CONST FMT16BIT= 2;

TYPE  XmTextScanType = { XmSELECT_POSITION, XmSELECT_WHITESPACE, XmSELECT_WORD,
               XmSELECT_LINE, XmSELECT_ALL, XmSELECT_PARAGRAPH } ;

TYPE XmHighlightMode = {XmHIGHLIGHT_NORMAL, XmHIGHLIGHT_SELECTED,
	      XmHIGHLIGHT_SECONDARY_SELECTED} ;


(* XmTextBlock's are used to pass text around. *)

TYPE  
  TextBlockRec = 
  RECORD
     ptr : char_star ; 
     length : INTEGER ; 
     format : TextFormat ; 
  END; (* TextBlockRec, *TextBlock*)
  TextBlock = UNTRACED REF TextBlockRec;

TYPE  
  TextVerifyCallbackStruct= 
  RECORD
     reason : INTEGER ; 
     event : X.XEventStar ; 
     doit : X.Bool ; 
     currInsert, newInsert : long ; 
     startPos, endPos : long ; 
     text : TextBlock ; 
  END; (* TextVerifyCallbackStruct, *TextVerifyPtr*)
  TextVerifyPtr = UNTRACED REF TextVerifyCallbackStruct;

(* functions renamed after 1.0 release due to resource name overlap *)

(************************************************************************
 *									*
 * VPaned Widget defines						*
 *									*
 ************************************************************************)

(************************************************************************
 *									*
 * 	DrawingArea defines
 *									*
 ************************************************************************)

(************************************************************************
 *									*
 *  DIALOG defines..  BulletinBoard and things common to its subclasses *
 *          CommandBox    MessageBox    Selection    FileSelection      *
 *									*
 ************************************************************************)

(* child type defines for Xm...GetChild() *)
CONST DIALOG_NONE = 0       (* a valid default button type *);
CONST DIALOG_APPLY_BUTTON = 1;
CONST DIALOG_CANCEL_BUTTON = 2;
CONST DIALOG_DEFAULT_BUTTON = 3;
CONST DIALOG_OK_BUTTON = 4;
CONST DIALOG_FILTER_LABEL = 5;
CONST DIALOG_FILTER_TEXT = 6;
CONST DIALOG_HELP_BUTTON = 7;
CONST DIALOG_LIST = 8;
CONST DIALOG_LIST_LABEL = 9;
CONST DIALOG_MESSAGE_LABEL = 10;
CONST DIALOG_SELECTION_LABEL = 11;
CONST DIALOG_SYMBOL_LABEL = 12;
CONST DIALOG_TEXT = 13;
CONST DIALOG_SEPARATOR = 14;
CONST DIALOG_DIR_LIST = 15;
CONST DIALOG_DIR_LIST_LABEL = 16;

VAR   RDialogStyle:char_star;

(*  dialog style defines  *)
CONST DIALOG_MODELESS = 0;
CONST DIALOG_PRIMARY_APPLICATION_MODAL = 1;
CONST DIALOG_FULL_APPLICATION_MODAL = 2;
CONST DIALOG_SYSTEM_MODAL = 3;

(************************************************************************
 * XmSelectionBox, XmFileSelectionBox and XmCommand - misc. stuff       *
 ***********************************************************************)

VAR   RFileTypeMask:char_star;


(* Defines for file type mask:
*)
CONST FILE_DIRECTORY = Word.Shift(1, 0);
CONST FILE_REGULAR   = Word.Shift(1, 1);
CONST FILE_ANY_TYPE  = Word.Plus(FILE_DIRECTORY,FILE_REGULAR);


(* Defines for selection dialog type:
*)
CONST DIALOG_WORK_AREA = 0;
CONST DIALOG_PROMPT = 1;
CONST DIALOG_SELECTION = 2;
CONST DIALOG_COMMAND = 3;
CONST DIALOG_FILE_SELECTION = 4;


(************************************************************************
 *  XmMessageBox           stuff not common to other dialogs            *
 ***********************************************************************)

VAR   RDefaultButtonType:char_star;
VAR   RDialogType:char_star;

(* defines for dialog type *)
CONST DIALOG_ERROR = 1;
CONST DIALOG_INFORMATION = 2;
CONST DIALOG_MESSAGE = 3;
CONST DIALOG_QUESTION = 4;
CONST DIALOG_WARNING = 5;
CONST DIALOG_WORKING = 6;


(************************************************************************
 *	Resource names used by XmScale
 ************************************************************************)
(*  Traversal direction defines  *)

CONST TRAVERSE_CURRENT = 0;
CONST TRAVERSE_NEXT = 1;
CONST TRAVERSE_PREV = 2;
CONST TRAVERSE_HOME = 3;
CONST TRAVERSE_NEXT_TAB_GROUP = 4;
CONST TRAVERSE_PREV_TAB_GROUP = 5;
CONST TRAVERSE_UP = 6;
CONST TRAVERSE_DOWN = 7;
CONST TRAVERSE_LEFT = 8;
CONST TRAVERSE_RIGHT = 9;


(**********************************************************************
 *
 *  Color generation function.
 *
 **********************************************************************)


TYPE 
  ColorProc = PROCEDURE ( 
 	bg_color,fg_color,sel_color,ts_color,bs_color:UNTRACED REF X.XColor;
     );

<*EXTERNAL "_XmSetColorCalculation":C*>
PROCEDURE SetColorCalculation(proc:ColorProc): ColorProc ;

<*EXTERNAL "_XmGetColorCalculation":C*>
PROCEDURE GetColorCalculation(): ColorProc ;

<*EXTERNAL "_XmGetColors":C*>
PROCEDURE GetColors (screen:X.ScreenStar;
	 color_map:X.Colormap;
	 background:Xt.Pixel;
	 foreground_ret,top_shadow_ret,
	 bottom_shadow_ret, select_ret:UNTRACED REF Xt.Pixel);

(**********************************************************************
 *
 *  extern for the string to unit type converter.
 *
 **********************************************************************)

<*EXTERNAL "_XmCvtStringToUnitType":C*>
PROCEDURE CvtStringToUnitType(args:Xrm.ValuePtr;
	                      num_args:int_star;
			      from_val,to_val: UNTRACED REF Xrm.Value); 

<*EXTERNAL "_XmSetFontUnit":C*>
PROCEDURE SetFontUnit(display:X.DisplayStar;value:INTEGER);

<*EXTERNAL "_XmSetFontUnits":C*>
PROCEDURE SetFontUnits(display: X.DisplayStar;
	               hvalue,vvalue:INTEGER);

(****************
 *
 *  Public functions with prototypes defined in XmString.c
 *
 *  HGG: Need to write Externals for all of them - see Xm.h
 ****************)

<*EXTERNAL "XmStringCreateSimple":C*>
PROCEDURE StringCreateSimple(str:char_star): String;

<*EXTERNAL "XmStringFree":C*>
PROCEDURE StringFree(str:String);



(***********************************************************************
 *
 * SimpleMenu declarations and definitions.
 *
 ***********************************************************************)
TYPE ButtonType =  unsigned_char; 
TYPE ButtonTypeTable = UNTRACED REF  ButtonType; 
TYPE KeySymTable = UNTRACED REF  X.KeySym; 
TYPE StringcharSetTable = UNTRACED REF  StringcharSet; 

CONST PUSHBUTTON = 1;
CONST TOGGLEBUTTON = 2;
CONST CHECKBUTTON = 2;
CONST RADIOBUTTON = 3;
CONST CASCADEBUTTON = 4;
CONST SEPARATOR = 5;
CONST DOUBLE_SEPARATOR = 6;
CONST TITLE = 7;

VAR   VaPUSHBUTTON:char_star;
VAR   VaTOGGLEBUTTON:char_star;
VAR   VaCHECKBUTTON:char_star;
VAR   VaRADIOBUTTON:char_star;
VAR   VaCASCADEBUTTON:char_star;
VAR   VaSEPARATOR:char_star;
VAR   VaSINGLE_SEPARATOR:char_star;
VAR   VaDOUBLE_SEPARATOR:char_star;
VAR   VaTITLE:char_star;


<*EXTERNAL "_XmCreateSimpleMenuBar":C*>
PROCEDURE CreateSimpleMenuBar(parent:Xt.Widget;
	                      name: String;
			      args: Xt.ArgList ;
			      arg_count:Cardinal ): Xt.Widget ;

<*EXTERNAL "_XmCreateSimplePopupMenu":C*>
PROCEDURE CreateSimplePopupMenu(parent:Xt.Widget;
	                      name: String;
			      args: Xt.ArgList ;
			      arg_count:Cardinal ): Xt.Widget ;

<*EXTERNAL "_XmCreateSimplePulldownMenu":C*>
PROCEDURE CreateSimplePulldownMenu(parent:Xt.Widget;
	                      name: String;
			      args: Xt.ArgList ;
			      arg_count:Cardinal ): Xt.Widget ;

<*EXTERNAL "_XmCreateSimpleOptionMenu":C*>
PROCEDURE CreateSimpleOptionMenu(parent:Xt.Widget;
	                      name: String;
			      args: Xt.ArgList ;
			      arg_count:Cardinal ): Xt.Widget ;

<*EXTERNAL "_XmCreateSimpleRadioBox":C*>
PROCEDURE CreateSimpleRadioBox(parent:Xt.Widget;
	                      name: String;
			      args: Xt.ArgList ;
			      arg_count:Cardinal ): Xt.Widget ;

<*EXTERNAL "_XmCreateSimpleCheckBox":C*>
PROCEDURE CreateSimpleCheckBox(parent:Xt.Widget;
	                      name: String;
			      args: Xt.ArgList ;
			      arg_count:Cardinal ): Xt.Widget ;

(*???
<*EXTERNAL _XmVaCreateSimpleMenuBar:C*>
PROCEDURE VaCreateSimpleMenuBar(Widget parent, String name, ...): Widget ;
extern Widget XmVaCreateSimplePopupMenu (Widget parent, String name,
VAR   callback, ...) : XtCallbackProc ; 
extern Widget XmVaCreateSimplePulldownMenu (Widget parent, String name,
                                     int post_from_button,
VAR   callback, ...) : XtCallbackProc ; 
extern Widget XmVaCreateSimpleOptionMenu (Widget parent, String name,
                                   XmString option_label,
                                   KeySym option_mnemonic,
                                   int button_set,
VAR   callback, ...) : XtCallbackProc ; 
extern Widget XmVaCreateSimpleRadioBox (Widget parent, String name,
VAR   button_set, XtC.allbackProc callback, ...) : INTEGER ; 
extern Widget XmVaCreateSimpleCheckBox (Widget parent, String name,
VAR   callback, ...) : XtCallbackProc ; 

*)

(***********************************************************************
 *
 * 	Misc Declarations
 * 
 ***********************************************************************)
TYPE ResourceBaseProc=  PROCEDURE(widget:Xt.Widget; 
                                  client_data:Xt.Pointer):Xt.Pointer; 

TYPE SecondaryResourceDataRec = RECORD 
       base_proc : ResourceBaseProc ; 
       client_data : Xt.Pointer ; 
       name : String ; 
       res_class : String ; 
       resources : Xt.ResourceList ; 
       num_resources : CARDINAL ;
     END (*record*); 

TYPE SecondaryResourceDataRec_star = UNTRACED REF SecondaryResourceDataRec;

(*???
extern Cardinal XmGetSecondaryResourceData (WidgetClass wclass,
VAR   **secondaryDataRtn) : SecondaryResourceData ; 
extern Widget XmTrackingLocate (Widget widget, Cursor cursor, 
#if NeedWidePrototypes
    int confineTo
#else
    Boolean confineTo
#endif 
);
???*)

<*EXTERNAL XmConvertUnits:C *>
PROCEDURE ConvertUnits (widget: Xt.Widget;
	                dimension,from_type,from_val,to_type:INTEGER): INTEGER;

<*EXTERNAL XmCvtFromHorizontalPixels:C*>
PROCEDURE CvtFromHorizontalPixels(screen: X.ScreenStar;
	                          from_val,to_type:INTEGER): INTEGER;

<*EXTERNAL XmCvtFromVerticalPixels:C*>
PROCEDURE CvtFromVerticalPixels(screen: X.ScreenStar;
	                        from_val,to_type:INTEGER): INTEGER ;

<*EXTERNAL XmCvtToHorizontalPixels:C*>
PROCEDURE CvtToHorizontalPixels(screen: X.ScreenStar;
	                        from_val,from_type:INTEGER): INTEGER ;

<*EXTERNAL XmCvtToVerticalPixels:C*>
PROCEDURE CvtToVerticalPixels(screen: X.ScreenStar;
	                        from_val,from_type:INTEGER): INTEGER ;



<*EXTERNAL XmCvtCTToXmString:C*>
PROCEDURE CvtCTToXmString( text: char_star): String ;

<*EXTERNAL XmCvtXmStringToCT:C*>
PROCEDURE CvtXmStringToCT( string: String) : char_star ; 

<*EXTERNAL XmCvtTextToXmString:C*>
PROCEDURE CvtTextToXmString(display : X.DisplayStar;
	                    args: Xrm.ValuePtr;
			    num_args: Xt.CardinalStar;
			    from_val: Xrm.ValuePtr;
			    to_val: Xrm.ValuePtr;
			    converter_data: Xt.Pointer): Xt.Boolean;

<*EXTERNAL XmCvtXmStringToText:C*>
PROCEDURE CvtXmStringToText(display : X.DisplayStar;
	                    args: Xrm.ValuePtr;
			    num_args: Xt.CardinalStar;
			    from_val: Xrm.ValuePtr;
			    to_val: Xrm.ValuePtr;
			    converter_data: Xt.Pointer): Xt.Boolean; 



TYPE NavigationType =  unsigned_char; 

<*EXTERNAL XmAddTabGroup:C*>
PROCEDURE AddTabGroup(tabGroup:Xt.Widget);

<*EXTERNAL XmRemoveTabGroup:C*>
PROCEDURE RemoveTabGroup(w: Xt.Widget);

<*EXTERNAL XmProcessTraversal:C*>
PROCEDURE ProcessTraversal(w: Xt.Widget; dir:INTEGER): Xt.Boolean ;

(*???
<*EXTERNAL _XmUninstallImage:C*>
PROCEDURE UninstallImage(XImage *image): BOOLEAN ;
<*EXTERNAL _XmDestroyPixmap:C*>
PROCEDURE DestroyPixmap(Screen *screen, Pixmap pixmap): BOOLEAN ;
<*EXTERNAL _XmInstallImage:C*>
PROCEDURE InstallImage(XImage *image, char *image_name): BOOLEAN ;
<*EXTERNAL _XmGetPixmap:C*>
PROCEDURE GetPixmap(Screen *screen, char *image_name, Pixel foreground, Pixel background): Pixmap ;
???*)

(* ???
<*EXTERNAL _XmGetMenuCursor:C*>
PROCEDURE GetMenuCursor(Display *display): Cursor ;
<*EXTERNAL _XmSetMenuCursor:C*>
PROCEDURE SetMenuCursor(Display *display, Cursor cursorId);
<*EXTERNAL _XmGetDestination:C*>
PROCEDURE GetDestination(Display *display): Widget ;
#endif (* _NO_PROTO *)
??? *)

TYPE Offset =  long; 
TYPE OffsetPtr = UNTRACED REF  Offset; 
TYPE OffsetPtr_star = UNTRACED REF OffsetPtr;

<*EXTERNAL XmUpdateDisplay:C*>
PROCEDURE UpdateDisplay(w: Xt.Widget);

<*EXTERNAL XmResolvePartOffsets:C*>
PROCEDURE ResolvePartOffsets(wclass: Xt.WidgetClass;
	                     offset: OffsetPtr_star);

<*EXTERNAL XmResolveAllPartOffsets:C *>
PROCEDURE ResolveAllPartOffsets (wclass: Xt.WidgetClass;
	                         offset,constraint_offset: OffsetPtr_star);

END Xm.
