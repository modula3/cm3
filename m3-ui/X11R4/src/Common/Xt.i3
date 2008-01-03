(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: Xt.i3							*)
(* Last modified on Wed Nov 24 15:29:38 PST 1993 by steveg      *)
(*      modified on Fri May  7 16:14:46 PDT 1993 by mjordan     *)
(*      modified on Fri Feb 28 11:18:40 PST 1992 by kalsow	*)
(*      modified on Thu Mar 14 02:25:53 1991 by muller		*)
(*      modified on Tue Apr 24 17:39:41 1990 by jerome		*)



UNSAFE INTERFACE Xt;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	contains:	/usr/include/X11/Intrinsic.h		*)
(*			/usr/include/X11/Composite.h		*)
(*			/usr/include/X11/Geometry.h ??		*)
(*==============================================================*)


FROM Ctypes  IMPORT	char, char_star, char_star_star, 
			const_char_star, const_char_star_star,
			int, int_star, short,
			unsigned_char, unsigned_int, 
			unsigned_long, unsigned_long_star;

FROM X       IMPORT	Atom, AtomStar, Cursor, DisplayStar, Enumeration,
			GC, XGCValuesStar, KeyCode, KeyCodeStar, 
                        KeyCodeStarStar, KeySym, KeySymStar,
			Region, ScreenStar, Time, TimeStar, Window, 
			XEventStar, XAnyEventStar, XSelectionRequestEventStar,
                        Argv;

IMPORT Word, Xrm, XMachine;

TYPE Int = int; Short = short; Char = char;

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


(*===================================================================
 * $XConsortium: Intrinsic.h,v 1.130 89/12/15 17:20:05 swick Exp $
 * $oHeader: Intrinsic.h,v 1.10 88/09/01 10:33:34 asente Exp $
 ====================================================================*)

CONST
  SpecificationRelease   = 4;

TYPE
  String            =  char_star;
  StringStar        =  char_star_star;
  ConstString       =  const_char_star;
  ConstStringStar   =  const_char_star_star;
  StringList        =  UNTRACED REF ARRAY [0..MaxSizeList] OF String;

  Widget            =  ADDRESS;
  WidgetStar        =  UNTRACED REF Widget;
  WidgetList        =  UNTRACED REF ARRAY [0..MaxSizeList] OF Widget;
  WidgetClass       =  ADDRESS;
  CompositeWidget   =  ADDRESS;

  ActionList        =  UNTRACED REF ARRAY [0..MaxSizeList] OF ActionsRec;
  EventTable        =  ADDRESS;
  BoundAccActions   =  ADDRESS;

  AppContext        =  ADDRESS;

  ValueMask         =  unsigned_long;
  IntervalId        =  unsigned_long;
  IntervalIdStar    =  UNTRACED REF IntervalId;
  InputId           =  unsigned_long;
  InputIdStar       =  UNTRACED REF InputId;
  WorkProcId        =  unsigned_long;
  GeometryMask      =  unsigned_int;
  GCMask            =  unsigned_long;(* Mask of values that are used by widget*)
  Pixel             =  unsigned_long;   (* Index into colormap		*)

(* ?!?!?!
#define XtNumber(arr)		((Cardinal) (sizeof(arr) / sizeof(arr[0])))
?!?!?! *)

TYPE
  CacheType		=  Int;

CONST
  CacheNone		=	  16_001;
  CacheAll		=	  16_002;
  CacheByDisplay	=	  16_003;
  CacheRefCount		=	  16_100;

(****************************************************************
 *
 * System Dependent Definitions; see spec for specific range
 * requirements.  Do not assume every implementation uses the
 * same base types!
 *
 *
 * XtArgVal ought to be a union of XtPointer, char *, long, Int *, and proc *
 * but casting to union types is not really supported.
 *
 * So the typedef for XtArgVal should be chosen such that
 *
 *	sizeof (XtArgVal) >=	sizeof(XtPointer)
 *				sizeof(char * )
 *				sizeof(long)
 *				sizeof(Int * )
 *				sizeof(proc * )
 *
 * ArgLists rely heavily on the above typedef.
 *
 ****************************************************************)

TYPE

  ArgVal               =  ADDRESS;(* and not Long because of the above union *)

  Boolean              =  Char;
  BooleanStar          =  UNTRACED REF Boolean;
  Cardinal             =  unsigned_int;
  CardinalStar         =  UNTRACED REF Cardinal;
  Dimension            =  XMachine.Dimension; (* size in pixels *)
  DimensionStar        =  UNTRACED REF Dimension;
  Enum                 =  unsigned_char;
  Opaque               =  char_star;
  Pointer              =  char_star;
  PointerStar          =  UNTRACED REF Pointer;
  Position             =  Short ;   (* Offset from 0 coordinate	*)
  PositionStar         =  UNTRACED REF Position;


TYPE

  Translations       =  ADDRESS;
  Accelerators       =  ADDRESS;
  Modifiers          =  unsigned_int;
  ModifiersStar      =  UNTRACED REF Modifiers;

  ActionProc         =  PROCEDURE (
     widget:                 Widget;
     event:                  XAnyEventStar;
     params:                 StringStar;
     num_params:             CardinalStar);

  ActionsRec         =  RECORD
			    string: String;
			    proc: ActionProc;
			END;

TYPE
  AddressMode		=  Enumeration;
(* address mode		parameter representation    *)
(* ------------		------------------------    *)
CONST
    Address		= 0; (* address			    *)
    BaseOffset		= 1; (* offset			    *)
    Immediate		= 2; (* constant		    *)
    ResourceString	= 3; (* resource name string	    *)
    ResourceQuark	= 4; (* resource name quark	    *)
    WidgetBaseOffset	= 5; (* offset from ancestor	    *)
    ProcedureArg	= 6; (* procedure to invoke	    *)

TYPE
  ConvertArgRec   =  RECORD
			address_mode: AddressMode;
			address_id: Pointer;
			size: Cardinal;
		     END;

  ConvertArgList  =  UNTRACED REF ARRAY [0..MaxSizeList] OF ConvertArgRec;

TYPE
  ConvertArgProc  =  PROCEDURE (
     widget:                 Widget;
     size:                   CardinalStar;
     value:                  Xrm.ValueStar);

TYPE
  WidgetGeometry   = RECORD
			request_mode: GeometryMask;
			x, y: Position;
			width, height, border_width: Dimension;
			sibling: Widget;
			stack_mode: Int   (* Above, Below, TopIf, BottomIf,
					     Opposite, DontChange *)
		     END;
  WidgetGeometryStar  =  UNTRACED REF WidgetGeometry;

CONST
  (* Additions to Xlib geometry requests: ask what would happen, don't do it *)
  CWQueryOnly      =   Word.Shift (1, 7);

  (* Additions to Xlib stack modes: don't change stack order *)
  SMDontChange	   =  5;

TYPE
  Converter  =  PROCEDURE (
     args:                   Xrm.ValueStar;
     num_args:               CardinalStar;
     from:                   Xrm.ValueStar;
     to:                     Xrm.ValueStar);

TYPE
  TypeConverter  =  PROCEDURE (
     dpy:                    DisplayStar;
     args:                   Xrm.ValueStar;
     num_args:               CardinalStar;
     from:                   Xrm.ValueStar;
     to:                     Xrm.ValueStar;
     converter_data:         PointerStar): Boolean;

TYPE
  Destructor  =  PROCEDURE (
     app:                    AppContext;
     to:                     Xrm.ValueStar;
     converter_data:         Pointer;
     args:                   Xrm.ValueStar;
     num_args:               CardinalStar);

TYPE
  CacheRef          =  Pointer;
  CacheRefStar      =  UNTRACED REF CacheRef;
  ActionHookId      =  Pointer;
  ActionHookIdStar  =  UNTRACED REF ActionHookId;

TYPE
  ActionHookProc  =  PROCEDURE (
     w:                      Widget;
     client_data:            Pointer;
     action_name:            String;
     event:                  XEventStar;
     params:                 StringStar;
     num_params:             CardinalStar);

TYPE
  KeyProc  =  PROCEDURE (
     dpy:                    DisplayStar;
     keycode:                KeyCodeStar;
     modifiers:              ModifiersStar;
     modifiers_return:       ModifiersStar;
     keysym_return:          KeySymStar);

TYPE
  CaseProc  =  PROCEDURE (
     keysym:                 KeySymStar;
     lower_return:           KeySymStar;
     upper_return:           KeySymStar);

TYPE
  EventHandler  =  PROCEDURE (
     widget:                 Widget;
     closure:                Pointer;
     event:                  XAnyEventStar;
     continue_to_dispatch:   BooleanStar);

TYPE
  EventMask      =  unsigned_long;
  InputMask      =  unsigned_long;
  ListPosition   =  Enumeration;

CONST
  ListHead       = 0;
  ListTail       = 1;

CONST
  InputNoneMask	   =  0;
  InputReadMask	   =  1;
  InputWriteMask   =  2;
  InputExceptMask  =  4;

TYPE
  TimerCallbackProc  =  PROCEDURE (
     closure:                Pointer;
     id:                     IntervalIdStar);

TYPE
  InputCallbackProc  =  PROCEDURE (
     closure:                Pointer;
     source:                 int_star;
     id:                     InputIdStar);

TYPE
  Arg                =  RECORD
				name: String;
				value: ArgVal;
			END;
  ArgList     	     =  UNTRACED REF ARRAY [0..MaxSizeList] OF Arg;
  VarArgsList        =  Pointer;

TYPE
  CallbackProc  =  PROCEDURE (
     widget:                 Widget;
     closure:                Pointer;	(* data the application registered *)
     call_data:              Pointer);	(* callback specific data *)

TYPE
  CallbackRec     =  RECORD
			callback: CallbackProc;
			closure: Pointer;
		     END;
  CallbackList    =  UNTRACED REF ARRAY [0..MaxSizeList] OF CallbackRec;

  CallbackStatus  =  Enumeration;

CONST
  CallbackNoList  = 0;
  CallbackHasNone = 1;
  CallbackHasSome = 2;

TYPE
  GeometryResult  =  Enumeration;

CONST
  GeometryYes	= 0;	  (* Request accepted. *)
  GeometryNo	= 1;	  (* Request denied. *)
  GeometryAlmost= 2;      (* Request denied, but willing to take replyBox. *)
  GeometryDone	= 3;	  (* Request accepted and done. *)

TYPE
  GrabKind        =  Enumeration;

CONST
  GrabNone		= 0;
  GrabNonexclusive	= 1;
  GrabExclusive		= 2;

TYPE
  PopdownIDRec   =  RECORD
			  shell_widget:	  Widget;
			  enable_widget:  Widget;
		    END;
  PopdownIDStar  =  UNTRACED REF PopdownIDRec;


TYPE
  Resource     =  RECORD
			resource_name: String;	   (* Resource name *)
			resource_class: String;	   (* Resource class *)
			resource_type: String;	   (* Representation type 
                                                      desired *)
			resource_size: Cardinal;   (* Size in bytes of 
                                                      representation *)
			resource_offset: Cardinal; (* Offset from base to put 
                                                      resource value *)
			default_type: String;	   (* representation type of
                                                      specified default *)
			default_addr: Pointer;	   (* Address of default 
                                                      resource *)
		 END;

  ResourceList      =  UNTRACED REF ARRAY [0..MaxSizeList] OF Resource;

TYPE
  ErrorMsgHandler  =  PROCEDURE (
     name:                   String;
     type:                   String;
     class:                  String;
     defaultp:               String;
     params:                 StringStar;
     num_params:             CardinalStar);

TYPE
  ErrorHandler  =  PROCEDURE (
     msg:                    String);

TYPE
  WorkProc  =  PROCEDURE (
     closure:                Pointer	(* data the application registered *)
): Boolean;

TYPE
  SubstitutionRec  =  RECORD  
			match: Char;
			substitution: String;
		      END;
  SubstitutionStar =  UNTRACED REF SubstitutionRec;

TYPE
  FilePredicate  =  PROCEDURE (filename: String): Boolean;

TYPE
  RequestId       =  Pointer;
  RequestIdStar   =  UNTRACED REF RequestId;

TYPE
    FallbackRes		=  String;
    FallbackResList	=  UNTRACED REF ARRAY [0..MaxSizeList] OF FallbackRes;

(*
 * Routine to get the value of a selection as a given type.  Returns
 * TRUE if it successfully got the value as requested, FALSE otherwise.  
 * selection is the atom describing the type of selection (e.g. 
 * primary or secondary). value is set to the pointer of the converted 
 * value, with length elements of data, each of size indicated by format.
 * (This pointer will be freed using XtFree when the selection has
 *  been delivered to the requestor.)  target is
 * the type that the conversion should use if possible; type is returned as
 * the actual type returned.  Format should be either 8, 16, or 32, and
 * specifies the word size of the selection, so that Xlib and the server can
 * convert it between different machine types. *)

TYPE
  ConvertSelectionProc  =  PROCEDURE (
     widget:                 Widget;
     selection:              AtomStar;
     target:                 AtomStar;
     type_return:            AtomStar;
     value_return:           PointerStar;
     length_return:          unsigned_long_star;
     format_return:          int_star): Boolean;

(*
 * Routine to inform a widget that it no longer owns the given selection.
 *)

TYPE
  LoseSelectionProc  =  PROCEDURE (
     widget:                 Widget;
     selection:              AtomStar);

(*
 * Routine to inform the selection owner when a selection requestor
 * has successfully retrieved the selection value.
 *)

TYPE
  SelectionDoneProc  =  PROCEDURE (
     widget:                 Widget;
     selection:              AtomStar;
     target:                 AtomStar);

(*
 * Routine to call back when a requested value has been obtained for a
 *  selection.
 *)

TYPE
  SelectionCallbackProc  =  PROCEDURE (
     widget:                 Widget;
     closure:                Pointer;
     selection:              AtomStar;
     type:                   AtomStar;
     value:                  Pointer;
     length:                 unsigned_long_star;
     format:                 int_star);

TYPE
  LoseSelectionIncrProc  =  PROCEDURE (
     widget:                 Widget;
     selection:              AtomStar;
     client_data:            Pointer);

TYPE
  SelectionDoneIncrProc  =  PROCEDURE (
     widget:                 Widget;
     selection:              AtomStar;
     target:                 AtomStar;
     receiver_id:            RequestIdStar;
     client_data:            Pointer);

TYPE
  ConvertSelectionIncrProc  =  PROCEDURE (
     widget:                 Widget;
     selection:              AtomStar;
     target:                 AtomStar;
     type:                   AtomStar;
     value:                  PointerStar;
     length:                 unsigned_long_star;
     format:                 int_star;
     max_length:             unsigned_long_star;
     client_data:            Pointer;
     receiver_id:            RequestIdStar): Boolean;

TYPE
  CancelConvertSelectionProc  =  PROCEDURE (
     widget:                 Widget;
     selection:              AtomStar;
     target:                 AtomStar;
     receiver_id:            RequestIdStar;
     client_data:            Pointer);

(***************************************************************
 *
 * Exported Interfaces
 *
 ****************************************************************)

<* EXTERNAL XtConvertAndStore *>
   PROCEDURE ConvertAndStore (
     widget:                 Widget;
     from_type:              ConstString;
     from:                   Xrm.ValueStar;
     to_type:                ConstString;
     to_in_out:              Xrm.ValueStar		
): Boolean;

<* EXTERNAL XtCallConverter *>
   PROCEDURE CallConverter (
     dpy:                    DisplayStar;
     converter:              TypeConverter;
     args:                   Xrm.ValuePtr;
     num_args:               Cardinal;
     from:                   Xrm.ValuePtr;
     to_return:              Xrm.ValueStar;
     cache_ref_return:       CacheRefStar		
): Boolean;

<* EXTERNAL XtDispatchEvent *>
   PROCEDURE DispatchEvent (
     event:                  XEventStar 		
): Boolean;

<* EXTERNAL XtCallAcceptFocus *>
   PROCEDURE CallAcceptFocus (
     widget:                 Widget;
     t:                      TimeStar		
): Boolean;

<* EXTERNAL XtPeekEvent *>
   PROCEDURE PeekEvent (
     event:                  XEventStar		
): Boolean;

<* EXTERNAL XtAppPeekEvent *>
   PROCEDURE AppPeekEvent (
     appContext:             AppContext;
     event:                  XEventStar		
): Boolean;

<* EXTERNAL XtIsSubclass *>
   PROCEDURE IsSubclass (
     widget:                 Widget;
     widgetClass:            WidgetClass 	
): Boolean;

<* EXTERNAL XtIsObject *>
   PROCEDURE IsObject (
     object:                 Widget 		
): Boolean;

<* EXTERNAL "_XtCheckSubclassFlag" *>
   PROCEDURE CheckSubclassFlag ( (* implementation-private *)
     object:                 Widget;
     type_flag:              Enum		
): Boolean;

<* EXTERNAL "_XtIsSubclassOf" *>
   PROCEDURE IsSubclassOf ( (* implementation-private *)
     object:                 Widget;
     widget_class:           WidgetClass;
     flag_class:             WidgetClass;
     type_flag:              Enum		
): Boolean;

<* EXTERNAL XtIsManaged *>
   PROCEDURE IsManaged (
     rectobj:                Widget 		
): Boolean;

<* EXTERNAL XtIsRealized *>
   PROCEDURE IsRealized (
     widget:                 Widget 		
): Boolean;

<* EXTERNAL XtIsSensitive *>
   PROCEDURE IsSensitive (
     widget:                 Widget 		
): Boolean;

(*
 * Set the given widget to own the selection.  The convertProc should
 * be called when someone wants the current value of the selection. If it
 * is not NULL, the
 * losesSelection gets called whenever the window no longer owns the selection
 * (because someone else took it). If it is not NULL, the doneProc gets
 * called when the widget has provided the current value of the selection
 * to a requestor and the requestor has indicated that it has succeeded
 * in reading it by deleting the property.
 *)

<* EXTERNAL XtOwnSelection *>
   PROCEDURE OwnSelection (
     widget:                 Widget;
     selection:              Atom;
     time:                   Time;
     convert:                ConvertSelectionProc;
     lose:                   LoseSelectionProc;
     done:                   SelectionDoneProc 
): Boolean;

(* incremental selection interface *)

<* EXTERNAL XtOwnSelectionIncremental *>
   PROCEDURE OwnSelectionIncremental (
     widget:                 Widget;
     selection:              Atom;
     time:                   Time;
     convert_callback:       ConvertSelectionIncrProc;
     lose_callback:          LoseSelectionIncrProc;
     done_callback:          SelectionDoneIncrProc;
     cancel_callback:        CancelConvertSelectionProc;
     client_data:            Pointer 		
): Boolean;

<* EXTERNAL XtMakeResizeRequest *>
   PROCEDURE MakeResizeRequest (
     widget:                 Widget;
     width:                  Dimension;
     height:                 Dimension;
     replyWidth:             DimensionStar;
     replyHeight:            DimensionStar		
): GeometryResult;

<* EXTERNAL XtTransformCoords *>
   PROCEDURE TransformCoords (
     widget:                 Widget;
     x:                      Position;
     y:                      Position;
     rootx:                  PositionStar;
     rooty:                  PositionStar		
);

(* %%% Caution: don't declare any functions past this point that
 * return one of the following types or take a pointer to one of
 * the following types.
 *)

<* EXTERNAL XtStringConversionWarning *>
   PROCEDURE StringConversionWarning (
     from:                   ConstString;
     toType:                 ConstString (* Type attempted to convert it to. *)
);

<* EXTERNAL XtDisplayStringConversionWarning *>
   PROCEDURE DisplayStringConversionWarning (
     dpy:                    DisplayStar;
     from:                   ConstString;  (* String attempted to convert. *)
     toType:                 ConstString   (* Type attempted to 
                                              convert it to. *)
);

(* ?!?!?!
externalref XtConvertArgRec colorConvertArgs[];
externalref XtConvertArgRec screenConvertArg[];
?!?!?! *)

<* EXTERNAL XtAppAddConverter *>
   PROCEDURE AppAddConverter ( (* obsolete *)
     app:                    AppContext;
     from_type:              ConstString;
     to_type:                ConstString;
     converter:              Converter;
     convert_args:           ConvertArgList;
     num_args:               Cardinal 		
);

<* EXTERNAL XtAddConverter *>
   PROCEDURE AddConverter ( (* obsolete *)
     from_type:              ConstString;
     to_type:                ConstString;
     converter:              Converter;
     convert_args:           ConvertArgList;
     num_args:               Cardinal 		
);

<* EXTERNAL XtSetTypeConverter *>
   PROCEDURE SetTypeConverter (
     from_type:              ConstString;
     to_type:                ConstString;
     converter:              TypeConverter;
     convert_args:           ConvertArgList;
     num_args:               Cardinal;
     cache_type:             CacheType;
     destructor:             Destructor 	
);

<* EXTERNAL XtAppSetTypeConverter *>
   PROCEDURE AppSetTypeConverter (
     app_context:            AppContext;
     from_type:              ConstString;
     to_type:                ConstString;
     converter:              TypeConverter;
     convert_args:           ConvertArgList;
     num_args:               Cardinal;
     cache_type:             CacheType;
     destructor:             Destructor 	
);

<* EXTERNAL XtConvert *>
   PROCEDURE Convert (
     widget:                 Widget;
     from_type:              ConstString;
     from:                   Xrm.ValueStar;
     to_type:                ConstString;
     to_return:              Xrm.ValueStar		
);

<* EXTERNAL XtDirectConvert *>
   PROCEDURE DirectConvert (
     converter:              Converter;
     args:                   Xrm.ValuePtr;
     num_args:               Cardinal;
     from:                   Xrm.ValuePtr;
     to_return:              Xrm.ValueStar		
);

(****************************************************************
 *
 * Translation Management
 *
 ****************************************************************)

<* EXTERNAL XtParseTranslationTable *>
   PROCEDURE ParseTranslationTable (
     source:                 ConstString	
): Translations;

<* EXTERNAL XtParseAcceleratorTable *>
   PROCEDURE ParseAcceleratorTable (
     source:                 ConstString	
): Accelerators;

<* EXTERNAL XtOverrideTranslations *>
   PROCEDURE OverrideTranslations (
     widget:                 Widget;
     new:                    Translations 	
);

<* EXTERNAL XtAugmentTranslations *>
   PROCEDURE AugmentTranslations (
     widget:                 Widget;
     new:                    Translations 	
);

<* EXTERNAL XtInstallAccelerators *>
   PROCEDURE InstallAccelerators (
     destination:            Widget;
     source:                 Widget		
);

<* EXTERNAL XtInstallAllAccelerators *>
   PROCEDURE InstallAllAccelerators (
     destination:            Widget;
     source:                 Widget		
);

<* EXTERNAL XtUninstallTranslations *>
   PROCEDURE UninstallTranslations (
     widget:                 Widget 		
);

<* EXTERNAL XtAppAddActions *>
   PROCEDURE AppAddActions (
     app:                    AppContext;
     actions:                ActionList;
     num_actions:            Cardinal
);

<* EXTERNAL XtAddActions *>
   PROCEDURE AddActions (
     actions:                ActionList;
     num_actions:            Cardinal 		
);

<* EXTERNAL XtAppAddActionHook *>
   PROCEDURE AppAddActionHook (
     app:                    AppContext;
     proc:                   ActionHookProc;
     client_data:            Pointer 		
): ActionHookId;

<* EXTERNAL XtRemoveActionHook *>
   PROCEDURE RemoveActionHook (
     id:                     ActionHookId 	
);

<* EXTERNAL XtCallActionProc *>
   PROCEDURE CallActionProc (
     widget:                 Widget;
     action:                 ConstString;
     event:                  XEventStar;
     params:                 StringStar;
     num_params:             Cardinal		
);

<* EXTERNAL XtRegisterGrabAction *>
   PROCEDURE RegisterGrabAction (
     action_proc:            ActionProc;
     owner_events:           Boolean;
     event_mask:             unsigned_int;
     pointer_mode:           Int;
     keyboard_mode:          Int	 		
);

<* EXTERNAL XtSetMultiClickTime *>
   PROCEDURE SetMultiClickTime (
     dpy:                    DisplayStar;
     milliseconds:           Int 		
);

<* EXTERNAL XtGetMultiClickTime *>
   PROCEDURE GetMultiClickTime (
     dpy:                    DisplayStar		
): Int;

<* EXTERNAL XtGetActionKeysym *>
   PROCEDURE GetActionKeysym (
     event:                  XEventStar;
     modifiers_return:       ModifiersStar		
): KeySym;

(***************************************************************
 *
 * Keycode and Keysym procedures for translation management
 *
 ****************************************************************)

<* EXTERNAL XtTranslateKeycode *>
   PROCEDURE TranslateKeycode (
     dpy:                    DisplayStar;
     keycode:                KeyCode;
     modifiers:              Modifiers;
     modifiers_return:       ModifiersStar;
     keysym_return:          KeySymStar		
);

<* EXTERNAL XtTranslateKey *>
   PROCEDURE TranslateKey (
     dpy:                    DisplayStar;
     keycode:                KeyCodeStar;
     modifiers:              ModifiersStar;
     modifiers_return:       ModifiersStar;
     keysym_return:          KeySymStar		
);

<* EXTERNAL XtSetKeyTranslator *>
   PROCEDURE SetKeyTranslator (
     dpy:                    DisplayStar;
     proc:                   KeyProc 		
);

<* EXTERNAL XtRegisterCaseConverter *>
   PROCEDURE RegisterCaseConverter (
     dpy:                    DisplayStar;
     proc:                   CaseProc;
     start:                  KeySym;
     stop:                   KeySym 		
);

<* EXTERNAL XtConvertCase *>
   PROCEDURE ConvertCase (
     dpy:                    DisplayStar;
     keysym:                 KeySym;
     lower_return:           KeySymStar;
     upper_return:           KeySymStar		
);

<* EXTERNAL XtGetKeysymTable *>
   PROCEDURE GetKeysymTable (
     dpy:                    DisplayStar;
     min_keycode_return:     KeyCodeStar;
     keysyms_per_keycode_return: int_star		
): KeySymStar;

<* EXTERNAL XtKeysymToKeycodeList *>
   PROCEDURE KeysymToKeycodeList (
     dpy:                    DisplayStar;
     keysym:                 KeySym;
     keycodes_return:        KeyCodeStarStar;
     keycount_return:        CardinalStar		
);

(****************************************************************
 *
 * Event Management
 *
 ****************************************************************)

CONST
  AllEvents  = -1;

<* EXTERNAL XtInsertEventHandler *>
   PROCEDURE InsertEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer;
     position:               ListPosition 	
);

<* EXTERNAL XtInsertRawEventHandler *>
   PROCEDURE InsertRawEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer;
     position:               ListPosition 	
);

<* EXTERNAL XtAddEventHandler *>
   PROCEDURE AddEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer 		:= NIL
);

<* EXTERNAL XtRemoveEventHandler *>
   PROCEDURE RemoveEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer 		:= NIL
);

<* EXTERNAL XtAddRawEventHandler *>
   PROCEDURE AddRawEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer 		:= NIL
);

<* EXTERNAL XtRemoveRawEventHandler *>
   PROCEDURE RemoveRawEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer 		:= NIL
);

(* !!!!! double definition in Intrinsic.h !!!!!
<* EXTERNAL XtInsertEventHandler *>
   PROCEDURE InsertEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer;
     position:               ListPosition 	
);

<* EXTERNAL XtInsertRawEventHandler *>
   PROCEDURE InsertRawEventHandler (
     widget:                 Widget;
     eventMask:              EventMask;
     nonmaskable:            Boolean;
     proc:                   EventHandler;
     closure:                Pointer;
     position:               ListPosition 	
);
   !!!!! double definition in Intrinsic.h !!!!! *)

<* EXTERNAL XtBuildEventMask *>
   PROCEDURE BuildEventMask (
     widget:                 Widget 		
): EventMask;

<* EXTERNAL XtAddGrab *>
   PROCEDURE AddGrab (
     widget:                 Widget;
     exclusive:              Boolean;
     spring_loaded:          Boolean 		
);

<* EXTERNAL XtRemoveGrab *>
   PROCEDURE RemoveGrab (
     widget:                 Widget 		
);

<* EXTERNAL XtProcessEvent *>
   PROCEDURE ProcessEvent (
     mask:                   InputMask 		
);

<* EXTERNAL XtAppProcessEvent *>
   PROCEDURE AppProcessEvent (
     app:                    AppContext;
     mask:                   InputMask 		
);

<* EXTERNAL XtMainLoop *>
   PROCEDURE MainLoop ();


<* EXTERNAL XtAppMainLoop *>
   PROCEDURE AppMainLoop (
     app:                    AppContext 		
);

<* EXTERNAL XtAddExposureToRegion *>
   PROCEDURE AddExposureToRegion (
     event:                  XEventStar;
     region:                 Region 		
);

<* EXTERNAL XtSetKeyboardFocus *>
   PROCEDURE SetKeyboardFocus (
     subtree:                Widget;
     descendent:             Widget 		
);

<* EXTERNAL XtLastTimestampProcessed *>
   PROCEDURE LastTimestampProcessed (
     dpy:                    DisplayStar		
): Time;

(****************************************************************
 *
 * Event Gathering Routines
 *
 ****************************************************************)

<* EXTERNAL XtAddTimeOut *>
   PROCEDURE AddTimeOut (
     interval:               unsigned_long;
     proc:                   TimerCallbackProc;
     closure:                Pointer 		:= NIL
): IntervalId;

<* EXTERNAL XtAppAddTimeOut *>
   PROCEDURE AppAddTimeOut (
     app:                    AppContext;
     interval:               unsigned_long;
     proc:                   TimerCallbackProc;
     closure:                Pointer 		:= NIL
): IntervalId;

<* EXTERNAL XtRemoveTimeOut *>
   PROCEDURE RemoveTimeOut (
     timer:                  IntervalId 	
);

<* EXTERNAL XtAddInput *>
   PROCEDURE AddInput (
     source:                 Int;
     condition:              Pointer;
     proc:                   InputCallbackProc;
     closure:                Pointer 		:= NIL
): InputId;

<* EXTERNAL XtAppAddInput *>
   PROCEDURE AppAddInput (
     app:                    AppContext;
     source:                 Int;
     condition:              Pointer;
     proc:                   InputCallbackProc;
     closure:                Pointer 		:= NIL
): InputId;

<* EXTERNAL XtRemoveInput *>
   PROCEDURE RemoveInput (
     id:                     InputId 		
);

<* EXTERNAL XtNextEvent *>
   PROCEDURE NextEvent (
     event:                  XEventStar 		
);

<* EXTERNAL XtAppNextEvent *>
   PROCEDURE AppNextEvent (
     appContext:             AppContext;
     event:                  XEventStar		
);

CONST
  IMXEvent		=  1;
  IMTimer		=  2;
  IMAlternateInput	=  4;
  IMAll			=  (IMXEvent + IMTimer + IMAlternateInput);

<* EXTERNAL XtPending *>
   PROCEDURE Pending (): InputMask;

<* EXTERNAL XtAppPending *>
   PROCEDURE AppPending (
     appContext:             AppContext 	
): InputMask;

(****************************************************************
 *
 * Random utility routines
 *
 ****************************************************************)

(* ?!?!?!?!
#define XtIsRectObj(object)	(_XtCheckSubclassFlag(object, (XtEnum)0x02))
#define XtIsWidget(object)	(_XtCheckSubclassFlag(object, (XtEnum)0x04))
#define XtIsComposite(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x08))
#define XtIsConstraint(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x10))
#define XtIsShell(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x20))
#define XtIsOverrideShell(widget) \
    (_XtIsSubclassOf(widget, (WidgetClass)overrideShellWidgetClass, \
		     (WidgetClass)shellWidgetClass, (XtEnum)0x20))
#define XtIsWMShell(widget)	(_XtCheckSubclassFlag(widget, (XtEnum)0x40))
#define XtIsVendorShell(widget)	\
    (_XtIsSubclassOf(widget, (WidgetClass)vendorShellWidgetClass, \
		     (WidgetClass)wmShellWidgetClass, (XtEnum)0x40))
#define XtIsTransientShell(widget) \
    (_XtIsSubclassOf(widget, (WidgetClass)transientShellWidgetClass, \
		     (WidgetClass)wmShellWidgetClass, (XtEnum)0x40))
#define XtIsTopLevelShell(widget) (_XtCheckSubclassFlag(widget, (XtEnum)0x80))
#define XtIsApplicationShell(widget) \
    (_XtIsSubclassOf(widget, (WidgetClass)applicationShellWidgetClass, \
		     (WidgetClass)topLevelShellWidgetClass, (XtEnum)0x80))
?!?!?! *)

<* EXTERNAL XtRealizeWidget *>
   PROCEDURE RealizeWidget (
     widget:                 Widget 		
);

<* EXTERNAL XtUnrealizeWidget *>
   PROCEDURE UnrealizeWidget (
     widget:                 Widget 		
);

<* EXTERNAL XtDestroyWidget *>
   PROCEDURE DestroyWidget (
     widget:                 Widget 		
);

<* EXTERNAL XtSetSensitive *>
   PROCEDURE SetSensitive (
     widget:                 Widget;
     sensitive:              Boolean 		
);

<* EXTERNAL XtSetMappedWhenManaged *>
   PROCEDURE SetMappedWhenManaged (
     widget:                 Widget;
     mappedWhenManaged:      Boolean 		
);

<* EXTERNAL XtNameToWidget *>
   PROCEDURE NameToWidget (
     root:                   Widget;
     name:                   ConstString	
): Widget;

<* EXTERNAL XtWindowToWidget *>
   PROCEDURE WindowToWidget (
     display:                DisplayStar;
     window:                 Window 		
): Widget;

(***************************************************************
 *
 * Arg lists
 *
 ****************************************************************)

(* ?!?!?!
#define XtSetArg(arg, n, d) \
    ((void)( (arg).name = (n), (arg).value = (XtArgVal)(d) ))
?!?!?! *)

<* EXTERNAL XtMergeArgLists *>
   PROCEDURE MergeArgLists (
     args1:                  ArgList;
     num_args1:              Cardinal;
     args2:                  ArgList;
     num_args2:              Cardinal 		
): ArgList;

(***************************************************************
 *
 * Vararg lists
 *
 ****************************************************************)

(* ?!?!?!
#define XtVaNestedList  "XtVaNestedList"
#define XtVaTypedArg    "XtVaTypedArg"

<* EXTERNAL XtVaCreateArgsList *>
   PROCEDURE VaCreateArgsList (
    Pointer		(*unused*), ...
): XtVarArgsList;
?!?!?! *)

(*************************************************************
 *
 * Information routines
 *
 ************************************************************)

(* We're not included from the private file, so define these *)

<* EXTERNAL XtDisplay *>
   PROCEDURE Display (
     widget:                 Widget 		
): DisplayStar;

<* EXTERNAL XtDisplayOfObject *>
   PROCEDURE DisplayOfObject (
     object:                 Widget 		
): DisplayStar;

<* EXTERNAL XtScreen *>
   PROCEDURE Screen (
     widget:                 Widget 		
): ScreenStar;

<* EXTERNAL XtScreenOfObject *>
   PROCEDURE ScreenOfObject (
     object:                 Widget 		
): ScreenStar;

<* EXTERNAL XtWindow *>
   PROCEDURE XtWindow (
     widget:                 Widget 		
): Window;

<* EXTERNAL XtWindowOfObject *>
   PROCEDURE WindowOfObject (
     object:                 Widget 		
): Window;

<* EXTERNAL XtName *>
   PROCEDURE Name (
     object:                 Widget 		
): String;

<* EXTERNAL XtSuperclass *>
   PROCEDURE Superclass (
     object:                 Widget 		
): WidgetClass;

<* EXTERNAL XtClass *>
   PROCEDURE Class (
     object:                 Widget 		
): WidgetClass;

<* EXTERNAL XtParent *>
   PROCEDURE Parent (
     widget:                 Widget 		
): Widget;

(* ?!?!?!?
#define XtMapWidget(widget)	XMapWindow(XtDisplay(widget), XtWindow(widget))
#define XtUnmapWidget(widget)	\
		XUnmapWindow(XtDisplay(widget), XtWindow(widget))
?!?!?! *)

<* EXTERNAL XtAddCallback *>
   PROCEDURE AddCallback (
     widget:                 Widget;
     callback_name:          ConstString;
     callback:               CallbackProc;
     closure:                Pointer 		:= NIL
);

<* EXTERNAL XtRemoveCallback *>
   PROCEDURE RemoveCallback (
     widget:                 Widget;
     callback_name:          ConstString;
     callback:               CallbackProc;
     closure:                Pointer 		:= NIL
);

<* EXTERNAL XtAddCallbacks *>
   PROCEDURE AddCallbacks (
     widget:                 Widget;
     callback_name:          ConstString;
     callbacks:              CallbackList 	
);

<* EXTERNAL XtRemoveCallbacks *>
   PROCEDURE RemoveCallbacks (
     widget:                 Widget;
     callback_name:          ConstString;
     callbacks:              CallbackList 	
);

<* EXTERNAL XtRemoveAllCallbacks *>
   PROCEDURE RemoveAllCallbacks (
     widget:                 Widget;
     callback_name:          ConstString 	
);

<* EXTERNAL XtCallCallbacks *>
   PROCEDURE CallCallbacks (
     widget:                 Widget;
     callback_name:          ConstString;
     call_data:              Pointer 		
);

<* EXTERNAL XtCallCallbackList *>
   PROCEDURE CallCallbackList (
     widget:                 Widget;
     callbacks:              CallbackList;
     call_data:              Pointer 		
);

<* EXTERNAL XtHasCallbacks *>
   PROCEDURE HasCallbacks (
     widget:                 Widget;
     callback_name:          ConstString 	
): CallbackStatus;

(****************************************************************
 *
 * Geometry Management
 *
 ****************************************************************)

<* EXTERNAL XtMakeGeometryRequest *>
   PROCEDURE MakeGeometryRequest (
     widget:                 Widget;
     request:                WidgetGeometryStar;
     reply_return:           WidgetGeometryStar	
): GeometryResult;

<* EXTERNAL XtQueryGeometry *>
   PROCEDURE QueryGeometry (
     widget:                 Widget;
     intended:               WidgetGeometryStar;
     reply_return:           WidgetGeometryStar	
): GeometryResult;

<* EXTERNAL XtCreatePopupShell *>
   PROCEDURE CreatePopupShell (
     name:                   ConstString;
     widgetClass:            WidgetClass;
     parent:                 Widget;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
): Widget;

<* EXTERNAL XtVaCreatePopupShell *>
   PROCEDURE VaCreatePopupShell (
     name:                   ConstString;
     widgetClass:            WidgetClass;
     parent:                 Widget;
(*    ...   *)
): Widget;

<* EXTERNAL XtPopup *>
   PROCEDURE Popup (
     widget:                 Widget;
     grab_kind:              GrabKind 		
);

<* EXTERNAL XtPopupSpringLoaded *>
   PROCEDURE PopupSpringLoaded (
     widget:                 Widget 		
);

<* EXTERNAL XtCallbackNone *>
   PROCEDURE CallbackNone (
     widget:                 Widget;
     closure:                Pointer		:= NIL;
     call_data:              Pointer 		:= NIL
);

<* EXTERNAL XtCallbackNonexclusive *>
   PROCEDURE CallbackNonexclusive (
     widget:                 Widget;
     closure:                Pointer		:= NIL;
     call_data:              Pointer 		:= NIL
);

<* EXTERNAL XtCallbackExclusive *>
   PROCEDURE CallbackExclusive (
     widget:                 Widget;
     closure:                Pointer		:= NIL;
     call_data:              Pointer 		:= NIL
);

<* EXTERNAL XtPopdown *>
   PROCEDURE Popdown (
     widget:                 Widget 		
);

<* EXTERNAL XtCallbackPopdown *>
   PROCEDURE CallbackPopdown (
     widget:                 Widget;
     closure:                Pointer		:= NIL;
     call_data:              Pointer 		:= NIL
);

<* EXTERNAL XtMenuPopupAction *>
   PROCEDURE MenuPopupAction (
     widget:                 Widget;
     event:                  XEventStar;
     params:                 StringStar;
     num_params:             CardinalStar		
);

<* EXTERNAL XtCreateWidget *>
   PROCEDURE CreateWidget (
     name:                   ConstString;
     widget_class:           WidgetClass;
     parent:                 Widget;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
): Widget;

<* EXTERNAL XtCreateManagedWidget *>
   PROCEDURE CreateManagedWidget (
     name:                   ConstString;
     widget_class:           WidgetClass;
     parent:                 Widget;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal		:= 0
): Widget;

<* EXTERNAL XtVaCreateWidget *>
   PROCEDURE VaCreateWidget (
     name:                   ConstString;
     widget:                 WidgetClass;
     parent:                 Widget;
(*    ...  *)
): Widget;

<* EXTERNAL XtVaCreateManagedWidget *>
   PROCEDURE VaCreateManagedWidget (
     name:                   ConstString;
     widget_class:           WidgetClass;
     parent:                 Widget;
(*    ...  *)
): Widget;

<* EXTERNAL XtCreateApplicationShell *>
   PROCEDURE CreateApplicationShell (
     name:                   ConstString;
     widget_class:           WidgetClass;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
): Widget;

<* EXTERNAL XtAppCreateShell *>
   PROCEDURE AppCreateShell (
     name:                   ConstString;
     class:                  ConstString;
     widget_class:           WidgetClass;
     display:                DisplayStar;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
): Widget;

<* EXTERNAL XtVaAppCreateShell *>
   PROCEDURE VaAppCreateShell (
     name:                   ConstString;
     class:                  ConstString;
     widget_class:           WidgetClass;
     display:                DisplayStar;
(*    ...  *)
): Widget;

(****************************************************************
 *
 * Toolkit initialization
 *
 ****************************************************************)

<* EXTERNAL XtToolkitInitialize *>
   PROCEDURE ToolkitInitialize ();

<* EXTERNAL XtDisplayInitialize *>
   PROCEDURE DisplayInitialize (
     appContext:             AppContext;
     dpy:                    DisplayStar;
     name:                   ConstString;
     class:                  ConstString;
     options:                Xrm.OptionDescList;
     num_options:            Cardinal;
     VAR argc:               Cardinal;
     argv:                   Argv
);

<* EXTERNAL XtAppInitialize *>
   PROCEDURE AppInitialize (
     VAR app_context_return: AppContext;
     application_class:      ConstString;
     options:                Xrm.OptionDescList;
     num_options:            Cardinal;		
     VAR argc_in_out:        Cardinal;	
     argv_in_out:            Argv;		
     fallback_resources:     FallbackResList	:= NIL;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal		:= 0
): Widget;

<* EXTERNAL XtVaAppInitialize *>
   PROCEDURE VaAppInitialize (
     VAR app_context_return: AppContext;
     application_class:      ConstString;
     options:                Xrm.OptionDescList;
     num_options:            Cardinal;
     VAR argc_in_out:        Cardinal;	
     argv_in_out:            Argv;
     fallback_resources:     ConstStringStar;
(*    ...  *)
): Widget;

<* EXTERNAL XtInitialize *>
   PROCEDURE Initialize (
     name:                   ConstString	:= NIL;
     class:                  ConstString	:= NIL;
     options:                Xrm.OptionDescList	:= NIL;
     num_options:            Cardinal		:= 0;
     VAR argc:               Cardinal;
     argv:                   Argv		:= NIL
): Widget;

<* EXTERNAL XtOpenDisplay *>
   PROCEDURE OpenDisplay (
     appContext:             AppContext;
     displayName:            ConstString	:= NIL;
     applName:               ConstString	:= NIL;
     className:              ConstString	:= NIL;
     urlist:                 Xrm.OptionDescList	:= NIL;
     num_urs:                Cardinal		:= 0;
     VAR argc:               Cardinal;
     argv:                   Argv		:= NIL
): DisplayStar;

<* EXTERNAL XtCreateApplicationContext *>
   PROCEDURE CreateApplicationContext (): AppContext;

<* EXTERNAL XtAppSetFallbackResources *>
   PROCEDURE AppSetFallbackResources (
     app_context:            AppContext;
     specification_list:     ConstStringStar 	
);

<* EXTERNAL XtDestroyApplicationContext *>
   PROCEDURE DestroyApplicationContext (
     appContext:             AppContext 	
);

<* EXTERNAL XtInitializeWidgetClass *>
   PROCEDURE InitializeWidgetClass (
     widget_class:           WidgetClass 	
);

<* EXTERNAL XtWidgetToApplicationContext *>
   PROCEDURE WidgetToApplicationContext (
     widget:                 Widget 		
): AppContext;

<* EXTERNAL XtDisplayToApplicationContext *>
   PROCEDURE DisplayToApplicationContext (
     dpy:                    DisplayStar		
): AppContext;

<* EXTERNAL XtDatabase *>
   PROCEDURE Database (
     dpy:                    DisplayStar		
): Xrm.Database;

<* EXTERNAL XtCloseDisplay *>
   PROCEDURE CloseDisplay (
     dpy:                    DisplayStar		
);

<* EXTERNAL XtCopyFromParent *>
   PROCEDURE CopyFromParent (
     widget:                 Widget;
     offset:                 Int;
     value:                  Xrm.ValueStar		
);

<* EXTERNAL XtCopyDefaultDepth *>
   PROCEDURE CopyDefaultDepth (
     widget:                 Widget;
     offset:                 Int;
     value:                  Xrm.ValueStar		
);

<* EXTERNAL XtCopyDefaultColormap *>
   PROCEDURE CopyDefaultColormap (
     widget:                 Widget;
     offset:                 Int;
     value:                  Xrm.ValueStar		
);

<* EXTERNAL XtCopyAncestorSensitive *>
   PROCEDURE CopyAncestorSensitive (
     widget:                 Widget;
     offset:                 Int;
     value:                  Xrm.ValueStar		
);

<* EXTERNAL XtCopyScreen *>
   PROCEDURE CopyScreen (
     widget:                 Widget;
     offset:                 Int;
     value:                  Xrm.ValueStar 		
);

<* EXTERNAL XrmCompileResourceList *>
   PROCEDURE mCompileResourceList (
     resources:              ResourceList;
     num_resources:          Cardinal 		
);

<* EXTERNAL XtGetApplicationResources *>
   PROCEDURE GetApplicationResources (
     widget:                 Widget;
     base:                   Pointer;
     resources:              ResourceList;
     num_resources:          Cardinal;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
);

<* EXTERNAL XtVaGetApplicationResources *>
   PROCEDURE VaGetApplicationResources (
     widget:                 Widget;
     base:                   Pointer;
     resources:              ResourceList;
     num_resources:          Cardinal;
(*    ...  *)
);

<* EXTERNAL XtGetSubresources *>
   PROCEDURE GetSubresources (
     widget:                 Widget;
     base:                   Pointer;
     name:                   ConstString;
     class:                  ConstString;
     resources:              ResourceList;
     num_resources:          Cardinal;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
);

<* EXTERNAL XtVaGetSubresources *>
   PROCEDURE VaGetSubresources (
     widget:                 Widget;
     base:                   Pointer;
     name:                   ConstString;
     class:                  ConstString;
     resources:              ResourceList;
     num_resources:          Cardinal;
(*    ...  *)
);

<* EXTERNAL XtSetValues *>
   PROCEDURE SetValues (
     widget:                 Widget;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
);

<* EXTERNAL XtVaSetValues *>
   PROCEDURE VaSetValues (
     widget:                 Widget;
(*    ...  *)
);

<* EXTERNAL XtGetValues *>
   PROCEDURE GetValues (
     widget:                 Widget;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
);

<* EXTERNAL XtVaGetValues *>
   PROCEDURE VaGetValues (
     widget:                 Widget;
(*    ...  *)
);

<* EXTERNAL XtSetSubvalues *>
   PROCEDURE SetSubvalues (
     base:                   Pointer;
     resources:              ResourceList;
     num_resources:          Cardinal;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
);

<* EXTERNAL XtVaSetSubvalues *>
   PROCEDURE VaSetSubvalues (
     base:                   Pointer;
     resources:              ResourceList;
     num_resources:          Cardinal;
(*    ...  *)
);

<* EXTERNAL XtGetSubvalues *>
   PROCEDURE GetSubvalues (
     base:                   Pointer;
     resources:              ResourceList;
     num_resources:          Cardinal;
     args:                   ArgList		:= NIL;
     num_args:               Cardinal 		:= 0
);

<* EXTERNAL XtVaGetSubvalues *>
   PROCEDURE VaGetSubvalues (
     base:                   Pointer;
     resources:              ResourceList;
     num_resources:          Cardinal;
(*    ...  *)
);

<* EXTERNAL XtGetResourceList *>
   PROCEDURE GetResourceList (
     widget_class:           WidgetClass;
     resources_return:       ResourceList;
     num_resources_return:   CardinalStar		
);

<* EXTERNAL XtGetConstraintResourceList *>
   PROCEDURE GetConstraintResourceList (
     widget_class:           WidgetClass;
     resources_return:       ResourceList;
     num_resources_return:   CardinalStar		
);

(* ?!?!?!
#define XtUnspecifiedPixmap	((Pixmap)2)
#define XtUnspecifiedShellInt	(-1)
#define XtUnspecifiedWindow	((Window)2)
#define XtUnspecifiedWindowGroup ((Window)3)
#define XtDefaultForeground	"XtDefaultForeground"
#define XtDefaultBackground	"XtDefaultBackground"
#define XtDefaultFont		"XtDefaultFont"
#define XtOffset(p_type,field) \
	((Cardinal) (((char * ) (&(((p_type)NULL)->field))) - ((char * ) NULL)))
#define XtOffsetOf(s_type,field) XtOffset(s_type*,field)
?!?!?! *)

(*************************************************************
 *
 * Error Handling
 *
 ************************************************************)

<* EXTERNAL XtAppSetErrorMsgHandler *>
   PROCEDURE AppSetErrorMsgHandler (
     app:                    AppContext;
     handler:                ErrorMsgHandler 	
): ErrorMsgHandler;

<* EXTERNAL XtSetErrorMsgHandler *>
   PROCEDURE SetErrorMsgHandler (
     handler:                ErrorMsgHandler 	
);

<* EXTERNAL XtAppSetWarningMsgHandler *>
   PROCEDURE AppSetWarningMsgHandler (
     app:                    AppContext;
     handler:                ErrorMsgHandler 	
): ErrorMsgHandler;

<* EXTERNAL XtSetWarningMsgHandler *>
   PROCEDURE SetWarningMsgHandler (
     handler:                ErrorMsgHandler 	
);

<* EXTERNAL XtAppErrorMsg *>
   PROCEDURE AppErrorMsg (
     app:                    AppContext;
     name:                   ConstString;
     type:                   ConstString;
     class:                  ConstString;
     defaultp:               ConstString;
     params:                 StringStar;
     num_params:             CardinalStar		
);

<* EXTERNAL XtErrorMsg *>
   PROCEDURE ErrorMsg (
     name:                   ConstString;
     type:                   ConstString;
     class:                  ConstString;
     defaultp:               ConstString;
     params:                 StringStar;
     num_params:             CardinalStar		
);

<* EXTERNAL XtAppWarningMsg *>
   PROCEDURE AppWarningMsg (
     app:                    AppContext;
     name:                   ConstString;
     type:                   ConstString;
     class:                  ConstString;
     defaultp:               ConstString;
     params:                 StringStar;
     num_params:             CardinalStar		
);

<* EXTERNAL XtWarningMsg *>
   PROCEDURE WarningMsg (
     name:                   ConstString;
     type:                   ConstString;
     class:                  ConstString;
     defaultp:               ConstString;
     params:                 StringStar;
     num_params:             CardinalStar		
);

<* EXTERNAL XtAppSetErrorHandler *>
   PROCEDURE AppSetErrorHandler (
     app:                    AppContext;
     handler:                ErrorHandler 	
): ErrorHandler;

<* EXTERNAL XtSetErrorHandler *>
   PROCEDURE SetErrorHandler (
     handler:                ErrorHandler 	
);

<* EXTERNAL XtAppSetWarningHandler *>
   PROCEDURE AppSetWarningHandler (
     app:                    AppContext;
     handler:                ErrorHandler 	
): ErrorHandler;

<* EXTERNAL XtSetWarningHandler *>
   PROCEDURE SetWarningHandler (
     handler:                ErrorHandler 	
);

<* EXTERNAL XtAppError *>
   PROCEDURE AppError (
     app:                    AppContext;
     message:                ConstString	
);

<* EXTERNAL XtError *>
   PROCEDURE Error (
     message:                ConstString	
);

<* EXTERNAL XtAppWarning *>
   PROCEDURE AppWarning (
     app:                    AppContext;
     message:                ConstString	
);

<* EXTERNAL XtWarning *>
   PROCEDURE Warning (
     message:                ConstString	
);

<* EXTERNAL XtAppGetErrorDatabase *>
   PROCEDURE AppGetErrorDatabase (
     app:                    AppContext 	
): Xrm.DatabaseStar;

<* EXTERNAL XtGetErrorDatabase *>
   PROCEDURE GetErrorDatabase (): Xrm.DatabaseStar;

<* EXTERNAL XtAppGetErrorDatabaseText *>
   PROCEDURE AppGetErrorDatabaseText (
     app:                    AppContext;
     name:                   ConstString;
     type:                   ConstString;
     class:                  ConstString;
     defaultp:               ConstString;
     buffer:                 String;
     nbytes:                 Int;
     database:               Xrm.Database 	
);

<* EXTERNAL XtGetErrorDatabaseText *>
   PROCEDURE GetErrorDatabaseText (
     name:                   ConstString;
     type:                   ConstString;
     class:                  ConstString;
     defaultp:               ConstString;
     buffer:                 String;
     nbytes:                 Int 		
);

(****************************************************************
 *
 * Memory Management
 *
 ****************************************************************)

(* ?!?!?!
#define XtNew(type) ((type * ) XtMalloc((unsigned) sizeof(type)))
#define XtNewString(str) \
    ((str) == NULL ? NULL : (strcpy(XtMalloc((unsigned)strlen(str) + 1), str)))
?!?!?! *)

<* EXTERNAL XtMalloc *>
   PROCEDURE Malloc (
     size:                   Cardinal 		
): char_star;

<* EXTERNAL XtCalloc *>
   PROCEDURE Calloc (
     num:                    Cardinal;
     size:                   Cardinal 		
): char_star;

<* EXTERNAL XtRealloc *>
   PROCEDURE Realloc (
     ptr:                    char_star;
     num:                    Cardinal 		
): char_star;

<* EXTERNAL XtFree *>
   PROCEDURE Free (
     ptr:                    char_star		
);

(*************************************************************
 *
 *  Work procs
 *
 **************************************************************)

<* EXTERNAL XtAddWorkProc *>
   PROCEDURE AddWorkProc (
     proc:                   WorkProc;
     closure:                Pointer 		
): WorkProcId;

<* EXTERNAL XtAppAddWorkProc *>
   PROCEDURE AppAddWorkProc (
     app:                    AppContext;
     proc:                   WorkProc;
     closure:                Pointer 		
): WorkProcId;

<* EXTERNAL  XtRemoveWorkProc *>
   PROCEDURE tRemoveWorkProc (
     id:                     WorkProcId 	
);

(****************************************************************
 *
 * Graphic Context Management
 *
 *****************************************************************)

<* EXTERNAL XtGetGC *>
   PROCEDURE GetGC (
     widget:                 Widget;
     valueMask:              GCMask;
     values:                 XGCValuesStar 		
): GC;

<* EXTERNAL XtDestroyGC *>
   PROCEDURE DestroyGC (
     gc:                     GC 			
);

<* EXTERNAL XtReleaseGC *>
   PROCEDURE ReleaseGC (
     object:                 Widget;
     gc:                     GC 			
);

<* EXTERNAL XtReleaseCacheRef *>
   PROCEDURE ReleaseCacheRef (
     cache_ref:              CacheRefStar		
);

<* EXTERNAL XtCallbackReleaseCacheRef *>
   PROCEDURE CallbackReleaseCacheRef (
     widget:                 Widget;
     closure:                Pointer;	(* CacheRef *)
     call_data:              Pointer 		
);

<* EXTERNAL XtCallbackReleaseCacheRefList *>
   PROCEDURE CallbackReleaseCacheRefList (
     widget:                 Widget;
     closure:                Pointer;	(* CacheRefStar *)
     call_data:              Pointer 		
);

<* EXTERNAL XtSetWMColormapWindows *>
   PROCEDURE SetWMColormapWindows (
     widget:                 Widget;
     list:                   WidgetStar;
     count:                  Cardinal		
);

<* EXTERNAL XtFindFile *>
   PROCEDURE FindFile (
     path:                   ConstString;
     substitutions:          SubstitutionStar;
     num_substitutions:      Cardinal;
     predicate:              FilePredicate	
): String;

<* EXTERNAL XtResolvePathname *>
   PROCEDURE ResolvePathname (
     dpy:                    DisplayStar;
     type:                   ConstString;
     filename:               ConstString;
     suffix:                 ConstString;
     path:                   ConstString;
     predicate:              FilePredicate 	
): String;

(****************************************************************
 *
 * Selections
 *
 *****************************************************************)

(* !!!! ?!?!?! based value too large: an unsigned integer cannot
be negative.
CONST
  XT_CONVERT_FAIL: Atom   = 16_80000001;
*)

(*
 * The given widget no longer wants the selection.  If it still owns it, then
 * the selection owner is cleared, and the window's losesSelection is called.
 *)

<* EXTERNAL XtDisownSelection *>
   PROCEDURE DisownSelection (
     widget:                 Widget;
     selection:              Atom;
     time:                   Time 		
);

(*
 * Get the value of the given selection.  
 *)

<* EXTERNAL XtGetSelectionValue *>
   PROCEDURE GetSelectionValue (
     widget:                 Widget;
     selection:              Atom;
     target:                 Atom;
     callback:               SelectionCallbackProc;
     closure:                Pointer;
     time:                   Time 		
);

<* EXTERNAL XtGetSelectionValues *>
   PROCEDURE GetSelectionValues (
     widget:                 Widget;
     selection:              Atom;
     targets:                AtomStar;
     count:                  Int;
     callback:               SelectionCallbackProc;
     closures:               PointerStar;
     time:                   Time 		
);

(* Set the selection timeout value, in units of milliseconds *)

<* EXTERNAL XtAppSetSelectionTimeout *>
   PROCEDURE AppSetSelectionTimeout (
     app:                    AppContext;
     timeout:                unsigned_long 	
);

<* EXTERNAL XtSetSelectionTimeout *>
   PROCEDURE SetSelectionTimeout (
     timeout:                unsigned_long 	
);

 (* Return the selection timeout value, in units of milliseconds *)

<* EXTERNAL XtAppGetSelectionTimeout *>
   PROCEDURE AppGetSelectionTimeout(
     app:                    AppContext 	
): unsigned_int;

<* EXTERNAL XtGetSelectionTimeout *>
   PROCEDURE GetSelectionTimeout(): unsigned_int;

<* EXTERNAL XtGetSelectionRequest *>
   PROCEDURE GetSelectionRequest (
     widget:                 Widget;
     selection:              Atom;
     request_id:             RequestId 	
): XSelectionRequestEventStar;

<* EXTERNAL XtGetSelectionValueIncremental *>
   PROCEDURE GetSelectionValueIncremental (
     widget:                 Widget;
     selection:              Atom;
     target:                 Atom;
     selection_callback:     SelectionCallbackProc;
     client_data:            Pointer;
     time:                   Time 		
);

<* EXTERNAL XtGetSelectionValuesIncremental *>
   PROCEDURE GetSelectionValuesIncremental (
     widget:                 Widget;
     selection:              Atom;
     targets:                AtomStar;
     count:                  Int;
     callback:               SelectionCallbackProc;
     client_data:            PointerStar;
     time:                   Time 		
);

<* EXTERNAL XtGrabKey *>
   PROCEDURE GrabKey (
     widget:                 Widget;
     keysym:                 KeySym;
     modifiers:              Modifiers;
     owner_events:           Boolean;
     pointer_mode:           Int;
     keyboard_mode:          Int 		
);

<* EXTERNAL XtUngrabKey *>
   PROCEDURE UngrabKey (
     widget:                 Widget;
     keysym:                 KeySym;
     modifiers:              Modifiers	 	
);

<* EXTERNAL XtGrabKeyboard *>
   PROCEDURE GrabKeyboard (
     widget:                 Widget;
     owner_events:           Boolean;
     pointer_mode:           Int;
     keyboard_mode:          Int;
     time:                   Time 		
): Int;

<* EXTERNAL XtUngrabKeyboard *>
   PROCEDURE UngrabKeyboard (
     widget:                 Widget;
     time:                   Time 		
);

<* EXTERNAL XtGrabButton *>
   PROCEDURE GrabButton (
     widget:                 Widget;
     button:                 Int;
     modifiers:              Modifiers;
     owner_events:           Boolean;
     event_mask:             unsigned_int;
     pointer_mode:           Int;
     keyboard_mode:          Int;
     confine_to:             Window;
     cursor:                 Cursor 		
);

<* EXTERNAL XtUngrabButton *>
   PROCEDURE UngrabButton (
     widget:                 Widget;
     button:                 unsigned_int;
     modifiers:              Modifiers	 	
);

<* EXTERNAL XtGrabPointer *>
   PROCEDURE GrabPointer (
     widget:                 Widget;
     owner_events:           Boolean;
     event_mask:             unsigned_int;
     pointer_mode:           Int;
     keyboard_mode:          Int;
     confine_to:             Window;
     cursor:                 Cursor;
     time:                   Time 		
): Int;

<* EXTERNAL XtUngrabPointer *>
   PROCEDURE UngrabPointer (
     widget:                 Widget;
     time:                   Time 		
);

<* EXTERNAL XtGetApplicationNameAndClass *>
   PROCEDURE GetApplicationNameAndClass (
     dpy:                    DisplayStar;
     name_return:            StringStar;
     class_return:           StringStar		
);


(*===================================================================
 * $XConsortium: Composite.h,v 1.10 89/12/12 20:08:44 swick Exp $ *
 * $oHeader: Composite.h,v 1.2 asente Exp $ *
 ====================================================================*)

<* EXTERNAL XtManageChildren *>
   PROCEDURE ManageChildren (children: WidgetList; num_children: Cardinal);

<* EXTERNAL XtManageChild *>
   PROCEDURE ManageChild (child: Widget);

<* EXTERNAL XtUnmanageChildren *>
   PROCEDURE UnmanageChildren (children: WidgetList; num_children: Cardinal);

<* EXTERNAL XtUnmanageChild *>
   PROCEDURE UnmanageChild (child: Widget);


(* =====================================================
	The Geometry.h file IS NOT PROVIDED in X11R4 !!!
   ===================================================== *)

<* EXTERNAL XtMakeresizeRequest *>
   PROCEDURE MakeresizeRequest (
     widget:                Widget;
     width, height:         Dimension;
     replyWidth, replyHeight:  ADDRESS
): GeometryResult;

<* EXTERNAL XtResizeWindow *>
   PROCEDURE ResizeWindow (
     widget:                Widget
);

<* EXTERNAL XtResizeWidget *>
   PROCEDURE ResizeWidget (
     widget:                Widget;
     height, width, borderWidth: Dimension
);

<* EXTERNAL XtConfigureWidget *>
   PROCEDURE ConfigureWidget (
     widget:                Widget;
     x, y:                  Position;
     height, width, borderWidth: Dimension
);

<* EXTERNAL XtMoveWidget *>
   PROCEDURE MoveWidget (
     widget:                Widget;
     x, y:                  Position
);

<* EXTERNAL XtTranslateCoords *>
   PROCEDURE TranslateCoords (
     widget:                Widget;
     x, y:                  Position;
     rootx, rooty:          PositionStar
);


END Xt.

