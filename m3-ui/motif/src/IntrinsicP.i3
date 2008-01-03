(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 08:27:30 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE INTERFACE IntrinsicP;
(*
Abstract: From X11R5 intrinsicP, Core and CoreP.h

8/1/94    H. George
          Initial version
*)

IMPORT X,Xt,Xrm;

FROM Ctypes IMPORT unsigned_long;

(*============*)
(*intrinsicP  *)
(*============*)

TYPE
  XtAcceptFocusProc=PROCEDURE(widget: Xt.Widget; 
                              time: X.Time);

  XtAlmostProc=PROCEDURE(old: Xt.Widget;
                         set: Xt.Widget;
			 request: Xt.WidgetGeometryStar;
			 reply:   Xt.WidgetGeometryStar);

  XtArgsFunc = PROCEDURE(widget: Xt.Widget;
                         args: Xt.ArgList;
			 num_args: Xt.CardinalStar): Xt.Boolean;
 
  XtArgsProc = PROCEDURE(widget: Xt.Widget;
                         args: Xt.ArgList;
			 num_args: Xt.CardinalStar);

  XtBoundActions = UNTRACED REF Xt.ActionProc;
  
  XtGeometryHandler = PROCEDURE(w: Xt.Widget;
                                request: UNTRACED REF Xt.GeometryResult;
				geometry_return: UNTRACED REF Xt.GeometryResult
                               ): Xt.GeometryResult;


  XtInitProc = PROCEDURE(request: Xt.Widget;
                         init: Xt.Widget;
			 args: Xt.ArgList;
			 num_args: Xt.CardinalStar);

  XtProc = PROCEDURE();

  XtRealizeProc = PROCEDURE(widget: Xt.Widget;
                            mask: Xt.ValueMask;
			    attributes: X.XSetWindowAttributes);
               
  XtSetValuesFunc = PROCEDURE(old:     Xt.Widget;
                              request: Xt.Widget;
			      new:     Xt.Widget;
			      args:    Xt.ArgList;
			      num_args:Xt.CardinalStar):Xt.Boolean; 

  XtStringProc = PROCEDURE(widget:Xt.Widget;
                           str: Xt.String);

  XtTMRec = RECORD
              translations: Xt.Translations; 
	      proc_table: XtBoundActions;
              current_state: ADDRESS;
	      lastEventTime: unsigned_long;
            END;	      

  XtVersionType = unsigned_long; 

  XtWidgetProc=PROCEDURE(widget:Xt.Widget);  

  XtWidgetClassProc = PROCEDURE(class:Xt.WidgetClass);

(*===============*)
(*Core and CoreP *)
(*===============*)
TYPE
  CorePart  = RECORD
                self: Widget;              (*pointer to widget itself*)
		widget_class: WidgetClass; (*pointer to Widget's ClassRec*)
                parent: Xt.Widget;         (*parent widget*)
		xrm_name: Xrm.Name;        (*widget resource name quarkified*)
		being_destroyed:Xt.Boolean;(*marked for destroy*)
		destroy_callbacks:Xt.CallbackList; (*who to call when widget destroyed*)
                constraints: Xt.Pointer;   (*constraint record*)
                x,y:Xt.Position;           (*window position*)
		width,height:Xt.Dimension; (*window dimensions*)
		border_width: Xt.Dimension;(*window border width*)
		managed:Xt.Boolean;        (*is geometry managed?*)
		sensitive: Xt.Boolean;     (*is sensitive to user events?*)
		ancestor_sensitive: Xt.Boolean; (*are all ancestors sensitive?*)
		event_table: Xt.EventTable;(*private event dispatcher*)
		tm: XtTMRec;               (*translation management*)
		accelerators:Xt.Translations; (*accelerator translations*)
		border_pixel: Xt.Pixel;    (*window border pixel*)
		border_pixmap: X.Pixmap;   (*window border pixmap or NIL*)
		popup_list: Xt.WidgetList; (*list of popups*)
		num_popups: Xt.Cardinal;   (*how many popups*)
		name: Xt.String;           (*widget resource name*)
		screen: X.ScreenStar;      (*window's screen*)
		colormap: X.Colormap;      (*colormap*)
		window: X.Window;          (*window ID*)
		depth: Xt.Cardinal;        (*number of planes in window*)
		background_pixel: Xt.Pixel;(*window background pixel*)
		background_pixmap: X.Pixmap;(*window background pixmap or NIL*)
		visible: Xt.Boolean;       (*is window mapped and not occluded?*)
		mapped_when_managed: Xt.Boolean; (*map window if it is managed?*)
              END;  
		
  CoreClassPart = RECORD
                superclass: Xt.WidgetClass;(*pointer to superclass ClassRec*)
                class_name: Xt.String;     (*widget resource class name*)
		widget_size:Xt.Cardinal;   (*size in bytes of widget record*)
		class_initialize: XtProc;  (*class initialization proc*)
		class_part_initialize: XtWidgetClassProc; (*dynamic initialization*)
		class_inited: Xt.Enum;     (*has class been initialized?*)
		initialize:XtInitProc;     (*initialize subclass fields*)
		initialize_hook:XtArgsProc;(*notify that initialize called*)
		realize: XtRealizeProc;    (*XCreateWindow for widget*)
		actions: Xt.ActionList;    (*widget semantics name to proc map*)
		num_actions: Xt.Cardinal;  (*number of entries in actions*)
		resources: Xt.ResourceList;(*resources fro subclass fields*)
		num_resources: Xt.Cardinal;(*number of entries in resources*)
		xrm_class: Xrm.Class;      (*resource class quarkified*)
		compress_motion: Xt.Boolean; (*compress MotionNotify for widget?*)
		compress_exposure: Xt.Enum;(*compress ExposureNotify for widget?*)
		compress_enterleave:Xt.Boolean; (*compress enter and leave events?*)
		visible_interest: Xt.Boolean;(*select for VisibilityNotify*)
		destroy: XtWidgetProc;     (*free data for subclass pointers*)
		resize: XtWidgetProc;      (*geom manager changed widget size*)
		expose: XtWidgetProc;      (*redisplay window*)
		set_values: XtSetValuesFunc;(*set subclass resource values*)
		set_values_hook: XtArgsFunc;(*notify that set_values called*)
		set_values_almost: XtAlmostProc; (*set_values got "Almost" geo reply*)
		get_values_hook: XtArgsProc;(*notify that get_values called*)
		accept_focus: XtAcceptFocusProc; (*assign input focus to widget*)
		version: XtVersionType;   (*version of intrinsics used*)
		callback_private: Xt.Pointer;(*list of callback offsets*)
		tm_table: Xt.String;       (*state machine*)
		query_geometry: XtGeometryHandler; (*return preferred geometry*)
		display_accelerator: XtStringProc; (*display your accelerator*)
                extension: Xt.Pointer;     (*pointer to extension record*)
	      END;  


  WidgetRec = RECORD
                core: CorePart;
              END;

  WidgetClassRec = RECORD
                core_class: CoreClassPart;
	      END;
    
  Widget    = UNTRACED REF WidgetRec;
  WidgetClass=UNTRACED REF WidgetClassRec;

<*EXTERNAL*> 
VAR coreWidgetClass: WidgetClass;

END IntrinsicP.
