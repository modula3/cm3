(* Copyright (C) 1995, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 16 08:32:36 PDT 1995 by kalsow   *)
(*                                                           *)
(* Contributed by Harry George (hgeorge@eskimo.com), 5/16/95 *)

UNSAFE INTERFACE Xmw;
(*
Abstract:  X Motif widgets.  For each, we provide:
              a) its class id as a VAR
              b) opaque treatment of class and widget types
                 (you have to do your own "Private" defs as needed)
              c) Is<classname> ftn, which is a macro in C
                 but done as a true ftn here.
              d) external refs to Create ftn and
                 assorted other ftns as needed

8/3/94     H. George
           Initial version
*)

IMPORT X, Xt, Xm;
FROM Ctypes IMPORT char_star, int, int_star, int_star_star;


(*=========================
   Template:
   Copy the template to correct spot in the
   list (alpha order).  Then do str replace
   XXX --> <widget name>.  Then add additional
   ftns as needed.

(*-------------------*)
(* XXX       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmXXXWidgetClass:Xt.WidgetClass;

TYPE XXXWidgetClass = UNTRACED ROOT;
TYPE XXXWidget      = UNTRACED ROOT;

PROCEDURE IsXXX(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateXXX":C *>
PROCEDURE CreateXXX( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

==========================*)



(*-------------------*)
(* ArrowButton       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmArrowButtonWidgetClass:Xt.WidgetClass;

TYPE ArrowButtonWidgetClass = UNTRACED ROOT;
TYPE ArrowButtonWidget      = UNTRACED ROOT;

PROCEDURE IsArrowButton(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateArrowButton":C *>
PROCEDURE CreateArrowButton( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* BulletinBoard       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmBulletinBoardWidgetClass:Xt.WidgetClass;

TYPE BulletinBoardWidgetClass = UNTRACED ROOT;
TYPE BulletinBoardWidget      = UNTRACED ROOT;

PROCEDURE IsBulletinBoard(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateBulletinBoard":C *>
PROCEDURE CreateBulletinBoard( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateBulletinBoardDialog":C *>
PROCEDURE CreateBulletinBoardDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;


(*-------------------*)
(* CascadeButton       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmCascadeButtonWidgetClass:Xt.WidgetClass;

<*EXTERNAL*> 
VAR xmCascadeButtonGadgetClass:Xt.WidgetClass;

TYPE CascadeButtonWidgetClass = UNTRACED ROOT;
TYPE CascadeButtonWidget      = UNTRACED ROOT;

TYPE CascadeButtonGadgetClass = UNTRACED ROOT;
TYPE CascadeButtonGadget      = UNTRACED ROOT;

PROCEDURE IsCascadeButton(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateCascadeButton":C *>
PROCEDURE CreateCascadeButton( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCascadeButtonHighlight":C*>
PROCEDURE CascadeButtonHighlight(w:Xt.Widget;
	                           highlight:Xt.Boolean);


(*-------------------*)
(* DialogShell       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmDialogShellWidgetClass:Xt.WidgetClass;

TYPE DialogShellWidgetClass = UNTRACED ROOT;
TYPE DialogShellWidget      = UNTRACED ROOT;

PROCEDURE IsDialogShell(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateDialogShell":C *>
PROCEDURE CreateDialogShell( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* DrawingArea       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmDrawingAreaWidgetClass:Xt.WidgetClass;

TYPE DrawingAreaWidgetClass = UNTRACED ROOT;
TYPE DrawingAreaWidget      = UNTRACED ROOT;

PROCEDURE IsDrawingArea(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateDrawingArea":C *>
PROCEDURE CreateDrawingArea( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* DrawnButton       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmDrawnButtonWidgetClass:Xt.WidgetClass;

TYPE DrawnButtonWidgetClass = UNTRACED ROOT;
TYPE DrawnButtonWidget      = UNTRACED ROOT;

PROCEDURE IsDrawnButton(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateDrawnButton":C *>
PROCEDURE CreateDrawnButton( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* Form              *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmFormWidgetClass:Xt.WidgetClass;

TYPE FormWidgetClass = UNTRACED ROOT;
TYPE FormWidget      = UNTRACED ROOT;

PROCEDURE IsForm(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateForm":C *>
PROCEDURE CreateForm( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateFormDialog":C *>
PROCEDURE CreateFormDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* Frame             *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmFrameWidgetClass:Xt.WidgetClass;

TYPE FrameWidgetClass = UNTRACED ROOT;
TYPE FrameWidget      = UNTRACED ROOT;

PROCEDURE IsFrame(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateFrame":C *>
PROCEDURE CreateFrame( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* Label             *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmLabelWidgetClass:Xt.WidgetClass;

TYPE LabelWidgetClass = UNTRACED ROOT;
TYPE LabelWidget      = UNTRACED ROOT;

PROCEDURE IsLabel(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateLabel":C *>
PROCEDURE CreateLabel( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* List              *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmListWidgetClass:Xt.WidgetClass;

CONST
  INITIAL       = 0;
  ADDITION      = 1;
  MODIFICATION  = 2;

TYPE ListWidgetClass = UNTRACED ROOT;
TYPE ListWidget      = UNTRACED ROOT;

PROCEDURE IsList(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateList":C *>
PROCEDURE CreateList( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateScrolledList":C *>
PROCEDURE CreateScrolledList( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmListAddItem"*>
PROCEDURE ListAddItem(w: Xt.Widget;
	                item: Xm.String;
			pos: int);

<*EXTERNAL "XmListAddItems"*>
PROCEDURE ListAddItems(w: Xt.Widget;
	                items: Xt.StringStar;
			item_count, pos: int);

<*EXTERNAL "XmListAddItemUnselected"*>
PROCEDURE ListAddItemUnselected(w: Xt.Widget;
	                item: Xm.String;
			pos: int);

<*EXTERNAL "XmListDeleteItem"*>
PROCEDURE ListDeleteItem(w: Xt.Widget;
	                item: Xm.String);

<*EXTERNAL "XmListDeleteItems"*>
PROCEDURE ListDeleteItems(w: Xt.Widget;
	                items: Xt.StringStar;
			item_count: int);

<*EXTERNAL "XmListDeletePos"*>
PROCEDURE ListDeletePos(w: Xt.Widget;
			pos: int);

<*EXTERNAL "XmListDeleteItemsPos"*>
PROCEDURE ListDeleteItemsPos(w: Xt.Widget;
			item_count,pos: int);


<*EXTERNAL "XmListDeleteAllItems"*>
PROCEDURE ListDeleteAllItems(w: Xt.Widget);

<*EXTERNAL "XmListReplaceItems"*>
PROCEDURE ListReplaceItems(w: Xt.Widget;
	                old_items: Xt.StringStar;
			item_count:int;
			new_items: Xt.StringStar);

<*EXTERNAL "XmListReplaceItemsPos"*>
PROCEDURE ListReplaceItemsPos(w: Xt.Widget;
	                new_items: Xt.StringStar;
			item_count,pos:int);

<*EXTERNAL "XmListSelectItem"*>
PROCEDURE ListSelectItem(w: Xt.Widget;
	                item: Xm.String;
			notify:Xt.Boolean);

<*EXTERNAL "XmListDeselectItem"*>
PROCEDURE ListDeselectItem(w: Xt.Widget;
	                item: Xm.String);

<*EXTERNAL "XmListDeselectPos"*>
PROCEDURE ListDeselectPos(w: Xt.Widget;
	                pos:int);

<*EXTERNAL "XmListDeselectAllItems"*>
PROCEDURE ListDeselectAllItems(w: Xt.Widget);

<*EXTERNAL "XmListSetPos"*>
PROCEDURE ListSetPos(w: Xt.Widget;
	                pos:int);

<*EXTERNAL "XmListSetBottomPos"*>
PROCEDURE ListSetBottomPos(w: Xt.Widget;
	                pos:int);

<*EXTERNAL "XmListSetItem"*>
PROCEDURE ListSetItem(w: Xt.Widget;
	                item: Xm.String);

<*EXTERNAL "XmListSetBottomItem"*>
PROCEDURE ListSetBottomItem(w: Xt.Widget;
	                item: Xm.String);

<*EXTERNAL "XmListSetAddMode"*>
PROCEDURE ListSetAddMode(w: Xt.Widget;
	                add_mode: Xt.Boolean);

<*EXTERNAL "XmListItemExists"*>
PROCEDURE ListItemExists(w: Xt.Widget;
	                item: Xm.String): Xt.Boolean;

<*EXTERNAL "XmListItemPos"*>
PROCEDURE ListItemPos(w: Xt.Widget;
	                item: Xm.String):int;

<*EXTERNAL "XmListGetMatchPos"*>
PROCEDURE ListGetMatchPos(w: Xt.Widget;
	                item: Xm.String;
			pos_list: int_star_star;
			pos_count: int_star);

<*EXTERNAL "XmListGetSelectedPos"*>
PROCEDURE ListGetSelectedPos(w: Xt.Widget;
			pos_list: int_star_star;
			pos_count: int_star);

<*EXTERNAL "XmListSetHorizPos"*>
PROCEDURE ListSetHorizPos(w: Xt.Widget;
			pos: int);


(*-------------------*)
(* MainWindow        *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmMainWindowWidgetClass:Xt.WidgetClass;

TYPE MainWindowWidgetClass = UNTRACED ROOT;
TYPE MainWindowWidget      = UNTRACED ROOT;

PROCEDURE IsMainWindow(w:Xt.Widget):Xt.Boolean;


<*EXTERNAL "XmCreateMainWindow":C *>
PROCEDURE CreateMainWindow( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;


<*EXTERNAL "XmMainWindowSetAreas":C *>
PROCEDURE MainWindowSetAreas (w, menu, command, hscroll, vscroll, wregion:Xt.Widget);

<*EXTERNAL "XmMainWindowSep1"*>
PROCEDURE MainWindowSep1(w: Xt.Widget): Xt.Widget;

<*EXTERNAL "XmMainWindowSep2"*>
PROCEDURE MainWindowSep2(w: Xt.Widget): Xt.Widget;

<*EXTERNAL "XmMainWindowSep3"*>
PROCEDURE MainWindowSep3(w: Xt.Widget): Xt.Widget;


(*-------------------*)
(* MenuShell       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmMenuShellWidgetClass:Xt.WidgetClass;

TYPE MenuShellWidgetClass = UNTRACED ROOT;
TYPE MenuShellWidget      = UNTRACED ROOT;

PROCEDURE IsMenuShell(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateMenuShell":C *>
PROCEDURE CreateMenuShell( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* MessageBox       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmMessageBoxWidgetClass:Xt.WidgetClass;

TYPE MessageBoxWidgetClass = UNTRACED ROOT;
TYPE MessageBoxWidget      = UNTRACED ROOT;

PROCEDURE IsMessageBox(w:Xt.Widget):Xt.Boolean;


<*EXTERNAL "XmCreateMessageBox":C *>
PROCEDURE CreateMessageBox( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateMessageDialog":C *>
PROCEDURE CreateMessageDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateErrorDialog":C *>
PROCEDURE CreateErrorDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateInformationDialog":C *>
PROCEDURE CreateInformationDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateQuestionDialog":C *>
PROCEDURE CreateQuestionDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateWarningDialog":C *>
PROCEDURE CreateWarningDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateWorkingDialog":C *>
PROCEDURE CreateWorkingDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;


<*EXTERNAL "XmMessageBoxGetChild":C *>
PROCEDURE MessageBoxGetChild( parent:Xt.Widget;
	                 child: int): Xt.Widget;



(*-------------------*)
(* PanedWindow       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmPanedWindowWidgetClass:Xt.WidgetClass;

TYPE PanedWindowWidgetClass = UNTRACED ROOT;
TYPE PanedWindowWidget      = UNTRACED ROOT;

PROCEDURE IsPanedWindow(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreatePanedWindow":C *>
PROCEDURE CreatePanedWindow( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;
(*-------------------*)
(* PushButton       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmPushButtonWidgetClass:Xt.WidgetClass;

<*EXTERNAL*> 
VAR xmPushButtonGadgetClass:Xt.WidgetClass;

TYPE PushButtonWidgetClass = UNTRACED ROOT;
TYPE PushButtonWidget      = UNTRACED ROOT;

TYPE PushButtonGadgetClass = UNTRACED ROOT;
TYPE PushButtonGadget      = UNTRACED ROOT;

PROCEDURE IsPushButton(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreatePushButton":C *>
PROCEDURE CreatePushButton( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* RowColumn       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmRowColumnWidgetClass:Xt.WidgetClass;

TYPE RowColumnWidgetClass = UNTRACED ROOT;
TYPE RowColumnWidget      = UNTRACED ROOT;

PROCEDURE IsRowColumn(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateRowColumn":C *>
PROCEDURE CreateRowColumn( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateRadioBox":C *>
PROCEDURE CreateRadioBoxn( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateWorkArea":C *>
PROCEDURE CreateWorkArea( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreatePopupMenu":C *>
PROCEDURE CreatePopupMenu( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreatePulldownMenu":C *>
PROCEDURE CreatePulldownMenu( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateOptionMenu":C *>
PROCEDURE CreateOptionMenu( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreateMenuBar":C *>
PROCEDURE CreateMenuBar( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmMenuPosition":C *>
PROCEDURE MenuPosition(w:Xt.Widget;
	                 event: X.XButtonPressedEventStar);

<*EXTERNAL "XmOptionLabelGadget":C *>
PROCEDURE OptionLabelGadget(w:Xt.Widget):Xt.Widget;

<*EXTERNAL "XmOptionButtonGadget":C *>
PROCEDURE OptionButtonGadget(w:Xt.Widget):Xt.Widget;

<*EXTERNAL "XmGetPostedFromWidget":C *>
PROCEDURE GetPostedFromWidget(w:Xt.Widget):Xt.Widget;

(*-------------------*)
(* Scale       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmScaleWidgetClass:Xt.WidgetClass;

TYPE ScaleWidgetClass = UNTRACED ROOT;
TYPE ScaleWidget      = UNTRACED ROOT;

PROCEDURE IsScale(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateScale":C *>
PROCEDURE CreateScale( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmScaleGetValue":C*>
PROCEDURE ScaleGetValue(w: Xt.Widget;
	                 value: int_star);

<*EXTERNAL "XmScaleSetValue":C*>
PROCEDURE ScaleSetValue(w: Xt.Widget;
	                 value: int);




(*-------------------*)
(* ScrollBar       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmScrollBarWidgetClass:Xt.WidgetClass;

TYPE ScrollBarWidgetClass = UNTRACED ROOT;
TYPE ScrollBarWidget      = UNTRACED ROOT;

PROCEDURE IsScrollBar(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateScrollBar":C *>
PROCEDURE CreateScrollBar( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmScrollBarGetValues":C*>
PROCEDURE ScrollBarGetValues(w: Xt.Widget;
	                 value: int_star;
			 slider_size: int_star;
			 increment: int_star;
			 page_increment: int_star);

<*EXTERNAL "XmScrollBarSetValues":C*>
PROCEDURE ScrollBarSetValues(w: Xt.Widget;
	                 value: int;
			 slider_size: int;
			 increment: int;
			 page_increment: int;
			 notify: Xt.Boolean);

<*EXTERNAL "_XmSetEtchedSlider":C *>
PROCEDURE SetEtchedSlider(w:ScrollBarWidget); 


(*-------------------*)
(* ScrolledWindow       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmScrolledWindowWidgetClass:Xt.WidgetClass;

TYPE ScrolledWindowWidgetClass = UNTRACED ROOT;
TYPE ScrolledWindowWidget      = UNTRACED ROOT;

PROCEDURE IsScrolledWindow(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateScrolledWindow":C *>
PROCEDURE CreateScrolledWindow( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "InitializeScrollBars"*>
PROCEDURE InitializeScrollBars(w: Xt.Widget);

<*EXTERNAL "XmScrolledWindowSetAreas"*>
PROCEDURE ScrolledWindowSetAreas(w: Xt.Widget;
                         hscroll: Xt.Widget;
			 vscroll: Xt.Widget;
			 wregion: Xt.Widget);

(*-------------------*)
(* SelectionBox       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmSelectionBoxWidgetClass:Xt.WidgetClass;

TYPE SelectionBoxWidgetClass = UNTRACED ROOT;
TYPE SelectionBoxWidget      = UNTRACED ROOT;

PROCEDURE IsSelectionBox(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateSelectionBox":C *>
PROCEDURE CreateSelectionBox( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;


<*EXTERNAL "XmCreateSelectionDialog":C *>
PROCEDURE CreateSelectionDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmCreatePromptDialog":C *>
PROCEDURE CreatePromptDialog( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmSelectionBoxGetChild":C *>
PROCEDURE SelectionBoxGetChild(w:Xt.Widget;
	                 child: int);

(*-------------------*)
(* Separator       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmSeparatorWidgetClass:Xt.WidgetClass;

<*EXTERNAL*> 
VAR xmSeparatorGadgetClass:Xt.WidgetClass;

TYPE SeparatorWidgetClass = UNTRACED ROOT;
TYPE SeparatorWidget      = UNTRACED ROOT;

TYPE SeparatorGadgetClass = UNTRACED ROOT;
TYPE SeparatorGadget      = UNTRACED ROOT;

PROCEDURE IsSeparator(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateSeparator":C *>
PROCEDURE CreateSeparator( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;




(*-------------------*)
(* Text ???? Incomplete???      *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmTextWidgetClass:Xt.WidgetClass;

TYPE TextWidgetClass = UNTRACED ROOT;
TYPE TextWidget      = UNTRACED ROOT;
TYPE TextSource      = UNTRACED ROOT;

PROCEDURE IsText(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateText":C *>
PROCEDURE CreateText( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-------------------*)
(* TextField         *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmTextFieldWidgetClass:Xt.WidgetClass;

TYPE TextFieldWidgetClass = UNTRACED ROOT;
TYPE TextFieldWidget      = UNTRACED ROOT;

PROCEDURE IsTextField(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateTextField":C *>
PROCEDURE CreateTextField( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

<*EXTERNAL "XmTextFieldGetString":C *>
PROCEDURE TextFieldGetString(w:Xt.Widget): char_star;
	                 
<*EXTERNAL "XmTextFieldGetLastPosition":C *>
PROCEDURE TextFieldGetLastPosition(w:Xt.Widget): Xm.TextPosition;

<*EXTERNAL "XmTextFieldSetString":C *>
PROCEDURE TextFieldSetString(w:Xt.Widget;
	                 value: char_star);

<*EXTERNAL "XmTextFieldReplace":C *>
PROCEDURE TextFieldReplace(w:Xt.Widget;
	                 from_pos,to_pos: Xm.TextPosition;                 
			 value: char_star);

<*EXTERNAL "XmTextFieldInsert":C *>
PROCEDURE TextFieldInsert(w:Xt.Widget;
	                 pos: Xm.TextPosition;                 
			 value: char_star);

<*EXTERNAL "XmTextFieldSetAddMode":C *>
PROCEDURE TextFieldSetAddMode(w:Xt.Widget;
	                 state: Xt.Boolean);
                    
<*EXTERNAL "XmTextFieldGetAddMode":C *>
PROCEDURE TextFieldGetAddMode(w:Xt.Widget):Xt.Boolean;
                    
<*EXTERNAL "XmTextFieldGetEditable":C *>
PROCEDURE TextFieldGetEditable(w:Xt.Widget):Xt.Boolean;
                    
<*EXTERNAL "XmTextFieldSetEditable":C *>
PROCEDURE TextFieldSetEditable(w:Xt.Widget;
	                 state: Xt.Boolean);

<*EXTERNAL "XmTextFieldGetMaxLength":C *>
PROCEDURE TextFieldGetMaxLength(w:Xt.Widget):int;

<*EXTERNAL "XmTextFieldSetMaxLength":C *>
PROCEDURE TextFieldSetMaxLength(w:Xt.Widget;
	                 max_length:int);

<*EXTERNAL "XmTextFieldGetCursorPosition":C *>
PROCEDURE TextFieldGetCursorPosition(w:Xt.Widget):Xm.TextPosition;

<*EXTERNAL "XmTextFieldSetCursorPosition":C *>
PROCEDURE TextFieldSetCursorPosition(w:Xt.Widget;
	                 position: Xm.TextPosition);

<*EXTERNAL "XmTextFieldGetInsertionPosition":C *>
PROCEDURE TextFieldGetInsertionPosition(w:Xt.Widget):Xm.TextPosition;

<*EXTERNAL "XmTextFieldSetInsertionPosition":C *>
PROCEDURE TextFieldSetInsertionPosition(w:Xt.Widget;
	                 position: Xm.TextPosition);

<*EXTERNAL "XmTextFieldGetSelectionPosition":C *>
PROCEDURE TextFieldGetSelectionPosition(w:Xt.Widget):Xm.TextPosition;

<*EXTERNAL "XmTextFieldSetSelectionPosition":C *>
PROCEDURE TextFieldSetSelectionPosition(w:Xt.Widget;                 
			 left,right: UNTRACED REF Xm.TextPosition): Xt.Boolean;

<*EXTERNAL "XmTextFieldGetSelection":C *>
PROCEDURE TextFieldGetSelection(w:Xt.Widget):char_star;

<*EXTERNAL "XmTextFieldRemove":C *>
PROCEDURE TextFieldRemove(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmTextFieldCopy":C *>
PROCEDURE TextFieldCopy(w:Xt.Widget;
                          clip_time: X.Time):Xt.Boolean;

<*EXTERNAL "XmTextFieldPaste":C *>
PROCEDURE TextFieldPaste(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmTextFieldClearSelection":C *>
PROCEDURE TextFieldClearSelection(w:Xt.Widget;
                          sel_time: X.Time);

<*EXTERNAL "XmTextFieldSetSelection":C *>
PROCEDURE TextFieldSetSelection(w:Xt.Widget;
                          first,last: Xm.TextPosition;
                          sel_time: X.Time);

<*EXTERNAL "XmTextFieldXYToPos":C *>
PROCEDURE TextFieldXYToPos(w:Xt.Widget;
                          position: Xm.TextPosition;
			  x,y: Xt.Position): Xm.TextPosition;

<*EXTERNAL "XmTextFieldPosToXY":C *>
PROCEDURE TextFieldPosToXY(w:Xt.Widget;
                          position: Xm.TextPosition;
			  x,y: Xt.PositionStar): Xt.Boolean;

<*EXTERNAL "XmTextFieldShowPosition":C *>
PROCEDURE TextFieldShowPosition(w:Xt.Widget;
                          position: Xm.TextPosition);

<*EXTERNAL "XmTextFieldSetHighlight":C *>
PROCEDURE TextFieldSetHighlight(w:Xt.Widget;
                          left,right: Xm.TextPosition;
			  mode: Xm.XmHighlightMode);

<*EXTERNAL "XmTextFieldGetBaseLine":C *>
PROCEDURE TextFieldGetBaseLine(w:Xt.Widget): int;

<*EXTERNAL "XmTextFieldGetBaseline":C *>
PROCEDURE TextFieldGetBaseline(w:Xt.Widget): int;


(*-------------------*)
(* ToggleButton       *)
(*-------------------*)
<*EXTERNAL*> 
VAR xmToggleButtonWidgetClass:Xt.WidgetClass;

TYPE ToggleButtonWidgetClass = UNTRACED ROOT;
TYPE ToggleButtonWidget      = UNTRACED ROOT;

PROCEDURE IsToggleButton(w:Xt.Widget):Xt.Boolean;

<*EXTERNAL "XmCreateToggleButton":C *>
PROCEDURE CreateToggleButton( parent:Xt.Widget;
	                 name: char_star;
			 args: UNTRACED REF Xt.ArgList;
			 argCount:Xt.Cardinal): Xt.Widget;

(*-----------------*)
END Xmw.

