(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Sat Sep  4 16:41:01 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>



INTERFACE DialogMenu;

IMPORT ZHandleVBT;

(* In this implementation only 2 level menus are supported *)

TYPE
 T = RECORD
   Level : CARDINAL := 0;
   (* 0 is the top level - subsequent levels are 1, 2... *)
   Label, Name : TEXT;
   (* The Name "RIDGE" is  reserved for ridges *)
   inForeGround : BOOLEAN := TRUE;
   isLocal : BOOLEAN := TRUE;
   executeAt : TEXT := "";
   initialState : TEXT := "Active";
   callback : TEXT := "";
 END;

PROCEDURE Initialize();
(* attach to procs *)

PROCEDURE LoadAttributes(nv : ZHandleVBT.T);
(* this loads up the attribute page with the menu in the given formnode  *)
(* ZHandleVBT is NARROWed to a NodeVBT.FormNode *)
(* (ZHandleVBT.T is being used  to avoid circular imports ) *)
(* if nv.Menu is NIL then the menu browser will be empty                 *)
(* subsequently, pressing apply causes the new menu to be installed in   *)
(* place of the old one. Till then a duplicate array is maintained for   *)
(* internal use                                                          *)
(* Note that the Apply button for the Menu applies to the menu alone     *)
(* The regular Apply button saves the attributes of the remaining fields *)
(* in the form *)


PROCEDURE ComputeMenuSX (n : ZHandleVBT.T) : TEXT; 
(* generates s-expression as a sequence of terms in the menu bar (hbox) *)


PROCEDURE ComputeMenuObjDefs (n : ZHandleVBT.T) : TEXT; 
(* generates a sequence of object defs *)

PROCEDURE ComputeMenuCallbacks (n : ZHandleVBT.T) : TEXT; 
(* generates a sequence of callback definitions *)

PROCEDURE ComputeMenuAttachments (n: ZHandleVBT.T): TEXT;
(* generates a sequence of attachments  *)

(* PROCEDURE RenderMenuBar (n : ZHandleVBT.T); (* render the menu bar *) *)

END DialogMenu.













