(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Oct  6 10:33:14 PDT 1992 by birrell*)

(* This module provides a user interface for moving a Trestle window
   from one screen to another. *)

INTERFACE Rescreen;

IMPORT Rsrc, VBT;

PROCEDURE DoIt(form, confirm, cancel: TEXT; path: Rsrc.Path; window: VBT.T);
  (* LL.sup = VBT.mu *)
  (* "form" names a FormsVBT form source, in which "confirm" and
     "cancel" name events, presumably buttons; "path" is for use with "form";
     "window" is an installed (or at least attached) Trestle window.  This
     procedure uses the form to create a window on each screen, then returns. 
     If one of these created windows receives a cancel event, all of them are
     deleted and nothing further happens.  Alternatively if one of these
     windows receives a confirm event the given window is moved to the screen
     containing the window that reeived the confirm event, and all the created
     windows are destroyed.   Any errors encountered are fatal. *)

PROCEDURE Screens(window: VBT.T): INTEGER;
  (* LL.sup = VBT.mu *)
  (* Returns the number of screens on the Trestle instance to which
     "window" is attached.  Any errors encountered are fatal *)

END Rescreen.
