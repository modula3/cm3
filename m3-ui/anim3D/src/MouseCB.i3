(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 22 16:30:02 PDT 1994 by najork                   *)
(*       Created on Fri Feb 18 09:37:37 PST 1994 by najork                   *)


(* In accordance with the terminology used by Trestle, we refer to mouse 
   button transitions as to {\em mouse events}. A mouse event is represented
   by a record "MouseCB.Rec".

   A {\em mouse event callback object} is an object which has one
   method, "invoke". Each geometric object has a stack of mouse callback
   objects. When a mouse event "mr" is relayed to a particular geometric
   "o", the the message "cb.invoke(mr)" is sent to the top callback
   object on "o"'s mouse callback stack (if "o"'s mouse callback stack is
   empty, the event is simply dropped). It is "cb"'s reponsibility to
   perform whatever action is appropriate in the current context. *)

INTERFACE MouseCB;

IMPORT CB, Point, ProxiedObj, VBT;

TYPE 
  T <: Public;
  Public = ProxiedObj.T OBJECT 
  METHODS 
    init () : T;
    invoke (mr : Rec) RAISES {CB.BadMethod};
  END;
(* "MouseCB.T" is the abstract class of mouse callback objects. If "o" is a 
   geometric object, "c" is the top object in its mouse callback stack, and 
   a mouse event "mr" gets relayed to "o", then "cb.invoke(mr)" will be called.
   It is up to the user to create subclasses of "MouseCB.T" that handle
   mouse events in a given context appropriately. *)

  Rec = RECORD
    pos2D       : Point.T;
    whatChanged : VBT.Button;
    modifiers   : VBT.Modifiers;
    clickType   : VBT.ClickType;
  END;
(* "MouseCB.Rec" is a record type containing information about a mouse event.
   "pos2D" is the position of the mouse when the button transition occurred.
   "whatChanged" is the button that went up or down.
   "modifiers" is the set of modifiers (Shift, Control, Mouse Buttons, etc.) 
   that was active when the transition took place.
   "clickType" indicates whether the button went down or up, and whether 
   any other button was already or still down.

   {\em NOTE: There is a fair chance that I will add other fields to "Rec",
   once we have gained more experience with event handling. So far, I pretty
   much mimick (part of) what is there in "VBT.MouseRec".} *)

END MouseCB.
