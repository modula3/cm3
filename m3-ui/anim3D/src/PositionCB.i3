(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 22 16:49:19 PDT 1994 by najork                   *)
(*       Created on Fri Feb 18 09:49:48 PST 1994 by najork                   *)


(* In accordance with the terminology used by Trestle, we refer to mouse 
   movements as to {\em position events}. A position event is represented
   by a record "PositionCB.Rec".

   A {\em position event callback object} is an object which has one
   method, "invoke". Each geometric object has a stack of position callback
   objects. When a position event "pr" is relayed to a particular geometric
   "o", the the message "cb.invoke(pr)" is sent to the top callback
   object on "o"'s position callback stack (if "o"'s position callback stack 
   is empty, the event is simply dropped). It is "cb"'s reponsibility to
   perform whatever action is appropriate in the current context. *)

INTERFACE PositionCB;

IMPORT CB, Point, ProxiedObj, VBT;

TYPE
  T <: Public;
  Public = ProxiedObj.T OBJECT
  METHODS
    init () : T;
    invoke (pr : Rec) RAISES {CB.BadMethod};
  END;
(* "PositionCB.T" is the abstract class of position callback objects. If "o" 
   is a geometric object, "c" is the top object in its position callback stack,
   and a position event "pr" gets relayed to "o", then "cb.invoke(pr)" will be
   called. It is up to the user to create subclasses of "PositionCB.T" that 
   handle position events in a given context appropriately. *)

  Rec = RECORD
    pos2D     : Point.T;
    modifiers : VBT.Modifiers;
  END;
(* "PositionCB.Rec" is a record type containing information about a position 
   event. "pos2D" is the position of the mouse when the movement occurred.
   "modifiers" is the set of modifiers (Shift, Control, Mouse Buttons, etc.) 
   that was active when the movement took place.

   {\em NOTE: There is a fair chance that I will add other fields to "Rec",
   once we have gained more experience with event handling. So far, I pretty
   much mimick (part of) what is there in "VBT.PositionRec".} *)

END PositionCB.
