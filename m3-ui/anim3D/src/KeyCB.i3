(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jul 22 17:31:34 PDT 1994 by najork                   *)
(*       Created on Fri Feb 18 09:53:07 PST 1994 by najork                   *)


(* In accordance with the terminology used by Trestle, we refer to key 
   transitions as to {\em key events}. A key event is represented
   by a record "KeyCB.Rec".

   A {\em key event callback object} is an object which has one
   method, "invoke". Each geometric object has a stack of key callback
   objects. When a key event "kr" is relayed to a particular geometric
   "o", the the message "cb.invoke(kr)" is sent to the top callback
   object on "o"'s key callback stack (if "o"'s key callback stack is
   empty, the event is simply dropped). It is "cb"'s reponsibility to
   perform whatever action is appropriate in the current context. *)

INTERFACE KeyCB;

IMPORT CB, ProxiedObj, VBT;

TYPE
  T <: Public;
  Public = ProxiedObj.T OBJECT 
  METHODS
    init () : T;
    invoke (kr : Rec) RAISES {CB.BadMethod};
  END;
(* "KeyCB.T" is the abstract class of key callback objects. If "o" is a 
   geometric object, "c" is the top object in its key callback stack, and 
   a key event "kr" gets relayed to "o", then "cb.invoke(kr)" will be called.
   It is up to the user to create subclasses of "KeyCB.T" that handle
   key events in a given context appropriately. *)

  Rec = RECORD
    whatChanged : VBT.KeySym;
    wentDown    : BOOLEAN;
    modifiers   : VBT.Modifiers;
  END;
(* "KeyCB.Rec" is a record type containing information about a key event.
   "whatChanged" is the key that went up or down, "wentDown" indicates 
   whether it went down or up.
   "modifiers" is the set of modifiers (Shift, Control, Mouse Buttons, etc.) 
   that was active when the transition took place.

   {\em NOTE: There is a fair chance that I will add other fields to "Rec",
   once we have gained more experience with event handling. So far, I pretty
   much mimick (part of) what is there in "VBT.KeyRec".} *)

END KeyCB.
