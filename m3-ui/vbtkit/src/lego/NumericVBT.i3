(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 11 21:34:28 PDT 1993 by meehan                   *)
(*      modified on Mon Feb  1 00:25:39 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 13:08:32 PDT 1992 by muller                   *)
(*      modified on Fri Mar 20 22:35:51 1992 by steveg                       *)
<* PRAGMA LL *>

(* A "NumericVBT" is a VBT class for displaying and changing an
   integer within some range.  A "NumericVBT" has three parts (from
   left to right): a minus button, a type-in field, and a plus button.
   The type-in field is restricted to contain an integer within a
   specified range; it can be changed by editing (it uses the default
   editing model), or by typing Return, or by clicking on the plus or
   minus buttons.  The plus/minus buttons are trill buttons, so
   clicking and holding will cause the value of the "NumericVBT" to
   continuously increment/decrement.

   The "NumericVBT" has a "callback" method that is called each time
   the user types Return or click the plus or minus button.  The
   default callback method is a no-op. *)

INTERFACE NumericVBT;

IMPORT AnyEvent, Filter, Font, Shadow, TypeinVBT,  VBT;

TYPE
  T <: Public;
  Public = Filter.T OBJECT
             typein: Typein := NIL; (* READONLY after init *)
           METHODS
             <* LL.sup <= VBT.mu *>
             init (min       : INTEGER  := FIRST (INTEGER);
                   max       : INTEGER  := LAST (INTEGER);
                   allowEmpty: BOOLEAN  := FALSE;
                   naked     : BOOLEAN  := FALSE;
                   font      : Font.T   := Font.BuiltIn;
                   shadow    : Shadow.T := NIL              ):
               T;
             callback (event: AnyEvent.T);
           END;
  Typein <: TypeinVBT.T;

(* The call to "v.init(...)" initializes "v" as a "NumericVBT"
   and returns "v".  The integer stored with "v", referred to as
   ``the value in'' "v", is constrained to be in the range
| [min .. MAX (min, max)]
   The initial value in "v" is equal to "min".

   If "allowEmpty" is "TRUE", then ``empty'' (no text in the type-in
   area) is a distinct and valid state, and can be tested by the
   procedure "IsEmpty".  The call "Get(v)" in the empty state will
   return "FIRST(INTEGER)", regardless of whether this is in the valid
   range. Clicking the plus/minus buttons has no effect when "v" is in
   the empty state.

   If "naked" is "TRUE", then the numeric interactor appears as just a
   type-in field, without plus or minus buttons.

   IF "v.typein" is "NIL" when "v.init(...)" is called, then a new
   "Typein" will be allocated and assigned to "v.typein".  Whether or
   not it was "NIL" at the time of the call, it will be initialized via
| v.typein(FALSE, 1.5, 1.5, font, shadow)
   That is, it will not be expandable, its margins will be 1.5 mm,
   and "font" and "shadow" will determine its appearance.

   The implementation calls
| v.callback(event)
   when the user clicks the plus/minus buttons, or types Return in the
   type-in area.  The "event" parameter reports the details of the
   event as either an "AnyEvent.Mouse" (clicking on the plus/minus
   buttons) or an "AnyEvent.Key" (typing Return in the type-in area).
   The value in "v" is changed before "v.callback" is invoked.

   The value in "v" is range-checked before the callback is called, and
   in every call to "Get".  If the number is out of range, the nearest number
   in range will be written into the type-in area, and that value will
   be returned to the caller of "Get".

 *)

PROCEDURE Put (v: T; n: INTEGER);
<* LL.sup = VBT.mu *>
(* Change the value in "v" to be
|  MIN(GetMax(v), MAX(GetMin(v), n))
   and display this value in the type-in area.  Note that
   "v.callback" is not invoked. *)

PROCEDURE PutBounds (v: T; min, max: INTEGER);
<* LL.sup = VBT.mu *>
(* Change "v.min" to be "min" and "v.max" to be "MAX(min, max)",
   and then call "Put(v, Get(v))".  The call to "Put" has the
   effect of projecting the value of "v" into the new bounds. *)

PROCEDURE Get (v: T)   : INTEGER; <* LL.sup = VBT.mu *>
(* Return the current value in "v".  This value is range-checked, in case
   the user typed an out-of-range value without typing Return. *)

PROCEDURE GetMin (v: T): INTEGER; <* LL.sup = VBT.mu *>
PROCEDURE GetMax (v: T): INTEGER; <* LL.sup = VBT.mu *>
(* Return the indicated value associated with "v". *)

PROCEDURE SetEmpty (v: T);
<* LL.sup = VBT.mu *>
(* Set "v" to the empty state.  This is a no-op unless "allowEmpty" was
   "TRUE" when "v" was initialized. *)

PROCEDURE IsEmpty (v: T): BOOLEAN;
<* LL.sup = VBT.mu *>
(* Test whether "v" is in the empty state.  If "allowEmpty" was
   not "TRUE" when "v" was initialized, this procedure will always
   return "FALSE". *)

PROCEDURE TakeFocus (v          : T; 
                     time       : VBT.TimeStamp;
                     alsoSelect : BOOLEAN       := TRUE):
  BOOLEAN;
<* LL = VBT.mu *>
(* Cause the type-in area to grab the keyboard focus.  If the
   focus could be grabbed and if "alsoSelect" is set, the type-in
   area will make its entire text the primary selection.  Returns
   whether the keyboard focus could be acquired. *)

END NumericVBT.

