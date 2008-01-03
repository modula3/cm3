(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri May 17 08:58:28 PDT 1996 by mhb    *)
(*      modified on Mon Aug 3 15:38:48 PDT 1992 by meehan *)
(* modified on Tue Jun 16 12:54:15 PDT 1992 by muller *)
(* modified on Fri Mar 27 02:20:14 1992 by steveg *)
(* modified on Mon Feb 11 16:06:46 PST 1991 by brooks *)
<* PRAGMA LL *>

(* An "AnyEvent.T" is an object that can hold any of the Trestle
   event-time events.  This object type is useful for "VBT"
   methods that are called in response to multiple styles of user
   gestures.  For instance, the "callback" method of a
   "NumericVBT" is invoked either because a user clicked on the
   plus or minus button or because the user typed a carriage
   return in the type-in field.  The Trestle event is passed to the
   "callback" method as an "AnyEvent.T", and the "callback"
   method can then use a "TYPECASE" to differentiate button clicks
   from carriage returns, and to retrieve the data specific to
   each type of event. 

   The locking level is arbitrary for all procedures in this interface. *)

INTERFACE AnyEvent;

IMPORT VBT, Wr;

TYPE
  T = BRANDED OBJECT END;
  Key = T OBJECT key: VBT.KeyRec END;
  Mouse = T OBJECT mouse: VBT.MouseRec END;
  Position = T OBJECT position: VBT.PositionRec END;
  Misc = T OBJECT misc: VBT.MiscRec END;
(* The four subtypes of "AnyEvent.T" correspond to the four
   event-time Trestle events: keyboard, mouse, position, and
   miscellaneous. *)

PROCEDURE FromKey (
            READONLY event: VBT.KeyRec): Key;
PROCEDURE FromMouse (
            READONLY event: VBT.MouseRec): Mouse;
PROCEDURE FromPosition (
            READONLY event: VBT.PositionRec): Position;
PROCEDURE FromMisc (
            READONLY event: VBT.MiscRec): Misc;
(* Return "event" as an appropriate subtype of "AnyEvent.T". *)

PROCEDURE TimeStamp (anyevent: T): VBT.TimeStamp;
(* Return the timestamp of the "anyevent".  It is a checked
   runtime error if "anyevent" is not a proper subtype of "AnyEvent.T". *)

PROCEDURE ToWr (anyevent: T; wr: Wr.T);
(* Put a textual representation of "anyevent" onto the writer "wr". *)

END AnyEvent.

