(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Dec 14 02:38:04 PST 1992 by msm     *)
(*      modified on Mon Feb 24 13:57:19 PST 1992 by muller  *)
(*      modified on Fri Sep  6 17:25:31 PDT 1991 by gnelson *)
<*PRAGMA LL*>

(* Every installed VBT has a DpyFilter above it to catch messages telling the
   window to move to a new display, or to add new cloned copies. *)

INTERFACE DpyFilter;

IMPORT VBT, JoinParent, JoinedVBT;

TYPE
  T <: Public;
  Public = JoinParent.T OBJECT
           METHODS
             init (ch: JoinedVBT.T; enabled := TRUE): T
           END;

VAR ChangeDisplay, AddDisplay: VBT.MiscCodeType;

TYPE
  Message = REF RECORD
                  oldAuth, newAuth           : TEXT;
                  x, y, width, height, screen: INTEGER;
                  iconic, status             : BOOLEAN;
                  newDisplay                 : REF ARRAY OF TEXT
                END;

(* In a MiscCode of type ChangeDisplay or AddDisplay, the first detail
   should be a MiscDetail.FromRef of a Message.  The status field of the
   message should be set to FALSE before returning if the message is
   unacceptable. *)

PROCEDURE New (ch: JoinedVBT.T; enabled := TRUE): T;

PROCEDURE SetEnabled (ch: VBT.T; enabled := TRUE);
(* Find the lowest T ancestor of ch, and set whether or not it
   automatically handles ChangeDisplay messages.  When disabled,
   ChangeDisplay and AddDisplay misc codes are simply relayed to the
   child. *)

PROCEDURE GetEnabled (ch: VBT.T): BOOLEAN;
(* Return the enabled state of the lowest T ancestor of ch, or TRUE if
   there is no such ancestor. *)

END DpyFilter.


