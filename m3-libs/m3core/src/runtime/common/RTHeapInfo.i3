(*| Copyright (C) 1990, Digital Equipment Corporation       *)
(*| All rights reserved.                                    *)
(*| See the file COPYRIGHT for a full description.          *)
(*|                                                         *)
(*| Last modified on Fri Feb 18 07:52:10 PST 1994 by kalsow *)

(* "RTHeapInfo" produces the "@M3shownew" data. *)

INTERFACE RTHeapInfo;

CONST
  SENDING_TYPES  = -23;
  SENDING_COUNTS = -24;

PROCEDURE Init ();

END RTHeapInfo.
