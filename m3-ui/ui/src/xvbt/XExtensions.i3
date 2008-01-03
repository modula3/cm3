(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue Nov 23 14:58:24 PST 1993 by steveg   *)
(*      modified on Thu Nov 11 10:44:39 PST 1993 by kalsow   *)

UNSAFE INTERFACE XExtensions;

IMPORT TrestleComm, XClient, XScreenType;
<* PRAGMA LL *>

(* as much as possible of X server extension calls which affect core
   modules, such as XClient, should be hidden behind this interface to
   isolate the extension code. *)

(* these procedures are called at the end of the initialisation of the
   relevant objects *)
PROCEDURE InitXClient (xclient: XClient.T) RAISES {TrestleComm.Failure}; <* LL = trsl *>

PROCEDURE InitXScreenType (st: XScreenType.T) RAISES {TrestleComm.Failure}; <* LL = {trsl} *>

END XExtensions.
