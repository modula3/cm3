(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Thu Dec 24 15:36:37 PST 1992 by jdd *)

MODULE RTHeapDep;

(* VM protection is currently unimplemented on this architecture.  If
   you wish to implement it, you might wish to use the DS3100 version as
   a guide. *)

PROCEDURE Protect (<* UNUSED *> p                 : Page;
                   <* UNUSED *> n                 : CARDINAL;
                   <* UNUSED *> readable, writable: BOOLEAN   ) =
  BEGIN
    <* ASSERT FALSE *>
  END Protect;

PROCEDURE TimeUsed (): REAL =
  BEGIN
    <* ASSERT FALSE *>
  END TimeUsed;

PROCEDURE VMFaultTime (): REAL =
  BEGIN
    <* ASSERT FALSE *>
  END VMFaultTime;

BEGIN END RTHeapDep.
