(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by wobber *)
(* $Id: TCPHackNull.m3,v 1.2 2003-07-28 14:12:14 wagner Exp $ *)

UNSAFE MODULE TCPHackNull EXPORTS TCPHack;

PROCEDURE RefetchError(<*UNUSED*> fd: INTEGER): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END RefetchError;

BEGIN
END TCPHackNull.
