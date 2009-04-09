(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by wobber *)
(* $Id: SupTCPHackNull.m3,v 1.1.1.1 2009-04-09 17:02:04 jkrell Exp $ *)

UNSAFE MODULE SupTCPHackNull EXPORTS SupTCPHack;

PROCEDURE RefetchError(<*UNUSED*> fd: INTEGER): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END RefetchError;

BEGIN
END SupTCPHackNull.
