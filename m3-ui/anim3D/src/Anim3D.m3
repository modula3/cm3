(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Feb 16 20:26:11 PST 1995 by najork                   *)
(*       Created on Sat May 21 17:48:26 PDT 1994 by najork                   *)


MODULE Anim3D;

IMPORT AnimServer, Clock, Wr;

VAR
  clock: Clock.T;


PROCEDURE Now () : LONGREAL =
  BEGIN
    IF clock = NIL THEN
      clock := NEW (Clock.T).init ();
    END;
    RETURN clock.time ();
  END Now;


PROCEDURE ChangeClock (c : Clock.T) =
  BEGIN
    clock := c;
  END ChangeClock;


PROCEDURE SetErrorWr (wr : Wr.T) =
  BEGIN
    AnimServer.SetErrorWr (wr);
  END SetErrorWr;


BEGIN
  lock := AnimServer.externalLock;
END Anim3D.
