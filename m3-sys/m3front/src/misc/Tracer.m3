(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Tracer.m3                                             *)
(* Last Modified On Tue Jun 28 09:56:03 PDT 1994 by kalsow     *)

MODULE Tracer;

(* IMPORT Error; *)

VAR
  emitting := FALSE;
  pending  : T := NIL;
  stack    : T := NIL;

PROCEDURE Schedule (t: T) =
  BEGIN
    IF (t = NIL) THEN RETURN END;

    IF (emitting) THEN
      (*** silently ignore the new event  ***
      Error.Msg ("nested <*TRACE*> pragmas are not allowed");
      ***)
      RETURN;
    END;

    IF (t.next = NIL) THEN
      t.next := pending;
      pending := t;
    END;
  END Schedule;

PROCEDURE Push (t: T) =
  BEGIN
    IF (t = NIL)  THEN RETURN END;

    IF (emitting) THEN
      (*** silently ignore the new event  ***
      Error.Msg ("nested <*TRACE*> pragmas are not allowed");
      ***)
      RETURN;
    END;

    IF (t.next = NIL) THEN
      t.next := stack;
      stack := t;
    END;
  END Push;

PROCEDURE Pop (tt: T) =
  VAR t := stack;
  BEGIN
    IF (tt = NIL) THEN RETURN END;
    IF (emitting) THEN RETURN END;
    IF (t = tt) THEN
      stack := t.next;
      t.next := NIL;
    END;
  END Pop;

PROCEDURE EmitPending () =
  VAR t: T;
  BEGIN
    IF (emitting) THEN RETURN END;
    emitting := TRUE;

    (* generate the one-shot traces *)
    WHILE (pending # NIL) DO
      t := pending;
      pending := t.next;
      t.next := NIL;
      t.apply ();
    END;

    (* generate the persistent traces *)
    t := stack;
    WHILE (t # NIL) DO
      t.apply ();
      t := t.next;
    END;

    emitting := FALSE;
  END EmitPending;

PROCEDURE Reset () =
  BEGIN
    emitting := FALSE;
    pending  := NIL;
    stack    := NIL;
  END Reset;


BEGIN
END Tracer.
