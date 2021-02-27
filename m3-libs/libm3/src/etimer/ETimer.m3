(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ETimer.m3                                             *)
(* Last modified on Thu Dec  1 10:13:12 PST 1994 by kalsow     *)
(*      modified on Fri Jan 15 17:00:09 PST 1993 by mjordan    *)

UNSAFE (*because of RTHeapRep*)
MODULE ETimer;

IMPORT Wr, Time, Fmt, RTHeapRep;

REVEAL
  T = BRANDED "ETimer.T" REF RECORD
    tag   : TEXT;
    cnt   : INTEGER := 0;
    time  : Time.T := 0.0d+0;
    next  : T := NIL;
  END;

TYPE
  StackIndex = [0..99];

TYPE
  Rec = RECORD
    timer : T;
    start : Time.T;
  END;

CONST
  Second      = 1.0d0;
  Millisecond = Second / 1000.0d0;

VAR
  mu := NEW (MUTEX);  (* protects the following global variables *)
  enabled    : BOOLEAN    := TRUE;
  all_timers : T          := NIL;
  misc       : T          := NIL;
  gc         : T          := NIL;
  tos        : StackIndex := 0;
  stack      : ARRAY StackIndex OF Rec;

PROCEDURE New (lab: TEXT): T =
  VAR t := NEW (T, tag := lab);
  BEGIN
    LOCK mu DO
      t.next := all_timers;
      all_timers := t;
    END;
    RETURN t;
  END New;

PROCEDURE Relabel (t: T;  lab: TEXT) =
  BEGIN
    t.tag := lab;
  END Relabel;

PROCEDURE Push (t: T) =
  VAR now: Time.T;
  BEGIN
    IF NOT enabled THEN RETURN END;
    LOCK mu DO
      now := Time.Now ();

      (* update the suspended timer *)
      IF (tos > 0) THEN
        WITH top = stack [tos-1], t = top.timer.time DO
          t := t + (now - top.start);
        END;
      END;

      (* start the new timer *)
      WITH top = stack [tos] DO
        top.start := now;
        top.timer := t;
      END;
      INC (t.cnt);

      INC (tos);
    END;
  END Push;

PROCEDURE Append (t: T; duration : LONGREAL) =
  BEGIN
    IF NOT enabled THEN RETURN END;
    LOCK mu DO
      t.time := t.time + duration;
    END;
  END Append;

PROCEDURE Pop () =
  VAR now: Time.T;
  BEGIN
    IF NOT enabled THEN RETURN END;
    LOCK mu DO
      now := Time.Now ();
      DEC (tos);

      (* update the popped timer *)
      WITH top = stack [tos], t = top.timer.time DO
        t := t + (now - top.start);
      END;

      (* restart the suspended timer *)
      IF (tos > 0) THEN
        stack[tos-1].start := now;
      END;
    END;
  END Pop;

PROCEDURE Dump (wr: Wr.T) =
  <*FATAL ANY*>
  CONST MinPrintable = 5.0d+0 * Millisecond;
  VAR t: T;  now, total: Time.T := 0.0d+0;
  BEGIN
    LOCK mu DO
      (* update the currently running timer, but leave it running *)
      IF (tos > 0) THEN
        now := Time.Now ();
        WITH top = stack [tos-1], t = top.timer.time DO
          t := t + (now - top.start);
          top.start := now;
        END;
      END;

      t := all_timers; (* capture the head of the list *)
    END;

    (* sleazy: don't lock the timers during the output since
       writing may cause allocation which may cause garbage
       collection which will cause a call to Push which will
       cause a deadlock... *)

    (* write the report *)
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, " seconds  #times  operation");
    Wr.PutText (wr, Wr.EOL);
    WHILE (t # NIL) DO
      IF (t.cnt > 0) AND (t.time >= MinPrintable) THEN
        Wr.PutText (wr, FmtTime (t.time));
        IF (t # misc)
          THEN Wr.PutText (wr, Fmt.Pad (Fmt.Int (t.cnt), 8));
          ELSE Wr.PutText (wr, "        ");
        END;
        Wr.PutText (wr, "  ");
        IF (t.tag # NIL) THEN Wr.PutText (wr, t.tag); END;
        Wr.PutText (wr, Wr.EOL);
      END;
      total := total + t.time;
      t := t.next;
    END;
    Wr.PutText (wr, "---------------------------------------------------");
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, FmtTime (total));
    Wr.PutText (wr, "          TOTAL");
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Wr.EOL);
  END Dump;

PROCEDURE FmtTime (t: Time.T): TEXT =
  BEGIN
    RETURN Fmt.Pad (Fmt.LongReal (t/Second, Fmt.Style.Fix, 2), 8);
  END FmtTime;

PROCEDURE Elapsed (t: T): LONGREAL =
  VAR now: Time.T;
  BEGIN
    LOCK mu DO
      (* update t if it's running, but leave it running *)
      IF (tos > 0) THEN
        WITH top = stack [tos-1], z = top.timer.time DO
          IF (top.timer = t) THEN
            now := Time.Now ();
            z := z + (now - top.start);
            top.start := now;
          END;
        END;
      END;

      (* finally, return t's accumulated time *)
      RETURN t.time / Second;
    END;
  END Elapsed;

PROCEDURE TotalElapsed (): LONGREAL =
  VAR t: T;  total: Time.T := 0.0d+0;
  BEGIN
    LOCK mu DO
      t := all_timers;
      WHILE (t # NIL) DO
        total := total + t.time;
        t := t.next;
      END;
    END;
    RETURN total / Second;
  END TotalElapsed;

PROCEDURE Next (prev: T): T =
  BEGIN
    LOCK mu DO
      IF (prev = NIL)
        THEN RETURN all_timers;
        ELSE RETURN prev.next;
      END;
    END;
  END Next;

TYPE
  GCClosure = RTHeapRep.MonitorClosure OBJECT OVERRIDES
    before := StartGC;
    after  := StopGC;
  END;

PROCEDURE StartGC (<*UNUSED*> cl: GCClosure) =
  BEGIN
    Push (gc);
  END StartGC;

PROCEDURE StopGC (<*UNUSED*> cl: GCClosure) =
  BEGIN
    Pop ();
  END StopGC;

PROCEDURE Enable () =
  BEGIN
    IF enabled THEN RETURN END;
    LOCK mu DO
      enabled := TRUE;
      RTHeapRep.RegisterMonitor (NEW (GCClosure));
    END;
  END Enable;

PROCEDURE Reset (t: T) =
  BEGIN
    LOCK mu DO
      t.cnt := 0;
      t.time := 0.0d+0;
      IF (tos > 0) AND (stack[tos-1].timer = t) THEN
        (* t is running, reset its start time *)
        stack[tos-1].start := Time.Now ();
      END;
    END;
  END Reset;

PROCEDURE ResetAll () =
  VAR t: T;
  BEGIN
    LOCK mu DO
      t := all_timers;
      WHILE (t # NIL) DO
        t.cnt := 0;
        t.time := 0.0d+0;
        t := t.next;
      END;
      IF (tos > 0) THEN
        (* reset the running timer *)
        stack[tos-1].start := Time.Now ();
      END;
    END;
  END ResetAll;

BEGIN
  misc := New ("other");
  gc   := New ("garbage collection");
  Push (misc);
  enabled := FALSE;
END ETimer.
