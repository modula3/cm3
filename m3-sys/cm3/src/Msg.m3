(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Sep 20 08:20:58 PDT 1994 by kalsow     *)

MODULE Msg;

IMPORT Atom, AtomList, Process, Stdio, Wr, Thread, Params;
IMPORT Utils, Arg;

VAR crashing := FALSE;

PROCEDURE SetLevel (new: Level) =
  BEGIN
    level := new; (** MAX (level, new); **)
  END SetLevel;

PROCEDURE UsageError (a, b, c: TEXT := NIL) =
  BEGIN
    Out (a, b, c, Wr.EOL);
    Out ("usage: ", Params.Get (0));
    Out (" [-?] [options] [sources...] [objs...] [libs...] [-o pgm|-a lib|-c] ");
    Out (Wr.EOL);
    FatalError (NIL, "bad usage");
  END UsageError;

PROCEDURE OSErr (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    RETURN msg;
  END OSErr;

PROCEDURE FatalError (args: AtomList.T;  a, b, c, d: TEXT := NIL) =
  VAR e := OSErr (args);
  BEGIN
    (** Out (Wr.EOL, "Fatal Error: ", a, b, c, d, Wr.EOL); **)
    crashing := TRUE;
    Out (Wr.EOL);
    Out ("Fatal Error: ", a, b, c, d, e);
    Out (Wr.EOL, Wr.EOL);
    Process.Exit (1);
  END FatalError;

PROCEDURE Error (args: AtomList.T;  a, b, c, d: TEXT := NIL) =
  VAR e := OSErr (args);
  BEGIN
    (** Out (Wr.EOL, "Fatal Error: ", a, b, c, d, Wr.EOL); **)
    Out (a, b, c, d, e, Wr.EOL);
  END Error;

PROCEDURE Debug (a, b, c, d: TEXT := NIL) =
  BEGIN
    IF (level >= Level.Debug) THEN Out (a, b, c, d) END;
  END Debug;

PROCEDURE Verbose (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF (level >= Level.Verbose) THEN Out (a, b, c, d, e, Wr.EOL) END;
  END Verbose;

PROCEDURE Commands (a, b, c, d, e, f: TEXT := NIL) =
  BEGIN
    IF (level >= Level.Commands) THEN Out (a, b, c, d, e, f, Wr.EOL) END;
  END Commands;

PROCEDURE Explain (a, b, c, d: TEXT := NIL) =
  BEGIN
    IF (level >= Level.Explain) THEN
      IF (level > Level.Explain) THEN Out (Wr.EOL) END;
      Out (a, b, c, d, Wr.EOL);
    END;
  END Explain;

PROCEDURE OutL (a, b: TEXT;  l: Arg.List) =
  VAR gap: TEXT := NIL;
  BEGIN
    Out (a, b);
    IF (l # NIL) THEN
      IF (b # NIL) THEN gap := " " END;
      VAR n := l.head; BEGIN
        WHILE (n # NIL) DO
          Out (gap, n.arg);
          gap := " ";
          n := n.next
        END;
      END;
    END;
    Out (Wr.EOL);
  END OutL;

PROCEDURE Out (a, b, c, d, e, f, g: TEXT := NIL) =
  VAR wr := Stdio.stdout;
  BEGIN
    TRY
      IF (a # NIL) THEN Wr.PutText (wr, a) END;
      IF (b # NIL) THEN Wr.PutText (wr, b) END;
      IF (c # NIL) THEN Wr.PutText (wr, c) END;
      IF (d # NIL) THEN Wr.PutText (wr, d) END;
      IF (e # NIL) THEN Wr.PutText (wr, e) END;
      IF (f # NIL) THEN Wr.PutText (wr, f) END;
      IF (g # NIL) THEN Wr.PutText (wr, g) END;
      Utils.FlushWriter (wr, "<stdout>");
    EXCEPT
    | Wr.Failure (args) =>
        IF NOT crashing THEN
          FatalError (args, "unable to write file: <stdout>");
        END;
    | Thread.Alerted =>
        IF NOT crashing THEN
          FatalError (NIL, "interrupted -- unable to write file: <stdout>");
        END;
    END;
  END Out;

BEGIN
END Msg.
