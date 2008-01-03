(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:26:54 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:13 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE Unique;

IMPORT Thread, Fmt;

VAR sequencerMutex: Thread.Mutex;
VAR sequencer: INTEGER;

PROCEDURE Identifier (): TEXT =
  BEGIN
  RETURN "A" & Fmt.Int (Integer());
  END Identifier;

PROCEDURE Integer (): INTEGER =
  BEGIN
  LOCK sequencerMutex DO
    sequencer := sequencer + 1;
    RETURN sequencer;
    END;
  END Integer;

  BEGIN
  sequencer := IntegerNever;
  sequencerMutex := NEW (MUTEX);

  END Unique.

