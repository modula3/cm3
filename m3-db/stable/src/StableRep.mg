(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:14:53 PST 1995 by kalsow     *)
(*      modified on Thu Jan 12 09:08:46 PST 1995 by chaiken    *)
(*      modified on Tue Sep 27 19:16:26 PDT 1994 by weich      *)

(* This generic module provides the part of the implementation of stable
   objects that is independent of {\tt Data.T}.
*)

GENERIC MODULE StableRep(StableData);

IMPORT StableError, LogManager, Pathname, Pickle;
IMPORT OSError, Rd, Wr, RdUtils, Thread, Atom, AtomList;

IMPORT Log;

<*FATAL Thread.Alerted*>

(* \subsection{Initialization Procedures} *)

PROCEDURE ReOpenLog (self: StableData.T) =
  BEGIN
    TRY
      IF self.log = NIL THEN
        self.log := self.lm.reOpenLog(self.nm)
      END
    EXCEPT
      OSError.E (err) =>
        StableError.Halt("cannot reopen log file: "
                           & RdUtils.FailureText(err))
    END
  END ReOpenLog;

PROCEDURE FlushLog (self: Public) RAISES {StableError.E} =
  BEGIN
    TRY
      IF self.log # NIL THEN Wr.Flush(self.log) END
    EXCEPT
      Wr.Failure (err) => RAISE StableError.E(err);
    END
  END FlushLog;

PROCEDURE FreeLog (self: Public) RAISES {StableError.E} =
  BEGIN
    TRY
      IF self.log # NIL THEN Wr.Close(self.log) END;
      self.log := NIL;
    EXCEPT
      Wr.Failure (err) => RAISE StableError.E(err);
    END
  END FreeLog;

(* \subsection{Procedure Init}
   Initialize the "nm" and "lm" fields for the stable object first
   (this is needed for the "Recover()" procedure.

   Recover if possible and set "recovered" to TRUE. Call "Checkpoint"
   if "Init" is called the first time (i.e. if "recoverable=FALSE")
   or if recovered from a crash (i.e. if "emptyLog=FALSE").
   This might be saver than necessary. I assume that the "Recover"
   procedure might consume the log file, so I test "emptyLog" before
   running "Recover()".

   Set the "forceToDisk" field after a possible recover. This is
   to set the field to a fresh value -- do not leave as it was
   when the stable object was saved the last time.
*)
PROCEDURE Init (    self       : Public;
                    nm        : Pathname.T;
                VAR recovered  : BOOLEAN;
                    forceToDisk               := TRUE;
                    lm         : LogManager.T := NIL   ): StableData.T
  RAISES {StableError.E} =
  BEGIN
    IF self.lm # NIL THEN
      RAISE StableError.E(
          AtomList.List1(
            Atom.FromText(
              "attempted to restabilize without intervening dispose")))
    END;
    TRY
      self.nm := nm;
      IF lm = NIL THEN
        self.lm := LogManager.default
      ELSE
        self.lm := lm
      END;
      VAR emptyLog:= TRUE;
      BEGIN
        IF self.lm.recoverable(nm) THEN
          emptyLog:= self.lm.emptyLog(nm);
          self := Recover(self);
          recovered := TRUE;
        ELSE
          recovered := FALSE
        END;
        IF NOT recovered OR NOT emptyLog THEN Checkpoint(self) END;
        self.forceToDisk := forceToDisk;
        RETURN NARROW(self, StableData.T)
      END
    EXCEPT
      OSError.E (err) => RAISE StableError.E(err);
    | Wr.Failure (err) => RAISE StableError.E(err);
    END
  END Init;

PROCEDURE Dispose (self: Public) RAISES {StableError.E} =
  BEGIN
    IF (self.lm # NIL) THEN
      TRY
        self.freeLog();
        self.lm.dispose(self.nm);
        self.lm := NIL;
      EXCEPT
        OSError.E (err) => RAISE StableError.E(err);
      END
    END
  END Dispose;


(* \subsection{Procedure Recover}
  "t" already has a "nm" value, a log manager and a
     valid "readCheckpoint" method.  "Recover" will use
     those to recover from "t.nm".
*)

PROCEDURE Recover (t: StableData.T): StableData.T
  RAISES {StableError.E, OSError.E} =

  VAR log, cp: Rd.T;
  BEGIN
    t.lm.recover(t.nm, log, cp); (* might open log *)
    TRY
      t := t.readCheckpoint(cp);
      Rd.Close(cp);
    EXCEPT
      Rd.Failure (err) =>
        RAISE StableError.E(
                AtomList.Cons(
                  Atom.FromText("error reading checkpoint"),
                  err));
    END;
    IF log # NIL THEN 
      t.replayLog(log); 
      TRY
        Rd.Close(log) (* get rid of the file handle *)
      EXCEPT
        Rd.Failure => (* well, it's not the end of the world *)
      END
    END; 
    RETURN t;
  END Recover;

(* \subsection{Procedure Checkpoint}
   Free the log and use the protocoll described in "LogManager.i3"
   to do a checkpoint.
*)

PROCEDURE Checkpoint (t: StableData.T)
  RAISES {StableError.E} =
  BEGIN
    TRY
      t.freeLog();
      VAR cp := t.lm.beginCheckpoint(t.nm);
      BEGIN
        Log.CrashPoint(102);
        t.writeCheckpoint(cp);
        Wr.Close(cp);
        t.log := t.lm.endCheckpoint(t.nm);
      END;
    EXCEPT
(***
    | OSError.E (err) => RAISE StableError.E(err);
***)
    | Wr.Failure (err) =>
        RAISE StableError.E(
                AtomList.Cons(
                  Atom.FromText("checkpoint error truncating logfile"),
                  err));
    END;
  END Checkpoint;

(* \subsection{Procedures Read- and WriteCheckpoint}
   These are the default procedures for reading and writing a checkpoint.
   They use the pickle package.
*)

PROCEDURE ReadCheckpoint (self: Public; cp: Rd.T):
  StableData.T RAISES {StableError.E} =
  BEGIN
    TRY
      TYPECASE Pickle.Read(cp) OF
        StableData.T (d) => RETURN d;
      ELSE
        RAISE StableError.E(
            AtomList.List1(
              Atom.FromText(
                "Checkpoint in " & self.nm
                  & " does not contain data of the correct type")))
      END;
    EXCEPT
    | Rd.EndOfFile =>
        RAISE StableError.E(
            AtomList.List1(
              Atom.FromText(
                "unexpected EOF encountered reading checkpointfile")))
    | Rd.Failure (err) =>
        RAISE StableError.E(
                AtomList.Cons(
                  Atom.FromText(
                    "error reading checkpointfile"), err))
    | Pickle.Error (msg) =>
        RAISE StableError.E(
                AtomList.List1(
                  Atom.FromText(
                    "pickle-error (" & msg
                      & ") reading checkpointfile")))
    END;
  END ReadCheckpoint;

PROCEDURE WriteCheckpoint (self: Public; wr: Wr.T)
  RAISES {StableError.E} =
  BEGIN
    TRY
      Pickle.Write(wr, self);
    EXCEPT
    | Wr.Failure (err) =>
        RAISE StableError.E(
                AtomList.Cons(
                  Atom.FromText(
                    "error writing checkpointfile"), err))
    | Pickle.Error (msg) =>
        RAISE StableError.E(
                AtomList.List1(
                  Atom.FromText(
                    "pickle-error (" & msg
                      & ") writing checkpointfile")))
    END
  END WriteCheckpoint;

BEGIN
END StableRep.
