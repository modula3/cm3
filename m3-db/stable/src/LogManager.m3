(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Wed Oct 11 15:11:00 PDT 1995 by najork     *)
(*      modified on Mon Jan 30 15:52:21 PST 1995 by kalsow     *)
(*      modified on Thu Sep 22 18:26:03 PDT 1994 by weich      *)

(* The "LogManager" provides an object which methods take a "Pathname.T"
   and produce readers and writers for log and checkpoint files.  It is
   used by stable objects.

   The default log mananger uses the "nm" paramter as the name of a
   directory in the filesystem.  In this directory it will put the log and
   checkpointfiles. *)

MODULE LogManager;

IMPORT Pathname, Thread, Wr, FileWr, Rd, FileRd, FS, OSError, Atom, AtomList, 
       FSError;

IMPORT Log;
FROM Log IMPORT CrashPoint;

REVEAL T = Public BRANDED OBJECT END;

REVEAL
  Default = DefaultPublic BRANDED OBJECT
    METHODS
      (* produce the complete filenames: *)
      cpfn    (nm: Pathname.T): Pathname.T RAISES {OSError.E} := Cpfn;
      newcpfn (nm: Pathname.T): Pathname.T RAISES {OSError.E} := NewCpfn;
      oldcpfn (nm: Pathname.T): Pathname.T RAISES {OSError.E} := OldCpfn;
      dummycp (nm: Pathname.T): Pathname.T RAISES {OSError.E} := DummyCp;
      logfn   (nm: Pathname.T): Pathname.T RAISES {OSError.E} := Logfn;

    OVERRIDES
      init := Init;
      reOpenLog       := ReOpenLog;
      beginCheckpoint := BeginCheckpoint;
      endCheckpoint   := EndCheckpoint;
      recover         := Recover;
      recoverable     := Recoverable;
      emptyLog        := EmptyLog;
      dispose         := Dispose;
    END;

(* \subsection{produce filenames}
 *)

PROCEDURE Logfn (<*UNUSED*> lm: Default; nm: Pathname.T): Pathname.T =
  (* log filename *)
  BEGIN
    RETURN Pathname.Join(nm, "log", NIL)
  END Logfn;

PROCEDURE Cpfn (<*UNUSED*> lm: Default; nm: Pathname.T): Pathname.T =
  (* checkpoint filename *)
  BEGIN
    RETURN Pathname.Join(nm, "checkpoint", NIL)
  END Cpfn;

PROCEDURE OldCpfn (lm: Default; nm: Pathname.T): Pathname.T
  RAISES {OSError.E} =
  (* old (backup) checkpoint filename *)
  BEGIN
    RETURN lm.cpfn(nm) & ".old"
  END OldCpfn;

PROCEDURE NewCpfn (lm: Default; nm: Pathname.T): Pathname.T
  RAISES {OSError.E} =
  (* new checkpoint filename *)
  BEGIN
    RETURN lm.cpfn(nm) & ".new"
  END NewCpfn;

PROCEDURE DummyCp (lm: Default; nm: Pathname.T): Pathname.T
  RAISES {OSError.E} =
  (* dummy checkpoint filename for bootstrapping *)
  BEGIN
    RETURN lm.cpfn(nm) & ".dummy"
  END DummyCp;


(* \subsection{Public Methods}
 *)

PROCEDURE Init(lm: Default): Default =
(* No dynamic initialization needed: *)
  BEGIN
    RETURN lm;
  END Init;

PROCEDURE ReOpenLog (lm: Default; nm: Pathname.T): Wr.T
  RAISES {OSError.E} =
  BEGIN
    RETURN FileWr.OpenAppend(lm.logfn(nm))
  END ReOpenLog;

PROCEDURE BeginCheckpoint (lm: Default; nm: Pathname.T): Wr.T
  RAISES {OSError.E} =
  BEGIN
    Log.PutText("BeginCheckpoint: ");
    (**)
    CrashPoint(101);
    TestDir(nm);
    IF TestFile(lm.cpfn(nm)) THEN
      FS.Rename(lm.cpfn(nm), lm.oldcpfn(nm));
      Log.PutText("renamed checkpointfile to " & lm.oldcpfn(nm));
      Log.Nl();
    ELSE
      TRY
        <*FATAL Thread.Alerted*>
        BEGIN
          Wr.Close(FileWr.Open(lm.dummycp(nm)));
        END
      EXCEPT
        Wr.Failure =>
          RAISE
            OSError.E(AtomList.List1(
                        Atom.FromText("error creating dummy checkpoint "
                                        & lm.dummycp(nm))));
      END;
      Log.PutText("created dummy checkpoint " & lm.dummycp(nm));
      Log.Nl();
    END;
    Log.PutText("new checkpointfile " & lm.newcpfn(nm));
    Log.Nl();
    RETURN FileWr.Open(lm.newcpfn(nm));
  END BeginCheckpoint;

PROCEDURE EndCheckpoint (lm: Default; nm: Pathname.T): Wr.T
  RAISES {OSError.E} =
  BEGIN
    Log.PutText("EndCheckpoint: ");
    Log.PutText("renaming checkpoint " & lm.newcpfn(nm));
    Log.Nl();
    (**)
    CrashPoint(103);
    FS.Rename(lm.newcpfn(nm), lm.cpfn(nm));
    (**)
    CrashPoint(104);
    VAR newLog := FileWr.Open(lm.logfn(nm));
    BEGIN
      IF TestFile(lm.dummycp(nm)) THEN
        Log.PutText("deleting dummy checkpoint" & lm.dummycp(nm));
        Log.Nl();
        (**)
        CrashPoint(105);
        FS.DeleteFile(lm.dummycp(nm));
      ELSE
        Log.PutText("deleting old checkpoint" & lm.oldcpfn(nm));
        Log.Nl();
        (**)
        CrashPoint(105);
        FS.DeleteFile(lm.oldcpfn(nm));
      END;
      (**)
      CrashPoint(106);
      RETURN newLog;
    END;
  END EndCheckpoint;


PROCEDURE Recover (lm: Default; nm: Pathname.T; VAR log, checkp: Rd.T)
  RAISES {OSError.E} =
  (* Check possible leftovers from checkpoint-crashes: This procedure
     tolerates absence of logfiles since itself deletes logfiles when
     invalid.  Usually there are always logfiles (with possible size zero)
     since there is no way of deleting logfiles using the default
     logmanager. *)

  BEGIN
    (* (1) renaming to final name is the commit for writing a new
       checkpoint.  If there is a checkpoint with temporary name then it is
       invalid and the log file belongs to the old version. *)
    IF TestFile(lm.newcpfn(nm)) OR NOT TestFile(lm.cpfn(nm)) THEN
      Log.PutText("found valid backup");
      Log.Nl();
      FS.Rename(lm.oldcpfn(nm), lm.cpfn(nm)); (* must exist or fatal
                                                 error *)
      IF TestFile(lm.newcpfn(nm)) THEN
        Log.PutText("removing new checkpoint");
        Log.Nl();
        FS.DeleteFile(lm.newcpfn(nm))
      END;
      IF NOT TestFile(lm.logfn(nm)) THEN
        log := NIL
      ELSE
        log := FileRd.Open(lm.logfn(nm));
      END;

      (* (2) there is a checkpoint with final name.  Thus it is valid.  If
         there is a backup checkpoint than the log is invalid. *)
    ELSIF TestFile(lm.cpfn(nm)) AND TestFile(lm.oldcpfn(nm)) THEN
      Log.PutText("found valid new checkpoint");
      Log.Nl();
      Log.PutText("deleting backup checkpoint " & lm.oldcpfn(nm));
      FS.DeleteFile(lm.oldcpfn(nm)); (* must exist of fatal error *)
      IF TestFile(lm.logfn(nm)) THEN (* may or may not exist depending on
                                        the crash *)
        Log.PutText("removing log");
        Log.Nl();
        FS.DeleteFile(lm.logfn(nm));
      END;
      log := NIL;

      (* (3) Finally if there is no temporary checkpoint there than the
         checkpoint and the log is valid. *)
    ELSE
      IF NOT TestFile(lm.logfn(nm)) THEN
        log := NIL
      ELSE
        log := FileRd.Open(lm.logfn(nm));
      END;
    END;
    checkp := FileRd.Open(lm.cpfn(nm));
  END Recover;

PROCEDURE Recoverable (lm: Default; nm: Pathname.T): BOOLEAN =
  BEGIN
    TRY
      (* is there such a directory? *)
      IF FS.Status(nm).type # FS.DirectoryFileType THEN RETURN FALSE END;

      (* Do not care about to logfile but there must be a version of the
         checkpointfile: *)
      RETURN TestFile(lm.cpfn(nm)) OR TestFile(lm.oldcpfn(nm));

    EXCEPT
      OSError.E => RETURN FALSE  (* nothing found *)
    END;
  END Recoverable;

PROCEDURE EmptyLog (lm: Default; nm: Pathname.T): BOOLEAN
  RAISES {OSError.E} =
  BEGIN
    IF NOT lm.recoverable(nm) THEN
      RAISE OSError.E(
              AtomList.List1(
                Atom.FromText(
                  "no checkpointfile for log in " & nm)));
    END;
    IF TestFile(lm.logfn(nm)) THEN
      RETURN FS.Status(nm).size > 0
    ELSE
      RETURN TRUE
    END;
  END EmptyLog;

PROCEDURE Dispose (lm: Default; nm: Pathname.T) RAISES {OSError.E} =
  BEGIN
    IF TestFile(nm) THEN
      Log.PutText("Erasing directory " & nm);
      Log.Nl();
      (**)
      CrashPoint(501);
      IF TestFile(lm.cpfn(nm)) THEN FS.DeleteFile(lm.cpfn(nm)) END;
      IF TestFile(lm.oldcpfn(nm)) THEN FS.DeleteFile(lm.oldcpfn(nm)) END;
      IF TestFile(lm.newcpfn(nm)) THEN FS.DeleteFile(lm.newcpfn(nm)) END;
      (**)
      CrashPoint(502);
      IF TestFile(lm.logfn(nm)) THEN FS.DeleteFile(lm.logfn(nm)) END;
      (**)
      CrashPoint(503);
      IF TestFile(lm.dummycp(nm)) THEN FS.DeleteFile(lm.dummycp(nm)) END;
      FS.DeleteDirectory(nm);
    END
  END Dispose;

(* \subsection{Utilities}
 *)

PROCEDURE TestFile (fname: Pathname.T): BOOLEAN RAISES {OSError.E} =
  (* Test for existence of a file named 'fname'.  Returns TRUE iff it
     exists and is readable.  Exceptions other than "file not found" are
     reraised. *)
  BEGIN
    TRY
      EVAL FS.Status(fname);
      RETURN TRUE;               (* ok, passed the test *)
    EXCEPT
      OSError.E (err) =>
        IF FSError.FileNotFound (err) THEN
          RETURN FALSE
        ELSE
          RAISE OSError.E(err)
        END
    END
  END TestFile;

PROCEDURE TestDir (dir: Pathname.T) RAISES {OSError.E} =
  (* This procedure assures that "dir" is a valid directory.  If it does
     not exist it creates it. *)
  BEGIN
    TRY
      IF FS.Status(dir).type # FS.DirectoryFileType THEN
        RAISE
          OSError.E(AtomList.List1(
                      Atom.FromText(
                        "name of stable obj is not a directory: " & dir)));
      END
    EXCEPT
      OSError.E =>
        Log.PutText("creating dir " & dir);
        Log.Nl();
        FS.CreateDirectory(dir)
    END
  END TestDir;

BEGIN
  default:= NEW(Default).init();
END LogManager.
