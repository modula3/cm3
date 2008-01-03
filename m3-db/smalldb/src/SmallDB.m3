(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* Implement the stable storage module. Michael B. Jones. 18-Jul-86 *)
(* Last modified on Wed Feb  1 09:30:09 PST 1995 by kalsow *)
(*      modified on Fri Apr 22 12:16:19 PDT 1994 by wobber *)
(*      modified on Mon Sep 17 13:54:22 PDT 1990 by birrell *)
(*      modified on Tue Dec 15 17:09:31 1987 by chan *)
(*      modified on Fri Sep  5 06:20:32 1986 by mbj *)

MODULE SmallDB;

IMPORT FS, FileRd, FileWr, OSError, OSSupport;
IMPORT Atom, AtomList, Lex, FloatMode, Fmt, FmtTime, Rd, Text,
       TextRd, Thread, Time, Wr, Word;

REVEAL
  T = Public BRANDED OBJECT
    dirname: TEXT; (* Base directory name (no trailing '/' *)
    version: INTEGER := 0; (* Current snapshot and log version # *)
    logName: TEXT := NIL;
    logWriter: OSSupport.T := NIL; (* Writer to the current logfile *)
    (* statistics *)
    snapshotByteCnt, logByteCnt: CARDINAL := 0;
    logEntries: CARDINAL := 0;
    lastSnapshot, lastLog: Time.T;
    pad: BOOLEAN; (* if TRUE, pad each update to a DiskPageSize boundary *)
    cl: Closure;
  OVERRIDES
    recover := Recover;
    update := Update;
    snapshot := Snapshot;
    close := Close;
    snapshotBytes := SnapshotBytes;
    logBytes := LogBytes;
    status := Status;
  END;

CONST
  DirSeparator = "/";
  SnapshotPrefix = "Snapshot.";
  LogfilePrefix = "Logfile.";
  VersionFile = "Version_Number";
  NewVersionFile = "New_Version_Number";

VAR BadUpdateLen, MalformedUpdate: AtomList.T;

<* FATAL Thread.Alerted *>

(* exported procedures *)

PROCEDURE New(dir: TEXT; cl: Closure; pad: BOOLEAN := TRUE): T
     RAISES {OSError.E, Failed} =
  (* Returns a "T" for the data that is maintained in files in the directory
     "dir". Raises an exception if the directory doesn't exist or is
     inaccessible. If the directory exists but contains no backing files,
     creates files corresponding to a NIL object with no updates. *)
  VAR t: T;
      ok: BOOLEAN := FALSE;
  BEGIN
    TRY
      ok := FS.Status(dir).type = FS.DirectoryFileType;
    EXCEPT
    | OSError.E =>
    END;
    IF NOT ok THEN FS.CreateDirectory(dir); END;
    t := NEW(T, dirname := dir, pad := pad, cl := cl);
    t.lastSnapshot := 0.0D0;
    t.lastLog := 0.0D0;
    GetVersion(t);
    IF t.version = 0 THEN
       (* Commit to version 1, so that we have the log file initialized
	correctly (and atomically).  We can't just create the log file,
	since we'd need special care to make the creation atomic - it's
         easier to just use the mechanisms already present in Snapshot. *)
      Snapshot(t, cl.new());
    END;
    RETURN t;
  END New;


(* private procedures *)

PROCEDURE FName(t: T; n1: TEXT): TEXT =
  (* * Build a file name in a stable storage directory. *)
  BEGIN RETURN t.dirname & DirSeparator & n1; END FName;

PROCEDURE VersionName(t: T; n1: TEXT; thisversion: INTEGER := 0): TEXT =
  (* * Build a file name in a stable storage directory. *)
  VAR version: INTEGER;
  BEGIN
    IF thisversion = 0 THEN
      version := t.version;
    ELSE
      version := thisversion;
    END;
    RETURN t.dirname & DirSeparator & n1 & Fmt.Int(version);
  END VersionName;

PROCEDURE WriteVersionFile(t: T; new: BOOLEAN) RAISES {OSError.E} =
  (* * Write a Version number file for t. *)
  VAR versionWriter: Wr.T; fname: TEXT;
  BEGIN
    TRY
      IF new THEN
        fname := FName(t, NewVersionFile);
      ELSE
        fname := FName(t, VersionFile);
      END;
      versionWriter := FileWr.Open(fname);
      Wr.PutText(versionWriter, Fmt.Int(t.version) & "\n");
      Wr.Close(versionWriter);
    EXCEPT
    | Wr.Failure(e) => RAISE OSError.E(e);
    END;
  END WriteVersionFile;

PROCEDURE DeleteNewVersionFile(t: T) RAISES {OSError.E} =
  (* * Delete a NewVersion number file for t. *)
  VAR fname: TEXT;
  BEGIN
    fname := FName(t, NewVersionFile);
    FS.DeleteFile(fname);
  END DeleteNewVersionFile;

PROCEDURE DeleteSnapshot(t: T; version: INTEGER) RAISES {OSError.E} =
  (* * Delete a NewVersion number file for t. *)
  VAR fname: TEXT;
  BEGIN
    IF version = 0 THEN RETURN; END; (* Never a version 0 *)
    fname := VersionName(t, SnapshotPrefix, version);
    FS.DeleteFile(fname);
  END DeleteSnapshot;

PROCEDURE DeleteLogfile(t: T; version: INTEGER) RAISES {OSError.E} =
  (* * Delete a log file for t. *)
  VAR fname: TEXT;
  BEGIN
    IF version = 0 THEN RETURN; END; (* Never a version 0 *)
    fname := VersionName(t, LogfilePrefix, version);
    FS.DeleteFile(fname);
  END DeleteLogfile;

PROCEDURE CloseLogfile(t: T) RAISES {OSError.E} =
  (* * Close the log file for t. *)
  BEGIN
    IF t.logWriter = NIL THEN RETURN; END;
    TRY
      Wr.Close(t.logWriter);
    EXCEPT
    | Wr.Failure(e) =>
        t.logWriter := NIL;
        RAISE OSError.E(e);
    END;
    t.logWriter := NIL;
  END CloseLogfile;

PROCEDURE OpenLogfile(t: T; truncate: BOOLEAN) RAISES {OSError.E} =
  (* * Open the log file for writing at byte number byte. *)
  VAR fname: TEXT;
  BEGIN
    TRY
      CloseLogfile(t); (* Close any open log file *)
    EXCEPT
    | OSError.E => (* Assume can proceed ok *)
    END;

    fname := VersionName(t, LogfilePrefix);
    t.logName := fname;
    t.logWriter := NEW(OSSupport.T).init(FS.OpenFile(fname, truncate));
    (* force changes to disk ??? *)
  END OpenLogfile;

PROCEDURE CreateFirstVersion(t: T) RAISES {OSError.E} =
  (* * Initialize a stable storage directory. *)
  BEGIN
    t.version := 0;
    (* No snapshot file for version 0 *)
    WriteVersionFile(t, FALSE);
  END CreateFirstVersion;

PROCEDURE IncrVersion(t: T) =
  (* * Increment the directory version number. *)
  BEGIN
    REPEAT t.version := t.version + 1; UNTIL t.version # 0;
    (* No snapshot file for version 0 *)
  END IncrVersion;

PROCEDURE CommitToNewVersion(t: T) RAISES {OSError.E} =
  (* * Take us from the state where we have a NewVersion file to where we
     don't. *)
  BEGIN
    WriteVersionFile(t, FALSE);
    DeleteNewVersionFile(t);
  END CommitToNewVersion;

PROCEDURE GetVersion(t: T) RAISES {OSError.E} =
  (* * Determines the current version number for the directory t. *)
  VAR versionReader: Rd.T;
  VAR version: INTEGER;
  BEGIN
    TRY
      versionReader := FileRd.Open(FName(t, NewVersionFile));
      TRY
        version := Lex.Int(versionReader);
        IF version <= 0 THEN
          RAISE Lex.Error; (* No number! *)
        END;
      FINALLY
        Rd.Close(versionReader);
      END;
      t.version := version;
      CommitToNewVersion(t);
    EXCEPT
    | OSError.E, Lex.Error, Rd.Failure, FloatMode.Trap =>

        TRY
          DeleteNewVersionFile(t); (* Make SURE it's not there *)
        EXCEPT
        | OSError.E =>
        END;
        
        TRY
          versionReader := FileRd.Open(FName(t, VersionFile));
          TRY
            version := Lex.Int(versionReader);
            IF version <= 0 THEN
              RAISE Lex.Error; (* No number! *)
            END;
          FINALLY
            Rd.Close(versionReader);
          END;
          t.version := version;
        EXCEPT
        | OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure =>
            CreateFirstVersion(t);
        END;
    END;
  END GetVersion;

PROCEDURE Snapshot(t: T; value: REFANY) RAISES {OSError.E} =
  (* Records this value as the current snapshot, and empties the log. *)
  VAR
    snapshotWriter: Wr.T;
    oldversion: INTEGER;
    fname: TEXT;
  BEGIN
    oldversion := t.version;
    IncrVersion(t);
    TRY
      fname := VersionName(t, SnapshotPrefix);
      snapshotWriter := FileWr.Open(fname);
      TRY
        t.cl.snapshot(snapshotWriter, value);
        t.snapshotByteCnt := Wr.Length(snapshotWriter);
        t.lastSnapshot := Time.Now();
      FINALLY
        Wr.Close(snapshotWriter);
      END;
    EXCEPT
    | Wr.Failure(e) => RAISE OSError.E(e);
    END;

    OpenLogfile(t, TRUE);
    t.logByteCnt := 0;
    t.logEntries := 0;
    WriteVersionFile(t, TRUE);
    CommitToNewVersion(t);
    DeleteSnapshot(t, oldversion);
    DeleteLogfile(t, oldversion);
  END Snapshot;

PROCEDURE Recover(t: T) : REFANY RAISES {OSError.E, Failed} =
  (* Returns a REFANY which is the value recorded in the current snapshot. *)
  VAR
    snapshotReader: Rd.T;
    snapshot: REFANY;
    fname: TEXT;
  BEGIN
    IF t.version = 0 THEN RETURN NIL END;
    TRY
      fname := VersionName(t, SnapshotPrefix);
      snapshotReader := FileRd.Open(fname);
      TRY
        snapshot := t.cl.recover(snapshotReader);
        t.snapshotByteCnt := Rd.Length(snapshotReader);
      FINALLY
        Rd.Close(snapshotReader);
      END;
    EXCEPT
    | Rd.Failure(e) => RAISE OSError.E(e);
    END;
    RETURN RecoverUpdates(t, snapshot);
  END Recover;

CONST
  DiskPageSize = 512;
  IntBytes = 4;   (* writing only 32 bits of update length *)
  ByteBits = BITSIZE(CHAR);
  
PROCEDURE Update(t: T; update: REFANY; forceToDisk: BOOLEAN := TRUE )
    RAISES {OSError.E} =
  (* Records this update in the log file for "t" *)
  VAR
    updateLen: Word.T;
    entryStart, entryEnd: CARDINAL;
  BEGIN
    
    <* ASSERT t.logWriter # NIL *>
    TRY
      entryStart := Wr.Index(t.logWriter);
      FOR i := 0 TO IntBytes - 1 DO (* Write zero update len *)
        Wr.PutChar(t.logWriter, VAL(0, CHAR)); (* Chars of update len *)
      END;

      t.cl.logUpdate(t.logWriter, update);

      entryEnd := Wr.Index(t.logWriter);
      updateLen := (entryEnd - entryStart) - IntBytes;

      IF forceToDisk THEN (* force update contents before updating length *)
        Wr.Flush(t.logWriter);
        OSSupport.Sync(t.logWriter);
      END;
      Wr.Seek(t.logWriter, entryStart);
      FOR i := 0 TO IntBytes-1 DO (* Write real update len *)
        Wr.PutChar(t.logWriter,
           VAL(Word.Extract(updateLen, i*ByteBits, ByteBits), CHAR));
      END;
      Wr.Seek(t.logWriter, entryEnd);

      (* don't start an entry at the end of page *)
      IF t.pad OR (entryEnd MOD DiskPageSize > DiskPageSize-IntBytes) THEN
        WHILE (Wr.Index(t.logWriter) MOD DiskPageSize) # 0 DO
          Wr.PutChar(t.logWriter, VAL(0, CHAR)); (* Fill to page boundary *)
        END;
      END;

      IF forceToDisk THEN
        Wr.Flush(t.logWriter);
        OSSupport.Sync(t.logWriter);
        (* force changes to disk *)
      END;
      t.logByteCnt := Wr.Index(t.logWriter);
      t.lastLog := Time.Now();
      INC(t.logEntries);
    EXCEPT
    | Wr.Failure(e) => RAISE OSError.E(e);
    END;
  END Update;

PROCEDURE RecoverUpdates(t: T; state: REFANY): REFANY
    RAISES {OSError.E, Failed} =
  VAR
    logReader: Rd.T;
    updateLen: Word.T;
    fname: TEXT;
    updateText: TEXT;
    pos: CARDINAL;
  BEGIN
    IF t.version = 0 THEN RETURN state; END;
    TRY
      fname := VersionName(t, LogfilePrefix);
      logReader := FileRd.Open(fname);
      TRY
        LOOP
          IF Rd.EOF(logReader) THEN EXIT END;
          
          updateLen := 0;
          FOR i := 0 TO IntBytes - 1 DO
            TRY
              updateLen := Word.Insert(
                updateLen, ORD(Rd.GetChar(logReader)), i*ByteBits, ByteBits);
            EXCEPT
            | Rd.EndOfFile => RAISE Failed(MalformedUpdate);
            END;
          END;

          IF updateLen = 0 THEN
            EXIT (* crashed while writing last log entry *)
          END;

          IF (updateLen < 0) THEN RAISE Failed(BadUpdateLen); END;

          updateText := Rd.GetText(logReader, updateLen);
          IF Text.Length(updateText) # updateLen THEN
            RAISE Failed(BadUpdateLen);
          END;

          state := t.cl.readUpdate(TextRd.New(updateText), state);

          t.logByteCnt := Rd.Index(logReader);
          pos := ((t.logByteCnt+DiskPageSize-1) DIV DiskPageSize) * DiskPageSize;
          IF t.pad OR ((pos-t.logByteCnt) < IntBytes) THEN
            Rd.Seek(logReader, pos); (* Seek to page boundary *)
            t.logByteCnt := pos;
          END;
          INC(t.logEntries);
        END;
      FINALLY
        Rd.Close(logReader);
      END;
    EXCEPT
    | Rd.Failure(e) => RAISE OSError.E(e);
    END;

    (* now reopen the file at the end *)
    OpenLogfile(t, FALSE); (* Prepare for possible appending *)
      (* this code truncates the log file, in case the
         last entry was incompletely written *)
    
    TRY
      Wr.Seek(t.logWriter, t.logByteCnt);
      OSSupport.Truncate(t.logWriter);
      IF t.pad OR (t.logByteCnt MOD DiskPageSize > DiskPageSize-IntBytes) THEN
        WHILE (Wr.Index(t.logWriter) MOD DiskPageSize) # 0 DO
          Wr.PutChar(t.logWriter, VAL(0, CHAR)); (* Fill to page boundary *)
        END;
      END;
    EXCEPT
    | Wr.Failure(e) => RAISE OSError.E(e);
    END;
    RETURN state;
  END RecoverUpdates;

PROCEDURE Close(t: T) RAISES {OSError.E} =
  (* Close a stable storage directory in an orderly manner *)
  BEGIN CloseLogfile(t); END Close;

PROCEDURE SnapshotBytes(t: T): CARDINAL =
  BEGIN RETURN t.snapshotByteCnt; END SnapshotBytes;

PROCEDURE LogBytes(t: T): CARDINAL = BEGIN RETURN t.logByteCnt; END LogBytes;

PROCEDURE Status(t: T) : TEXT =
  VAR out: TEXT;
  BEGIN
    out := "   Stable storage status for directory \"" & t.dirname & "\"\n";
    out := out & "      Snapshot version " & Fmt.Int(t.version)
              & ", size " & Fmt.Int(t.snapshotByteCnt) & " bytes.\n";
    IF t.lastSnapshot # 0.0D0 THEN
      out := out & "      Last snapshot was written at " &
         FmtTime.Long(t.lastSnapshot) & "\n";
    ELSE
      out := out & "      No snapshot has been made in this run.\n";
    END;
    out := out & "      Log has " & Fmt.Int(t.logEntries)
               & " entries, total size "
               & Fmt.Int(t.logByteCnt) & " bytes.\n";
    IF t.lastLog # 0.0D0 THEN
      out := out & "      Last log entry was written at " &
         FmtTime.Long(t.lastLog) & "\n";
    ELSE
      out := out & "      No log entries have been written in this run.\n";
    END;
    RETURN out;
  END Status;

BEGIN
  BadUpdateLen := AtomList.List1(Atom.FromText("SmallDB.BadUpdateLen"));
  MalformedUpdate := AtomList.List1(Atom.FromText("SmallDB.MalformedUpdate"));
END SmallDB.
