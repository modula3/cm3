(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Sat Aug 17 08:30:27 PDT 1996 by steveg *)

MODULE AppBackup;

IMPORT
  App, FileRd, FileWr, Fmt, FS, OSError, 
  Rd, RdUtils, RegularFile, Thread, Time, Wr;

REVEAL
  T = TPublic BRANDED OBJECT
    threadWrite: Thread.T;
    cvChanged: Thread.Condition;
    deltaWait: Time.T;
    lastModified: Time.T;
    changed: BOOLEAN;
  OVERRIDES
    init := Init;
    modified := Modified;
    read := ReadDefault;
    write := WriteDefault;
  END;

CONST
  InitialTime = 0.0D0;

PROCEDURE Init(self: T; fileName: TEXT; wait: Time.T; log: App.Log): T =
  BEGIN
    self.changed := FALSE;
    self.deltaWait := wait;
    self.lastModified := InitialTime;
    self.name := fileName;
    self.log := log;
    self.cvChanged := NEW(Thread.Condition);
    (* self.threadRead := Thread.Fork(NEW(BackupReadClosure, backup := self));
 I now 2/5/96 think this is a bad idea.  There should be a command for this.  *)
    self.threadWrite := Thread.Fork(NEW(BackupWriteClosure, backup := self));
    RETURN self;
  END Init;

PROCEDURE Modified(self: T) =
  BEGIN
    LOCK self DO
      self.changed := TRUE;
    END;
    Thread.Signal(self.cvChanged);
  END Modified;

PROCEDURE ReadDefault(self: T; <* UNUSED *> rd: Rd.T; <* UNUSED *> initial: BOOLEAN) RAISES {App.Error} =
  BEGIN
    self.log.log(Fmt.F("No read method give for %s", self.name), 
                 App.LogStatus.Error);
  END ReadDefault;

PROCEDURE WriteDefault(self: T; <* UNUSED *> wr: Wr.T) RAISES {App.Error} =
  BEGIN
    self.log.log(Fmt.F("No write method give for %s", self.name), 
                 App.LogStatus.Error);
  END WriteDefault;

PROCEDURE LockFile(file: RegularFile.T; t: T) 
  RAISES {App.Error} =
  CONST
    MaxTry = 10;
    RetryInterval = 1.0D0;
  VAR try := 1;
  BEGIN
    TRY
      WHILE NOT file.lock() DO
        IF try = MaxTry THEN
          t.log.log(Fmt.F("Could not lock file: %s", t.name), 
                    App.LogStatus.Error);
        END;
        INC(try);
        Thread.Pause(RetryInterval);
      END;
      (* Check for Windows NT problem *)
      IF file.lock() THEN
        t.log.log("WARNING: Could lock file TWICE", App.LogStatus.Status);
      END;
    EXCEPT
    | OSError.E(cause) => 
        t.log.log(Fmt.F("Could not lock file: %s (error: %s)",
                        t.name, RdUtils.FailureText(cause)),
                  App.LogStatus.Error);
    END;
  END LockFile;

PROCEDURE UnlockFile(file: RegularFile.T; t: T) RAISES {App.Error} =
  BEGIN
    TRY
      file.unlock();
    EXCEPT
    | OSError.E(cause) => 
        t.log.log(Fmt.F("Could not UNlock file: %s (error: %s)",
                        t.name, RdUtils.FailureText(cause)),
                App.LogStatus.Error);
    END;
  END UnlockFile;

PROCEDURE SynchronousRead(t: T; initial: BOOLEAN) RAISES {App.Error} =
  BEGIN
    LOCK t DO ReadBackupFile(t, initial); t.changed := FALSE END;
  END SynchronousRead;

PROCEDURE ReadBackupFile (t: T; initial: BOOLEAN)
  RAISES {App.Error} =
  VAR
    file: RegularFile.T;
    rd  : Rd.T;
  BEGIN
    TRY
      file := FS.OpenFile(t.name, truncate := FALSE,
                          access := FS.AccessOption.OnlyOwnerCanRead);
    EXCEPT
    | OSError.E (cause) =>
        t.log.log(
          Fmt.F("WARNING: Could not open file: %s (error: %s)", t.name,
                RdUtils.FailureText(cause)), App.LogStatus.Status);
        RETURN;
    END;

    TRY
      LockFile(file, t);

      TRY
        rd := NEW(FileRd.T).init(file);
        IF Rd.Length(rd) > 0 THEN t.read(rd, initial); END;
        t.changed := FALSE;
      FINALLY
        UnlockFile(file, t);
        Rd.Close(rd);
      END;
    EXCEPT
    | OSError.E (cause) =>
        t.log.log(
          Fmt.F("Problem reading backup file: %s (error: %s)\n", t.name,
                RdUtils.FailureText(cause)), App.LogStatus.Error);
    | Rd.Failure, Thread.Alerted =>
        t.log.log(Fmt.F("Problem reading backup file: %s\n", t.name),
                  App.LogStatus.Error);
    END;
  END ReadBackupFile;

PROCEDURE SynchronousWrite(t: T) RAISES {App.Error} =
  BEGIN
    LOCK t DO WriteBackupFile(t) END;
  END SynchronousWrite;

PROCEDURE WriteBackupFile (t: T) RAISES {App.Error} =
  VAR
    file: RegularFile.T;
    wr  : Wr.T;
    now                 := Time.Now();
  BEGIN
    TRY
      FS.Rename(t.name, t.name & "-OLD");
      file := FS.OpenFile(t.name, truncate := TRUE,
                          access := FS.AccessOption.OnlyOwnerCanRead);
    EXCEPT
    | OSError.E (cause) =>
        t.log.log(
          Fmt.F("WARNING: Could not open file: %s (error: %s)", t.name,
                RdUtils.FailureText(cause)), App.LogStatus.Status);
        RETURN;
    END;

    TRY
      LockFile(file, t);

      TRY
        wr := NEW(FileWr.T).init(file);
        t.write(wr);
        Wr.Flush(wr);
        t.changed := FALSE;
      FINALLY
        UnlockFile(file, t);
        Wr.Close(wr);
        t.lastModified := now;
      END;
    EXCEPT
    | OSError.E (cause) =>
        t.log.log(
          Fmt.F("Problem writing backup file: %s (error: %s)\n", t.name,
                RdUtils.FailureText(cause)), App.LogStatus.Error);
    | Wr.Failure, Thread.Alerted =>
        t.log.log(Fmt.F("Problem writing backup file: %s\n", t.name),
                  App.LogStatus.Error);
    END;
  END WriteBackupFile;

(*
TYPE
  BackupReadClosure = Thread.Closure OBJECT
    backup: T;
  OVERRIDES
    apply := BackupRead;
  END;

PROCEDURE BackupRead (self: BackupReadClosure): REFANY =
  VAR
    lastMod: Time.T;
  BEGIN
    TRY
      LOOP
        TRY
          LOCK self.backup DO
            (* file rewritten? *)
            TRY
              lastMod := FS.Status(self.backup.name).modificationTime;
            EXCEPT
            | OSError.E =>
                lastMod := InitialTime; (* file does not exist *)
            END;
            IF lastMod > self.backup.lastModified THEN
              ReadBackupFile(
                self.backup, self.backup.lastModified = InitialTime);
              IF self.backup.lastModified = InitialTime THEN
                Thread.Signal(self.backup.cvChanged);
                self.backup.lastModified :=
                  FS.Status(self.backup.name).modificationTime;
              END;
            END;
          END;
        EXCEPT
        | OSError.E (cause) =>
            self.backup.log.log(
              Fmt.F("Problem reading backup file: %s (error: %s)\n",
                    self.backup.name, RdUtils.FailureText(cause)),
              App.LogStatus.Error);
        END;
        Thread.Pause(self.backup.deltaWait);
      END;
    EXCEPT
    | App.Error =>
    END;
    RETURN NIL;
  END BackupRead;
*)

TYPE
  BackupWriteClosure = Thread.Closure OBJECT
    backup: T;
  OVERRIDES
    apply := BackupWrite;
  END;

PROCEDURE BackupWrite (self: BackupWriteClosure): REFANY =
  BEGIN
    LOOP
      LOCK self.backup DO
        WHILE NOT self.backup.changed DO
          Thread.Wait(self.backup, self.backup.cvChanged);
        END;
        TRY
          WriteBackupFile(self.backup); 
        EXCEPT
        | App.Error =>
        END;
      END;
      Thread.Pause(self.backup.deltaWait);
    END;
  END BackupWrite;

BEGIN
END AppBackup.
