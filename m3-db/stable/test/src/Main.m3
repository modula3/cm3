(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:34:50 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 18:30:00 PDT 1994 by weich      *)

(* The test consists of allocating a stable test object, checkpoint,
   modify it and let the program crash at different places.  In a second
   run the leftover of the crash is compared with the object samples that
   contain the state the recovered objects should have.
   To start the test use:
|
|   Test [-verbose] [-tmp] [check] [CrashPointNumber]
|
   There are two main modes of the "Test" program:
   \begin{enumerate}
   \item   Manipulating a stable test object:
                 \begin{itemize}
                 \item Reset the stable object (using "LogManager.T.cleanup()"),
                 \item initialize it (using static initializations),
                 \item first modification,
                 \item second modification,
                 \item write a checkpoint, or cleanup (when testing cleanup crashes, see below)
                 \item terminate.
                 \end{itemize}
    \item  Check whether a checkpoint and log read leads to an object that
                 has the right value.
    \end{enumerate}
    The second mode is entered when the "check" option is present in the run
    string. The "CrashPointNumber" is a integer that specifies where the
    program shall crash in the first mode. In the check mode the number is used
    to determine, which value of the recovered object should be expected.
    If the check is positiv, "Test" will terminate normally, if not "Process.Exit(1)"
    is used to stop the program. The "-verbose" option will turn on the
    logging messages of the log manager and the implementation module of the
    stable object. The defaults are no verbose logging, first mode, "CrashPointNumber"
    equals 0.  The "-tmp" option causes the program to use "/tmp/stableobjtest"
    as the directory for stable storage.  The default directory is
    "stableobjtest" in the current directory.

\subsection{Testcases}
This section is used to collect all cases that have to be tested when
something is changed in the implementation of the stable object
moduls. The numbers in brackets are "CrashPointNumber"s that
will test the case.

\subsubsection{Normal operation}
Use {\bf (0)} to let the program run without crash.

\subsubsection{Crashes outside}
Test crashes that occure before and after the client did anything. Usually
this also tests the normal operation.
\begin{description}
\item[(-1)]     Terminate before clients initialisation.
\item[(999)] Terminate just before normal client termination.
\end{description}

\subsubsection{Bootstrapping}
The first time a checkpoint is written, severel conditions arise that
are unique. The following should be tested:
\begin{description}
\item[(101-106)]Crashes during the first checkpoint (see next subsection
                                about possible crashes during checkpointing). Add 100
                                to the crash point number to get the aquivalent number
                                of the next section.
\end{description}

\subsubsection{Crashes during Checkpointing}
We see six different ways to crash during checkpointing. In the
current implementation "LogManager.CrashPoint"'s are included
with numbers as follows:
\begin{description}
\item[(201)]	Crash during renaming the checkpoint to the backup name.
\item[(202)]	Crash during writing the new checkpoint.
\item[(203)]	Crash during renaming the new checkpoint to the final name.
\item[(204)]	Crash during truncating the log.
\item[(205)]	Crash during deleting the backup checkpoint.
\item[(206)]	Crash {\em after} all checkpoint actions.
\end{description}
Note that renaming and deleting files is considered to be an atomic
action. So ``crash during rename/deletion'' means a crash before the
renaming/deletion was finished.
All these must be recovered without loss of data (i.e.\ without loosing
anything that was flushed to the log).

\subsubsection{Crashes in client code}
\begin{description}
\item[(301)] Client crashes are simulated between the two modifications
                          of the stable object. 
\end{description}

\subsubsection{Crashes in update methods}
Update methods can crash while writing parameters to the log or during
executing the method doing the update. All the crashes are simulated
in the first update method following the "301" crash point (i.e.\ before
any of the changes in the second modification part was done).
\begin{description}
\item[(401)] Crash occurs during writing to the log {\em before} executing
                         the super call to update method.
\item[(402)] Like "401" but {\em after} the super call and {\em before} the
                         message about successful termination of the update date method
                         could be written to the log.
\item[(403)] Crash occurs {\em inside} the super call.
\end{description}

\subsubsection{Crashes while cleaning up}
The cleanup method might crash before or after deleting the checkpoint.
Crashes before deleting the checkpoint must leave valid data. Crashes after
deleting the checkpoint must leave a state that is treated as if there is no
leftover at all. If startet with "CrashPointNumber"'s "501", "502" or "503", the
test program will not call "StableTestObj.Checkpoint()" but "dispose"
before terminating (such there is a checkpoint {\em and} a redo-log to
delete).
\begin{description}
\item[(501)]         Crash before deleting checkpoint.
\item[(502-503)]Crash after deleting checkpoint and after deleting
                                log repectively.
\end{description}
There are several special cases when crashing while cleaning up leftovers from
a crashed checkpoint write. No effort is made in the current implementation to
handle these situations since they seem to be extremly rare.
Two crashes have to follow each in a certain order during two subsequent
calls to a program. Such situations can be handled manually by removing
the checkpoint using the operating system.
*)

MODULE Main;

IMPORT TestObj, StableTestObj, StableError, Log, LogManager;
IMPORT Text, OSError, Fmt, Scan, IO, Lex, FloatMode,
       Process, Params;
FROM Log IMPORT CrashPoint;

CONST
  Usage = "Test [-tmp] [-verbose] [check] [CrashPointNumber]\n"
          & "Defaults: not verbose, no checkmode, CrashPointNumber=0";
VAR
  Dir := "stableobjtest";

EXCEPTION WrongUsage;


(* \subsection{Testclient} *)

PROCEDURE Client(crash: INTEGER)
  RAISES {StableError.E, OSError.E} =
  BEGIN
    IO.Put("Running test "); IO.PutInt(crash); IO.Put("\n");
    Log.InProc("Client");
    IF NOT (201<=crash AND crash<=206) AND
      NOT (401<=crash AND crash<=403) AND
      NOT (501<=crash AND crash<=503)
     THEN
      Log.crash:= crash;
    END;
    CrashPoint(-1);
    NEW(LogManager.Default).dispose(Dir);

(* \paragraph{Testclient} Allocate a stable object and modify it twice.
   "init()" will write a checkpoint, it will crash on "crash" equals
   101..106 (bootstrapping crash test). "crash" in 201..206 should test the
   second (now explicit) checkpoint. Thus the "Log.crash" variable has to
   be set {\em after} initialization in this cases. This is done before the
   call to "Checkpoint()". Values of "crash" in 401..403 have to be set {\em after}
   the first modification suite (in order to have a non empty log when crashing).
   Values of "crash" in 501..503 require doing a
   cleanup. Thus no checkpoint is written in this cases. *)
    VAR
      rec: BOOLEAN;
      testObj:= NEW(StableTestObj.T).init(Dir, rec);
    BEGIN
      Log.PutText("Allocated test object"); Log.Nl();
      IF rec THEN Failed("cleanup() did not clean up") END;
      Log.PutText("first modification suite"); Log.Nl();

      TestObj.Modify1(testObj);

      CrashPoint(301);
      IF 401<=crash AND crash<=403 THEN Log.crash:= crash END;
      Log.PutText("second modification suite"); Log.Nl();

      TestObj.Modify2(testObj);

      IF 201<=crash AND crash<=206 THEN Log.crash:= crash-100 END;
      IF 501<=crash AND crash<=503 THEN
        Log.crash:= crash;
        Log.PutText("cleaning up"); Log.Nl();
        testObj.dispose();
      ELSE
        Log.PutText("writing checkpoint"); Log.Nl();
        StableTestObj.Checkpoint(testObj);
      END;

      CrashPoint(999);

      (* DLC: 1/95 - more checks for dispose/init interaction *)
      Log.PutText("extra dispose/init testing"); Log.Nl();
      testObj.dispose();
      testObj:= NEW(StableTestObj.T).init(Dir, rec);
      IF rec THEN
        Failed("state should not be recovered after dispose\n");
      END;
      TestObj.Modify1(testObj);
      testObj.dispose();
      testObj.dispose();
      IF NOT TestObj.Equal1(testObj) THEN
        Failed("state should not be modified after dispose\n");
      END;
      testObj := testObj.init(Dir, rec);
      IF NOT TestObj.Equal1(testObj) THEN
        Failed("state shoule not be modified after restabilization\n");
      END;
      testObj.dispose();
      TestObj.Modify2(testObj);
      testObj := testObj.init(Dir, rec);

    END;
    Log.ExitProc();
  END Client;


(* \subsection{Check Leftovers from "Client()"} *)

PROCEDURE Failed(msg: TEXT) =
  BEGIN
    IO.Put("Test failed: "&msg&"\n");
    Process.Exit(1);
  END Failed;

PROCEDURE Check(crash: INTEGER) RAISES {StableError.E} =
  BEGIN
    IO.Put("Checking leftover from "); IO.PutInt(crash);
    IO.Put("\n");
    VAR
      rec: BOOLEAN;
      testObj:= NEW(StableTestObj.T).init(Dir, rec);
    BEGIN

(* There are four possible states of the leftover: Empty, initial checkpoint
   successful, after first modification, after second modification. No others are valid *)
      CASE crash OF
      | -1, 101..103, 502, 503=>
          Log.PutText("there should be nothing to recover from");
          Log.Nl();
          IF rec THEN
            Failed(Fmt.Int(crash)&" leftover should not be there")
          END;
      | 104..106=>
          Log.PutText("recovered object should equal an empty one");
          Log.Nl();
          IF NOT rec THEN
            Failed(Fmt.Int(crash)&" could not recover")
          END;
          IF NOT TestObj.Equal0(testObj) THEN
            Failed(Fmt.Int(crash))
          END;
      | 301, 401..403=>
          Log.PutText("recovered object should have state as ");
          Log.PutText("after first modification suite");
          Log.Nl();
          IF NOT rec THEN 
            Failed(Fmt.Int(crash)&" could not recover")
          END;
          IF NOT TestObj.Equal1(testObj) THEN
            Failed(Fmt.Int(crash))
          END;
      | 0, 201..206, 501, 999=>
          Log.PutText("all changes should be saved");
          Log.Nl();
          IF NOT rec THEN
            Failed(Fmt.Int(crash)&" could not recover") END;
          IF NOT TestObj.Equal2(testObj) THEN
            Failed(Fmt.Int(crash))
          END;
        ELSE Failed(Fmt.Int(crash)&" unimplemented CrashPointNr");
      END; (*CASE*)

      (* Attempt to clean up.
         A dispose might fail if the stable object is stored in a
         NFS directory and one of the files is still open. *)
      testObj.dispose(); 
    END;
    IO.Put("passed\n");
  END Check;


(* \subsection{Parsing the parameters} *)

PROCEDURE GetRunstring(VAR check, verbose: BOOLEAN;
                        VAR crash: INTEGER)
  RAISES {WrongUsage} =
  VAR count:= Params.Count;
      nr:= 1;
  BEGIN
    verbose:= FALSE; check:= FALSE; crash:= 0;
    IF count > 5 THEN RAISE WrongUsage END;
    WHILE nr < count DO
      IF Text.Equal(Params.Get(nr), "-v") OR
        Text.Equal(Params.Get(nr), "-verbose") THEN
        verbose:= TRUE;
        INC(nr);
      ELSIF Text.Equal(Params.Get(nr), "-t") OR
        Text.Equal(Params.Get(nr), "-tmp") THEN
        Dir := "/tmp/stableobjtest";
        INC(nr)
      ELSIF Text.Equal(Params.Get(nr), "check") THEN
        check:= TRUE;
        INC(nr)
      ELSE
        TRY crash:= Scan.Int(Params.Get(nr))
        EXCEPT Lex.Error, FloatMode.Trap=> RAISE WrongUsage
        END;
        IF nr # count-1 THEN RAISE WrongUsage END;
        INC(nr);
      END;
    END;
  END GetRunstring;


(* \subsection{Main of test program} *)

VAR check, verbose: BOOLEAN;
    crash: INTEGER;
BEGIN
  TRY
    GetRunstring(check, verbose, crash);
    IF verbose THEN Log.level:= 1 ELSE Log.level:= 0 END;
    IF check THEN Check(crash) ELSE Client(crash) END
  EXCEPT
  | WrongUsage=> IO.Put(Usage); IO.Put("\n"); Process.Exit(2);
  | StableError.E(err)=>
         IO.Put("Test: StableError.E occured: "
                &StableError.ToText(err)&"\n");
         Process.Exit(2);
  | OSError.E(err)=>
         IO.Put("Test: OSError occured: "
                &StableError.ToText(err)&"\n");
         Process.Exit(2);
  END
END Main.
