(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich and Greg Nelson                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 12:56:06 PST 1995 by kalsow     *)
(*      modified on Thu Sep 22 18:25:39 PDT 1994 by weich      *)

(* A "LogManager" provides an object which manages readers and writers
   for the log and checkpoint files used by stable objects.

   The default log manager stores the stable state in the file system.
   Since the default is satisfactory for most applications, 
   most clients of stable objects don't need to use the LogManager 
   interface directly.  
   
   A situation in which you might want to use this interface is 
   if you want to test whether a checkpoint is available before initializing 
   a stable object.  In this case the "recoverable" method of the 
   log manager will be useful to you.  As another example, if you 
   don't want to use the file system for storing the stable state, 
   you will need to study the specifications in this interface and 
   implement your own log manager that stores the stable state in 
   your preferred form of stable storage.  *)

INTERFACE LogManager;

  IMPORT Pathname, Wr, Rd, OSError;

  TYPE T <: Public;

    Public = OBJECT METHODS
        beginCheckpoint(nm: Pathname.T): Wr.T (* new checkpoint *)
            RAISES {OSError.E};
        endCheckpoint(nm: Pathname.T):Wr.T (* new log *)
            RAISES {OSError.E};
        reOpenLog(nm: Pathname.T): Wr.T RAISES {OSError.E};
        recover(nm: Pathname.T; VAR log, checkp: Rd.T)
            RAISES {OSError.E};
        recoverable(nm: Pathname.T): BOOLEAN
            RAISES {OSError.E};
        emptyLog(nm: Pathname.T): BOOLEAN
            RAISES {OSError.E};
        dispose(nm: Pathname.T) 
            RAISES {OSError.E};
      END;

    DefaultPublic = T OBJECT METHODS init(): Default END;
    Default <: DefaultPublic;

    VAR default: Default;

END LogManager.

(* A "LogManager.T" manages a repository of named stable snapshots. 
   A snapshot consists of two sequences of bytes, a {\it checkpoint} 
   and a {\it redo log}.  The repository must be stable; that is, 
   it must survive program crashes.  Here are specifications for 
   the methods of a "LogManager.T" named "lm": 

   The  "beginCheckpoint" and "endCheckpoint" methods are
   used to write a new stable snapshot.  The call

| lm.beginCheckpoint(nm)

   returns a writer whose target is the checkpoint named "nm"
   in "lm"'s repository.  The call

|  lm.endCheckpoint(nm)

   should be made only after a previous call to "lm.beginCheckpoint(nm)". 
   The "endCheckpoint" method commits the bytes that have been written 
   to the writer that was returned by "beginCheckpoint", making them 
   become the new checkpoint. The "endCheckpoint" method also empties 
   the redo log and returns a writer that can be used to extend 
   the now-empty redo log. 

   Therefore, to make a new checkpoint, you should execute the following 
   steps: 

| wr1 := lm.beginCheckpoint(nm);
| `write a new checkpoint to ` wr1;
| wr2 := lm.endCheckpoint(nm);
| `new checkpoint is made; log updates to ` wr2

   If the application exits or crashes before the call to "endCheckpoint", 
   any bytes written to "wr1" will be discarded, and the previous 
   checkpoint will not be changed.  (Of course these steps are performed
   by the "Checkpoint" procedure in the generic "Stable" interface, so
   there is no reason for the typical client of stable objects to
   recode this procedure.  But if you are implementing your own
   log managers, it is important to know how the methods will
   be used by your clients.)

   An application may close the writer returned by "endCheckpoint" 
   (for example, to free up file descriptors or other resources associated 
   with inactive stable objects).  If this writer has been closed, 
   the call 

| lm.reopenLog(nm)

   will return a writer which will append to the current redo
   log.  

   The call

| lm.recoverable(nm)

   returns "TRUE" if and only if the repository managed by "lm" contains 
   a snapshot named "nm". 

   The call

| lm.emptyLog(nm)

   returns "TRUE" if and only if the repository managed by "lm" contains 
   a snapshot "nm" which has an empty log. The log will be empty if the
    program that created a stable object did a checkpoint right
    before terminating. A non empty log might indicate a crashed program.
    "emptyLog()" will raise the exception if there is no snapshot "nm".

   The call

| lm.recover(nm, cprd, logrd)

   sets "cprd" and "logrd" to readers whose sources are the
   checkpoint and redo log named "nm", respectively, assuming
   a snapshot exists under that name in the repository managed
   by "lm".

   The call

| lm.dispose(nm)

   discards any snapshot named "nm" from "lm"'s repository and
   reclaims any associated stable storage.

So much for the methods of a general "LogManager.T".  Almost all
clients will use a "LogManager.Default", which uses the
ordinary file system as its source of stable storage.
A "LogManager.Default" must be initialized by the client;
to obtain one, call

| VAR lm := NEW(LogManager.Default).init(); ...

The methods of a "LogManager.Default" interpret "nm" as a directory 
in the file system, in which they expects to find files containing a
checkpoint and log.  The "beginCheckpoint" method will create the
directory if necessary.  File renaming is used to make checkpoint
atomic with respect to crashes.

The variable "LogManager.default" is initialized by the
"LogManager" module to a valid "LogManager.Default".

*)

