(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* LockRestart.i3 *)
(* Last modified on Tue Feb 22 13:30:11 PST 1994 by wobber *)

(* this module builds a lock database from scratch *)

INTERFACE LockRestart;

PROCEDURE Recover ();
  (* this procedure attempts to recreate the package lock database *)
  (* it does the following things ("initReps := PathMap.GetReps()"):
      1) adds "initReps" to the lock DB
      2) enumerates all dirs in the lock DB
      3) foreach dir, enumerates all available siphon sites
      4) foreach dir, reconstructs local entries according to the
            remote enumerations,
            each entry so created gets a curVN of InitialVN. this ensures
            we'll (eventually) fetch from the remote siphon, that is unless
            we later find a newer local version.
      5) for all packages in the local lock DB:
            a)  contact all known replicas to determine the most recent
                version number.
            b)  modify the lock DB to reflect the existence of this version.
  *)
  
PROCEDURE Restart (): BOOLEAN;
  (* this restarts the database ... deals with transactions that
     were in progress at the time of the previous crash *)

END LockRestart.
