(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* File: ServerLog.i3,  server logging utilities              *)
(* Last modified on Fri Apr 29 12:56:53 PDT 1994 by wobber     *)

INTERFACE ServerLog;

  (* Logging facilities for servers

     This interface provides a minimal set of logging and statistics
     gathering tools that might be of use to server implementations. In
     particular it provides a writer facility intended to work in a
     concurrent environment.  Statistics and status information about the
     running program can be made available to the local "ServerStatDaemon"
     which co-resides with the SRC RPCAgent on all machines.  This data can
     be extracted and monitored remotely with the serverstat(1) utility.

     Clients should explicitly include ServerLog.a at link time.

     Index: servers; logging *)

IMPORT Time, Wr;

TYPE
  T <: REFANY;
  
    (* A ServerLog.T is essentially a name plus a writer plus an list
       of statistics objects.

       The name is the name of the server, e.g. "packageserver".

       The writer represents the log file of the server.  The ServerLog
       package provides a convenient way for multiple threads to share this
       writer without concurrency errors.

       Each statistics object represents a set of statistics. A
       statistic consists of an up/down event counter and a average
       elapsed time. You can use only the event counter or only the
       timer or both. *)


(* initialization *)

PROCEDURE Init(
  logWr:            Wr.T;
  lazyWriter:       BOOLEAN := FALSE;
  flusherPause:     Time.T := 0.0D0;
  showMilliseconds: BOOLEAN := FALSE;
  maxQueueLength:   CARDINAL := 0
  ): T;
  (* Creates a new ServerLog.T with the specified writer, and an
     empty list of statistics objects.

     Having given the writer to Init, you must never use it again (except
     indirectly via calls to this interface).  Using the writer again would
     be an unchecked error.

     If lazyWriter is FALSE (the default), the writer will be flushed on
     every call to WriteText.  If the writer is to a disk file, the file
     will be synced after the writer is flushed.  In some applications,
     syncing this frequently is too expensive.  If lazyWr is TRUE,
     flusherPause is the interval at which the background flusher operates.
     If 0 is passed a default value is chosen (currently 10).  If
     showMilliseconds is TRUE, then milliseconds are shown on every log entry.
     maxQueueLength is the maximum number of writes that may be queued for a
     lazy logger.  Any more writes than that are lost and a message is given
     to the logging thread.
  *)

PROCEDURE WriteText(t: T; text: TEXT);
(*     Writes text directly to the log t. *)

PROCEDURE DumpStats(t: T; resetTimes: BOOLEAN := TRUE) : TEXT;
(*    Returns a summary of all statistics the log t holds.
      The list is subject to change, but currently includes:

          for each "Stats" created on "t":
             the tag string (from NewStats)
             the elapsed time since "t" was created
             named statistics (from all Stats objects)

       Resets the named statistics to zero if resetTimes is TRUE; otherwise
       leaves them alone. *)


(* statistic manipulators *)

TYPE
  Stats <: StatsPublic;
  StatsPublic = OBJECT METHODS
    incr(istat: CARDINAL; n: CARDINAL := 1);
    decr(istat: CARDINAL; n: CARDINAL := 1);
    time(istat: CARDINAL; start: Time.T);
  END;
  (* These methods are used to modify the state of individual statistics.
     For each "Stats" method, the "istat" parameter specifies the index of
     a specific statistic.   The range of valid statistic indices is
     specified by "NewStats".  It is a checked runtime error to supply
     a "istat" which is out of range. *)

  (* The "time" method records the time interval that has elapsed
     since "start".  These elapsed intervals are summed and averaged for
     the statistics output. *)

PROCEDURE NewStats(t: T; tag: TEXT; VAR statNames: ARRAY OF TEXT): Stats;
  (* Defines a set of named statistics to be kept for this log "T".  A
     single log "T" can have multiple attached sets of statistics. The
     "tag" field is used to preceed the statistics output.  The "statNames"
     are used to identify each statistic in the output.  The length of
     the "statNames" array defines the range of valid statistics indices
     for the resultant "Stats" object. *)

END ServerLog.


