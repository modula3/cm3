(* Copyright (C) 1994, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Apr 22 12:17:25 PDT 1994 by wobber *)

INTERFACE SmallDB;

IMPORT AtomList, OSError, Rd, Wr;

(* This package will maintain a copy of a data structure on secondary storage,
   and will update the secondary storage as updates are made to the data
   structure. A client can use this package to ensure that the current value
   of the data structure can be recovered after any crash. This package is
   efficient: the cost of recording an update is about one disk write.

   The secondary storage strategy is to record values in files using a
   representation of the caller's choosing. Two sorts of files are kept:
   snapshots and a log. At any instant, one snapshot is "current". The log
   consists of a sequence of updates that have occurred since the current
   snapshot was taken. The current stable state is the value of the
   snapshot, as modified by the sequence of updates in the log. From time to
   time the client of this package instructs the package to make a new
   snapshot and clear the log. This package arranges disk writes such that
   updates are stable and atomic: no update is lost, and each update either is
   recorded completely in the log or not at all. Making a new snapshot is also
   atomic.

   Normal use for maintaing a database is as follows. The client maintains his
   data structure in virtual memory. As updates happen to the structure, the
   client informs this package by calling "t.update". Periodically, the client
   calls "t.snapshot". On a restart, the client calls "t.recover" to obtain
   the latest snapshot and the following sequence of updates; the client
   applies the updates to the snapshot to obtain the state that existed
   before the crash.
*)

TYPE
  T <: Public;
  Public = OBJECT METHODS
    recover(): REFANY RAISES {OSError.E, Failed};
    update(value: REFANY; forceToDisk: BOOLEAN := TRUE) RAISES {OSError.E};
    snapshot(value: REFANY) RAISES {OSError.E};
    close() RAISES {OSError.E};
    snapshotBytes() : CARDINAL;
    logBytes() : CARDINAL;
    status() : TEXT;
  END;
  
EXCEPTION Failed(AtomList.T);

  (* A "T" is a handle on an open stable storage directory.
  
     Methods are as follows:
     
     recover:
         Returns the REFANY recorded in the current snapshot,
         as recovered by calling "closure.recover" and then
         subsequently invoking "closure.readUpdate" to apply any
         logged updates to the state.

     update:
         Records this update in the log file for "t", by calling
         "closure.logUpdate".  This method must not be called until
         after "recover" has been invoked.  If NOT forceToDisk,
         the update is buffered until the buffer is full.
     
     snapshot:
         Records this value as the current snapshot, by invoking
         "closure.snapshot", and then empties the log.
     
     close:
         Close a stable storage directory in an orderly manner

     snapshotBytes:
         Returns the size of the snapshot file.

     logBytes:
         Returns the size of the log file.

     status:
         Returns human readable status information.
*)
  
TYPE
  Closure = OBJECT METHODS
    new(): REFANY RAISES {Failed};
    recover(rd: Rd.T): REFANY RAISES {Failed, Rd.Failure};
    snapshot(wr: Wr.T; r: REFANY) RAISES {Wr.Failure};
    readUpdate(rd: Rd.T; state: REFANY): REFANY RAISES {Failed, Rd.Failure};
    logUpdate(wr: Wr.T; r: REFANY) RAISES {Wr.Failure};
  END;

(* Each client must implement a closure object.  The readers and writers
   passed to the closure object methods will not raise "Thread.Alerted".
   The methods are to be implemented as follows:

     new        -- no database exists: create and return a new instance of
                   the desired in-core data structure, or raise Failed.

     recover    -- read from "rd" the client-specific representation
                   of a database snapshot.  Decode this representation
                   and return the resulting "REFANY". 
 
     snapshot   -- write a client-specific representation for "r" to "wr".

     readUpdate -- read an stably logged update from "rd" and apply it
                   to the current snapshot value in "state", and returns
                   an updated value of "state".

     logUpdate  -- write a update to "wr" to be recorded in stable storage.
*)


PROCEDURE New(dir: TEXT; cl: Closure; pad: BOOLEAN := TRUE): T
     RAISES {OSError.E, Failed};
(* Returns a "T" for the data that is maintained in files in the directory
   "dir". Raises "OSError.E" if the directory doesn't exist or is
   inaccessible. If the directory exists but contains no backing files,
   creates files corresponding to a new object with no updates.

   If 'pad' then pad out updates to a disk page boundary. *)

END SmallDB.
