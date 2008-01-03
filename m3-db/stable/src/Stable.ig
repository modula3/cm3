(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich and Greg Nelson                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:09:06 PST 1995 by kalsow     *)
(*      modified on Thu Jan 12 09:31:24 PST 1995 by chaiken    *)
(*      modified on Tue Sep 27 11:53:14 PDT 1994 by weich      *)

(*  A {\it stable object} is an object whose state is stored on the disk
    or other medium whence its state can be recovered if a program crashes.
    
    The generic interface "Stable" defines a subtype of a given object
    type that is just like that object type, but stable.  The generic
    argument to "Stable" is an interface named "Data", which is
    assumed to contain an object type named "Data.T".  Thus the type
    "Stable(Data).T" is like a "Data.T", but stable.  In case of a
    failure (of either the program or the system) such objects can
    recover by resuming their last recorded state.

    The state of a stable object is stored as a checkpoint together 
    with a redo log; the log contains entries for all updates performed 
    since the last checkpoint. These updates are recorded by logging 
    a number identifying an update method, together with the arguments 
    of the method. The typical cost for an update is therefore on 
    the order of the cost of a single disk write. 

    In order to keep the redo log from growing without bound (which 
    would cause the recovery time to grow without bound), it is necessary 
    to periodically write a new checkpoint.  While writing a new 
    checkpoint the data structure is unavailable for updating.  As 
    a client of the "Stable" package, you can control how often checkpoints 
    are made. 
    
    The strategy used by the "Stable" package is described more fully 
    by Andrew D. Birrell, Michael B. Jones, and Edward P. Wobber 
    in ``A simple and efficient implementation for small databases'', 
    {\it Proceedings of the 11th ACM Symposium on Operating System 
    Principles}.  This paper is also available as SRC Research Report 
    24, January 1988. 

    The stable stub generator "stablegen" will automatically produce 
    an implementation of any generic instance "Stable(Data)". The 
    default implementation produced by "stablegen" reads and writes 
    checkpoints using the pickles package, and  reads and writes 
    the redo log using the same marshaling mechanism as the network 
    objects stub generator ("stubgen").  This is suitable for many 
    applications, but not for all.  The comments at the end of the 
    interface explain how to customize the behavior of the stable 
    package in several different ways.  

    The initialization method for a stable object will reset it to 
    the stable state, if the stable state is present; otherwise the 
    method creates and initializes the stable state. 
 
    By default, the stable state (checkpoint and log) for an object 
    is stored in a directory in the ordinary file system.  It is 
    possible to override this default.  For example, in a system 
    that contains some form of stable random access memeory, it may 
    be preferable for reasons of speed to keep the stable data in 
    the stable RAM instead of on the disk.  This flexibility is provided 
    by an abstraction called a "LogManager", which is basically an 
    object that provides streams for logs and checkpoints.  A "LogManager" 
    may be provided when a stable object is initialized; if omitted, 
    the default log manager is used, which stores the log and checkpoint 
    in the ordinary file system. See "LogManager.i3" for details. 

    The stub generator produces overrides for all update methods 
    of the stable object. The override method writes its parameters 
    together with a method number to the log, and then calls the 
    corresponding method of its supertype.  Finally they write an 
    additional "commit" record to the log to record that the method 
    terminated. (If a crash occurs while the method is running, the 
    commit record will not be present and the call that crashed will 
    not be repeated during recovery.) 
    
    The recovery process is started by "init" if a stable 
    backup is found for the object being initialized.    To recover, 
    the log manager is used to obtain a reader on the checkpoint, 
    the "readCheckpoint" method of the object is used to reconstruct 
    an object from the checkpoint.  The log manager is also used 
    to obtain a reader on the redo log.  The entries in the redo 
    log are read, and the recorded updates are replayed.  Any returned 
    values or exceptions from these methods are ignored. If update 
    methods use "VAR" parameters, the recovery procedure will pass 
    dummies to them that contain the same values the originally passed 
    variables had. Notice that if an update method reads or writes 
    any state other that the fields of the stable object itself, 
    the recovery process may not work as expected. 
    
    A "Stable(Data).T" is unmonitored. The client is responsible 
    for ensuring that it is not updated concurrently nor updated 
    while a checkpoint is written.  Clients are also responsible to 
    write checkpoints periodically by calling the "Checkpoint()" 
    procedure. 
*)

GENERIC INTERFACE Stable(Data);

(* Where "Data.T" is an object type.  Also, the "Data" interface should
contain a pragma indicating which methods of "Data.T" are update methods,
as explained in the man page for the stub generator "stablegen". *)

IMPORT LogManager, StableError, Pathname, Wr, Rd;

CONST 
  Brand = "(Stable" & Data.Brand & ")";
  DefaultBrand = "(Default " & Brand & ")";
(* This branding is required for all generic interfaces.
   The Data interface must contain:

| CONST Brand = <text-constant>;

*)

TYPE T <: Public;
     Public = Data.T OBJECT METHODS
        init(nm: Pathname.T; VAR recovered: BOOLEAN;
            forceToDisk := TRUE; lm: LogManager.T := NIL): T
            RAISES {StableError.E};
        dispose() RAISES {StableError.E};
        flushLog()  RAISES {StableError.E};
        freeLog() RAISES {StableError.E};
        writeCheckpoint (wr: Wr.T) RAISES {StableError.E};
        readCheckpoint (rd: Rd.T): T RAISES {StableError.E};
     END;

(* The call "st.init(nm, recovered, forceToDisk, lm)" makes the
   object "st" stable, with stable state stored by the log manager
   "lm" under the name "nm".  If this stable state is present
   when "init" is called, then recovery is initiated,
   "recovered" is set to "TRUE", and the recovered object is
   returned.  Otherwise, the state of "st" is recorded stably
   and "st" itself is returned.  It is a runtime error to call
   "st.init" twice without an intervening "st.dispose" (see below).

   If "forceToDisk" is "TRUE", the log will be flushed after
   every update method.  If "forceToDisk" is false, the
   log will be flushed only when the client calls the "flushLog"
   method.  Leaving "forceToDisk" false is more efficient, but
   if "forceToDisk" is false and "flushLog" is not called
   frequently, recovery may fail to replay some update
   methods.  The number of lost updates depends on how much
   buffering is performed by the log manager.

   If "lm" is "NIL", then the file system log manager is used.
   This means that "nm" is interpreted as the name of a file
   system directory, in which the stable state is stored as
   a checkpoint and redo log file. 
   
   The method call "st.freeLog()" closes the log writer for "st".  
   The log will be reopened by the next call to an update method. 
   Freeing the log might be necessary, for example, to prevent running 
   out of file descriptors if many stable objects are allocated but 
   few are active at once. 
   
   The "readcheckpoint" method must reconstruct a data structure
   equivalent to the one written by "writecheckpoint".  By default, 
   both of these methods use pickles, but you can override them if 
   you want to. 
   
   The call "st.dispose()" deletes the stable state of "st".  It
   should be called when stability for "st" is no longer desired. 
   The stability of an object and modifications to its state are
   orthogonal.  That is, update methods may be called after
   a call to "st.dispose." 
*)

PROCEDURE Checkpoint(t: T) RAISES {StableError.E};
(* Write a new checkpoint for "t" and empty its redo log. *)

(* You are responsible for calling "Checkpoint" periodically, to
   prevent the redo log from growing without bound.
   It is a runtime error to call "Checkpoint" after calling
   "st.dispose," unless the object has subsequently been restabilized 
   with "st.init."
*)

END Stable.

(* \subsection{Exceptions}
   The "StableError.E" exception is raised in various circumstances.  The first
   element of the "AtomList.T" argument identifies the nature of the
   error.  Subsequent elements may identify further details of the
   error, especially in the case when lower-level exceptions are
   propagated by the "Stable" package. 
   

\subsection{Customizations}
    \begin{itemize}
    \item  If you don't want every update to be flushed
                 to disk, set "forceToDisk" to "FALSE" when initializing the
                 stable object, and flush the log
                 manually with the "flushLog" method.
    \item  If you don't want to use "Pickle" for checkpointing, override
                 the "writeCheckpoint()" and "readCheckpoint()" methods of the stable
                 object to read and write checkpoints in your preferred format.
                 (If you are aiming at long-lived persistent state you should
                 avoid pickles, since new versions of the Modula-3 system can't always  
                 read pickles written by earlier versions.)
    \item  If you don't want to store the stable state in the ordinary file system, 
                 implement your own log manager (as explained in the "LogManager" interface)
                 and pass your own manager to the "init" method instead of the default.
    \item  If you don't want to use network-object style marshaling for recording
                 the update method calls in the redo log, you will have to write
                 you own implementation of of "Stable(Data).m3" instead of using the
                 stub generator "stablegen".   (You may still find it
                 helpful to use the generated code as starting point.)
    \end{itemize}
*)




