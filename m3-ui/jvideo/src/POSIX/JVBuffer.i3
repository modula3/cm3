(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Wed Mar 22 18:03:48 PST 1995 by msm      *)
(*      modified on Sat Oct 23 18:42:12 PDT 1993 by sfreeman *)

(* shared memory buffers for use with JVideo stuff. *)

INTERFACE JVBuffer;

IMPORT Ctypes, OSError, Thread, Tick, Utime;

TYPE Serial = CARDINAL;

(* -- the T -- *)

TYPE
  T <: Public;
  Public =
    MUTEX OBJECT
      serial   : Serial                 := 0; (* id of frame *)
      timestamp: Utime.struct_timeval;
      localTime: Tick.T;
      (* time the previous stage in the pipeline finished processing *)
      length, frameLength: CARDINAL     := 0;
      shmid              : Ctypes.int   := -1;
      subtype            : CARDINAL     := 0;
      addr               : ADDRESS      := NIL;
      ready              : ReadyClosure := NIL; (* see note below *)
    METHODS
      init (shmid: Ctypes.int; address: ADDRESS): T RAISES {OSError.E};
      (* the "shmid" should have been attached to "address" before calling
         this method sets length from shmid *)
      free ();                   (* LL < {t, t.pool} *)
      (* readers must call this when finished with the T *)
    END;

(* -- Ready Closure --

   if the writer of a "T" assigns one of these it, then each reader must
   call its "apply()" method at some point or pass it on to the next stage
   in the pipeline to be called by another reader.  This is used to support
   flow control.  The endToEnd field should be set true if every stage that
   has a successor managed to get it to at least on successor.  *)

TYPE
  ReadyClosure =
    MUTEX OBJECT endToEnd := FALSE; METHODS apply () RAISES {Thread.Alerted}; END;

(* -- Factory -- used to create a buffer and free it

   this is a virtual type, it should be subclassed to provide an
   implementation.  It will be protected by the Pool lock whenever it is
   called *)
TYPE
  Factory =
    OBJECT
      subtype: CARDINAL := 0
    METHODS
      make (wait := TRUE; subtype: CARDINAL := 0): T
            RAISES {Thread.Alerted, OSError.E};
      (* create a brand new buffer, or return NIL if none can be made and
         wait is FALSE. *)
      reset (t: T);
      (* re-initialise the buffer, called when a buffer is reused *)
      destroy (t: T) RAISES {Thread.Alerted, OSError.E};
      (* destroy the buffer *)
      (* LL >= t *)
    END;

(* -- Buffer Pool -- holds a fixed number of buffers.

   A Pool has two types of clients: readers and writers.  A writer acquires
   a free buffer and fills it with data, then inserts it into the pool.  A
   reader can can get the most recent buffer, or wait until a new buffer is
   inserted.  Multiple readers can get hold of the most recent buffer which
   will be returned to the pool when all of them have freed it, so readers
   must free their buffers as soon as possible.

   Any storage held in a buffer pool will be freed if the pool is
   collected *)

EXCEPTION
  Closed;                        (* the writer has stopped.  The Pool is
                                    now useless until something is reset *)

TYPE
  Pool <: PoolPublic;
  PoolPublic =
    MUTEX OBJECT                 (* all methods are LL < {self} *)
    METHODS
      (* administration methods *)
      init (factory: Factory; maxBuffers: CARDINAL): Pool
            RAISES {Thread.Alerted, OSError.E};
      (* set up the Pool to use "factory" to create new buffers.  The pool
         may hold up to "maxBuffers" buffers. *)

      setSize (maxBuffers: CARDINAL) RAISES {Thread.Alerted, OSError.E};
      (* set how many buffers may be held in the pool.  If more than
         "maxBuffers" buffers are already in the pool, the surplus will not
         be destroyed until they have been released by all clients *)

      (* methods for the buffer reader *)
      getCurrentBuffer (): T;
      (* just get the current buffer.  May return NIL.  Any buffer returned
         must be freed *)

      waitForChange (): T RAISES {Thread.Alerted, Closed};
      (* block the thread until the Pool releases a buffer containing a new
         frame.  The caller must call "buffer.free()" when it finished with
         it. *)

      (* methods for the buffer writer *)
      getFreeBuffer (wait := FALSE; subtype: CARDINAL := 0): T
                     RAISES {Thread.Alerted, OSError.E};
      (* get a free buffer from the pool.  If "wait" = FALSE then return a
         free buffer if one is available or return NIL.  Otherwise, block
         thread until a buffer becomes free.  The returned buffer must
         either be inserted or freed *)

      insert (buffer: T);
      (* set "buffer" to be the current value in the Pool *)

      join  ();
      leave ();
      (* readers from the pool should call "join" when they are interested
         in receiving buffers and "leave" when they cease to be interested.
         The pool will not deliver free buffers to the writer (via
         "getFreeBuffer") unless there is at least one reader.  It is a
         checked runtime error to call leave too many times, and wastes
         processor time to call it too few times *)

      signalClosed ();
      (* tell any readers that the writer has died. *)
      clearClosed ();
      (* clear the closed flag in the pool *)
    END;

END JVBuffer.
