(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Mar 22 17:30:23 PST 1995 by msm      *)
(*      modified on Thu Oct 14 16:26:21 PDT 1993 by sfreeman *)

(* subclass of JVBuffer which uses a Jvs.T to allocate shared memory
   buffers *)

INTERFACE JvsBuffer;

IMPORT JVBuffer, Jvs, OSError;

TYPE T = JVBuffer.T;

TYPE
  Factory <: FactoryPublic;
  FactoryPublic = JVBuffer.Factory OBJECT
                  METHODS
                    init (jvs: Jvs.T; type: Jvs.BufferType): Factory;
                    (* the "jvs" is used to create shared memory buffers.
                       It should already have been opened *)

                    newBuf (): T;
                    (* this is called by "make" to create a new T.  It must
                       be overriden by the subclass *)
                  END;

(* This is where we allocate shared memory buffers.  It turns out that the
   jvdriver doesn't deallocate them, so we should keep the ones we have on
   a free list.  In addition, there are only two sizes of shared memory
   buffer allocated: small (for compressed images) and large (for
   decompressed images).  Note that the shared memory images last as long
   as the connection to the server which was used to create them, so do not
   break the connection while still using the memory buffers. *)

(*
  No longer exported
  
PROCEDURE AllocateBuffer (jvs: Jvs.T; type: Jvs.BufferType; wait := TRUE):
  Jvs.ShmBufId RAISES {OSError.E, Thread.Alerted};
(* find or create a shared memory buffer of the given buffer type and
   return its id.  Return 0 if wait is false and no buffer is available. *)

PROCEDURE FreeBuffer (type: Jvs.BufferType; id: Jvs.ShmBufId);
(* we are no longer using the shared memory buffer *)
*)

VAR
  shmNotAttached: OSError.Code;

PROCEDURE BufferAddress (id: Jvs.ShmBufId): ADDRESS RAISES {OSError.E};
(* to avoid multiple attachements to the same shared memory buffer, we do
   the attach in /AllocateBuffer/.  This procedure returns that address for
   shared memory segments acquired from /AllocateBuffer/.  Raises OSError.E
   if the shared memory segment has not been attached *)

PROCEDURE Subtype(width, height: CARDINAL): CARDINAL;
(* Return the appropriate subtype code to use for a height by width buffer *)

PROCEDURE Subtype2(len: CARDINAL): CARDINAL;
(* Return the appropriate subtype code to use for a length len buffer *)

END JvsBuffer.
