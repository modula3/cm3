(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue May  2 11:42:57 PDT 1995 by kalsow  *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

INTERFACE RTMachine;

IMPORT Csetjmp;

(*--------------------------------------------------------- thread state ---*)

TYPE
  State = Csetjmp.jmp_buf;
  (* The machine state is saved in a "State".  This type is really
     opaque to the client, i.e. it does not need to be an array. *)

<*EXTERNAL "_setjmp" *>
PROCEDURE SaveState (VAR s: State): INTEGER;
(* Capture the currently running thread's state *)

CONST
  FramePadBottom = 2;
  FramePadTop    = 0;
  (* Additional padding words from above and below an existing
     thread's stack pointer to copy when creating a new thread *)

(*------------------------------------------------------------------ heap ---*)

(* The heap page size is machine-dependent, since it might depend on the
   architecture's VM page size (if VM is TRUE).  Otherwise, 8192 bytes is a
   reasonable page size.  The page size must be a power of two. *)

CONST
  BytesPerHeapPage    = 8192;        (* bytes per page *)
  LogBytesPerHeapPage = 13;
  AdrPerHeapPage      = 8192;        (* addresses per page *)
  LogAdrPerHeapPage   = 13;

(* The collector supports the use of VM protection to achieve incremental,
   generational collection.  This is not possible on all architectures, and
   it may not be implemented in all cases where it is possible.  The
   boolean constant "VMHeap" is "TRUE" iff all necessary support is
   present for this architecture.  "VMHeap" is "TRUE" for the DS3100,
   whose implementation you might use as a reference. *)

CONST
  VMHeap = FALSE;

(*--------------------------------------------------------- thread stacks ---*)

CONST
  PointerAlignment = 1;
  (* The C compiler allocates all pointers on 'PointerAlignment'-byte
     boundaries.  The garbage collector scans thread stacks, but only
     looks at these possible pointer locations.  Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

CONST
  StackFrameAlignment = 4;
  (* Stack frames must be aligned to this constraint (in ADRSIZE units). 
     It's not a big deal if this value is too large, but it may break 
     the thread mechanism to make it too small. *)

(*----------------------------------------------- exception stack walking ---*)
(* The "FrameInfo" type must minimally include fields named "pc" and "sp". *)

CONST
  Has_stack_walker = FALSE;
  (* Indicates whether this platform supports the stack walking functions
     defined in the "RTStack" interface. *)

TYPE FrameInfo = RECORD pc, sp: ADDRESS END;

END RTMachine.


