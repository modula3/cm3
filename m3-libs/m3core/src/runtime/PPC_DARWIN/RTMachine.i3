(* Copyright according to COPYRIGHT-CMASS. *)
(* FIXME: copied from FreeBSD3 target. Probably needs to be changed. *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

INTERFACE RTMachine;

IMPORT Csetjmp;

(*--------------------------------------------------------- thread state ---*)

TYPE
  State = Csetjmp.fpjmp_buf;
  (* The machine state is saved in a "State".  This type is really
     opaque to the client, i.e. it does not need to be an array. *)

<*EXTERNAL "_setjmp" *>
PROCEDURE SaveState (VAR s: State): INTEGER;
(* Capture the currently running thread's state *)

CONST
  FramePadBottom = 14;
  FramePadTop    = 0;
  (* Additional padding words from above and below an existing
     thread's stack pointer to copy when creating a new thread *)

(*------------------------------------------------------------------ heap ---*)

(* The heap page size is machine-dependent, since it might depend on the
   architecture's VM page size (if VM is TRUE).  Otherwise, 8192 bytes is a
   reasonable page size.  The page size must be a power of two. *)

CONST
  BytesPerHeapPage    = 4096;        (* bytes per page *)
  LogBytesPerHeapPage = 12;
  AdrPerHeapPage      = 4096;        (* addresses per page *)
  LogAdrPerHeapPage   = 12;

(*** hooks for the C wrapper functions ***)

<*EXTERNAL *> VAR RTHeapRep_Fault: ADDRESS;  (* => RTHeapRep.Fault *)
<*EXTERNAL *> VAR RTCSRC_FinishVM: ADDRESS;  (* => RTCollectorSRC.FinishVM *)

(*--------------------------------------------------------- thread stacks ---*)

CONST
  PointerAlignment = 4;
  (* The C compiler allocates all pointers on 'PointerAlignment'-byte
     boundaries.  The garbage collector scans thread stacks, but only
     looks at these possible pointer locations.  Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

CONST
  StackFrameAlignment = 16;
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

