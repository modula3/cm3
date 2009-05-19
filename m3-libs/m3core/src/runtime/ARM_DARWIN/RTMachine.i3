(* Copyright according to COPYRIGHT-CMASS. *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

INTERFACE RTMachine;

IMPORT Csetjmp, Word;
FROM Upthread IMPORT pthread_t;
FROM Uucontext IMPORT arm_thread_state_t;

(*--------------------------------------------------------- thread state ---*)

TYPE
  State = Csetjmp.jmp_buf;
  (* The machine state is saved in a "State".  This type is really
     opaque to the client, i.e. it does not need to be an array. *)

<*EXTERNAL "_setjmp" *>
PROCEDURE SaveState (VAR s: State): INTEGER;
(* Capture the currently running thread's state *)

(*--------------------------------------------------------------------------*)

(* stub support for old user threads implementation *)
CONST
  FramePadBottom = 0;
  FramePadTop    = 0;
  StackFrameAlignment = 0;

(*------------------------------------------------------------------ heap ---*)

(* The heap page size used to be machine-dependent, since it could depend
   on the architecture's VM page size (if VM was TRUE). VM is now always
   FALSE. Otherwise, 8192 bytes is a reasonable page size. The page size must
   be a power of two. *)

  BytesPerHeapPage    = Word.LeftShift(1, LogBytesPerHeapPage); (* bytes per page *)
  LogBytesPerHeapPage = 13;
  AdrPerHeapPage      = BytesPerHeapPage;   (* addresses per page *)
  LogAdrPerHeapPage   = LogBytesPerHeapPage;

(*--------------------------------------------------------- thread stacks ---*)

  PointerAlignment = BYTESIZE(INTEGER);
  (* The C compiler allocates all pointers on 'PointerAlignment'-byte
     boundaries.  The garbage collector scans thread stacks, but only
     looks at these possible pointer locations.  Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

(*----------------------------------------------- exception stack walking ---*)
(* The "FrameInfo" type must minimally include fields named "pc" and "sp". *)

CONST
  Has_stack_walker = FALSE;
  (* Indicates whether this platform supports the stack walking functions
     defined in the "RTStack" interface. *)

TYPE FrameInfo = RECORD pc, sp: ADDRESS END;

(*------------------------------------------------------ pthreads support ---*)

TYPE ThreadState = arm_thread_state_t;

CONST
  SaveRegsInStack: PROCEDURE(): ADDRESS = NIL;

<*EXTERNAL RTMachine__SuspendThread*>
PROCEDURE SuspendThread (t: pthread_t): BOOLEAN;
<*EXTERNAL RTMachine__RestartThread*>
PROCEDURE RestartThread (t: pthread_t);
<*EXTERNAL RTMachine__GetState*>
PROCEDURE GetState (t: pthread_t; VAR context: ThreadState): ADDRESS;

END RTMachine.
