(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue May  2 11:42:57 PDT 1995 by kalsow  *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

INTERFACE RTMachine;

IMPORT Csetjmp;
FROM Upthread IMPORT pthread_t;

(*--------------------------------------------------------- thread state ---*)

TYPE
  State = Csetjmp.jmp_buf;
  (* The machine state is saved in a "State". This is opaque to the client. *)

<*EXTERNAL "_setjmp" *>
PROCEDURE SaveState (VAR s: State): INTEGER;
(* Capture the currently running thread's state *)

(*------------------------------------------------------------------ heap ---*)

(* The heap page size used to be machine-dependent, since it could depend
   on the architecture's VM page size (if VM was TRUE). VM is now always
   FALSE. Otherwise, 8192 bytes is a reasonable page size. The page size must
   be a power of two. *)

CONST
  BytesPerHeapPage    = 8192;               (* bytes per page *)
  LogBytesPerHeapPage = 13;
  AdrPerHeapPage      = BytesPerHeapPage;   (* addresses per page *)
  LogAdrPerHeapPage   = LogBytesPerHeapPage;

(*--------------------------------------------------------- thread stacks ---*)

  PointerAlignment = BYTESIZE(INTEGER);
  (* The C compiler allocates all pointers on 'PointerAlignment'-byte
     boundaries. The garbage collector scans thread stacks, but only
     looks at these possible pointer locations. Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

(*----------------------------------------------- exception stack walking ---*)
(* The "FrameInfo" type must minimally include fields named "pc" and "sp". *)

  Has_stack_walker = FALSE;
  (* Indicates whether this platform supports the stack walking functions
     defined in the "RTStack" interface. *)

TYPE FrameInfo = RECORD pc, sp: ADDRESS END;

CONST
  SuspendThread: PROCEDURE(t: pthread_t): BOOLEAN = NIL;
  RestartThread: PROCEDURE(t: pthread_t) = NIL;
  GetState: PROCEDURE(t: pthread_t; VAR state: ThreadState): ADDRESS = NIL;
  SaveRegsInStack: PROCEDURE(): ADDRESS = NIL;

TYPE ThreadState = RECORD END;

END RTMachine.
