(* File: Jmpbufs.i3  *)

(* Manage jmp_bufs used by some versions of exception handling
   -- the type passed to setjmp/longjmp.
   In particular, count the number of TRYs in a function.
   At the start of a function, allocate a jmpbuf per TRY.
   At the n'th TRY, use the n'th jmpbuf.
   
   Allocation is done wither with alloca(external variable * n),
   or in future, for the C backend only, using "jmp_buf".
*)

INTERFACE Jmpbufs;

IMPORT M3ID;
IMPORT M3CG AS CG;

TYPE
  CheckState = RECORD
    proc      : Proc := NIL;
    try_count := 0;
    save : REF CheckState := NIL;
  END;

  Proc = REF RECORD (* procedure or module *)
    try_count    := 0;
    jmpbufs      : REF ARRAY OF CG.Var := NIL;
    name         : M3ID.T := 0;
  END;

  Try = RECORD (* opaque *)
    proc         : Proc := NIL;
    try_index    := 0;
  END;

PROCEDURE CheckProcPush (VAR cs: CheckState; name: M3ID.T := 0): Proc;
PROCEDURE CheckProcPop (VAR cs: CheckState; proc: Proc);
PROCEDURE CheckTry (VAR cs: CheckState; VAR try: Try);

PROCEDURE CompileTryGetJmpbuf (VAR try: Try): CG.Var;
PROCEDURE CompileProcAllocateJmpbufs (proc: Proc);

END Jmpbufs.
