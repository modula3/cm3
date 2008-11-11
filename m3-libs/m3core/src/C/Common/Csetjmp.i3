(* NOTE This is NOT common to all systems, but it is common to a few.
It is common to systems for which:
  - There is no user thread support.
  - longjmp is "_longjmp"
     In general on Posix, there are two sets of setjmp/longjmp,
      one that saves/restores the thread's signal mask, one that does not.

  If a system does not meet the requirements, make a separate version for it.
*)

INTERFACE Csetjmp;

FROM Ctypes IMPORT int;

TYPE
  jmp_buf = RECORD
  (* For systems without user threads, the size of this need not be declared correctly.
     It is at the end of a record allocated by the compiler that the runtime merely needs
     the address of. The runtime does need to know the size. The compiler does not know the
     size. In future, the compiler should feed that size to here. *)
    opaque : INTEGER;
  END;

<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.
