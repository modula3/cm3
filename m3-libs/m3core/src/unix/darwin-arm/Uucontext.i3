(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uucontext;

FROM Ctypes IMPORT unsigned_int;

(*** <mach/thread_status.h> ***)
(*** <mach/arm/thread_state.h> ***)
(*** <mach/arm/thread_status.h> ***)

TYPE
  arm_thread_state_t = RECORD
    r0: unsigned_int;
    r1: unsigned_int;
    r2: unsigned_int;
    r3: unsigned_int;
    r4: unsigned_int;
    r5: unsigned_int;
    r6: unsigned_int;
    r7: unsigned_int;
    r8: unsigned_int;
    r9: unsigned_int;
    r10: unsigned_int;
    r11: unsigned_int;
    r12: unsigned_int;
    r13: unsigned_int;
    r14: unsigned_int;
    r15: unsigned_int;
    r16: unsigned_int; (* Apple's thread_state has this 17th reg, bug?? *)
  END;

END Uucontext.
