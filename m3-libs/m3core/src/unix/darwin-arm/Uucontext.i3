(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Do we really need this duplicated in Modula-3? Probably not. *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, unsigned_int;

(*** <mach/thread_status.h> ***)

TYPE
  natural_t = unsigned_int;
  thread_state_t = UNTRACED REF natural_t; (* Variable-length array *)

(* THREAD_STATE_MAX is now defined in <mach/machine/thread_state.h> *)
  thread_state_data_t = ARRAY [0..THREAD_STATE_MAX-1] OF natural_t;

  thread_state_flavor_t = int;
  thread_state_flavor_array_t = UNTRACED REF thread_state_flavor_t;

CONST
  THREAD_STATE_FLAVOR_LIST = 0;       (* List of valid flavors *)

(*** <mach/arm/thread_state.h> ***)

  ARM_THREAD_STATE_MAX = 17 * 4;    (* Size of biggest state possible *)
  THREAD_STATE_MAX = ARM_THREAD_STATE_MAX;

(*** <mach/arm/thread_status.h> ***)

CONST
  THREAD_STATE_NONE = 1;
  ARM_THREAD_STATE = 1;
  ARM_THREAD_STATE_COUNT = 17;

TYPE
  arm_thread_state_t = struct_arm_thread_state;
  struct_arm_thread_state = RECORD
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
