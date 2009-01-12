(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Uwaitpid;

FROM Utypes IMPORT pid_t;
FROM Ctypes IMPORT int;
IMPORT Ctypes;

TYPE
  uint8_t = Ctypes.unsigned_char;
  uint16_t = Ctypes.unsigned_short;

(* C waitpid returns a 32 bit integer that is a union
of a few forms, containing bit fields, crackable with macros.
The Modula-3 representation is historically endian-dependent
with regard to the bitfield layout, and system-dependent with
regard to the NOHANG flag.

Here is a more portable version, implemented via a thin C wrapper.

http://www.opengroup.org/onlinepubs/009695399/functions/waitpid.html
*)

TYPE
    waitpid_status_t = RECORD
(*
    For reference, here is how I think to read bit fields.
    I have never seen this documented, but I experimented.
    RECORD
        a : BITS 1;
        b : BITS 29;
        c : BITS 2;
    END;
    --- BIG ENDIAN ---
        a: 1
        b: 4
        c: 0x80000000
    
    --- LITTLE ENDIAN ---
        c: 1
        b: 2
        a: 0x80000000

    w_A = BITS 32 FOR unsigned

    --- BIG ENDIAN ---

    terminated process status
    w_T = BITS 32 FOR RECORD
        w_Filler  : BITS 16 FOR [0..16_FFFF];
        w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
        w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
        w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
    END;

    stopped process status
    nobody uses this
    w_S = BITS 32 FOR RECORD
        w_Filler  : BITS 16 FOR [0..16_FFFF];
        w_Stopsig : BITS  8 FOR [0..16_FF];  (* signal that stopped us *)
        w_Stopval : BITS  8 FOR [0..16_FF];  (* == W_STOPPED if stopped *)
    END;
    (* M3 view of return code *)
    w_M3 = RECORD
        w_Filler  : BITS 16 FOR [0..16_FFFF];
        w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
        w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
        w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
    END;

    --- LITTLE ENDIAN ---

    (* terminated process status *)
    w_T = RECORD
        w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
        w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
        w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
        w_Filler  : BITS 16 FOR [0..16_FFFF];
    END;

    (* M3 view of return code *)
    w_M3 = RECORD
        w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)
        w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
        w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
        w_Filler  : BITS 16 FOR [0..16_FFFF];
    END;

    (* stopped process status *)
    w_S = RECORD
        w_Stopval : BITS  8 FOR [0..16_FF];  (* == W_STOPPED if stopped *)
        w_Stopsig : BITS  8 FOR [0..16_FF];  (* signal that stopped us *)
        w_Filler  : BITS 16 FOR [0..16_FFFF];
    END;

    The layout is such that w_Filler is the most significant bits.
    w_Retcode is the least significant bits.

    There is code that treats the M3 view as a 32bit integer,
    which is handled below with w_Loophole.
*)
        (* sort by size and then by name *)
        (* w_Coredump << 15 | w_Termsig << 8 | w_Retcode
         to replace the form LOOPHOLE(m3status, w_A) *)
        w_Loophole : uint16_t := 0;

        (* boolean: WIFCONTINUED *) (* not used *)
        w_Continued : uint8_t := 0;

        (* boolean: core dump indicator *)
        w_Coredump: uint8_t := 0;

        (* boolean: WIFEXITED *) (* not used *)
        w_Exited  : uint8_t := 0;

        (* exit code if w_termsig == 0 *)
        w_Retcode : uint8_t := 0;

        (* These are not used. *)
        w_Signaled: uint8_t := 0; (* boolean: WIFSIGNALED *) (* not used *)
        w_Stopped : uint8_t := 0; (* boolean: WIFSTOPPED (aka Stopval aka W_STOPPED) *) (* not used *)
        w_Stopsig : uint8_t := 0; (* signal that stopped us *) (* not used *)

        w_Termsig : uint8_t := 0; (* termination signal *)
  END;

CONST
    WNOHANG = 1; (* flag to waitpid, can be defined arbitrarily, system-independent, C wrapper translates *)

<*EXTERNAL waitpid*>
PROCEDURE waitpid (pid: pid_t; (*out*) VAR status: int; options: int := 0): pid_t;

END Uwaitpid.
