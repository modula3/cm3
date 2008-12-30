(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Uwaitpid;

FROM Utypes IMPORT uint8_t, pid_t;
FROM Ctypes IMPORT int;

(* C waitpid returns a 32 bit integer that is a union
of a few forms, containing bit fields, crackable with macros.
The Modula-3 representation is historically endian-dependent.
Here is a more portable rendition.

http://www.opengroup.org/onlinepubs/009695399/functions/waitpid.html
*)

TYPE
    w_M3 = RECORD
(*
    terminated process status
        w_Filler  : BITS 16 FOR [0..16_FFFF];
        w_Coredump: BITS  1 FOR [0..16_01];
        w_Termsig : BITS  7 FOR [0..16_7F];
        w_Retcode : BITS  8 FOR [0..16_FF];

    stopped process status
    nobody uses this
        w_Filler  : BITS 16 FOR [0..16_FFFF];
        w_Stopsig : BITS  8 FOR [0..16_FF];
        w_Stopval : BITS  8 FOR [0..16_FF];

    M3 view of return code
        w_Filler  : BITS 16 FOR [0..16_FFFF];
        w_Coredump: BITS  1 FOR [0..16_01];  (* core dump indicator *)
        w_Termsig : BITS  7 FOR [0..16_7F];  (* termination signal *)
        w_Retcode : BITS  8 FOR [0..16_FF];  (* exit code if w_termsig == 0 *)

    above is all big endian
    for little endian, reverse the field order

    That is, w_Filler is the most significant bits.
    w_Retcode is the least significant bits.
*)
        (* The "M3" output fields: *)
        w_A_Loophole : uint16_t := 0; (* w_Coredump << 15 | w_Termsig << 8 | w_Retcode *)
        w_Coredump: uint8_t := 0; (* boolean: core dump indicator *)
        w_Termsig : uint8_t := 0; (* termination signal *)
        w_Retcode : uint8_t := 0; (* exit code if w_termsig == 0 *)

        (* These are not used. *)
        w_Stopsig : uint8_t := 0; (* signal that stopped us *)
        w_Exited  : uint8_t := 0; (* boolean: WIFEXITED *)
        w_Signaled: uint8_t := 0; (* boolean: WIFSIGNALED *)
        w_Stopped : uint8_t := 0; (* boolean: WIFSTOPPED (aka Stopval aka W_STOPPED) *)

        (* This is not used and not portable. *)
        w_Continued : uint8_t := 0; (* boolean: WIFCONTINUED *)
  END;

CONST
    WNOHANG = 1; (* flag to waitpid, can be defined arbitrarily, system-independent, C wrapper translates *)

<*EXTERNAL m3_waitpid*>
PROCEDURE waitpid (pid: pid_t; (*out*) VAR status: w_M3; options: int): pid_t;

END Uwaitpid.
