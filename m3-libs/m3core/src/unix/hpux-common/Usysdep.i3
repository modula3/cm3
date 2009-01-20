(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

FROM Cstdint IMPORT int32_t;
FROM Ctypes IMPORT int;
FROM Cstddef IMPORT size_t;

CONST
    (* trick from darwin-generic/Upthread.i3 *)
    X32 = ORD(BITSIZE(INTEGER) = 32);
    X64 = ORD(BITSIZE(INTEGER) = 64);

(* INTERFACE Unix; *)

CONST
    MaxPathLen = 1024;

    MAX_FDSET = 1024;

TYPE
    mode_t = uint16_t;

(* INTERFACE Upthread; *)

    pthread_t = int32_t; (* opaque *)
    pthread_attr_t = int32_t; (* opaque *)
    pthread_mutex_t = RECORD opaque: ARRAY [1..11 * X64 + 22 * X32] OF INTEGER; END;  (* 88 opaque bytes with size_t alignment *)
    pthread_cond_t = RECORD opaque: ARRAY [1..7 * X64 + 14 * X32] OF INTEGER; END;  (* 56 opaque bytes with size_t alignment *)
    pthread_key_t = int32_t; (* opaque *)

(* INTERFACE Usocket; *)

    struct_linger = RECORD
        l_onoff: int32_t;
        l_linger: int32_t;
    END;

(* INTERFACE Utime; *)

    struct_timeval = RECORD
        tv_sec: INTEGER;
        tv_usec: INTEGER;
    END;

    struct_tm = RECORD
        tm_sec:   int;
        tm_min:   int;
        tm_hour:  int;
        tm_mday:  int;
        tm_mon:   int;
        tm_year:  int;
        tm_wday:  int;
        tm_yday:  int;
        tm_isdst: int;
    END;

(* INTERFACE Utypes; *)

    gid_t = int32_t;
    pid_t = int32_t;
    time_t = INTEGER;
    uid_t = int32_t;

    socklen_t = size_t;
    hostent_addrtype_t = int32_t; (* hostent_t.h_addrtype *)
    hostent_length_t = int32_t; (* hostent_t.h_length *)

(* RTMachine.i3 *)
CONST SIG_SUSPEND = 44;  (* SIGRTMAX *)

END Usysdep.
