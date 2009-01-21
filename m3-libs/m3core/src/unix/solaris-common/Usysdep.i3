(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

FROM Cstdint IMPORT uint32_t, uint32_t;
FROM Ctypes IMPORT int;

CONST
    (* trick from darwin-generic/Upthread.i3 *)
    X32 = ORD(BITSIZE(INTEGER) = 32);
    X64 = ORD(BITSIZE(INTEGER) = 64);

(* INTERFACE Unix; *)

    MaxPathLen = 1024;

    MAX_FDSET = (X32 * 1024) + (X64 * 65536);

TYPE
    mode_t = uint32_t;

(* INTERFACE Upthread; *)

    pthread_t = int32_t; (* opaque *)
    pthread_attr_t = int32_t; (* opaque *)
    pthread_mutex_t = RECORD opaque: ARRAY [1..4] OF LONGINT; END; (* 32 bytes with 64 bit alignment *)
    pthread_cond_t = RECORD opaque: ARRAY [1..2] OF LONGINT; END; (* 16 bytes with 64 bit alignment *)
    pthread_key_t = int32_t; (* opaque *)

(* INTERFACE Usocket; *)

    struct_linger = RECORD
        l_onoff: int32_t;
        l_linger: int32_t;
    END;

(* INTERFACE Utime; *)

    struct_timeval = RECORD
        tv_sec: int32_t;
        tv_usec: int32_t;
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

    socklen_t = uint32_t;
    hostent_addrtype_t = int32_t; (* hostent_t.h_addrtype *)
    hostent_length_t = int32_t; (* hostent_t.h_length *)

END Usysdep.
