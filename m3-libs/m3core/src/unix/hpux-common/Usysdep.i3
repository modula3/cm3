(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

FROM Cstdint IMPORT int32_t;
FROM Ctypes IMPORT int;
FROM Cstddef IMPORT size_t;

CONST
(* INTERFACE Unix; *)

    MaxPathLen = 1024;

    MAX_FDSET = 1024;

TYPE
(* INTERFACE Upthread; *)

    pthread_t = int32_t; (* opaque *)

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

END Usysdep.
