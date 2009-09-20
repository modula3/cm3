(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

FROM Ctypes IMPORT int;

CONST
    (* trick from darwin-generic/Upthread.i3 *)
    X32 = ORD(BITSIZE(INTEGER) = 32);
    X64 = ORD(BITSIZE(INTEGER) = 64);

(* INTERFACE Unix; *)
    MaxPathLen = 1024;
    MAX_FDSET = (X32 * 1024) + (X64 * 65536);

TYPE
(* INTERFACE Utime; *)
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

END Usysdep.
