(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

(* INTERFACE Unix; *)

IMPORT Ctypes;
FROM Ctypes IMPORT int, const_char_star, char_star_star, char, unsigned_int;
FROM Cstdint IMPORT uint8_t, uint32_t, int32_t;

(* This is the only system that uses this. *)
(* CONST *)
<*EXTERNAL "Usysdep__P_NOWAIT"*> VAR P_NOWAIT: int;
<*EXTERNAL*> PROCEDURE spawnve (mode: int; name: const_char_star; argv, envp: char_star_star): int;

(* Unix.i3 *)

CONST
  MaxPathLen = 1024;
  MAX_FDSET = 1024;

TYPE
(* INTERFACE Utime; *)

TYPE
  struct_timeval = RECORD
    tv_sec: INTEGER;
    tv_usec: INTEGER;
  END;

  struct_tm = RECORD
    tm_sec:   int32_t;
    tm_min:   int32_t;
    tm_hour:  int32_t;
    tm_mday:  int32_t;
    tm_mon:   int32_t;
    tm_year:  int32_t;
    tm_wday:  int32_t;
    tm_yday:  int32_t;
    tm_isdst: int32_t;
  END;

(* INTERFACE Utypes; *)

  time_t = int32_t; (* ideally always 64 bits *)

(* INTERFACE Utermio *)

TYPE
    cc_t = uint8_t;
    tcflag_t = unsigned_int; (* uint32_t not compatible with SerialPort.m3 *)
    speed_t = uint32_t;

    struct_termios = RECORD
        c_iflag: tcflag_t := 0;
        c_oflag: tcflag_t := 0;
        c_cflag: tcflag_t := 0;
        c_lflag: tcflag_t := 0;
        c_line:  char := VAL(0, char);
        c_cc := ARRAY[0..17] OF cc_t{0,..};
        c_pad:  char := VAL(0, char);
        c_ispeed: speed_t := 0;
        c_ospeed: speed_t := 0;
    END;

END Usysdep.
