(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

(* INTERFACE Unix; *)

IMPORT Ctypes;
FROM Ctypes IMPORT int, const_char_star, char_star_star, char, unsigned_int;
FROM Cstdint IMPORT uint8_t, uint16_t, uint32_t;

(* This is the only system that uses this. *)
(* CONST *)
<*EXTERNAL "Usysdep__P_NOWAIT"*> VAR P_NOWAIT: int;
<*EXTERNAL*> PROCEDURE spawnve (mode: int; name: const_char_star; argv, envp: char_star_star): int;

(* Unix.i3 *)

CONST
  MaxPathLen = 1024;
  MAX_FDSET = 1024;

TYPE
(* INTERFACE Usocket; *)

  struct_linger = RECORD
    l_onoff: uint16_t;
    l_linger: uint16_t;
  END;

(* INTERFACE Utime; *)

TYPE
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

  time_t = int; (* ideally always 64 bits *)

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
