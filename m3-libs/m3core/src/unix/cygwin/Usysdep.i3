(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

(* INTERFACE Unix; *)

IMPORT Ctypes;
FROM Ctypes IMPORT int, const_char_star, char_star_star, char, unsigned_int;
FROM Cstdint IMPORT uint8_t, uint16_t, uint32_t, int16_t, int32_t;

(* This is the only system that uses this. *)
(* CONST *)
<*EXTERNAL "Usysdep_P_NOWAIT"*> VAR P_NOWAIT: int;
<*EXTERNAL*> PROCEDURE spawnve (mode: int; name: const_char_star; argv, envp: char_star_star): int;

(* Unix.i3 *)

CONST
  MaxPathLen = 1024;

  MAX_FDSET = 1024;

TYPE
  mode_t = int;

(* INTERFACE Upthread; *)

  pthread_t = ADDRESS; (* opaque *)
  pthread_attr_t = ADDRESS; (* opaque *)
  pthread_mutex_t = ADDRESS; (* opaque *)
  pthread_cond_t = ADDRESS; (* opaque *)
  pthread_key_t = ADDRESS; (* opaque *)

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

  (* use Cstddef.size_t instead *)
  size_t = uint32_t;

  (* ideally uint64_t or uint64_t, on all platforms, deal with it later *)
  time_t = int32_t;

  uid_t = uint32_t;
  pid_t = int32_t;
  gid_t = uint32_t;

  socklen_t = int32_t;
  hostent_addrtype_t = int16_t;
  hostent_length_t = int16_t;

(* RTMachine.i3 *)
CONST SIG_SUSPEND = 19; (* SIGUSR2 *)

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
