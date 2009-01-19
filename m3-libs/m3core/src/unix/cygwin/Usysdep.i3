(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Usysdep;

(* INTERFACE Unix; *)

IMPORT Ctypes;
FROM Ctypes IMPORT int, const_char_star, char_star_star;
FROM Cstdint IMPORT uint16_t, uint32_t, int16_t, int32_t;

(* This is the only system that uses this. *)
(* CONST *)
<*EXTERNAL "Usysdep_P_NOWAIT"*> VAR P_NOWAIT: int;
<*EXTERNAL*> PROCEDURE spawnve (mode: int; name: const_char_star; argv, envp: char_star_star): int;

(* Unix.i3 *)

CONST
  MaxPathLen = 1024;
  MSETUID = 0;
  MSETGID = 0;
  MSTICKY = 0;

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

END Usysdep.
