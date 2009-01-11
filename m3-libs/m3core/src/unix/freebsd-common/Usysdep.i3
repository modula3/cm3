(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* copied from openbsd-common *)

INTERFACE Usysdep;

FROM Ctypes IMPORT int, char_star, void_star;
FROM Cstdint IMPORT uint32_t, int32_t, int64_t;

(* INTERFACE Unix; *)

CONST
  MaxPathLen = 1024;
  MSETUID = 8_4000;
  MSETGID = 8_2000;
  MSTICKY = 8_1000;

  O_RDONLY = 16_0000;
  O_RDWR = 16_0002;
  O_CREAT = 16_0200;
  O_EXCL = 16_0800;
  O_TRUNC = 16_0400;
  O_NONBLOCK = 16_0004;

  MAX_FDSET = 1024;

TYPE
  mode_t = uint32_t;

(* INTERFACE Upthread; *)

  pthread_t = ADDRESS;
  pthread_attr_t = ADDRESS;
  pthread_mutex_t = ADDRESS;
  pthread_cond_t = ADDRESS;
  (* pthread_mutexattr_t = ADDRESS; *)
  (* pthread_condattr_t = ADDRESS; *)
  pthread_key_t = int;

  destructor_t = PROCEDURE(arg: ADDRESS);
  start_routine_t = PROCEDURE(arg: ADDRESS): ADDRESS;

CONST
  PTHREAD_MUTEX_INITIALIZER : pthread_mutex_t = NIL;
  PTHREAD_COND_INITIALIZER : pthread_cond_t = NIL;

(* INTERFACE Usignal; *)

(*
  SIGHUP = 1;
  SIGINT = 2;
  SIGQUIT = 3;
  SIGABRT = 6;
  SIGKILL = 9;
  SIGSEGV = 11;
  SIGPIPE = 13;
  SIGTERM = 15;
*)

TYPE
  SignalActionHandler = PROCEDURE (sig: int; sip: siginfo_t_star; uap: ucontext_t_star);
  SignalHandler = PROCEDURE (sig: int);
  siginfo_t_star = ADDRESS;

(* INTERFACE Usocket; *)

CONST
  SOCK_STREAM = 1;
  SOCK_DGRAM = 2;

  SO_REUSEADDR = 16_0004;
  SO_KEEPALIVE = 16_0008;
  SO_LINGER = 16_0080;

  SOL_SOCKET = 16_FFFF;
  AF_INET = 2;
  MSG_PEEK = 2;

TYPE
  struct_linger = RECORD
    l_onoff: int;
    l_linger: int;
  END;

(* INTERFACE Utime; *)

  struct_timeval = RECORD
    tv_sec: time_t;
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
    tm_gmtoff:INTEGER;
    tm_zone:  char_star;
  END;

(* INTERFACE Utypes; *)

  (* ideally always 64 bits *)
  clock_t = int32_t;

  gid_t = uint32_t;

  pid_t = int32_t;

  (* ideally always 64 bits *)
  time_t = INTEGER;

  uid_t = uint32_t;

  socklen_t = uint32_t;
  hostent_addrtype_t = int32_t;
  hostent_length_t = int32_t;

(* INTERFACE Uucontext; *)

  sigset_t = ARRAY [0..3] OF uint32_t;
  ucontext_t_star = void_star;

(* RTMachine.i3 *)
CONST SIG_SUSPEND = 31;

END Usysdep.
