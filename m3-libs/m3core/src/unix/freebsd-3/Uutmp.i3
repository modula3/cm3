(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* File: Uutmp.i3                                             *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Apr 16 15:59:54 1990 by jerome        *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char, char_star, long, short;
FROM Utypes IMPORT pid_t, time_t;

(*** <utmp.h> ***)

(*
 * Structure of utmp and wtmp files.
 *
 *)

CONST
  (* Constants for the ut_type field : *)
  UT_UNKNOWN = 0;

  (* Size of user name *)
  UT_NAMESIZE = 8;

  (* Other things *)
  RUN_LVL = 1;
  BOOT_TIME = 2;
  NEW_TIME = 3;
  OLD_TIME = 4;

  INIT_PROCESS = 5;
  LOGIN_PROCESS = 6;
  USER_PROCESS = 7;
  DEAD_PROCESS = 8;

TYPE
  struct_utmp = RECORD
    ut_type : short;                 (* type of login *)
    ut_pid  : pid_t;                 (* pid of login process *)
    ut_line : ARRAY [0..11] OF char; (* name of tty - "/dev", null-term  *)
    ut_id   : ARRAY [0..1] OF char;  (* abbrev. ttyname, as 01, s1, etc *)
    ut_time : time_t;                (* login time *)
    ut_user : ARRAY [0..UT_NAMESIZE-1] OF char;  (* user id *)
    ut_host : ARRAY [0..15] OF char; (* host name, if remote *)
    ut_addr : long                   (* IP addr of remote host *)
  END;

CONST
  UTMP_FILE      = "/etc/utmp";
  WTMP_FILE      = "/var/adm/wtmp";
  UTMP_FILEMNAME = UTMP_FILE;
  WTMP_FILENAME  = WTMP_FILE;

(*	Special strings or formats used in the "ut_line" field when	*)
(*	accounting for something other than a process.			*)
(*	No string for the ut_line field can be more than 7 chars +	*)
(*	a NULL in length.						*)

(* These constants may not be used under Linux - use ut_type and the
   constants specified above for this purpose instead *)

   EMPTY	= "";
   BOOT_MSG	= "~";
   OTIME_MSG	= "|";
   NTIME_MSG	= "}";


(*** getlogin(3) ***)

<*EXTERNAL*> PROCEDURE getlogin (): char_star;


END Uutmp.
