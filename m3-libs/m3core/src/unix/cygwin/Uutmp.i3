(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* File: Uutmp.i3                                             *)
(* Last modified on Mon Jan  5 11:47:09 GMT 1998 by rrw       *)
(*      modified on Mon May  2 08:50:25 PDT 1994 by kalsow    *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Mon Apr 16 15:59:54 1990 by jerome        *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char, char_star, long, short;
FROM Utypes IMPORT pid_t, time_t;
FROM Utime IMPORT struct_timeval;
IMPORT Upaths;

(*** <utmp.h> ***)

(*
 * Structure of utmp and wtmp files.
 *
 *)


CONST
  (* Constants for the ut_type field : *)
  UT_UNKNOWN = 0;

  (* Other things *)
  RUN_LVL = 1;
  BOOT_TIME = 2;
  NEW_TIME = 3;
  OLD_TIME = 4;

  INIT_PROCESS = 5;
  LOGIN_PROCESS = 6;
  USER_PROCESS = 7;
  DEAD_PROCESS = 8;
  
  ACCOUNTING = 9;

  UT_LINESIZE = 32;
  UT_NAMESIZE = 32;
  UT_HOSTSIZE = 256;

TYPE
  struct_exit_status = RECORD
    e_termination : short; (* Termination status *)
    e_exit : short; (* Exit status *)
  END;

  struct_utmp = RECORD
    ut_type : short;                 (* type of login *)
    ut_pid  : pid_t;                 (* pid of login process *)
    ut_line : ARRAY [0..UT_LINESIZE-1] OF char; (* name of tty - "/dev", null-term  *)
    ut_id   : ARRAY [0..3] OF char;  (* abbrev. ttyname, as 01, s1, etc *)
    ut_time : time_t;                (* login time *)
    ut_user : ARRAY [0..UT_NAMESIZE-1] OF char;  (* user id *)
    ut_host : ARRAY [0..UT_HOSTSIZE-1] OF char; (* host name, if remote *)
    ut_exit : struct_exit_status;  (* Exit status of a process marked DEAD *)
    ut_session : long;              (* Session ID *)
    ut_tv : struct_timeval;         (* Time entry was made *)
    ut_addr : ARRAY [0..3] OF INTEGER;  (* IP addr of remote host *)
    pad : ARRAY [0..19] OF CHAR;   (* Padding *)
  END;

CONST
  UTMP_FILE      = Upaths.UTMP;
  WTMP_FILE      = Upaths.WTMP;
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
