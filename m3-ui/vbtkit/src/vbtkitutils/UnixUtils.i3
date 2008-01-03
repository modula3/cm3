(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Feb 28 20:01:19 PST 1993 by meehan                   *)
(*      modified on Tue Jun 30 19:25:56 1992 by mhb                          *)
(*      modified on Mon Jun 22 17:34:52 PDT 1992 by muller                   *)

(* The "UnixUtils" interface contain assorted procedures that
   access the Unix file system.  These calls should eventually
   be subsumed by the Modula-3 core libraries. *)

INTERFACE UnixUtils;

IMPORT TextList, Ctypes, Utypes;

PROCEDURE Directory (dirname: TEXT): TextList.T RAISES {Error};
(* Return a list of all the files in the named directory.
   Raise an exception if "opendir(3)" returns "NULL", which
   happens ``if the specified filename can not be accessed, or if
   insufficient memory is available to open the directory
   file.'' *)

EXCEPTION Error(TEXT);
(* The argument to the "Error" exception is "strerror(errno)"
   converted to "TEXT". *)

PROCEDURE IsDirectory (pathname: TEXT): BOOLEAN;
(* Does "pathname" specify a directory? *)

PROCEDURE ProbeFile (file: TEXT; error: BOOLEAN): BOOLEAN
  RAISES {Error};
(* If "file" exists, return TRUE.  Otherwise, if "error" is TRUE, raise
   an exception.  Otherwise return FALSE.  *)

TYPE Seconds = Utypes.time_t;
(* This is an "INTEGER" representing the number of seconds since
   January 1, 1970 GMT. *)

PROCEDURE FileModifyTime (file: TEXT): Seconds;
(* When was "file" last modified?  Return the "st_mtime" from the
   call to "stat(2)". *)

PROCEDURE GetWD (): TEXT RAISES {Error};
(* Return the value of calling "getwd(3)", raising the exception
   if "getwd" reports any problems. *)

TYPE AccessMode = {Execute, Write, Read};

PROCEDURE Accessible (file: TEXT; modes := SET OF AccessMode {}): BOOLEAN;
(* Test whether "file" can be accessed according to "modes". For
   details, see the manpage for "access(2)".  The default value
   of "modes" will test whether the directories leading to the file
   can be searched and whether the file exists. *)

<* EXTERNAL *> PROCEDURE access (path: Ctypes.char_star; mode: Ctypes.int):
  Ctypes.int;

END UnixUtils.

