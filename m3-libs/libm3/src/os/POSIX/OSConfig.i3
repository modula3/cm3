(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE OSConfig;

CONST
  ArcSep  : CHAR = '/';   (* separates arcs of a pathname *)
  PathSep : CHAR = ':';   (* separates paths in a path search list *)
  LineSep : TEXT = "\n";  (* separates lines in text files *)

CONST
  UnixEpoch : LONGREAL = 0.0d0;
  (* "Time.T" value that corresponds to the Unix epoch, 00:00 Jan 1, 1970 GMT *)

PROCEDURE HostName (): TEXT;
(* Returns the name of the host system. *)

PROCEDURE HostArchitecture (): TEXT;
(* Returns the name of the host machine architecture. *)

PROCEDURE OSName (): TEXT;
(* Returns the name of the host operating system. *)

PROCEDURE OSVersion (): TEXT;
(* Returns the version of the host operating system. *)

PROCEDURE UserName (): TEXT;
(* Returns the logon name of the current user *)

PROCEDURE UserHome (): TEXT;
(* Returns the home directory of the current user *)

END OSConfig.
