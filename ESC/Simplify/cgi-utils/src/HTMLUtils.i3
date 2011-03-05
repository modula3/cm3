(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Mon Dec  4 11:56:24 PST 1995 by detlefs
*)

(* This interface is derived from cgi-src/util.c. *)

INTERFACE HTMLUtils;

IMPORT Rd;

PROCEDURE FMakeWord(rd: Rd.T; stop: CHAR; VAR cl: INTEGER): TEXT;

(* MODIFIES str^ *)
PROCEDURE PlusToSpace(str: TEXT);

PROCEDURE UnEscapeURL(old: TEXT): TEXT;

(* Splits "str" into two parts: the part up to but not including the
   first occurrence of "stop" is returned, and "str" is modified to
   hold the remainder. *)
PROCEDURE MakeWord(VAR str: TEXT; stop: CHAR): TEXT;

END HTMLUtils.
