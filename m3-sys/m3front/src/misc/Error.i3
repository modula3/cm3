(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Error.i3                                              *)
(* Last modified on Fri Jun 24 09:15:20 PDT 1994 by kalsow     *)
(*      modified on Fri Mar 22 08:29:20 1991 by muller         *)

INTERFACE Error;

IMPORT M3, M3ID;

PROCEDURE Msg (msg: TEXT);
PROCEDURE Int (n: INTEGER;  msg: TEXT);
PROCEDURE ID  (id: M3ID.T;  msg: TEXT);
PROCEDURE QID (READONLY q: M3.QID;  msg: TEXT);
PROCEDURE Txt (str, msg: TEXT);

PROCEDURE Warn   (level: INTEGER;  msg: TEXT);
PROCEDURE WarnID (level: INTEGER;  id: M3ID.T;  msg: TEXT);

PROCEDURE IgnoreWarning (offset: INTEGER);

PROCEDURE Info (msg: TEXT);

PROCEDURE Count (VAR nErrors, nWarnings: INTEGER);

PROCEDURE Reset ();

END Error.

(***************************
Error levels:

  0 - informational messages
  1 - "fussy" warnings  (i.e. unsued formals, incomplete CASE...)
  2 - warnings
  3 - errors

***************************)
