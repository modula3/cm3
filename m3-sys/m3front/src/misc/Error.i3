(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Error.i3                                              *)
(* Last modified on Fri Jun 24 09:15:20 PDT 1994 by kalsow     *)
(*      modified on Fri Mar 22 08:29:20 1991 by muller         *)

INTERFACE Error;

IMPORT M3ID, M3;

(* Msg, Int, ID, QID, Txt are all implicitly level 3. *)
PROCEDURE Msg (msg: TEXT);
PROCEDURE Int (n: INTEGER;  msg: TEXT);
PROCEDURE ID  (id: M3ID.T;  msg: TEXT);
PROCEDURE QID (READONLY q: M3.QID;  msg: TEXT);
PROCEDURE Txt (str, msg: TEXT);

(* Despite the names Warn and WarnId, these can be any level. *)
PROCEDURE Warn   (level: INTEGER;  msg: TEXT);
PROCEDURE WarnID (level: INTEGER;  id: M3ID.T;  msg: TEXT);

PROCEDURE IgnoreWarning (offset: INTEGER);

(* Info, InfoInt are both implicitly level 0. *)
PROCEDURE Info (msg: TEXT);
PROCEDURE InfoInt (n: INTEGER;  msg: TEXT);

PROCEDURE Count (VAR nErrors, nWarnings: INTEGER);

PROCEDURE Reset ();

END Error.

(***************************
Error levels:

  0 - informational messages
  1 - "fussy" warnings  (i.e. unused formals, incomplete CASE...)
  2 - warnings
  3 - errors

***************************)
