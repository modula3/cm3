(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Feb  5 16:24:11 PST 1993 by mjordan *)

INTERFACE TimeWin32;

IMPORT Time, WinBase;

(* Conversions between a "Time.T" and a "WinBase.FILETIME". *)

<*EXTERNAL TimeWin32__ToFileTime*>
(* VAR is to avoid passing struct by value *)
PROCEDURE ToFileTime(n: Time.T; (*OUT*)VAR ft: WinBase.FILETIME);

<*EXTERNAL TimeWin32__FromFileTime*>
(* READONLY is to avoid passing struct by value *)
PROCEDURE FromFileTime(READONLY ft: WinBase.FILETIME): Time.T;

END TimeWin32.
