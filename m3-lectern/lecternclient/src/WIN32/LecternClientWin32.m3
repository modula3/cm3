(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Thu May 23 17:22:05 PDT 1996 by mcjones        *)

(* Send a request to an instance of Lectern, starting one if necessary. *)

(* Inspired by emacsclient. *)

(* No-op for now; should rewrite LecternClientPosix using named pipes or Winsock. *)

MODULE LecternClientWin32 EXPORTS LecternClient;

PROCEDURE Send(<*UNUSED*>READONLY params: ARRAY OF TEXT) RAISES {} =
  BEGIN
  END Send;

BEGIN
END LecternClientWin32.
