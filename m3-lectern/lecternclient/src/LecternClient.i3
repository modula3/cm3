(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Mon Apr 25 14:56:05 PDT 1994 by mcjones        *)

(* Send a request to an instance of Lectern, starting one if necessary. *)

(* Inspired by emacsclient. *)

INTERFACE LecternClient;

EXCEPTION Error(TEXT);

PROCEDURE Send(READONLY params: ARRAY OF TEXT) RAISES {Error};

END LecternClient.

