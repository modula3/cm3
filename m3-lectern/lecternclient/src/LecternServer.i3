(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Apr  7 15:50:58 PDT 1994 by mcjones    *)

(* The "LecternServer" interface allows Lectern to receive a request
   (e.g., to open a specified document) via a shell command. *)

INTERFACE LecternServer;

EXCEPTION Error(TEXT);

PROCEDURE AwaitRequest(): REF ARRAY OF TEXT RAISES {Error};
(* Block until the next request is received from a ``matching''
   invocation of the shell command "LecternClient(1)", then return an
   array of the texts constituting the command-line parameters
   ("argv[1] ... "argv[argc]") from that invocation of
   "LecternClient".  Here ``matching'' means that the process calling
   "AwaitRequest" has the same effective user ID (see "geteuid(2)"),
   and is running on the same machine, as the process executing
   "LecternClient".

   If "AwaitRequest" raises "Error", the associated "TEXT" should be
   displayed to the user. *)

END LecternServer.









