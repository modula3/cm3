(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SiphonServer.i3 *)
(* Last modified on Fri Apr 29 12:33:29 PDT 1994 by wobber  *)
(*      modified on Fri Jun 26 16:16:02 GMT+2:00 1992 by prusker *)

INTERFACE SiphonServer;

IMPORT Siphon;

PROCEDURE New(sitename: TEXT): Siphon.T;

END SiphonServer.
