(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE OpSys;

EXCEPTION Error;

PROCEDURE GetHostName (): TEXT RAISES {Error};

END OpSys.
