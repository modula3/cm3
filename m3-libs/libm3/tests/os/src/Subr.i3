(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Mar 30 13:52:37 PST 1995 by mcjones *)
 
INTERFACE Subr;

IMPORT OSError, Pathname, RefList, Sx;

EXCEPTION Usage(TEXT);

TYPE T = PROCEDURE(args: RefList.T)
  : Sx.T RAISES {Usage, OSError.E, Pathname.Invalid};

CONST Brand = "Subr-1.0";

END Subr.
