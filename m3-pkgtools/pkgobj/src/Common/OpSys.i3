(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OpSys.i3 *)
(* Last modified on Wed Apr 21 12:18:11 PDT 1993 by wobber *)

INTERFACE OpSys;

EXCEPTION Error;

PROCEDURE Init () RAISES {Error};

PROCEDURE GetHostName (): TEXT RAISES {Error};

PROCEDURE GetUser (): TEXT RAISES {Error};

PROCEDURE SetUser (who: TEXT) RAISES {Error};

END OpSys.

