(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Nov 18 16:00:55 PST 1994 by kalsow     *)
(*      modified on Wed Jan 27 22:49:37 PST 1993 by mjordan    *)

UNSAFE MODULE RTOS;

IMPORT Unix, Uuio, Cstdlib;

(*--------------------------------------------------- process termination ---*)

PROCEDURE Exit (n: INTEGER) =
  BEGIN
    Unix.exit (n);
  END Exit;

PROCEDURE Crash () =
  BEGIN
    Cstdlib.abort ();
    LOOP END; (* wait *)
  END Crash;

(*------------------------------------------------------------------- I/O ---*)

PROCEDURE Write (a: ADDRESS;  n: INTEGER) =
  BEGIN
    EVAL Uuio.write (2, a, n);
  END Write;

BEGIN
END RTOS.
