(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Oct  6 11:37:52 PDT 1994 by ericv         *)
(*      modified on Fri Mar 16 12:16:52 1990 by muller        *)

MODULE Utime;

IMPORT Unix;

BEGIN
  CLK_TCK := Unix.sysconf( Unix.SC_CLK_TCK );
END Utime.
