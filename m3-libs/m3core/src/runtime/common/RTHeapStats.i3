(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu May  5 08:10:43 PDT 1994 by kalsow     *)

INTERFACE RTHeapStats;

PROCEDURE ReportReachable ();
(* reports the number of reachable objects and bytes from
   each compilation unit, thread stack, and the individual roots
   that reach the most bytes.  The report is written to
   stderr.  The Modula-3 process is frozen during the scanning
   and reporting.  *)

END RTHeapStats.

   
   
