INTERFACE RTRefStats;

PROCEDURE ReportReachable (printTexts := FALSE);
(* reports the number of reachable objects and bytes from each brand /
   typecode. The report is written to stderr. The Modula-3 process
   is frozen during the scanning and reporting. *)

END RTRefStats.
