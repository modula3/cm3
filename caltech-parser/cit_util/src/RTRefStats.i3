INTERFACE RTRefStats;
IMPORT Wr;
IMPORT Time;

PROCEDURE ReportReachable (printTexts := FALSE; wr : Wr.T := NIL);
(* reports the number of reachable objects and bytes from each brand /
   typecode. The report is written to stderr if wr is NIL. The Modula-3 process
   is frozen during the scanning and reporting. *)

TYPE
  Reporter <: PublicReporter;

  PublicReporter = OBJECT METHODS
    init(interval : Time.T; printTexts := FALSE; wr : Wr.T := NIL) : Reporter;
    (* start a reporting thread, see above for args *)

    start();
    stop();
  END;

END RTRefStats.
