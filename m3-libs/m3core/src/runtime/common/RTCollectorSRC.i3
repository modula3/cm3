(*| Copyright (C) 1993, Digital Equipment Corporation    *)
(*| All rights reserved.                                 *)
(*| See the file COPYRIGHT for a full description.       *)
(*| Last modified on Sun Feb 21 14:29:21 PST 1993 by jdd *)

(* "RTCollectorSRC" is an extension of "RTCollector", specific to the SRC
   Modula-3 implementation. *)

INTERFACE RTCollectorSRC;

(* \paragraph*{When to collect.}

   "StartCollection" and "FinishCollection" allow the programmer direct
   control over when to collect. *)

PROCEDURE StartCollection();
(* Start a total collection, if none is in progress and if collection and
   motion are enabled. *)

PROCEDURE FinishCollection();
(* Finish the current collection, if one is on progress. *)

(* \paragraph*{Disabling VM protection.}

   The SRC collector uses VM protection to implement incremental and
   generational collection.  The SRC collector's use of VM protection is
   normally invisible to programs, but can complicate debugging Modula-3
   programs.

   The "DisableVM" and "EnableVM" procedures are analogous to
   "RTCollector"'s "DisableMotion" and "EnableMotion", and allow the
   programmer to disable the use of VM protection.  Disabling VM protection
   does not disable the collector, but collections will be neither
   incremental nor generational.

   The "@M3novm" flag performs an initial call to "DisableVM".

   The SRC collector cannot use VM protection at all on some
   architectures. *)

PROCEDURE DisableVM();
(* Disable the use of VM protection.  While VM protection is disabled, no
   objects on the heap will be protected.*)

PROCEDURE EnableVM();
(* Reenable the use of VM protection if "EnableVM" has been called as many
   times as "DisableVM".  It is a checked runtime error to call "EnableVM"
   more times than "DisableVM". *)

PROCEDURE FinishVM();
(* Equivalent to "DisableVM{}; EnableVM()".  "FinishVM" unprotects all heap
   pages, and is intended for use from the debugger. *)

(* \paragraph*{Tuning the SRC collector.}

   The following read/write parameters tune the SRC collector's
   performance.  They may be set by the client at any point, although they
   may not have an immediate effect. *)

VAR gcRatio := 1.0;              (* collector work / mutator work *)
(* On the average, for every page allocated by the mutator, the collector
   will copy "gcRatio" pages.  Increase the ratio to keep the heap smaller;
   decrease it to spend less time in the collector. *)

VAR incremental := TRUE;         (* incremental collection *)
(* The collector can be incremental or stop-and-copy.  Incremental
   collection has much smaller interruptions of service, but takes more
   total time and more space.

   Assume there are ``A'' pages of accessible objects.  If "incremental" is
   FALSE, the heap must contain up to A * (2 + 1 / gcRatio) pages.  If
   "incremental" is TRUE, the heap must contain up to A * (2 + 2 / gcRatio)
   pages.  In other words, to keep the same space bounds, "gcRatio" must be
   twice as large in the incremental case.

   If VM protection is disabled or not available on the current
   architecture, the collector will behave as if "incremental" = FALSE.

   Use incremental collection when the program is interactive.
   Stop-and-copy collection gives better performance. *)

VAR generational := TRUE;        (* generational collection *)
(* Generational collection causes most collections to take much less time
   than specified above, while using only a little more memory.
   Generational collection has the greatest benefit when the program has a
   large number of accessible objects, but most new objects are discarded
   shortly after they are allocated.

   If VM protection is disabled or not available on the current
   architecture, the collector will behave as if "generational" = FALSE.

   Generational collection almost always leads to performance
   improvement. *)

(* \paragraph{Background Collection.}

   There is an optional ``background'' mode, which extends incremental mode
   with a background thread that moves collection ahead in the absence of
   program activity.  The background thread is tuned to cause insignificant
   interruption of other activities, but may therefore move the collection
   forward quite slowly. *)

PROCEDURE StartBackgroundCollection();
(* Starts the background thread, if not already started *)

END RTCollectorSRC.
