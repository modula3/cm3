(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(* This interface exists to bridge SchedulerPosix to WIN32 threads *)
INTERFACE ThreadInternal;

FROM ThreadF IMPORT State;
FROM Thread IMPORT T;

PROCEDURE PerfChanged (s: State);
PROCEDURE PerfRunning ();
PROCEDURE XTestAlert (self: T): BOOLEAN;

VAR
  perfOn: BOOLEAN := FALSE;		 (* LL = perfMu *)

END ThreadInternal.
