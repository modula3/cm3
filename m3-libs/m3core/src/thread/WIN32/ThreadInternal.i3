(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(* This interface exists to bridge SchedulerPosix to WIN32 threads *)

UNSAFE INTERFACE ThreadInternal;

FROM ThreadF IMPORT Id, State;
FROM Thread IMPORT T;

PROCEDURE PerfChanged (id: Id; s: State);
PROCEDURE PerfRunning (id: Id);
PROCEDURE XTestAlert (self: T): BOOLEAN;

VAR
  perfOn: BOOLEAN := FALSE;		 (* LL = perfMu *)

END ThreadInternal.
