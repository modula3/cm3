(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(* This interface exists to bridge SchedulerPosix to
two different implementations of Thread (pthreads and cygwin).

Perhaps this all belongs in ThreadF, except perfOn, which
could be ThreadFVar. (don't export variables outside of libraries)

However, putting these here saves us from duplicating in two ThreadF.i3,
and from exporting any of this.
*)

INTERFACE ThreadInternal;

FROM ThreadF IMPORT Id, State;
FROM Thread IMPORT T;

PROCEDURE PerfChanged (id: Id; s: State);
PROCEDURE PerfRunning (id: Id);
PROCEDURE XTestAlert (self: T): BOOLEAN;

VAR
  perfOn: BOOLEAN := FALSE;		 (* LL = perfMu *)

END ThreadInternal.
