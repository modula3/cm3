(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)
INTERFACE ThreadPThread;
FROM Upthread IMPORT pthread_t;

TYPE
  Activation = UNTRACED REF RECORD
    (* global doubly-linked, circular list of all active threads *)
    next, prev: Activation := NIL;	 (* LL = activeMu *)
    (* thread handle *)
    handle: pthread_t;			 (* LL = activeMu *)
    (* base of thread stack for use by GC *)
    stackbase: ADDRESS := NIL;
    sp: ADDRESS := NIL;
    lastStopCount: CARDINAL := 0;
    signal := 0;
    (* index into global array of active, slotted threads *)
    slot: INTEGER;			 (* LL = slotMu *)
    idle: BOOLEAN := FALSE;		 (* LL = idleMu *)
  END;

END ThreadPThread.
