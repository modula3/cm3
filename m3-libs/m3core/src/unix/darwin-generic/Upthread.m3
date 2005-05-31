(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

MODULE Upthread;

FROM Ctypes IMPORT int;

PROCEDURE suspend (thread: pthread_t): int =
  VAR mach_thread := mach_thread_np (thread);
  BEGIN
    RETURN thread_suspend (mach_thread);
  END suspend;

PROCEDURE continue (thread: pthread_t): int =
  VAR mach_thread := mach_thread_np (thread);
  BEGIN
    RETURN thread_resume (mach_thread);
  END continue;

BEGIN
END Upthread.
