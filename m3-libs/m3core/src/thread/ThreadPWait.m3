(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

UNSAFE MODULE ThreadPWait EXPORTS SchedulerPosix;

FROM Ctypes IMPORT int;
FROM Uwaitpid IMPORT waitpid_status_t, waitpid, WNOHANG;
IMPORT Cerrno, Uerror;
FROM Thread IMPORT Pause;

(* Putting all this code in one file leads to likely
dead code linking into binaries, but it keeps
similar code together, so that changes in one are more likely
to be made in the other, as appropriate. *)

(* Efficiently let waitpid "hang"; it will yield to other threads
while waiting for the process to end. *)

PROCEDURE WaitProcessFast (pid: int): int =
  VAR
    status: waitpid_status_t;
  BEGIN
    LOOP
      WITH r = waitpid(pid, status) DO
        IF r > 0 THEN
          RETURN status.w_Loophole;
        END;
        <*ASSERT r < 0*>
      END;
      <*ASSERT Cerrno.GetErrno() = Uerror.EINTR*>
    END;
  END WaitProcessFast;


(* inefficient waitpid(nohang) poll/sleep loop for user threads *)

PROCEDURE WaitProcessSlow (pid: int): int =
  VAR
    result: int;
    status: waitpid_status_t;
  CONST Delay = 0.1D0;
  BEGIN
    LOOP
      result := waitpid(pid, status, WNOHANG);
      IF result # 0 THEN
        EXIT
      END;
      Pause(Delay);
    END;
    <* ASSERT result > 0 *>
    RETURN status.w_Loophole;
  END WaitProcessSlow;


PROCEDURE WaitProcess (pid: int): int =
  BEGIN
    IF DoesWaitPidYield() THEN
        RETURN WaitProcessFast(pid);
    ELSE
        RETURN WaitProcessSlow(pid);
    END;
  END WaitProcess;

BEGIN
END ThreadPWait.
