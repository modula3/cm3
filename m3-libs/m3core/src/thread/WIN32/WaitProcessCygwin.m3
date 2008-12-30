UNSAFE MODULE WaitProcessCygwin EXPORTS SchedulerPosix;

IMPORT Uwaitpid;
FROM Ctypes IMPORT int;

PROCEDURE WaitProcess (pid: int): int =
  (* This is identical to ThreadPThread.WaitProcess. *)
  VAR
    status: Uwaitpid.w_M3;
    result := Uwaitpid.waitpid(pid, status);
  BEGIN
    <* ASSERT result > 0 *>
    RETURN status.w_Loophole;
  END WaitProcess;

BEGIN
END WaitProcessCygwin.
