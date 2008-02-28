UNSAFE MODULE WaitProcessCygwin EXPORTS SchedulerPosix;

IMPORT Uexec;
FROM Ctypes IMPORT int;

PROCEDURE WaitProcess (pid: int): int =
  (* This is identical to ThreadPThread.WaitProcess. *)
  VAR
    statusT: Uexec.w_T;
    result := Uexec.waitpid(pid, ADR(statusT));
    statusM3 := Uexec.w_M3 { w_Filler := 0,
                             w_Coredump := statusT.w_Coredump,
                             w_Termsig := statusT.w_Termsig,
                             w_Retcode := statusT.w_Retcode };
  BEGIN
    <* ASSERT result > 0 *>
    RETURN LOOPHOLE(statusM3, Uexec.w_A);
  END WaitProcess;

BEGIN
END WaitProcessCygwin.
