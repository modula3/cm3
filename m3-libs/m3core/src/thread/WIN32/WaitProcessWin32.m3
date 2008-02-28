MODULE WaitProcessWin32 EXPORTS SchedulerPosix;
FROM Ctypes IMPORT int;

PROCEDURE WaitProcess (<*UNUSED*>pid: int): int =
  BEGIN
    <* ASSERT FALSE *>
    RETURN 0; <* NOWARN *>
  END WaitProcess;

BEGIN
END WaitProcessWin32.
