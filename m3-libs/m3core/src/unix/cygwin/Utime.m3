(* $Id: Utime.m3,v 1.1 2008-02-11 14:27:30 jkrell Exp $ *)

MODULE Utime;

FROM Ctypes IMPORT int;

PROCEDURE setitimer (which: int; 
                     VAR value, old_value: struct_itimerval): int =
(* cygwin\src\winsup\cygwin\timer.cc:
extern "C" int
setitimer (int which, const struct itimerval *value, struct itimerval *ovalue)
{
  if (which != ITIMER_REAL)
    {
      set_errno (EINVAL);
      return -1;
    }

ProcessPosix.m3:
    IF Utime.setitimer(Utime.ITIMER_VIRTUAL, nit, oit) < 0 THEN
      <* ASSERT FALSE *>
    END;

RTPerfTool.m3:
    IF Utime.setitimer (Utime.ITIMER_VIRTUAL, nit, oit) = -1 THEN
      ClosePipe (toTool);
      ClosePipe (fromTool);
      RETURN FALSE;
    END;

ThreadPosix.m3 (which we never use):
      IF Utime.setitimer (Utime.ITIMER_VIRTUAL, it, oit) # 0 THEN
        RAISE InternalError;
      END;

so when Cygwin would always do nothing and fail, we do nothing and succeed.
*)
BEGIN
  IF which # ITIMER_REAL THEN
    RETURN 0;
  END;
  RETURN setitimer_ (which, value, old_value);
END setitimer;

BEGIN
END Utime.
