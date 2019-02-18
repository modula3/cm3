(* $Id$ *)

UNSAFE MODULE UtimeWrap;
IMPORT UtimeOpsC;
IMPORT SchedulerIndirection;

PROCEDURE make_T() : T =
  BEGIN
    SchedulerIndirection.DisableSwitching();
    TRY
      RETURN UtimeOpsC.make_T()
    FINALLY
      SchedulerIndirection.EnableSwitching()
    END
  END make_T;

PROCEDURE delete_T(t : T) = 
  BEGIN
    SchedulerIndirection.DisableSwitching();
    TRY
      UtimeOpsC.delete_T(t)
    FINALLY
      SchedulerIndirection.EnableSwitching()
    END
  END delete_T;

BEGIN END UtimeWrap.
