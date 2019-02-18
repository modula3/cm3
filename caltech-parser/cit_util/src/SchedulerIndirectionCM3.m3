(* $Id$ *)

MODULE SchedulerIndirectionCM3 EXPORTS SchedulerIndirection;
IMPORT Scheduler;

PROCEDURE DisableSwitching() = 
  BEGIN Scheduler.DisableSwitching() END DisableSwitching;

PROCEDURE EnableSwitching() = 
  BEGIN Scheduler.EnableSwitching() END EnableSwitching;

BEGIN END SchedulerIndirectionCM3.
