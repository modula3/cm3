(* $Id$ *)

MODULE SchedulerIndirectionPM3 EXPORTS SchedulerIndirection;
IMPORT SchedulerPosix AS Scheduler;

PROCEDURE DisableSwitching() = 
  BEGIN Scheduler.DisableSwitching() END DisableSwitching;

PROCEDURE EnableSwitching() = 
  BEGIN Scheduler.EnableSwitching() END EnableSwitching;

BEGIN END SchedulerIndirectionPM3.
