(* $Id$ *)

INTERFACE SchedulerIndirection;

(* turn off thread switching on the platforms that support it.

   heavily used by time conversion routines that use static memory areas
   in libc and/or malloc 

   we do not as yet have a WIN32 implementation but it's just ThreadF.SuspendOthers / ThreadF.ResumeOthers
*)

PROCEDURE DisableSwitching();

PROCEDURE EnableSwitching();

END SchedulerIndirection.
