INTERFACE PMPTimer;

(* a timer for automated Poor Man's Profiling in m3gdb *)

PROCEDURE Start(initialDelay := 10.0D0; repeatDelay := 1.0D0);

END PMPTimer.
