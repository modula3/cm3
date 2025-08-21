(* $Id$ *)

INTERFACE DebugClass;
IMPORT DebugStreamList;

(* Debug.GetLevel return value exposed here.  Do not change it via this interface. *)

VAR level := 0;

VAR streams : DebugStreamList.T;
VAR mu : MUTEX;

PROCEDURE DoInit(); (* idempotent *)

END DebugClass.
