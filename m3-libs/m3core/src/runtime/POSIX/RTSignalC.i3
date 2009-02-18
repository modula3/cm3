(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* derived from LINUXLIBC6 *)

<*EXTERNAL*> UNSAFE INTERFACE RTSignalC;

<*EXTERNAL "RTSignalC_InstallHandlers"*> PROCEDURE InstallHandlers();
<*EXTERNAL "RTSignalC_RestoreHandlers"*> PROCEDURE RestoreHandlers();

END RTSignalC.
