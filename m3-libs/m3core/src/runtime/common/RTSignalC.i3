(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* derived from LINUXLIBC6 *)

<*EXTERNAL*> UNSAFE INTERFACE RTSignalC;

TYPE Texts_t = RECORD
    aborted := "aborted";
    segv := "Segmentation violation - possible attempt to dereference NIL";
END;

<*EXTERNAL "RTSignalC_InstallHandlers"*> PROCEDURE InstallHandlers(VAR Texts:Texts_t);
<*EXTERNAL "RTSignalC_RestoreHandlers"*> PROCEDURE RestoreHandlers();

END RTSignalC.
