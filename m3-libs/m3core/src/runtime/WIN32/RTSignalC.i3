(* Copyright 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE RTSignalC;

<* EXTERNAL "RTSignalC_i386" *>  PROCEDURE i386() : INTEGER;
<* EXTERNAL "RTSignalC_Amd64" *> PROCEDURE Amd64() : INTEGER;
<* EXTERNAL "RTSignalC_Arm64" *> PROCEDURE Arm64() : INTEGER;
<* EXTERNAL "RTSignalC_Arch" *>  PROCEDURE Arch() : INTEGER;

END RTSignalC.
