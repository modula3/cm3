(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE OSConfigPosixC;

FROM Ctypes IMPORT const_char_star;

(* Callback from C to Modula3 so that C does not traffic in traced references. *)
PROCEDURE InitFromC (host_name, host_arch, os_name, os_version: const_char_star);

<*EXTERNAL OSConfigPosixC__InitC*>
PROCEDURE InitC ();

END OSConfigPosixC.
