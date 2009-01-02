(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE OSConfigPosixC;

FROM Ctypes IMPORT int;

<*EXTERNAL OSConfigPosixC__Init*>
PROCEDURE
Init (
    VAR host_name: TEXT;
    VAR host_arch: TEXT;
    VAR os_name: TEXT;
    VAR os_version: TEXT) : int;

END OSConfigPosixC.
