(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE OSPOSIX EXPORTS OS;

PROCEDURE GetDiskSpace (<*UNUSED*> dir: TEXT): INTEGER =
  BEGIN
    RETURN LAST (INTEGER); (* big disk! *)
  END GetDiskSpace;

BEGIN
END OSPOSIX.
