(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE OSPOSIX EXPORTS OS;

PROCEDURE GetDiskSpace (<*UNUSED*> dir: TEXT): INTEGER =
  BEGIN
    RETURN LAST (INTEGER); (* big disk! *)
  END GetDiskSpace;

BEGIN
END OSPOSIX.
