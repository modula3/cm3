(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE RegistryPOSIX EXPORTS Registry;

PROCEDURE Lookup (<*UNUSED*> name:TEXT): TEXT =
  BEGIN
    RETURN NIL;
  END Lookup;

PROCEDURE LookupByExtension (<*UNUSED*> ext: TEXT): TEXT =
  BEGIN
    RETURN NIL;
  END LookupByExtension;

BEGIN
END RegistryPOSIX.

