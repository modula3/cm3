(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

INTERFACE Registry;

PROCEDURE Lookup(name: TEXT): TEXT;
PROCEDURE LookupByExtension(name: TEXT): TEXT;

END Registry.

