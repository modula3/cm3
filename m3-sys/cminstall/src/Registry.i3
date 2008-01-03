(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE Registry;

PROCEDURE Lookup(name: TEXT): TEXT;
PROCEDURE LookupByExtension(name: TEXT): TEXT;

END Registry.

