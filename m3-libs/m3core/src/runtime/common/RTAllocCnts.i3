(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE INTERFACE RTAllocCnts;

IMPORT RT0;

VAR (*READONLY*)
  n_types   : CARDINAL := 0;
  n_objects : UNTRACED REF ARRAY RT0.Typecode OF INTEGER := NIL;
  n_bytes   : UNTRACED REF ARRAY RT0.Typecode OF INTEGER := NIL;

(* These arrays record the number of objects and bytes allocated
   for each runtime type.  For fixed-size types (everything except
   REF open array) only the number of objects is recorded.  For
   REF open arrays, both the number of objects and their total
   size is recorded.  The actual bounds of the arrays are [0..n_types-1]. *)

END RTAllocCnts.

