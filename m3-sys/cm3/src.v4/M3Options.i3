(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

(* "M3Options" defines various global flags and settings *)

INTERFACE M3Options;

VAR
  exit_code  : CARDINAL := 0;
  heap_stats : BOOLEAN  := FALSE;
  major_mode : Mode     := Mode.Build;

TYPE
  Mode = { Build, Clean, Ship, Find };

END M3Options.

