(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE WiredStandard;

IMPORT M3Context;

PROCEDURE Set(c: M3Context.T) RAISES {};
(* initialise 'c' with a hard-wired version of the standard interface. *)

END WiredStandard.
