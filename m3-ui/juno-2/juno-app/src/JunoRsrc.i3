(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Feb 16 14:50:22 PST 1994 by heydon                   *)

INTERFACE JunoRsrc;

(* Defines a path for reading Juno resource files. The path consists of the
   pathname named by the "JunoPATH" environment variable, followed by the
   Juno resource bundle. *)

IMPORT Rsrc;

VAR (* CONST *)
  Path: Rsrc.Path;

END JunoRsrc.
