(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Feb 20 09:55:03 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

INTERFACE QCompiler;

IMPORT Quake;

PROCEDURE CompileFile (path: TEXT): Quake.CodeStream
  RAISES {Quake.Error};

END QCompiler.
