(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Mon Sep 26 14:17:22 PDT 1994 by kalsow   *)

(* An "MasmObjFile.T" is an object file represented as
   ASCII text that can be fed to the Microsoft assembler MASM. *)

INTERFACE MasmObjFile;

IMPORT M3ObjFile, Wr, Thread;

TYPE
  T <: M3ObjFile.T;

PROCEDURE New (): T;

PROCEDURE Dump (t: T; wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};

END MasmObjFile.
