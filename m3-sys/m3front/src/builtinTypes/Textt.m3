(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Textt.m3                                              *)
(* Last Modified On Mon Feb 22 16:08:04 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:26:13 1989 By muller         *)

MODULE Textt;

IMPORT ObjectRef, Tipe;

PROCEDURE Initialize () =
  BEGIN
    T := Tipe.DefineOpaque ("TEXT", ObjectRef.T);
  END Initialize;

BEGIN
END Textt.
