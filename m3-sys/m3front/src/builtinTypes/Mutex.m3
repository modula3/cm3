(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Mutex.m3                                              *)
(* Last Modified On Mon Feb 22 10:40:41 PST 1993 By kalsow     *)
(*      Modified On Thu Jul 27 17:26:29 1989 By muller         *)

MODULE Mutex;

IMPORT ObjectRef, Tipe;

PROCEDURE Initialize () =
  BEGIN
    T := Tipe.DefineOpaque ("MUTEX", ObjectRef.T);
  END Initialize;

BEGIN
END Mutex.
