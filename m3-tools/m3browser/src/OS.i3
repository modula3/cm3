(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OS.i3                                                 *)
(* Last modified on Thu Dec  8 08:45:12 PST 1994 by kalsow     *)
(*      modified on Tue Nov 12 14:05:29 PST 1991 by muller     *)


(* This interface defines the OS-specific functions needed by
   the m3browser. *)

INTERFACE OS;

IMPORT Time;

CONST NO_TIME = 0.0d0;

PROCEDURE ModTime (file: TEXT): Time.T;

END OS.
