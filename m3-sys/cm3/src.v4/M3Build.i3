(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

INTERFACE M3Build;

IMPORT Quake, Thread;

TYPE T <: Quake.Machine;

PROCEDURE NewMachine (): T;
(* Return a freshly allocated and initialized interpreter. *)

PROCEDURE SetUp (t: T;  pkg, to_pkg, build_dir: TEXT)  RAISES {Quake.Error};
(* Initialize internal state to start building "pkg" at "to_pkg" *)

PROCEDURE Run (t: T;  makefile: TEXT) RAISES {Quake.Error, Thread.Alerted};
(* Evaluate the specified makefile... *)

END M3Build.

