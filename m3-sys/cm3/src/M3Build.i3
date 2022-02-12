(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3Build;

IMPORT Quake, Thread, QVTbl, QVSeq;

TYPE T <: Quake.Machine;

PROCEDURE NewMachine (): T;
(* Return a freshly allocated and initialized interpreter. *)

PROCEDURE SetUp (t: T;  pkg, to_pkg, build_dir: TEXT)  RAISES {Quake.Error};
(* Initialize internal state to start building "pkg" at "to_pkg" *)

PROCEDURE Run (t: T;  makefile: TEXT) RAISES {Quake.Error, Thread.Alerted};
(* Evaluate the specified makefile... *)

PROCEDURE RealClean ();
(* Remove the complete target directory for derived files *)

PROCEDURE SystemLibs (t: T) : QVTbl.T; (* Available system libraries *)
PROCEDURE SystemLibOrder (t: T) : QVSeq.T; (* link order for the system libraries *)

VAR
  done := FALSE;
  noM3ShipResolution := FALSE;
  groupWritable := FALSE;
END M3Build.

