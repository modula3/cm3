(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE Makefile;

IMPORT TextTextTbl;

PROCEDURE ScanCommandLine1 ();
(* Pre-scan the command line arguments for -help/-version.
*)

PROCEDURE ScanCommandLine2 () : TextTextTbl.T;
(* Pre-scan the command line arguments to determine the major mode
   we're operating in, and print any requested help or version information.
   Return a set of pre-defined values for the quake evaluation.
*)

PROCEDURE Build (src_dir: TEXT): TEXT;
(* Create and return the name of an m3makefile based on
   the command line arguments, the existing m3makefile,
   and the contents of the source directory. *)

END Makefile.

