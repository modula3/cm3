(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Carsten Weich                                    *)
(* Last modified on Fri Sep 23 15:24:54 PDT 1994 by weich      *)

(* This module declares the error exception used by the stable stub
   generator.  There is an exception "E" which is used to propagate
   error messages to the top level procedure in "Main.m3". "Warning()"
   and "Failure()" are used to print out warnings to the standard
   error stream. They do not halt the generation of stub code. Further
   there is a "Fatal()" procedure which halts the stub generator in
   case of an unexpected error.  *)

INTERFACE StablegenError;

EXCEPTION E(TEXT);

PROCEDURE Warning (msg: TEXT);
(* Write "msg" to "Stdio.stderr", preceeded by
   ''stablegen (warning):`` and followed by a newline *)

PROCEDURE Failure (msg: TEXT);
(* Write "msg" to "Stdio.stderr", preceeded by
   ''stablegen error:`` and followed by a newline *)

PROCEDURE Fatal (msg: TEXT);
(* Write "msg" to "Stdio.stderr", preceeded by ''stablegen:
   ***fatal error***`` and and halt the program
   ("Process.Exit()") *)

END StablegenError.
