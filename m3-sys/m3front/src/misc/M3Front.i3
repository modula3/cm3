(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Front.i3                                            *)
(* Last modified on Thu Apr 11 1996 by kalsow@cmass.com        *)

INTERFACE M3Front;

(* This module defines the front-end interface of the Modula-3
   compiler.  The compiler is a monitor -- only one compilation
   can be active at a time.
*)

IMPORT M3ID, M3Compiler;

PROCEDURE ParseImports (READONLY input : SourceFile;
                                 env   : Environment): IDList;
(* Returns the names of the interfaces directly imported by 'input'. *)

PROCEDURE Compile (READONLY input    : SourceFile;
                            env      : Environment;
                   READONLY options  : ARRAY OF TEXT): BOOLEAN;

(* Reads and compiles a Modula-3 unit from "input".  Evironmental queries
   and reports are made through "env".  The listing and diagnostic options
   of the compiler are set by "options".  Returns "TRUE" iff the compilation
   succeeded with no errors.  It is the caller's responsibility to
   initialize the Target interface prior to calling Compile. *)

TYPE
  IDList      = REF RECORD interface: M3ID.T;  next: IDList  END;
  SourceFile  = M3Compiler.SourceFile;
  Environment = M3Compiler.Environment;

END M3Front.

(* The recognized options are:

     -v             verbose mode
     -g             generate debugging information
     -S             don't generate version stamps
     -wX            print warnings at level "X" and above
     -w             don't print warnings  == -w99
     -Z             generate line-based profiling.
     -En            die on the "n-th" error message.
     -NoChecks      disable all runtime checks
     -NoAsserts     disable <*ASSERT*> checks
     -NoNarrowChk   disable narrow checks
     -NoRangeChk    disable subscript and range checks
     -NoReturnChk   disable checks for functions that fail to return values
     -NoCaseChk     disable checks for unhandled CASE selectors
     -NoTypecaseChk disable checks for unhandled TYPECASE selectors
     -NoNilChk      disable explicit NIL checks
     -NoRaisesChk   disable checks for unhandled exceptions
     -InitFloats    initialize all floating point values to zero
     -vsdebug       generate a trace of the fingerprint computations
     -builtins      "compile" and emit the builtin symbols
     -times         print the elapsed time profile
     -load_map      generate the load map comment in the output
     -No_load_map   don't generate the load map comment in the output
     -IncGC         generate software read barrier for incremental GC
     -GenGC         generate software write barrier for generational GC
     -widechar16    WIDECHAR has 16-bit range
     -widecharuni   WIDECHAR has Unicode range
*)
 

