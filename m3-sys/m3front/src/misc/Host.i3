(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Host.i3                                               *)
(* Last modified on Tue Jun 20 15:21:32 PDT 1995 by kalsow     *)
(* Last modified on Thu Jun 15 11:49:31 PDT 1995 by ericv      *)
(*      modified on Sat May 12 07:05:52 1990 by muller         *)

INTERFACE Host;

IMPORT M3ID, File, M3Compiler;

VAR (* parameters to the top-level compile *)
  filename      : TEXT := NIL;
  source        : File.T;
  env           : M3Compiler.Environment;

VAR (* misc. options *)
  verbose       : BOOLEAN := FALSE; (* => diagnostic listing of phases *)
  errorDie      : INTEGER := -1;    (* N>=0 => abort on the Nth error *)
  warnings      : INTEGER := 2;     (* => ignore levels less than this value *)
  coverage      : BOOLEAN := FALSE; (* => generate coverage counts *)
  versionStamps : BOOLEAN := TRUE;  (* do emit version stamps *)
  emitBuiltins  : BOOLEAN := FALSE; (* don't compile, emit the builtins *)
  init_floats   : BOOLEAN := FALSE; (* initialize all floats to zero *)
  vs_debug      : BOOLEAN := FALSE; (* print version stamp debugging *)
  load_map      : BOOLEAN := TRUE;  (* print unit's load map as a comment *)
  stack_walker  : BOOLEAN := TRUE;  (* use the Target specified stack walker *)

  nested_calls : BOOLEAN := FALSE;
  (* can calls to procedures be nested? *)

  nested_procs_first : BOOLEAN := TRUE;
  (* should nested procedures be generated before or after their parent *)

  inline_nested_procs : BOOLEAN := TRUE;
  (* should the code for nested procedures be generated inline *)

  direct_struct_assign : BOOLEAN := TRUE;
  (* avoid unnecessary copying of records and arrays (where possible)
     by passing the final destination to constructors and large-result
     procedures *)

  clean_stores : BOOLEAN := FALSE;
  (* must the stack be empty after every store *)

  clean_jumps : BOOLEAN := TRUE;
  (* must the stack be empty before every jump or call *)

VAR (* runtime checks *)
  doNarrowChk : BOOLEAN := TRUE;
  doRangeChk  : BOOLEAN := TRUE;
  doReturnChk : BOOLEAN := TRUE;
  doCaseChk   : BOOLEAN := TRUE;
  doTCaseChk  : BOOLEAN := TRUE;
  doAsserts   : BOOLEAN := TRUE;
  doNilChk    : BOOLEAN := TRUE;
  doRaisesChk : BOOLEAN := TRUE;
  doProcChk   : BOOLEAN := FALSE;
  doDebugs    : BOOLEAN := TRUE;

VAR (* GC checks *)
  doIncGC := TRUE;
  doGenGC := TRUE;

VAR
  new_adr      : BOOLEAN := FALSE;  (* TRUE =>  "ADR (t: T): UNTRACED REF T" *)
  report_stats : BOOLEAN := FALSE;

PROCEDURE Initialize (READONLY options: ARRAY OF TEXT): BOOLEAN;

PROCEDURE OpenUnit (name: M3ID.T; interface, generic: BOOLEAN;
                      VAR(*OUT*) filename: TEXT): File.T;

PROCEDURE CloseFile (rd: File.T);

PROCEDURE FileTail (path: TEXT): TEXT;
 (* returns the 'tail' of 'path' -- after any slashes or even spaces *)

END Host.
