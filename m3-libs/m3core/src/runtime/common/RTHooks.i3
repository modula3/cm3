(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jul 25 16:36:20 PDT 1994 by kalsow     *)
(*      modified on Tue Sep  8 11:51:46 PDT 1992 by jdd        *)
(*      modified on Thu Mar 12 12:10:27 PST 1992 by muller     *)

(*
   RTHooks defines the procedures and variables that the compiler
   calls directly.  The parameters to most of these entry points
   are unchecked.  Passing bad parameters will cause disaster!
*)

UNSAFE INTERFACE RTHooks;

(*----------------------------------------------------------------- types ---*)

PROCEDURE CheckIsType (ref: REFANY;  type: ADDRESS(*RT0.TypeDefn*)): INTEGER;
(* If "ref" is a subtype of "type" return ORD(TRUE).  Otherwise,
   return ORD(FALSE). *)

PROCEDURE ScanTypecase (ref: REFANY;
                        x: ADDRESS(*ARRAY [0..] OF TypecaseCell*)): INTEGER;
(* Return the first "i" such that "ref" is a subtype of the type referenced
   to by "x[i]".  If "ref" is "NIL", return 0.  If "x[i].uid" is zero,
   return "i".  If "x[i].defn" is "NIL", resolve it to the type corresponding
   to "x[i].uid". *)

TYPE
  TypecaseCell = RECORD
    defn: ADDRESS; (* RT0.TypeDefn, resolved lazily *)
    uid : INTEGER; (* a type UID, or zero to terminate the list. *)
  END;

(*------------------------------------------------------------ exceptions ---*)

PROCEDURE Raise (ex     : ADDRESS; (*RT0.ExceptionPtr*)
                 arg    : ADDRESS; (*RT0.ExceptionArg*)
                 module : ADDRESS; (*RT0.ModulePtr*)
                 line   : INTEGER) RAISES ANY;
(* called by the compiler to raise 'ex(arg)'. *)

PROCEDURE ResumeRaise (a: ADDRESS (*VAR RT0.RaiseActivation*)) RAISES ANY;
(* called by the compiler to resume the raising of 'a.exception(a.arg)'. *)

PROCEDURE PushEFrame (frame: ADDRESS);
(* called by the compiler to push an exception frame. *)

PROCEDURE PopEFrame (frame: ADDRESS);
(* called by the compiler to pop an exception frame. *)

PROCEDURE LatchEHReg () : ADDRESS;

(*----------------------------------------------- builtin TEXT operations ---*)

PROCEDURE Concat (a, b: TEXT): TEXT;
(* Returns "a & b" .*)

PROCEDURE MultiCat (READONLY x: ARRAY OF TEXT): TEXT;
(* Returns "x[0] & x[1] & ... & x[LAST[x]]". *)

(* Methods for compiler generated literals *)
PROCEDURE TextLitInfo         (t: TextLiteral;  VAR i: TextInfo);
PROCEDURE TextLitGetChar      (t: TextLiteral;  i: CARDINAL): CHAR;
PROCEDURE TextLitGetWideChar  (t: TextLiteral;  i: CARDINAL): WIDECHAR;
PROCEDURE TextLitGetChars     (t: TextLiteral;
                               VAR a: ARRAY OF CHAR;  start: CARDINAL);
PROCEDURE TextLitGetWideChars (t: TextLiteral;
                               VAR a: ARRAY OF WIDECHAR; start: CARDINAL);
TYPE
  TextLiteral <: TEXT;
  TextInfo = RECORD
    start  : ADDRESS;  (* non-NIL => string is at [start .. start+length) *)
    length : CARDINAL; (* length of string in characters *)
    wide   : BOOLEAN;  (* => string contains WIDECHARs. *)
  END;

(*------------------------------------------------------------- allocator ---*)
(* The parameters are declared as ADDRESSs to avoid sucking in RT0
   in every compilation.  Besides, the compiler calls these procedures
   and is trusted to pass the right values. *)

TYPE ArrayShape = ARRAY OF INTEGER;

(* deprecated -- needed only with unbootstrapped compilers *)
PROCEDURE Allocate (t: ADDRESS(*RT0.TypeDefn*)): REFANY;

PROCEDURE AllocateTracedObj (t: ADDRESS(*RT0.TypeDefn*)): ROOT;

PROCEDURE AllocateTracedRef (t: ADDRESS(*RT0.TypeDefn*)): REFANY;

PROCEDURE AllocateOpenArray (t: ADDRESS(*RT0.TypeDefn*);
                               READONLY sizes: ArrayShape): REFANY;

PROCEDURE AllocateUntracedObj (t: ADDRESS(*RT0.TypeDefn*)): UNTRACED ROOT;

PROCEDURE AllocateUntracedRef (t: ADDRESS(*RT0.TypeDefn*)): ADDRESS;

PROCEDURE AllocateUntracedOpenArray (t: ADDRESS(*RT0.TypeDefn*);
                                     READONLY sizes: ArrayShape): ADDRESS;

PROCEDURE DisposeUntracedRef (VAR a: ADDRESS);

PROCEDURE DisposeUntracedObj (VAR a: UNTRACED ROOT);

PROCEDURE CheckLoadTracedRef (ref: REFANY);

PROCEDURE CheckStoreTraced (ref: REFANY);

(*-------------------------------------------------------- runtime errors ---*)

PROCEDURE ReportFault (module: ADDRESS(*RT0.ModulePtr*);  info: INTEGER)
  RAISES ANY;
(* report the runtime fault in the specified module.  "info" encodes
   the source line number and fault code [info = line*32 + ORD(RuntimeError.T)].
   32: see M3CG.RuntimeError, RuntimeError.T
*)

PROCEDURE AssertFailed (module: ADDRESS(*RT0.ModulePtr*);  line: INTEGER;
                        msg: TEXT) RAISES ANY;
(* Signal an assertion failure with the attached message. *)

(*------------------------------------------------------------- debugging ---*)

PROCEDURE DebugMsg (module: ADDRESS(*RT0.ModulePtr*);  line: INTEGER;
                     READONLY msg: ARRAY OF TEXT) RAISES ANY;
(* Print debugging information from the program *)

(*----------------------------------------------------- some useful types ---*)
(* These types are declared here so that anonymous instances of them
   elsewhere in the libraries will have a name and so that redundant
   copies of their typecells are created everywhere. *)

TYPE
  Null         = NULL;
  Address      = ADDRESS;
  Refany       = REFANY;
  Root         = ROOT;
  UntracedRoot = UNTRACED ROOT;
  Text         = TEXT;
  CharBuffer   = REF ARRAY OF CHAR;
  IntBuffer    = REF ARRAY OF INTEGER;
  RefInt       = REF INTEGER;
  RefChar      = REF CHAR;
  PtrInt       = UNTRACED REF INTEGER;
  PtrChar      = UNTRACED REF CHAR;

END RTHooks.
