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

(*------------------------------------------ linker initialized variables ---*)

(* these variables are read and written directly by compiler or linker
   generated code.  Changing their names, types or values is very dangerous. *)

VAR bottom_of_stack : ADDRESS;
VAR top_of_stack    : ADDRESS;
(* the limits of the currently running thread's stack.
   The stack grows from 'bottom' to 'top'. *)    

(*----------------------------------------------------------------- RAISE ---*)

PROCEDURE Raise (exception: ADDRESS;  arg: ADDRESS) RAISES ANY;
(* called by the compiler to raise 'exception(arg)'. *)

PROCEDURE ResumeRaise (info: ADDRESS) RAISES ANY;
(* called by the compiler to resume the raising of 'exception(arg)'. *)

PROCEDURE PushEFrame (frame: ADDRESS);
(* called by the compiler to push an exception frame. *)

PROCEDURE PopEFrame (frame: ADDRESS);
(* called by the compiler to pop an exception frame. *)

(*------------------------------------------------------------ MUTEX/LOCK ---*)

PROCEDURE LockMutex   (m: MUTEX);
PROCEDURE UnlockMutex (m: MUTEX);

(*----------------------------------------------- builtin TEXT operations ---*)

PROCEDURE Concat (a, b: TEXT): TEXT;

(*------------------------------------------------------------- allocator ---*)
(* The parameters are declared as ADDRESSs to avoid sucking in RT0
   in every compilation.  Besides, the compiler calls these procedures
   and is trusted to pass the right values. *)

TYPE ArrayShape = ARRAY OF INTEGER;

PROCEDURE Allocate (t: ADDRESS(*RT0.TypeDefn*)): REFANY;

PROCEDURE AllocateOpenArray (t: ADDRESS(*RT0.TypeDefn*);
                               READONLY sizes: ArrayShape): REFANY;

PROCEDURE AllocateUntracedObj (t: ADDRESS(*RT0.TypeDefn*)): UNTRACED ROOT;

PROCEDURE AllocateUntracedRef (t: ADDRESS(*RT0.TypeDefn*)): ADDRESS;

PROCEDURE AllocateUntracedOpenArray (t: ADDRESS(*RT0.TypeDefn*);
                                     READONLY sizes: ArrayShape): ADDRESS;

PROCEDURE DisposeUntracedRef (VAR a: ADDRESS);

PROCEDURE DisposeUntracedObj (VAR a: UNTRACED ROOT);

(*-------------------------------------------------------- runtime errors ---*)

PROCEDURE ReportFault (module: ADDRESS(*RT0.ModulePtr*);  info: INTEGER);
(* report the runtime fault in the specified module.  "info" encodes
   the source line number and fault code [info = line*16 + code].
   Where the fault codes are:
     0 - assertion failure
     1 - value out of range
     2 - subscript out of range
     3 - incompatible array shape
     4 - attempt to dereference NIL
     5 - NARROW failure
     6 - missing RETURN in function
     7 - missing CASE arm
     8 - missing TYPECASE arm
     9 - stack overflow
*)

(*----------------------------------------------------- some useful types ---*)
(* These types are declared here so that anonymous instances of them
   elsewhere in the libraries will have a name and so that redundant
   copies of their typecells are created everywhere. *)

TYPE
  CharBuffer = REF ARRAY OF CHAR;
  IntBuffer  = REF ARRAY OF INTEGER;
  RefInt     = REF INTEGER;
  RefChar    = REF CHAR;
  PtrInt     = UNTRACED REF INTEGER;
  PtrChar    = UNTRACED REF CHAR;

END RTHooks.




