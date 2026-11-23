(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE RTDebug;

IMPORT RT0;

PROCEDURE RegisterHandler (p: Handler);
(* Registers the procedures "p" to be called when <*DEBUG*> pragmas are
   executed.   The default handler prints its arguments. *)

TYPE
  Handler = PROCEDURE (m: RT0.ModulePtr; line: INTEGER;
                       READONLY msg: ARRAY OF TEXT) RAISES ANY;

PROCEDURE Init();

(* Formatting for debuggers.  A debugger stopped in a Modula-3 program
   calls these (as "RTDebug__TypeName" and "RTDebug__Format") to learn what
   a traced reference points to.  They return NUL-terminated C strings in
   static storage, valid until the next call; they do not allocate, take
   no locks, and accept any address, returning NIL for one that is not a
   reference they can recognise: a traced reference or an untraced object.
   A plain UNTRACED REF's referent has no header to identify it; a debugger
   shows it through its static type. *)

PROCEDURE TypeName (r: ADDRESS): ADDRESS;
(* The name of the dynamic type of "r^", as its typecell gives it. *)

PROCEDURE Format (r: ADDRESS): ADDRESS;
(* A one-line description of "r^": a TEXT's contents, an open array's
   type and shape, or otherwise its type, typecode and size. *)

(* These functions are like Windows functions of the same name. *)
<*EXTERNAL RTDebug__IsDebuggerPresent*>PROCEDURE IsDebuggerPresent():BOOLEAN;
<*EXTERNAL RTDebug__DebugBreak*>PROCEDURE DebugBreak();

END RTDebug.
