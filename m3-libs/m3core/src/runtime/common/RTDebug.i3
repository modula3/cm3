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

(* These functions are like Windows functions of the same name. *)
<*EXTERNAL RTDebug__IsDebuggerPresent*>PROCEDURE IsDebuggerPresent():BOOLEAN;
<*EXTERNAL RTDebug__DebugBreak*>PROCEDURE DebugBreak();

END RTDebug.
