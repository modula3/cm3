INTERFACE M3CGoList;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT M3AST_AS;
IMPORT M3CUnit, M3Context, M3Conventions;

PROCEDURE CompileUnitsInContext(
    VAR (*inout*) context: M3Context.T;
    READONLY interfaces, modules, pathNames: ARRAY OF TEXT;
    VAR (*inout*) phases: M3CUnit.Status;
    headerOnly := FALSE;
    setPrimarySource := TRUE;
    setCompTime: BOOLEAN := FALSE
    ) RAISES {};  
(* (Try to) compile all the units in the arrays "interfaces" and "modules", 
and "pathNames". Any imported/exported units will be looked for in 
"context" first, which is created if it is NIL.  If any 
notification callbacks have been registered, they will be called
just before a unit is compiled and just after; any error messages
will already have been generated. If a unit cannot be found
an error message will be generated, and the notifications are
not called. The value of "phases" controls exactly which 
phases are run - the usual value is" AllPhases", but it can be convenient 
to restrict to just parsing, for example.  "phases" is always updated
with any error flags that occur, which provides a cheap check
for a clean compilation.

"interfaces" and "modules" are expect to contain unit names, which are
searched for on the search path. "pathNames" are treated literally
and, if they have M3 source extensions, are opened and compiled.

"headerOnly" causes parsing to abort after parsing the exports and 
import clauses, which is to support fast dependency analysis.
"setPrimarySource" causes "M3Conventions.PrimarySource" to be set in
the "fe_status" field of all the modules and interfaces that were
explicitly compiled (i.e. those in "interfaces" + "modules").
"setCompTime = TRUE", causes an "M3Conventions.CompTime" record to be created 
for each unit compiled, which will be passed to any "notify" methods. *)


CONST AllPhases = M3CUnit.AllPhases; (* convenience re-export *)

PROCEDURE Current(): M3AST_AS.Compilation_Unit RAISES {};
(* This procedure returns the compilation unit which is currently
being compiled. *)

TYPE
  NotifyMode = {Before, After};
  Notification <: Notification_public;
  Notification_public = OBJECT
  METHODS
    notify(
      context: M3Context.T;
      nm: NotifyMode; 
      name: TEXT;
      ut: M3CUnit.Type;
      uf: M3CUnit.Form;
      cu: M3AST_AS.Compilation_Unit;
      compTime: M3Conventions.CompTime := NIL
      ) RAISES {};
  END;

PROCEDURE AddNotification(e: Notification) RAISES {};
(* Register "e.notify" to be called for each compiled unit resulting 
from a call of "M3CGoList.CompileInContext".  "e.notify"is called just 
before and just after the compilation ("cu = NIL" at before). If 
"setCompTime = TRUE", on the call to "CompileUnitsInContext", then
the "M3Conventions.CompTime" record created for this unit
is passed to "notify", else it gets NIL. The notifications are
called in the order that they were added. *)


PROCEDURE RemoveNotification(e: Notification) RAISES {};
(* Remove notification "e". *)

END M3CGoList.
