INTERFACE M3CScope;

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

(* This module implements scope checking. It assumes that it is to be used
by a single threaded compiler working its way sequentially through the module
being compiled.

  The module provides several routines for handling the different constructs
which introduce a new scope. When entering a new scope the compiler calls the
appropriate routine in 'Enter' mode, causing 'M3CScope' to note the names
introduced in that scope. On leaving the scope the same routine is called in
'Exit' mode, causing 'M3CScope' to forget the names introduced in that scope.
Calls can be nested but each call in 'Enter' mode must be matched by a similar
call in 'Exit' mode.

  'M3CScope' provides the procedure 'Lookup', which can be called to find out
the 'DEF_ID' currently associated with any identifer. *)

IMPORT M3AST_AS;


PROCEDURE Standard(standard: M3AST_AS.Compilation_Unit) RAISES {};
(* Must be called before any other procedure in the module in order to add
the standard identifiers to the scope structures *)


TYPE
  Change = {Enter, Exit};

PROCEDURE CompilationUnit(
    cu: M3AST_AS.Compilation_Unit;
    change: Change)
    RAISES {};
(* Called at the beginning and end of scope checking for the given module. In
enter mode:
i) If the unit is a module notes all identifiers introduced into the scope due
 to exported interfaces.
ii) Notes all identifiers introduced into the scope due to import statements.
iii) Notes all the identifiers introduced into the scope due to top level
 declarations.
Error messages are generated if the same identifier is introduced into the
scope twice with different bindings, or if an identifier mentioned in a FROM ..
IMPORT clause cannot be resolved.
  In exit mode this procedure forgets all identifiers introduced into the scope
by the enter mode call *)

PROCEDURE Procedure(proc: M3AST_AS.Proc_decl; change: Change) RAISES {};
(* Called at the beginning and end of scope checking for the given procedure.
  In enter mode notes all the formal parameter identifiers and all the
identifiers declared in the procedure body block. Error messages are generated
if any of these names clash.
  In exit mode this procedure forgets all identifiers introduced into the scope
by the enter mode call *)

PROCEDURE Method(meth: M3AST_AS.Method; change: Change) RAISES {};
(* Called at the beginning and end of scope checking for the given procedure.
  In enter mode notes all the formal parameter identifiers and all the
identifiers declared in the procedure body block. Error messages are generated
if any of these names clash.
  In exit mode this procedure forgets all identifiers introduced into the scope
by the enter mode call *)

PROCEDURE Block(block: M3AST_AS.Block; change: Change) RAISES {};
(* Called at the beginning and end of scope checking for the given block.
  In enter mode notes all the identifiers declared in the block. Error messages
are generated if any of these names clash.
  In exit mode this procedure forgets all identifiers introduced into the scope
by the enter mode call.
  This procedure has no effect if called on a block which is the body of a
procedure or compilation unit (because such blocks will have been taken care of
by the corresponding 'CompilationUnit' or 'Procedure' call) *)

PROCEDURE DefId(defId: M3AST_AS.DEF_ID; change: Change) RAISES {};
(* Called at the beginning and end of a scope which introduces just one new
identifier (e.g. a FOR statement). In enter mode notes the identifier, in exit
mode forgets it *)


PROCEDURE Lookup(usedId: M3AST_AS.USED_ID) RAISES {};
(* Looks up 'usedId' in the current scope. If 'usedId' is bound to some item
then 'usedId' is bound to the appropriate 'DEF_ID', otherwise an error message
is given and 'usedId' is not changed (presumably leaving it unset).
  Note that multiple failed lookups of the same identifier may not cause
multiple error messages. The implementation reserves the right to note that a
certain 'identifier' is not declared and may issue just one error message, for
the first failed lookup *)

END M3CScope.
