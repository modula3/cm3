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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3Context;

(* This module defines a compilation context, in terms of a set of
compilation units. The set is keyed by the text 'name' which is passed
as argument to the 'Add' procedure. At most one "interface" and one
"module" AST may be associated with this name, and these may be
generic definitions, instantations or normal ASTs. The assumption
is that the unit name in the AST matches 'name' and that the AST actually 
corresponds to the indicated type. However this is not checked and in fact
the 'as_root' field may be NIL when the Add takes place. *)

IMPORT Property;
IMPORT M3AST_AS, M3CUnit, M3CId;

TYPE T <: Property.Set;  
(* A handle on a compilation context. It is handy to be able to
   associate values pertaining to context, which might otherwise
   have to be global variables, hence the definition as a subtype
   of "Property.Set". *)

(*****************************************************************************)
(*               Creation/Adding/Removing units                              *)
(*****************************************************************************)

EXCEPTION Duplicate;

PROCEDURE New(): T RAISES {};
(* create an empty context *)

PROCEDURE Add(
    t: T;
    name: TEXT;
    unitType: M3CUnit.Type;
    cu: M3AST_AS.Compilation_Unit) RAISES {Duplicate};
(* Add a new unit to the context. If 'unitType IN M3CUnit.Interfaces'
and there already exists another interface with the same 'name'
the Duplicate exception will be raised. Similarly for modules. *)

PROCEDURE Remove(t: T; name: TEXT; unitType: M3CUnit.Type) RAISES {};
(* If 'unitType' IN M3CUnit.Interfaces, remove the interface 'name' from 
this context, else remove the module 'name'. *)

(*****************************************************************************)
(*                            Finding units                                  *)
(*****************************************************************************)

PROCEDURE Find(
    t: T;
    name: TEXT;
    unitType: M3CUnit.Type;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit;
    ): BOOLEAN
    RAISES {};
(* If 'unitType IN M3CUnit.Interfaces', find the interface 'name', else
find the module. Returns true and sets 'cu' appropriately if successful, 
otherwise returns false and sets 'cu' to NIL. *)

PROCEDURE FindExact(
    t: T;
    name: TEXT;
    unitType: M3CUnit.Type;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit;
    ): BOOLEAN
    RAISES {};
(* This is like 'Find', except that the type of the interface or module
has to match exactly. So, if a generic definition was added under 'name',
the call will only succeed if 'unitType = M3CUnit.Type.Interface_gen_def'. *)

PROCEDURE FindFromId(
    t: T;
    name: M3CId.T;
    unitType: M3CUnit.Type;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit;
    ): BOOLEAN
    RAISES {};
(* As Find, but using a hash-id. *)

(*****************************************************************************)
(*                            Iteratation                                    *)
(*****************************************************************************)

TYPE
  Iter <: REFANY;

PROCEDURE NewIter(
    t: T; unitType: 
    M3CUnit.Type;
    findStandard := TRUE
    ): Iter RAISES {};
(* create an iterator on context 't'. 'unitType' is treated as an exact match 
so, for example, one can iterate generic interfaces only. 
The 'Standard' interface will only be included if 'findStandard = TRUE
AND unitType = M3CUnit.Type.Interface'. *)

PROCEDURE Next(
    iter: Iter;
    VAR (*out*) name: TEXT;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit;
       
    ): BOOLEAN
    RAISES {};
(*  The iterator should be used as follows:

  i := M3Context.NewIter(unitType);
  WHILE M3Context.Next(iter, name, cu) DO
    ... code using 'name', 'cu' ...
  END; (* while *)

To iterate over all kinds of interfaces and modules use the above within a 
FOR loop.
*)

TYPE
  Closure <: Closure_public;
  Closure_public = OBJECT
    context: T;
  METHODS
     callback(
         ut: M3CUnit.Type;
         name: TEXT;
         cu: M3AST_AS.Compilation_Unit) RAISES ANY;
     init(): Closure RAISES {};
  END;
  (* Create by NEW(Closure, callback := YourCallback).init() *)

EXCEPTION Aborted;

PROCEDURE Apply(t: T; cl: Closure; findStandard := TRUE) RAISES ANY;
(* Apply p to all units in 't'.  This is just really just a convience
to avoid driving the iterator directly. 'cl' may be subtyped by client
to hold state; 'cl.callback' is called for each unit in the context.
'cl.t' is set to 't' before any callbacks. 'findStandard' is TRUE
for backward compatibility; most clients will set it to FALSE (see
ApplyToSet below). *)

PROCEDURE ApplyToSet(
    t: T;
    cl: Closure;
    unitTypeSet := M3CUnit.AllTypes;
    findStandard := FALSE) RAISES ANY;
(* As 'Apply', but to a restricted set of unit types, e.g. excluding
  generic definitions. *)

PROCEDURE AbortApply()
    RAISES {Aborted};
(* The current iteration is aborted by raising the 'Aborted' exception, which
is caught by 'Apply', thus control returns to caller of that procedure. *)

(*****************************************************************************)
(*                            Miscellaneous                                  *)
(*****************************************************************************)

(* These procedures keep track of the special 'Standard' interface - the
interface that makes available the standard identifiers such as 'INTEGER' etc.
The 'Standard' interface is an implicit member of every context - it is a
global constant.  These procs operate without a context but once a context
exists, they have a simple rewrite given in the comment. *)

PROCEDURE SetStandard(cu: M3AST_AS.Compilation_Unit) RAISES {};
(*  Add(t, M3Conventions.Standard, M3CUnit.Type.Interface, cu); *)

PROCEDURE Standard(): M3AST_AS.Compilation_Unit RAISES {};
(*  Find(t, M3Conventions.Standard, M3CUnit.Type.Interface, cu, TRUE); *)

END M3Context.
