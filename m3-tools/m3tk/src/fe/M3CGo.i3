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

INTERFACE M3CGo;

IMPORT Rd;
IMPORT M3AST_AS;
IMPORT M3CUnit, M3Context, M3Conventions;

(* This interface supports the compilation of a single unit, which is
already open on a given stream.  A callback mechanism is included to
cope with the need to compile imported/exported interfaces.
*)

TYPE ImportedUnitProc = PROCEDURE(
    name: TEXT;
    unitType: M3CUnit.Type;
    context: M3Context.T;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit
    ): BOOLEAN RAISES {};
(* A call to this procedure is a request for the given unit to be compiled.
The context argument will be the same as that passed in to CompileUnit.
A "FALSE" result means that the unit could not be found.
*)

PROCEDURE CompileUnit(
    cu: M3AST_AS.Compilation_Unit;
    context: M3Context.T; 
    stream: Rd.T;
    p: ImportedUnitProc;
    VAR (*inout*) phases: M3CUnit.Status;
    compTime: M3Conventions.CompTime;
    headerOnly := FALSE
    ) RAISES {};
(* Compile the unit whose tree root is "cu", and is open on "stream",
and which is a member of "context".  "p" will be called to resolve
imported units.  If "compTime # NIL" fill in the parse and semantic
analysis times. "phases" controls exactly which phases are run - the
usual value is AllPhases, but it can be convenient to restrict to just
parsing, for example.  If you ask for a phase which depends on a
previous one that has not been done, the error state for that phase is
set and the call returns.  Unless parsing is enabled, "stream" is
ignored completely.  "headerOnly" causes parsing to abort after
parsing the exports and import clauses, which is to support fast
dependency analysis.  *)

CONST AllPhases = M3CUnit.AllPhases;

PROCEDURE Current(): M3AST_AS.Compilation_Unit RAISES {};
(* This procedure returns the compilation unit which is currently
being compiled. I.e. the "cu" that was passed in the CompileUnit. *)

(* Compiler Extension.

   A client can register extra passes to be applied after the
   standard passes. The "extend" method for each registered
   extension will be called with the values of "context" and "phases", 
   as passed in to "CompileUnit". The value of "cu" will also
   be that passed in, unless the unit is a generic instantation,
   in which case the value of "cu.as_root.sm_ins_comp_unit" is
   passed instead. The client must explicitly check that a given
   phase has actually occured (by looking at "cu.fe_status").
   The extension code can report errors with "M3Error", and these
   will be displayed along with those from other passes, after all
   the extensions have completed. The extensions will be applied
   in the order they that they were registered. *)
   
TYPE
  Extension <: Extension_public;
  Extension_public = OBJECT
  METHODS
    extend(
      context: M3Context.T;
      cu: M3AST_AS.Compilation_Unit;
      VAR (*inout*) phases: M3CUnit.Status;
      ) RAISES {};
  END;

PROCEDURE AddExtension(e: Extension) RAISES {};

PROCEDURE RemoveExtension(e: Extension) RAISES {};

END M3CGo.


