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

INTERFACE M3CFETool;

IMPORT M3Args;
IMPORT M3Context, M3CUnit;

PROCEDURE CompileInContext(
    VAR (*inout*) context: M3Context.T;
    phases := M3CUnit.AllPhases;
    headerOnly := FALSE;
    setPrimarySource := TRUE;
    ): INTEGER RAISES {};
(* This procedure invokes the compiler tool to compile the interfaces
and modules defined by the "interfaces" and "modules" entries in the
arg table.  If 'context = NIL', a new context is created and
is assigned as result to 'context', else the calls reuse 'context'. 
'phases' controls exactly which  phases are run - the usual value is 
AllPhases, but it can be convenient  to restrict to just parsing, for example.
Any errors cause a result < 0.

'headerOnly' causes parsing to abort after parsing the exports and 
import clauses, which is to support fast dependency analysis.
'setPrimarySource' causes M3Conventions.PrimarySource to be set in
the fe_status field of all the modules and interfaces that were
explicitly compiled (i.e. not just compiled because they were imported).
*)

PROCEDURE GetTool(): M3Args.T RAISES {};
(* Return the tool handle *)

(* Keywords understood by this tool; see M3Args for /x info *)

CONST 
  PathNames_Arg = "PathNames";     (* /l/p *)
  Modules_Arg = "Modules";         (* /l *)
  Interfaces_Arg = "Interfaces";   (* /l *)
  PrintUnits_Arg = "PrintUnits";   (* /f *)
  Timings_Arg = "CpuTime";      (* /f *)


END M3CFETool.
