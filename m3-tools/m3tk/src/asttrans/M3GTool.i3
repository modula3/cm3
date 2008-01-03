INTERFACE M3GTool;

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

IMPORT Wr, OSError;
IMPORT M3Args;
IMPORT M3AST_AS;
IMPORT M3Context;

PROCEDURE Run(c: M3Context.T): INTEGER RAISES {};
(* Run the generic tool on the ASTs in 'c'. All the generic instantations
marked as primary (M3Conventions.PrimarySource IN cu.fe_status), will be
transformed and written out to the standard output.
Errors return <0  *)

PROCEDURE RunWithAST(
    c: M3Context.T;
    cu: M3AST_AS.Compilation_Unit;
    s: Wr.T)
    RAISES {Wr.Failure, OSError.E};
(* As 'Run', but on a specific AST (in 'c'), sending output to 's'. *)

PROCEDURE Get(): M3Args.T RAISES {};
(* return the tool handle *)

CONST
  SubstituteAP_Arg = "SubstituteActualParameters"; (* /f *)
  (* This parameter controls whether the generic actual parameters
  (interfaces) are substituted directly in the body of the transformed
  unit. If this arg is NOT set, they are not substituted and an 
  "IMPORT Actuals AS Formals" statement will be generated. *)

END M3GTool.
