INTERFACE M3ASTPickle;

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

IMPORT Rd, Wr, Thread, Pickle, M3AST_AS, M3AST_FE, M3Context;

PROCEDURE Read(
    context: M3Context.T; 
    rd: Rd.T;
    p: ImportedUnitProc;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit
    ) RAISES {Rd.Failure, Thread.Alerted, Pickle.Error};
(* Unpickle an AST from "rd", using "p" to locate units that are referenced
   by the pickle. *)

TYPE 
  ImportedUnitProc = PROCEDURE(
      unitName: TEXT;
      unitType: M3AST_FE.Unit_type;
      unitUid: M3AST_FE.Unit_uid;
      context: M3Context.T;
      VAR (*out*) cu: M3AST_AS.Compilation_Unit
      ): BOOLEAN RAISES ANY;
(* A call to this procedure is a request for the given unit to be found.
   The "M3Context.T" argument will be the same as that passed in to "Read".
   The "M3CUnit.Uid" argument must match the found unit.
   A "FALSE" result means that the unit could not be found or did not match.
*)

PROCEDURE Write(cu: M3AST_AS.Compilation_Unit; wr: Wr.T) 
    RAISES {Wr.Failure, Thread.Alerted, Pickle.Error};
(* Pickle "cu" to "wr", including the structure to allow unpickling
   later. *)

END M3ASTPickle.
