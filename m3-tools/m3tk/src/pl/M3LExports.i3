INTERFACE M3LExports;

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

IMPORT M3AST_AS, M3Context;

TYPE
  Closure <: Closure_public;
  Closure_public = M3Context.Closure OBJECT
  METHODS
    report(
        cu: M3AST_AS.Compilation_Unit; 
        n: M3AST_AS.SRC_NODE
        ) RAISES {};
  END;

PROCEDURE Check(c: M3Context.T; cl: Closure) RAISES {};
(* Check that all procedures/opaque-types defined in the interfaces in 'c' 
have been implemented.  If not call 'cl.report' with the interface unit and 
the node not implemented. *)

END M3LExports.
