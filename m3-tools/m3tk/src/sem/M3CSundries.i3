INTERFACE M3CSundries;

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

IMPORT AST, ASTWalk;


TYPE
  Handle <: REFANY;

PROCEDURE Check(h: Handle; any: AST.NODE; v: ASTWalk.VisitMode) RAISES {};
(* performs sundry semantic checks on the node given by 'any'. This procedure
should be called by the tree walker. It assumes that all type specs and
expression values (if constant) have been set. The checks it performs are:
a) procedures in interfaces must have no body; procedures in modules are
   required to have a body.
b) proper procedure calls are not allowed in expressions and function
   procedure calls are not allowed as statements.
c) EXIT can only appear inside a loop
d) the sets labelling different arms of a case must be distinct (this check
   is farmed out to 'M3CDuplicate')
e) the exceptions labelling different arms of an except clause must be distinct
   (this check is farmed out to 'M3CDuplicate')
f) the members of an enumeration, the fields of a record, the fields and
   methods of an object type and the parameter names of a procedure type must
   have distinct names which do not clash with the standard identifiers (these
   checks are farmed out to 'M3CNameClash')
g) EXCEPTION declarations and REVEALs can only appear at the top level
h) For any occurences of 'T.m' (object type dot method) checks that type 'T'
   does have a method 'm'; just gives a warning if it does not.
*)

PROCEDURE NewHandle(
    inModule, isProperCall, inLoop, inProc: BOOLEAN)
    : Handle
    RAISES {};
(* 'Check' needs some state information, which is kept in a handle variable.
Before the tree walk is started 'NewHandle' should be called to create a
handle. The arguments are:
'inModule'     is the walk starting in a module (as opposed to an interface)
'isProperCall' is the walk starting at a 'Call' node which represents a proper
               procedure call?
'inLoop'       is the walk starting inside a loop?
'inProc'       is the walk starting inside a procedure?
*)

END M3CSundries.
