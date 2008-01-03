INTERFACE M3CWordProcs;

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


TYPE
  T = {Plus, Times, Minus, Divide, Mod, LT, LE, GT, GE, And, Or, Xor,
       Not, Shift, RightShift, Rotate, RightRotate, Extract, Insert};

CONST
  MaxArgCount = 4;

PROCEDURE IsWordCall(call: M3AST_AS.Call; VAR t: T): BOOLEAN RAISES {};
(* Is the given call a call of a procedure in the 'Word' interface? If so
return TRUE and set 't' to indicate which procedure is being called. If not
return FALSE and leave 't' untouched. *)

PROCEDURE ArgCount(t: T): CARDINAL RAISES {};
(* Return the number of arguments required by the given call. *)

END M3CWordProcs.
