INTERFACE M3CStdTypes;

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

(* Return (shared) instances of standard predefined TYPE_SPECs. 
This gets the node which is associated with the defining occurrence
of the type.  It is read-only: do not use this package for creating
new (syntactic) instances. *)

IMPORT  M3AST_AS, M3AST_SM;


PROCEDURE Integer(): M3AST_AS.Integer_type RAISES {};

PROCEDURE Real(): M3AST_AS.Real_type RAISES {};

PROCEDURE LongReal(): M3AST_AS.LongReal_type RAISES {};

PROCEDURE Extended(): M3AST_AS.Extended_type RAISES {};

PROCEDURE Null(): M3AST_AS.Null_type RAISES {};

PROCEDURE Any(): M3AST_SM.Any_type RAISES {};

PROCEDURE Type(): M3AST_SM.Type_type RAISES {};

PROCEDURE Void(): M3AST_SM.Void_type RAISES {};

PROCEDURE RefAny(): M3AST_AS.RefAny_type RAISES {};

PROCEDURE Address(): M3AST_AS.Address_type RAISES {};

PROCEDURE Root(): M3AST_AS.Root_type RAISES {};

PROCEDURE Untraced_Root(): M3AST_AS.Root_type RAISES {};

(* These are not actually primitive, so must be registered *)

PROCEDURE Char(): M3AST_AS.TYPE_SPEC RAISES {};

PROCEDURE Text(): M3AST_AS.TYPE_SPEC RAISES {};

PROCEDURE Boolean(): M3AST_AS.TYPE_SPEC RAISES {};

PROCEDURE Cardinal(): M3AST_AS.TYPE_SPEC RAISES {};

PROCEDURE Mutex(): M3AST_AS.TYPE_SPEC RAISES {};

PROCEDURE RegisterChar(t: M3AST_AS.TYPE_SPEC) RAISES {};
PROCEDURE RegisterText(t: M3AST_AS.TYPE_SPEC) RAISES {};
PROCEDURE RegisterBoolean(t: M3AST_AS.TYPE_SPEC) RAISES {};
PROCEDURE RegisterCardinal(t: M3AST_AS.TYPE_SPEC) RAISES {};
PROCEDURE RegisterMutex(ts: M3AST_AS.TYPE_SPEC) RAISES {};

END M3CStdTypes.
