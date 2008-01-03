INTERFACE M3CExpsMisc;

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
IMPORT M3Error;


PROCEDURE IsDesignator(
    exp: M3AST_AS.EXP;
    VAR writable: BOOLEAN)
    : BOOLEAN
    RAISES {};
(* is the given expression a designator? 'writable' is only set if 'exp' is a
designator, and indicates whether the designator is writable. If there is an
error (e.g. unset type fields, invalid expression - due to error in program
source) the result will usually be optimistic i.e. both result and 'writable'
TRUE *)

TYPE
  Class = {Normal, Type, Interface, Method, Exception};
  ClassSet = SET OF Class;

CONST
  NormalOnly = ClassSet{Class.Normal};
  AnyClass = ClassSet{FIRST(Class)..LAST(Class)};

PROCEDURE Classify(exp: M3AST_AS.EXP): Class RAISES {};
(* because types, modules, methods and exceptions have names they can appear in
expressions (though it is almost always an error). 'Classify' can be used to
spot such such bogus expressions. Usually optimistic (returns 'Normal') in the
case of errors in 'exp' *)

PROCEDURE IsId(
    exp: M3AST_AS.EXP;
    VAR defId: M3AST_AS.DEF_ID)
    : BOOLEAN
    RAISES {};
(* If 'exp' is an 'Exp_used_id' or a qualified id by an interface
masquerading a a selection 'IsId' gets the def id corresponding to this id.
Providing the def id is not unset returns TRUE and sets up 'defId'. Returns
FALSE and leaves 'defId' unchanged in all other cases *)

PROCEDURE WrongClass(en: M3Error.ERROR_NODE; class: Class) RAISES {};
(* if an expression is found to be of an unexpected class this procedure can
be used write out a "wrong class" message. 'class' is the unexpected class,
e.g if an expression is found to be a type when it should not be 'class' would
be 'Class.Type' *)

END M3CExpsMisc.
