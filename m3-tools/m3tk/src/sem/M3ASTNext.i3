INTERFACE M3ASTNext;

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

IMPORT M3AST_LX, M3AST_AS, M3AST_SM;
IMPORT SeqM3AST_AS_Var_decl, SeqM3AST_AS_Fields, SeqM3AST_AS_Formal_param,
    SeqM3AST_AS_Case, SeqM3AST_AS_Handler, SeqM3AST_AS_Tcase,
    SeqM3AST_AS_IMPORTED;

(*-------------------------------*) 
(* FLATTENING COMPLEX ITERATIONS *)
(*-------------------------------*)

(*  A series of iterators for flattening nasty M3 AST iterations e.g. the 'Var'
iterator just iterates all the 'Var_id's in a 'seqVar_decl', so the user
doesn't have to iterate the 'Var_decl's then the 'Var_id's for each decl.
  They are all used in a similar manner; here is an example using the 'Var'
iterator.

    VAR
      iter := M3ASTNext.IterVar(seqVar_decl);
    BEGIN
      WHILE M3ASTNext.Var(iter, varId) DO
        ..your code..
      END;
*)

TYPE
  IterVar <: REFANY;

PROCEDURE NewIterVar(seqVar_decl: SeqM3AST_AS_Var_decl.T): IterVar RAISES {};

PROCEDURE Var(
    VAR (* INOUT *) iter: IterVar;
    VAR (* OUT *) var_id: M3AST_AS.Var_id)
    : BOOLEAN
    RAISES {};


TYPE
  IterField <: REFANY;

PROCEDURE NewIterField(seqFields: SeqM3AST_AS_Fields.T): IterField RAISES {};

PROCEDURE Field(
    VAR (* INOUT *) iter: IterField;
    VAR (* OUT *) field_id: M3AST_AS.Field_id)
    : BOOLEAN
    RAISES {};


TYPE
  IterFormal <: REFANY;

PROCEDURE NewIterFormal(
    seqFormal_param: SeqM3AST_AS_Formal_param.T)
    : IterFormal
    RAISES {};

PROCEDURE Formal(
    VAR (* INOUT *) iter: IterFormal;
    VAR (* OUT *) formal_param: M3AST_AS.Formal_param;
    VAR (* OUT *) formal_id: M3AST_AS.FORMAL_ID)
    : BOOLEAN
    RAISES {};


TYPE
  IterCaseLabel <: REFANY;

PROCEDURE NewIterCaseLabel(
    seqCase: SeqM3AST_AS_Case.T)
    : IterCaseLabel
    RAISES {};

PROCEDURE CaseLabel(
    VAR (* INOUT *) iter: IterCaseLabel;
    VAR (* OUT *) m3_case: M3AST_AS.Case;
    VAR (* OUT *) label: M3AST_AS.RANGE_EXP)
    : BOOLEAN
    RAISES {};


TYPE
  IterHandlerLabel <: REFANY;

PROCEDURE NewIterHandlerLabel(
    seqHandler: SeqM3AST_AS_Handler.T)
    : IterHandlerLabel
    RAISES {};

PROCEDURE HandlerLabel(
    VAR (* INOUT *) iter: IterHandlerLabel;
    VAR (* OUT *) handler: M3AST_AS.Handler;
    VAR (* OUT *) label: M3AST_AS.Qual_used_id)
    : BOOLEAN
    RAISES {};


TYPE
  IterTypeCaseLabel <: REFANY;

PROCEDURE NewIterTypeCaseLabel(
    seqTcase: SeqM3AST_AS_Tcase.T)
    : IterTypeCaseLabel
    RAISES {};

PROCEDURE TypeCaseLabel(
    VAR (* INOUT *) iter: IterTypeCaseLabel;
    VAR (* OUT *) tcase: M3AST_AS.Tcase;
    VAR (* OUT *) label: M3AST_AS.M3TYPE)
    : BOOLEAN
    RAISES {};


TYPE
  IterImportedId <: REFANY;

PROCEDURE NewIterImportedId(i: SeqM3AST_AS_IMPORTED.T): IterImportedId RAISES {};

PROCEDURE ImportedId(
    VAR (*inout*) iter: IterImportedId;
    VAR (*out*) used_intf_id: M3AST_AS.Used_interface_id)
    : BOOLEAN
    RAISES {};
(* Returns the sequence of imported interface ids in the order they
appear in the AST. *)


(*--------------*)
(* OBJECT TYPES *)
(*--------------*)

(* A series of iterators for object types. They provide ways of finding the
supertype and iterating fields and methods *)

PROCEDURE SimpleSuperType(
    type: M3AST_AS.Object_type;
    VAR (* OUT *) superType: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* If 'type' has no supertype (other than ones common to all object types)
returns FALSE and leaves 'superType' unchanged.
  Otherwise returns TRUE. If the supertype is unresolved (e.g. an undeclared
name) or recursive sets 'superType' to unset otherwise sets 'superType' to
the supertype, even if it is unsuitable i.e. not an object or opaque type.
  This routine assumes that names (in particular the name of the supertype of
'type', if any!) are resolved. *)

PROCEDURE SuperType(
    type: M3AST_AS.Object_type;
    VAR (* OUT *) superType: M3AST_AS.Object_type)
    : BOOLEAN
    RAISES {};
(* This routine is intended for use when iterating fields and methods. If
'type' has a supertype (other than the ones common to all object types e.g.
REFANY) this supertype can validly be either an opaque type which is revealed
to be an object type or a simple object type.
  If 'type' has an opaque supertype which is revealed to be an object type and
that object type is not the root object type (i.e. OBJANY) TRUE is returned and
'superType' is set to be the revealed object type.
  If 'type' has a supertype which is a simple object type TRUE is returned
and 'superType' is set to be that object type.
  Otherwise FALSE is returned and 'superType' is not altered.
  This routine assumes that names (in particular the name of the supertype of
'type', if any!) are resolved. It also uses 'M3CConcTypeSpec.CurrentReveal' to
find the current revelation for an opaque supertype *)

TYPE
  IterObjectField <: REFANY;

PROCEDURE NewIterObjectField(
    o: M3AST_AS.Object_type)
    : IterObjectField
    RAISES {};

PROCEDURE ObjectField(
    VAR (* INOUT *) iter: IterObjectField;
    VAR (* OUT *) field_id: M3AST_AS.Field_id)
    : BOOLEAN
    RAISES {};
(* Starts with the root super-type and goes through the current type iterating
each type's fields.  *)

TYPE
  IterObjectMethod <: REFANY;

PROCEDURE NewIterObjectMethod(
    o: M3AST_AS.Object_type)
    : IterObjectMethod
    RAISES {};

PROCEDURE ObjectMethod(
    VAR (* INOUT *) iter: IterObjectMethod;
    VAR (* OUT *) method: M3AST_AS.METHOD_OVERRIDE;
    VAR (* OUT *) overrides: BOOLEAN)
    : BOOLEAN
    RAISES {};
(* Starts with the root super-type and goes through the current type iterating
each the type's methods.  Overrides is true if the method only overrides a
super-type's method procedure.  This is a convenience for backward 
compatibility, since "overrides = ISTYPE(method, M3AST_AS.Override)" *)

TYPE
  IterFieldOrMethod <: REFANY;

PROCEDURE NewIterFieldOrMethod(
    o: M3AST_AS.Object_type)
    : IterFieldOrMethod
    RAISES {};

PROCEDURE FieldOrMethod(
    VAR iter: IterFieldOrMethod;
    VAR field: M3AST_AS.Field_id;
    VAR method: M3AST_AS.Method;
    VAR symrep: M3AST_LX.Symbol_rep)
    : BOOLEAN
    RAISES {};
(* Iterator for the fields and methods in an object. The iteration order is
fields then methods at the current level then, if the object type has any
supertypes, fields then methods of the supertype and so on.
  If 'FieldOrMethod' succeeds in finding another field or method it returns
TRUE and sets 'symrep' to be the name of the field or method. If a field was
found 'field' is set appropriately and 'method' is set to NIL otherwise
'method' is set and 'field' is set to NIL. The iteration skips any method
overrides.
  If there are no more fields or methods FALSE is returned.
  The iteration uses the 'SuperType' procedure; hence names must be resolved
and the current revelation of any opaque super type must be available.
  So a typical iteration might be:
    IDL.InitSEQIterator(iter);
    WHILE M3ASTNext.FieldOrMethod(o, iter, field, method, symrep) DO
      .. whatever ..
    END; (* while *);
*)


(*-------------*)
(* ARRAY TYPES *)
(*-------------*)

(* An iterator for multi dimensional arrays *)

PROCEDURE Array(
    array: M3AST_AS.Array_type;
    VAR (* OUT *) elementType: M3AST_SM.TYPE_SPEC_UNSET;
    VAR (* OUT *) openArray: BOOLEAN;
    VAR (* OUT *) indexType: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN (* elementtype is an array i.e. array is multi-dimensional *)
    RAISES {};
(* A bit of a mess, but useful.  Take an array and return it element and
   index types.  If the array is open, then there is no indexType.
   Return TRUE if the elements of the array are arrays.  This requires
   sm_norm_type to have been computed.

   Loop would look like:

   array := ...
   LOOP
     multiDim := M3ASTNext.Array(array, elementType, openArray, indexType);
     ...
     IF NOT multiDim THEN EXIT END;
   END;
*)

END M3ASTNext.
