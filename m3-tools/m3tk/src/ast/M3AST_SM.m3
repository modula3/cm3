MODULE M3AST_SM;

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

(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(********************************************************************
IMPORT AST_Init
IMPORT M3AST, M3AST_AS;
IMPORT M3AST_SM_F;

PROCEDURE IsA_INIT_ID(n: M3AST.NODE;
    VAR(*out*) init_id: INIT_ID): BOOLEAN RAISES {} =
  BEGIN
    RETURN n.IsA_INIT_ID(init_id);
  END IsA_INIT_ID;

PROCEDURE IsA_CCV_ID(n: M3AST.NODE;
    VAR(*out*) ccv_id: CCV_ID): BOOLEAN RAISES {} =
  BEGIN
    RETURN n.IsA_CCV_ID(ccv_id);
  END IsA_CCV_ID;

PROCEDURE IsA_RECOBJ_ID(n: M3AST.NODE;
    VAR(*out*) recobj_id: RECOBJ_ID): BOOLEAN RAISES {} =
  BEGIN
    RETURN n.IsA_RECOBJ_ID(recobj_id)
  END IsA_RECOBJ_ID;

PROCEDURE IsA_REDEF_ID(n: M3AST.NODE;
    VAR(*out*) redef_id: REDEF_ID): BOOLEAN RAISES {} =
  BEGIN
    RETURN n.IsA_REDEF_ID(redef_id)
  END IsA_REDEF_ID;

PROCEDURE IsA_SCOPE(n: M3AST.NODE;
    VAR(*out*) scope: SCOPE): BOOLEAN=
  BEGIN
    RETURN n.IsA_SCOPE(scope);
  END IsA_SCOPE;

PROCEDURE NewType_type(): Type_type RAISES {}=
  VAR v := NEW(Type_type);
  BEGIN
    EVAL v.init(); RETURN v;    
  END NewType_type;

PROCEDURE NewAny_type(): Any_type RAISES {}=
  VAR v := NEW(Any_type);
  BEGIN
    EVAL v.init(); RETURN v;    
  END NewAny_type;

PROCEDURE NewVoid_type(): Void_type RAISES {}=
  VAR v := NEW(Void_type);
  BEGIN
    EVAL v.init(); RETURN v;    
  END NewVoid_type;

PROCEDURE NewOpaque_type_Revln(): Opaque_type_Revln RAISES {}=
  VAR v := NEW(Opaque_type_Revln);
  BEGIN
    EVAL v.init(); RETURN v;    
  END NewOpaque_type_Revln;

PROCEDURE NewTypeActual(): TypeActual RAISES {}=
  VAR v := NEW(TypeActual);
  BEGIN
    EVAL v.init(); RETURN v;    
  END NewTypeActual;

PROCEDURE NewINIT_ID(): INIT_ID RAISES {} =
  BEGIN
    RETURN NEW(INIT_ID).init();
  END NewINIT_ID;
            
PROCEDURE NewCCV_ID(): CCV_ID RAISES {} =
  BEGIN
    RETURN NEW(CCV_ID).init();
  END NewCCV_ID;
            
PROCEDURE NewREDEF_ID(): REDEF_ID RAISES {} =
  BEGIN
    RETURN NEW(REDEF_ID).init();
  END NewREDEF_ID;
            
PROCEDURE NewRECOBJ_ID(): RECOBJ_ID RAISES {} =
  BEGIN
    RETURN NEW(RECOBJ_ID).init();
  END NewRECOBJ_ID;

PROCEDURE NewSCOPE(): SCOPE RAISES {} =
  BEGIN
    RETURN NEW(SCOPE).init();
  END NewSCOPE;
*************************************************************************)
            
VAR
  ue_g := NEW(EXP_NULL_UNSET OBJECT END);
  ud_g := NEW(DEF_ID_NULL_UNSET OBJECT END);

PROCEDURE UNSET_EXP(): EXP_NULL_UNSET RAISES {}=
  BEGIN
    RETURN ue_g;
  END UNSET_EXP;

PROCEDURE UNSET_DEF_ID(): DEF_ID_NULL_UNSET RAISES {}=
  BEGIN
    RETURN ud_g;
  END UNSET_DEF_ID;

BEGIN
END M3AST_SM.
