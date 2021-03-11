(* $Id$ *)

INTERFACE TypeTranslator;

(* 
 * Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
 * Author : Mika Nystrom <mika@alum.mit.edu> 
 *)

(* Translate the SRC Type.T into a SchemeObject.T for the Mscheme 
   environment *)
IMPORT M3AST_all; (* compiler bug *)

IMPORT Type, SchemePair;

FROM Type IMPORT Qid, Exception;

PROCEDURE Translate(type : Type.T) : SchemePair.T;

PROCEDURE TranslateQid(q : Qid) : SchemePair.T;

PROCEDURE TranslateException(x : Exception) : SchemePair.T;

CONST Brand = "TypeTranslator";

VAR protoList, basetypeList : SchemePair.T := NIL;

END TypeTranslator.
