(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Susan Owicki                                     *)
(* Last modified on Fri Sep  2 14:43:20 PDT 1994 by weich      *)

(* Convert the toolkit type specification into a "Type.T" *)

INTERFACE AstToType;

IMPORT M3AST_AS, Type;

PROCEDURE Convert(m3type: M3AST_AS.M3TYPE): Type.T;
(* Recursivly parse "m3type" and produce an equivalent "Type.T" as
   return. *)

END AstToType.
