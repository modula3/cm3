(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

INTERFACE M3Scope;

IMPORT M3ID, M3AST;

TYPE
  Defn = RECORD
    ast   : M3AST.T;          (* ast containing the definition *)
    loc   : M3AST.NodeIndex;  (* defining node in "ast" *)
    class : Class;            (* symbol definition class *)
    info  : REFANY;           (* client data *)
    uid   : INTEGER;          (* internal UID of the definition *)
  END;

  Class = { GenericArg, Import, Const, Var, Formal, Type, Exception,
            Procedure, Module };

PROCEDURE LookUp (ast: M3AST.T;  loc: M3AST.NodeIndex;  sym: M3ID.T;
                  VAR(*OUT*) defn: Defn): BOOLEAN;
(* Returns "TRUE" and sets "defn" to correspond to the definition of
   the symbol named "sym" in effect at the node "loc" of "ast".  If "sym" is
   not defined at that location, "FALSE" is returned. *)

PROCEDURE Define (READONLY defn: Defn;  info: REFANY);
(* Sets the client data corresponding to "defn" to "info". *)
   
END M3Scope.
