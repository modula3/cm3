(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3ASTScope;

IMPORT ASTWalk, AST, M3AST_AS, M3AST_SM;

(* This interface provides support for managing scopes, in
   terms of the "M3AST_SM.SCOPE" class. The assumption is
   that a client is using the "ASTWalk" package to visit
   each node in the AST, on entry and exit. The "Set" procedure
   should be called each time a node is entered, and this
   will return a "SCOPE" object that can be used for identifier
   lookups.
*)

TYPE Closure <: Closure_public;
     Closure_public = ASTWalk.Closure OBJECT
    scope: M3AST_SM.SCOPE := NIL;
  END;
(* Create a subtype of "Closure", with "callback" bound to
   the procedue that will call "Set", and pass it to
   "ASTWalk.ModeVisitNodes". *)

PROCEDURE Set(cl: Closure; n: AST.NODE; vm: ASTWalk.VisitMode);
(* This procedure updates the value of "cl.scope" according to
   the type of "n" and the value of "vm", (Entry/Exit). *)
  
PROCEDURE Lookup(s: M3AST_SM.SCOPE; 
                 used_id: M3AST_AS.USED_ID): M3AST_AS.DEF_ID;
(* This procedure tries to bind the identifier occurrence "used_id"
   to a definition. If the search fails, NIL is returned, else
   the node of the defining occurrence. *)
  

END M3ASTScope.
