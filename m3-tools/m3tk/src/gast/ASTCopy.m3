MODULE ASTCopy EXPORTS ASTCopy, AST_CopyRep;

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

IMPORT AST;
IMPORT ASTWalk;

REVEAL
  Handle = Handle_public BRANDED OBJECT
    ignoreChildren := FALSE;
    callOnEntry: BOOLEAN := FALSE;
    callOnExit: BOOLEAN := FALSE;
    closure: Closure := NIL;
  OVERRIDES
      Copy := DoCopy;
  END;

  Closure = OBJECT
    METHODS
      callback(n, ncopy: AST.NODE; vm: ASTWalk.VisitMode) RAISES ANY;
      init(): Closure;
    END
    BRANDED OBJECT 
      handle: Handle := NIL; 
    OVERRIDES
      init := Init;
  END;

PROCEDURE Nodes(
    n: AST.NODE;
    ): AST.NODE RAISES {}=
  BEGIN
    RETURN DoNodes(NEW(Handle), n);
  END Nodes;

PROCEDURE ModeNodes(
    n: AST.NODE;
    c: Closure;
    vm : ASTWalk.VisitModeControl;    
    ): AST.NODE=
  VAR
    handle := NEW(Handle);
  BEGIN
    c.handle := handle;
    handle.closure := c;
    handle.callOnEntry := ASTWalk.VisitMode.Entry IN vm;
    handle.callOnExit := ASTWalk.VisitMode.Exit IN vm;
    RETURN DoNodes(handle, n);
  END ModeNodes;

PROCEDURE DoNodes(handle: Handle; n: AST.NODE): AST.NODE=
  BEGIN
    IF n = NIL THEN RETURN NIL END;
    RETURN DoCopy(handle, n);
  END DoNodes;


PROCEDURE DoCopy(handle: Handle; n: AST.NODE): AST.NODE=
  <*FATAL ANY*>
  VAR
    cn: AST.NODE := NIL;
  BEGIN
    IF handle.callOnEntry THEN 
      handle.closure.callback(n, cn, ASTWalk.VisitMode.Entry); 
    END;
    IF NOT handle.ignoreChildren THEN
      cn := n.copy(handle) 
    END;
    IF handle.callOnExit THEN 
      handle.closure.callback(n, cn, ASTWalk.VisitMode.Exit); 
    END;    
    RETURN cn;
  END DoCopy;

(*PUBLIC*)
PROCEDURE IgnoreChildren(c: Closure) RAISES {} =
  BEGIN
    c.handle.ignoreChildren := TRUE;
  END IgnoreChildren;


PROCEDURE Null(<*UNUSED*> n: NODE; <*UNUSED*>handle: Handle): AST.NODE RAISES {}=
  BEGIN
    RETURN NIL;
  END Null;

PROCEDURE Init(c: Closure): Closure RAISES {}=
  BEGIN
    RETURN c;
  END Init;

BEGIN

END ASTCopy.
