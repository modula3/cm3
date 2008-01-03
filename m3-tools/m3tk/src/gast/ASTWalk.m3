MODULE ASTWalk EXPORTS ASTWalk, AST_WalkRep;

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

REVEAL
  Handle = Handle_public BRANDED OBJECT
    ignoreChildren: BOOLEAN := FALSE;
    callOnEntry, callOnExit: BOOLEAN := TRUE;
    closure: Closure;
  OVERRIDES
      Visit := DoVisit;
  END;
    
REVEAL
  Closure = Closure_public BRANDED OBJECT 
      handle: Handle := NIL; 
  OVERRIDES
      init := Init;
  END;
 
PROCEDURE VisitNodes(n: AST.NODE; closure: Closure) RAISES ANY=
  VAR
    handle := NEW(Handle, callOnExit := FALSE);
  BEGIN
    closure.handle := handle;
    handle.closure := closure;
    TRY
      DoVisit(handle, n);
    EXCEPT
      Aborted =>
    END;
  END VisitNodes;

PROCEDURE ModeVisitNodes(n: AST.NODE; closure: Closure; 
    vm: VisitModeControl) RAISES ANY=
  VAR
    handle := NEW(Handle);
  BEGIN
    handle.callOnEntry := VisitMode.Entry IN vm;
    handle.callOnExit := VisitMode.Exit IN vm;
    closure.handle := handle;
    handle.closure := closure;
    TRY
      DoVisit(handle, n);
    EXCEPT
      Aborted =>
    END;
  END ModeVisitNodes;

PROCEDURE DoVisit(handle: Handle; n: AST.NODE) RAISES ANY =
  BEGIN
    IF handle.callOnEntry THEN handle.closure.callback(n) END;
    IF NOT handle.ignoreChildren THEN n.walk(handle) END;
    IF handle.callOnExit THEN handle.closure.callback(n, VisitMode.Exit) END;
    handle.ignoreChildren := FALSE;
  END DoVisit;

PROCEDURE Init(c: Closure): Closure RAISES {}=
  BEGIN
    RETURN c;
  END Init;

TYPE
  NPClosure =
    Closure OBJECT
      np: NodeCallbackProc;
    OVERRIDES
      callback := CallNodeProc;
    END;

PROCEDURE CallNodeProc(c: NPClosure; n: AST.NODE;
                       <*UNUSED*> vm: VisitMode) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    c.np(n);
  END CallNodeProc;

PROCEDURE NodeProcClosure(p: NodeCallbackProc): Closure RAISES {}=
  BEGIN
    RETURN NEW(NPClosure, np := p);
  END NodeProcClosure;

PROCEDURE IgnoreChildren(cl: Closure) RAISES {} =
  BEGIN
    cl.handle.ignoreChildren := TRUE;
  END IgnoreChildren;

PROCEDURE Abort() RAISES {Aborted} =
  BEGIN
    RAISE Aborted;
  END Abort;

PROCEDURE Null(<*UNUSED*> n: NODE; <*UNUSED*> handle: Handle) RAISES {}=
  BEGIN
  END Null;

BEGIN
END ASTWalk.
