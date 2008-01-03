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
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3ASTDisplay EXPORTS M3ASTDisplay, AST_DisplayRep;

IMPORT Wr, M3AST, ASTWalk;

IMPORT AST_DisplayRep, M3ASTDisplay_handle;

REVEAL
  AST_DisplayRep.Handle = M3ASTDisplay_handle.T BRANDED OBJECT
    ignoreChildren: BOOLEAN := FALSE;
    callOnEntry: BOOLEAN := FALSE;
    callOnExit: BOOLEAN := FALSE;
    closure: Closure := NIL;
  OVERRIDES
      Visit := DoVisit;
  END;
    
REVEAL
  Closure = ASTWalk.Closure BRANDED OBJECT 
      handle: Handle := NIL; 
  END;

PROCEDURE Nodes(
    n: M3AST.NODE;
    s: Wr.T) 
    RAISES {Wr.Failure}=
  VAR
    handle := NEW(Handle);
  BEGIN
    DoNodes(handle, n, s);
  END Nodes;

PROCEDURE ModeNodes(
    n: M3AST.NODE;
    c: Closure;
    vm : ASTWalk.VisitModeControl;    
    s: Wr.T)
    RAISES {Wr.Failure} =
  VAR
    handle := NEW(Handle, closure := c);
  BEGIN
    c.handle := handle;
    handle.callOnEntry := ASTWalk.VisitMode.Entry IN vm;
    handle.callOnExit := ASTWalk.VisitMode.Exit IN vm;
    DoNodes(handle, n, s);
  END ModeNodes;

PROCEDURE DoNodes(
    handle: Handle;
    n: M3AST.NODE;
    s: Wr.T) 
    RAISES {Wr.Failure}=
  <*FATAL ANY*>
  BEGIN
    handle.stream := s; 
    TRY
      DoVisit(handle, n);
    EXCEPT
      ASTWalk.Aborted =>
    END;
  END DoNodes;

PROCEDURE DoVisit(handle: Handle; n: M3AST.NODE) RAISES ANY=
  BEGIN
    IF handle.callOnEntry THEN handle.closure.callback(n); END;
    IF NOT handle.ignoreChildren THEN
      n.display(handle) 
    END;
    IF handle.callOnExit THEN handle.closure.callback(n); END;
    handle.ignoreChildren := FALSE;    
  END DoVisit;

(*PUBLIC*)
PROCEDURE IgnoreChildren(c: Closure) RAISES {} =
  BEGIN
    c.handle.ignoreChildren := TRUE;
  END IgnoreChildren;


BEGIN
END M3ASTDisplay.
