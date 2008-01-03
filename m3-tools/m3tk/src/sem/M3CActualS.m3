MODULE M3CActualS;

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

IMPORT AST, M3AST_AS;

IMPORT M3AST_SM_F;

IMPORT ASTWalk;

IMPORT M3CStdProcs, M3CConsActualS, M3CStdActualS, M3CProcActualS;


PROCEDURE Set(
    <*UNUSED*> cl: ASTWalk.Closure;
    an: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  VAR
    pf: M3CStdProcs.T;
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Constructor(constructor) =>
        M3CConsActualS.Set(constructor);
    | M3AST_AS.Call(call) =>
        IF M3CStdProcs.IsStandardCall(call, pf) THEN
          IF pf # M3CStdProcs.T.New THEN
            M3CStdActualS.Set(call, pf);
          END;
        ELSE
          M3CProcActualS.Set(call);
        END; (* if *)
    ELSE
      (* do nothing *)
    END; (* case *)
  END Set;


BEGIN
END M3CActualS.
