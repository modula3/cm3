MODULE M3CIntDef;

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

IMPORT M3AST, M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;



(* In fact, non-void values of sm_int_def/sm_concrete_proc_id are set
by M3CScope.  This module just turns Unset into void, and handles 
formal ids. *)

PROCEDURE Set(an: M3AST.NODE; unit: M3AST_AS.UNIT) RAISES {}=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Proc_decl(procDecl) =>
        WITH procId = procDecl.as_id DO
          procId := NARROW(an, M3AST_AS.Proc_decl).as_id;
          IF ISTYPE(unit, M3AST_AS.Interface) THEN
            (* sm_int_def refers to itself. *)
            procId.vREDEF_ID.sm_int_def := procId;
            (* if sm_concrete_proc_id still Unset set it to void *)
            IF procId.sm_concrete_proc_id = M3AST_SM.UNSET_DEF_ID() THEN
              procId.sm_concrete_proc_id := NIL;
            END; (* if *)
          ELSE (* KModule *)
            (* sm_concrete_proc_id refers to itself. *)
            procId.sm_concrete_proc_id := procId;
            (* if sm_int_def still Unset set it to void. *)
            IF procId.vREDEF_ID.sm_int_def = M3AST_SM.UNSET_DEF_ID() THEN
              procId.vREDEF_ID.sm_int_def := NIL;
            END; (* if *)
          END; (* if *)
        END;
    ELSE
    END;
  END Set;


BEGIN
END M3CIntDef.
