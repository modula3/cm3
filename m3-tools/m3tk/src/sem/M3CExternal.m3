MODULE M3CExternal;

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

IMPORT M3AST, M3AST_AS, M3AST_PG;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_PG_F;

IMPORT SeqM3AST_AS_Const_decl, SeqM3AST_AS_Exc_decl, SeqM3AST_AS_TYPE_DECL;

IMPORT M3ASTNext;
IMPORT M3Error;


PROCEDURE Set(an: M3AST.NODE; cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    isModule: BOOLEAN;
    unitExternal, external: M3AST_PG.External_NULL;
    ed: M3AST_PG.EXTERNAL_DECL;
  BEGIN
    IF NOT M3AST_PG.IsA_EXTERNAL_DECL(an, ed) THEN RETURN END;

    TYPECASE cu.as_root OF <*NOWARN*>
    | M3AST_AS.Interface(interface) =>
        isModule := FALSE;
        unitExternal := interface.vEXTERNAL_DECL.pg_external;
    | M3AST_AS.Module =>
        isModule := TRUE;
        unitExternal := NIL;
    END;

    external := ed.pg_external;
    IF external # NIL THEN
      IF isModule THEN
        M3Error.Report(an, "EXTERNAL is not allowed in a module");
      END; (* if *)
    ELSE
      external := unitExternal;
    END; (* if *)

    TYPECASE an OF <*NOWARN*>
    | M3AST_AS.Interface(interface) =>
        NARROW(interface.as_id, M3AST_AS.Interface_id).vEXTERNAL_ID.pg_external :=
            external;
    | M3AST_AS.Proc_decl(proc_decl) =>
        proc_decl.as_id.vEXTERNAL_ID.pg_external := external;
    | M3AST_AS.Var_decl_s(var_decl_s) =>
        VAR
          iter := M3ASTNext.NewIterVar(
              var_decl_s.as_var_decl_s);
          varId: M3AST_AS.Var_id;
        BEGIN
          WHILE M3ASTNext.Var(iter, varId) DO
            varId.vEXTERNAL_ID.pg_external := external;
          END; (* while *)
        END;
    | M3AST_AS.Const_decl_s(const_decl_s) =>
        VAR
          iter := SeqM3AST_AS_Const_decl.NewIter(
              const_decl_s.as_const_decl_s);
          constDecl: M3AST_AS.Const_decl;
        BEGIN
          WHILE SeqM3AST_AS_Const_decl.Next(iter, constDecl) DO
            constDecl.as_id.vEXTERNAL_ID.pg_external := external;
          END; (* while *)
        END;
    | M3AST_AS.Exc_decl_s(exc_decl_s) =>
        VAR
          iter := SeqM3AST_AS_Exc_decl.NewIter(
              exc_decl_s.as_exc_decl_s);
          excDecl: M3AST_AS.Exc_decl;
        BEGIN
          WHILE SeqM3AST_AS_Exc_decl.Next(iter, excDecl) DO
            excDecl.as_id.vEXTERNAL_ID.pg_external := external;
          END; (* while *)
        END;
    | M3AST_AS.Type_decl_s(type_decl_s) =>
        VAR
          iter := SeqM3AST_AS_TYPE_DECL.NewIter(
              type_decl_s.as_type_decl_s);
          typeDecl: M3AST_AS.TYPE_DECL;
        BEGIN
          WHILE SeqM3AST_AS_TYPE_DECL.Next(iter, typeDecl) DO
            TYPECASE typeDecl OF <*NOWARN*>
            | M3AST_AS.Concrete_decl(concDecl) =>
                concDecl.as_id.vEXTERNAL_ID.pg_external := external;
            | M3AST_AS.Subtype_decl(subtypeDecl) =>
                subtypeDecl.as_id.vEXTERNAL_ID.pg_external := external;
            END;
          END; (* while *)
        END;
    END; (* case *)
  END Set;


BEGIN
END M3CExternal.
