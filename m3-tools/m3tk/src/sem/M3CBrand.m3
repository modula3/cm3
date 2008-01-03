MODULE M3CBrand;

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

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT Text, TextExtras, Fmt;
IMPORT AST, M3AST_AS, M3AST_SM;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F;

IMPORT ASTWalk;
IMPORT M3CId, M3CLiteral, M3CSrcPos;
IMPORT M3CBackEnd;


REVEAL
  Handle = BRANDED REF RECORD
    unitId: M3AST_AS.UNIT_ID;
    unitBody: M3AST_AS.Block;
    block: M3AST_AS.Block;
    typeDecl: M3AST_AS.Concrete_decl;
    revelation: M3AST_AS.Concrete_reveal;
    count := 0;
  END;


PROCEDURE NewHandle(
    unit: M3AST_AS.UNIT_NORMAL;
    block: M3AST_AS.Block := NIL;
    typeDecl: M3AST_AS.Concrete_decl := NIL;
    revelation: M3AST_AS.Concrete_reveal := NIL)
    : Handle
    RAISES {}=
  BEGIN
    RETURN NEW(Handle, unitId := unit.as_id, unitBody := unit.as_block,
        block := block, typeDecl := typeDecl, revelation := revelation);
  END NewHandle;


<*INLINE*> PROCEDURE UnitSep(unitId: M3AST_AS.UNIT_ID): Text.T RAISES {}=
  BEGIN
    IF ISTYPE(unitId, M3AST_AS.Module_id) THEN
      RETURN ",";
    ELSE
      RETURN ".";
    END;
  END UnitSep;


<*INLINE*> PROCEDURE UnitText(unitId: M3AST_AS.UNIT_ID): Text.T RAISES {}=
  BEGIN
    IF unitId.lx_symrep = NIL THEN RETURN "" END;
    RETURN M3CId.ToText(unitId.lx_symrep);
  END UnitText;


PROCEDURE UnnamedBrand(
    unitId: M3AST_AS.UNIT_ID;
    srcPos: M3CSrcPos.T)
    : M3AST_SM.Exp_value
    RAISES {}=
  VAR
    unitSep := UnitSep(unitId);
    pos: CARDINAL;
    line := M3CSrcPos.Unpack(srcPos, pos);
  BEGIN
    RETURN TextToExp_value(TextExtras.Join(
        UnitText(unitId), unitSep, Fmt.Int(line), unitSep, Fmt.Int(pos)));
  END UnnamedBrand;

PROCEDURE NamedBrand(
    unitId: M3AST_AS.UNIT_ID;
    typeId: M3AST_AS.Type_id;
    VAR count: INTEGER)
    : M3AST_SM.Exp_value
    RAISES {}=
  VAR
    unitSep := UnitSep(unitId);
    unitText := UnitText(unitId);
    typeText := M3CId.ToText(typeId.lx_symrep);
    result: Text.T;
  BEGIN
    IF count = 0 THEN
      result := TextExtras.Join(unitText, unitSep, typeText);
    ELSE
      result :=
          TextExtras.Join(unitText, unitSep, typeText, unitSep, Fmt.Int(count));
    END;
    INC(count);
    RETURN TextToExp_value(result);
  END NamedBrand;


PROCEDURE TextToExp_value(t: TEXT): M3AST_SM.Exp_value RAISES {}=
  VAR
    er: M3AST_SM.Exp_value;
    text_lit: M3AST_AS.Text_literal := NEW(M3AST_AS.Text_literal).init();
  BEGIN
    text_lit.lx_litrep := M3CLiteral.Enter(
      TextExtras.Join("\"", t, "\""));
    EVAL M3CBackEnd.LiteralValue(text_lit, er);
    RETURN er
  END TextToExp_value;


PROCEDURE Set(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Block(block) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          IF block # h.unitBody AND h.block = NIL THEN h.block := block END;
        ELSE
          IF block = h.block THEN h.block := NIL END;
        END;
    | M3AST_AS.Concrete_decl(typeDecl) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          h.count := 0;
          h.typeDecl := typeDecl;
        ELSE
          h.typeDecl := NIL;
        END;
    | M3AST_AS.Concrete_reveal(revelation) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          h.count := 0;
          h.revelation := revelation;
        ELSE
          h.revelation := NIL;
        END;
    | M3AST_AS.Brand(brand) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          IF brand.as_exp = NIL THEN
            IF h.typeDecl # NIL AND h.block = NIL THEN
              (* Top level type declaration *)
              IF h.typeDecl.as_id.lx_symrep # NIL THEN
                brand.sm_brand :=
                    NamedBrand(h.unitId, h.typeDecl.as_id, h.count);
              END;
            ELSIF h.revelation # NIL THEN
              (* Revelation (bound to be top level) *)
              TYPECASE h.revelation.as_qual_id.as_id.sm_def OF
              | NULL =>
              | M3AST_AS.Type_id(typeId) =>
                  brand.sm_brand := NamedBrand(h.unitId, typeId, h.count);
              ELSE
              END;
            ELSE
              brand.sm_brand := UnnamedBrand(h.unitId, brand.lx_srcpos);
            END; (* if *)
          ELSE
            brand.sm_brand := brand.as_exp.sm_exp_value;
          END; (* if *)
        END;
    ELSE
    END; (* typecase *)
  END Set;


BEGIN

END M3CBrand.
