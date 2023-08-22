MODULE Feature;

IMPORT AST,
       M3AST_AS,
       (* M3AST_SM,*)
       M3AST_AS_F,
       M3AST_TL_F,
       ASTWalk,
       M3Context,
       M3CUnit,
       PropertyV,
       M3CSrcPos,
       M3CId,
       M3AST_SM_F,
       (* to get unit *)
       M3AST_TM_F,
       (* to get fe_uid *)
       M3AST_FE_F,
       M3CExpsMisc;
IMPORT AstToType,Type;
IMPORT Debug, Fmt, Text;

TYPE
  ContextClosure = M3Context.Closure OBJECT
                     h : AstToType.Handle;
                     unit     : TEXT;
                     symLoc : M3CSrcPos.T;        (* current loc of sym *)
                     defLoc : M3CSrcPos.T;        (* loc of sym definition *)
                     typeLoc : M3CSrcPos.T;       (* loc of type definition *)
                     hoverMsg : TEXT      := NIL;
                     symDef : TEXT        := NIL; (* file loc of symbol *)
                     typeDef : TEXT       := NIL; (* file loc of type *)
                   OVERRIDES
                     callback := VisitUnit;
                   END;

  WalkClosure = ASTWalk.Closure OBJECT
                  c: ContextClosure;
                OVERRIDES
                  callback := Node;
                END;

PROCEDURE D(msg,extra : TEXT; pos : M3CSrcPos.T) =
  BEGIN
    Debug.Write(msg & " " & extra & " " & PosTxt(pos) & "\n");
  END D;

PROCEDURE M(cl : WalkClosure; symTxt, flag : TEXT) : TEXT =
  VAR res : TEXT;
  BEGIN
    res := symTxt & flag & 
      " : symDef " & cl.c.symDef &
      " : symloc : " & PosTxt(cl.c.symLoc) &
      " : typeDef " & cl.c.typeDef  &
      " : typeloc " & PosTxt(cl.c.typeLoc) &
      " : defloc " & PosTxt(cl.c.defLoc);
    RETURN res;
  END M;

PROCEDURE PosTxt(pos : M3CSrcPos.T) : TEXT =
  VAR res : TEXT;
  BEGIN
    res := Fmt.Int(pos.line) &
           " " & Fmt.Int(pos.col) &
           " " & Fmt.Int(pos.len);
    RETURN res;
  END PosTxt;

PROCEDURE Compare(x,y : M3CSrcPos.T) : BOOLEAN =
  BEGIN
    IF x.line = y.line AND
      (y.col >= x.col AND y.col < x.col + x.len) THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Compare;

PROCEDURE SearchNodes(c: M3Context.T; unit : TEXT; line,col : INTEGER) : ContextClosure =
  VAR cl: ContextClosure;
  BEGIN
    cl := NEW(ContextClosure, unit := unit,
              symLoc := M3CSrcPos.T{line := line + 1, col := col, len := 0});

    cl.h := AstToType.NewHandle(c);
    M3Context.Apply(c, cl, findStandard := FALSE); (* ignore 'standard' unit *)
    RETURN cl;
  END SearchNodes;

PROCEDURE GetSymbolType (c: M3Context.T; unit: TEXT; line, col: INTEGER;
                         VAR (*out*) startCol,len : INTEGER): TEXT =
  VAR cl: ContextClosure;
  BEGIN
    cl := SearchNodes(c, unit, line, col);
    startCol := cl.symLoc.col;
    len := cl.symLoc.len;
    RETURN cl.hoverMsg;
  END GetSymbolType;

PROCEDURE GetDeclaration (c: M3Context.T; unit: TEXT; line, col: INTEGER;
                      VAR (*out*) defLine, defCol, defLen : INTEGER): TEXT =
  VAR cl: ContextClosure;
  BEGIN
    cl := SearchNodes(c, unit, line, col);
    defLine := cl.defLoc.line - 1;
    defCol := cl.defLoc.col;
    defLen := cl.defLoc.len;
    RETURN cl.symDef;
  END GetDeclaration;

PROCEDURE GetTypeDefinition (c: M3Context.T; unit: TEXT; line, col: INTEGER;
                      VAR (*out*) defLine, defCol, defLen : INTEGER): TEXT =
  VAR cl: ContextClosure;
  BEGIN
    cl := SearchNodes(c, unit, line, col);
    defLine := cl.typeLoc.line - 1;
    defCol := cl.typeLoc.col;
    defLen := cl.typeLoc.len;
    RETURN cl.typeDef;
  END GetTypeDefinition;

PROCEDURE VisitUnit (cl  : ContextClosure;
                     ut  : M3CUnit.Type;
                     name: TEXT;
                     cu  : M3AST_AS.Compilation_Unit) RAISES {} =
  <* FATAL ANY *>
  VAR
    wc: WalkClosure;
  BEGIN
    Debug.Write("In VistUnit " & name & " " & cl.unit & "\n");

    IF NOT Text.Equal(name, cl.unit) THEN RETURN; END;

    (* if it is a generic instantiation, get to actual instantiated tree *)
    cu := M3CUnit.ToGenIns(cu, ut);
    wc := NEW(WalkClosure, c := cl);
    ASTWalk.VisitNodes(cu, wc);
  END VisitUnit;

PROCEDURE UnitFile(unit : M3AST_AS.UNIT) : TEXT =
  VAR
    cu : M3AST_AS.Compilation_Unit;
  BEGIN
    cu := unit.sm_comp_unit;
    RETURN M3CUnit.TextName(cu.fe_uid);
  END UnitFile;
 
PROCEDURE Node (cl: WalkClosure; n: AST.NODE; vm: ASTWalk.VisitMode) =
  VAR
    defId: M3AST_AS.DEF_ID;
    symTxt,typeTxt : TEXT;
    type : Type.T;
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF

      | M3AST_AS.TYPE_DECL (type_decl) =>
          TYPECASE type_decl.as_type OF
          | M3AST_AS.TYPE_SPEC (ts) =>
             (* save the typename in a table for use later *) 
             PropertyV.Put(ts.tl_pset, type_decl.as_id.lx_symrep);
(*
             D("type decl - type spec: ", type_decl.as_id.lx_symrep.toText(), type_decl.lx_srcpos);
*)            
          ELSE
          END;

      | M3AST_AS.DEF_ID (defId) =>

          IF Compare(defId.lx_srcpos, cl.c.symLoc) THEN
            symTxt := defId.lx_symrep.toText();
            cl.c.symLoc := defId.lx_srcpos;
            cl.c.symDef := UnitFile(defId.tmp_unit_id.sm_spec);

            TYPECASE defId OF
            | M3AST_AS.TYPED_ID(tt) =>
              type := AstToType.ProcessM3Type(cl.c.h, tt.sm_type_spec);
              (* display the name of the type not its expanded rep.
                 Change byName to FALSE if not. *)
              typeTxt := Type.ToText(type,byName := TRUE);
              (* for loop locals do not have a tmp_unit_id *)
              IF tt.sm_type_spec.tmp_unit_id # NIL THEN
                cl.c.typeDef := UnitFile(tt.sm_type_spec.tmp_unit_id.sm_spec);
              END;
              cl.c.typeLoc :=  tt.sm_type_spec.lx_srcpos;
            | M3AST_AS.UNIT_ID(tt) =>
              cl.c.typeDef := UnitFile(tt.tmp_unit_id.sm_spec);
              typeTxt := cl.c.typeDef;
            ELSE
              (* fixme these debug type names *)
              cl.c.typeDef := "ELSE typeDef";
              typeTxt := "ELSE type";
            END;

            cl.c.hoverMsg := symTxt & " : " & typeTxt;

(* debug 
            cl.c.hoverMsg := M(cl, symTxt, " DEF: "); 
*)
          END;
         (* 
          D("found def : ", defId.lx_symrep.toText(), defId.lx_srcpos);
         *)
      | M3AST_AS.USED_ID (defId) =>

          IF Compare(defId.lx_srcpos, cl.c.symLoc) THEN
            symTxt := defId.lx_symrep.toText();
            cl.c.symLoc := defId.lx_srcpos;

            typeTxt := "";
            IF defId.sm_def # NIL THEN 
              TYPECASE defId.sm_def OF
              | M3AST_AS.TYPED_ID(tt) =>
                cl.c.symDef := UnitFile(tt.tmp_unit_id.sm_spec);
                cl.c.defLoc := tt.lx_srcpos;
                type := AstToType.ProcessM3Type(cl.c.h, tt.sm_type_spec);
                typeTxt := Type.ToText(type,byName := TRUE);
                cl.c.typeDef := UnitFile(tt.sm_type_spec.tmp_unit_id.sm_spec);
                cl.c.typeLoc :=  tt.sm_type_spec.lx_srcpos;
              | M3AST_AS.UNIT_ID(tt) =>
                cl.c.symDef := UnitFile(tt.sm_spec);
                cl.c.defLoc := tt.lx_srcpos;
                typeTxt := cl.c.symDef;
                cl.c.typeDef := "NO typeDef";
              ELSE
                (* fixme these debug type names *)
                cl.c.typeDef := "ELSE typeDef";
                typeTxt := "ELSE type";
              END;

            END;
            cl.c.hoverMsg := symTxt & " : " & typeTxt;
(* debug
            cl.c.hoverMsg := M(cl, symTxt, " USED: "); 
*)
          END;

      | M3AST_AS.EXP (exp) =>
          (*exp.lx_symrep.toText()  does not exist *)

          IF Compare(exp.lx_srcpos, cl.c.symLoc) THEN
            IF M3CExpsMisc.IsId(exp, defId) THEN
              symTxt := defId.lx_symrep.toText();
              cl.c.symDef := UnitFile(defId.tmp_unit_id.sm_spec);
              cl.c.symLoc := exp.lx_srcpos;
              cl.c.defLoc := defId.lx_srcpos;
(*
 D("Exp (defid): ", symTxt, cl.c.defLoc);
*)
              type := AstToType.ProcessM3Type(cl.c.h, exp.sm_exp_type_spec);
              typeTxt := Type.ToText(type,byName := TRUE);

              (* the location of the type for this symbol *)
              WITH typeDef = exp.sm_exp_type_spec.tmp_unit_id DO
                IF typeDef # NIL THEN
                  cl.c.typeDef := UnitFile(typeDef.sm_spec);
                ELSE
                  cl.c.typeDef := "NO TYPE";
                END;
              END;

              cl.c.typeLoc :=  exp.sm_exp_type_spec.lx_srcpos;
(*
D("Expr type loc: ", cl.c.typeDef, cl.c.typeLoc);
*)
              cl.c.hoverMsg := symTxt & " : " & typeTxt;
(* debug
              cl.c.hoverMsg := M(cl, symTxt, " EXP: "); 
*)
            ELSE
              D("Exp not an id: ", " -- ", exp.lx_srcpos);
            END;
          END

      ELSE
      END;
    END;
  END Node;

BEGIN
END Feature.
