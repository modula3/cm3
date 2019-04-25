(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RecordExpr.m3                                         *)
(* Last modified on Tue Jun 20 15:47:54 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 12:45:05 PDT 1995 by ericv      *)
(*      modified on Wed Nov 28 02:47:43 1990 by muller         *)

MODULE RecordExpr;
(* A record constructor. *) 

IMPORT M3, M3ID, CG, Error, Type, RecordType, Module;
IMPORT Value, Field, AssignStmt, M3Buf;
IMPORT Expr, ExprRep, KeywordExpr, RangeExpr, ArrayExpr;

TYPE
  Info = RECORD
    field : Value.T;
    type  : Type.T;
    expr  : Expr.T;
    name  : M3ID.T;
    done  : BOOLEAN;
  END;

TYPE
  P = Expr.T OBJECT
        tipe          : Type.T;
        args          : Expr.List;
        map           : REF ARRAY OF Info;
        finalVal      : CG.Val;
        finalValUseCt : INTEGER;
        evalAttempted : BOOLEAN; (* TRUE even if Evaluate was called
                                    unsuccessfully. *) 
        is_const      : BOOLEAN; (* Meaningless if NOT evalAttempted.
                                    Otherwise, Evaluate was successful. *)
      OVERRIDES
        typeOf       := ExprRep.NoType;
        check        := Check;
        need_addr    := NeedsAddress;
        prep         := Prep;
        compile      := Compile;
        prepLV       := PrepLV;
        compileLV    := CompileLV;
        prepBR       := ExprRep.NotBoolean;
        compileBR    := ExprRep.NotBoolean;
        evaluate     := Evaluate;
        isEqual      := EqCheck;
        getBounds    := ExprRep.NoBounds;
        isWritable   := ExprRep.IsNever;
        isDesignator := ExprRep.IsNever;
        isZeroes     := IsZeroes;
        genFPLiteral := GenFPLiteral;
        prepLiteral  := PrepLiteral;
        genLiteral   := GenLiteral;
        note_write   := ExprRep.NotWritable;
        usesAssignProtocol := UsesAssignProtocol;
      END;

(* EXPORTED: *)
PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.type      := type;
    p.tipe      := type;
    p.args      := args;
    p.map       := NIL;
    p.finalVal       := NIL;
    p.finalValUseCt  := 0;
    p.evalAttempted    := FALSE;
    p.is_const  := FALSE;
    p.directAssignableType := TRUE;
    RETURN p;
  END New;

(* EXPORTED: *)
PROCEDURE Is (e: Expr.T): BOOLEAN =
  BEGIN
    RETURN (TYPECODE (e) = TYPECODE (P));
  END Is;

(* EXPORTED: *)
PROCEDURE Qualify (e: Expr.T;  id: M3ID.T;  VAR result: Expr.T): BOOLEAN =
  VAR
    p         : P;
    val       : Value.T;
    fieldInfo : Field.Info;
    argExpr   : Expr.T;
    argId     : M3ID.T;
    splitExpr : Expr.T;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(x) => p := x;
    ELSE      RETURN FALSE;
    END;
    IF NOT RecordType.LookUp (p.tipe, id, val) THEN RETURN FALSE END;
    Field.Split (val, fieldInfo);
    FOR i := 0 TO LAST (p.args^) DO
      argExpr := p.args[i];
      IF (KeywordExpr.Split (argExpr, argId, splitExpr)) THEN
        IF (argId = id) THEN result := splitExpr; RETURN TRUE END;
      ELSIF (i = fieldInfo.index) THEN
        result := argExpr;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END Qualify;

(* Externally dispatched-to: *)
PROCEDURE Check (p: P;  VAR cs: Expr.CheckState) =
  VAR
    fieldCt   : INTEGER;
    fieldNo   : INTEGER;
    splitExpr, e : Expr.T;
    dfault    : Expr.T;
    fieldList : Value.T;
    v         : Value.T;
    Id        : M3ID.T;
    fieldInfo : Field.Info;
    posOK     : BOOLEAN;
  BEGIN
    p.tipe := Type.Check (p.tipe);
    FOR i := 0 TO LAST (p.args^) DO Expr.TypeCheck (p.args[i], cs) END;
    p.type := p.tipe;
    IF NOT RecordType.Split (p.tipe, fieldList) THEN
      Error.Msg ("record constructor must specify a record type");
      RETURN;
    END;

    (* count the fields *)
    v := fieldList;
    fieldCt := 0;
    WHILE (v # NIL) DO INC (fieldCt);  v := v.next END;

    (* build vectors to map field numbers (of the record type) to Info nodes.*)
    p.map := NEW (REF ARRAY OF Info, fieldCt);
    v := fieldList;
    fieldNo := 0;
    WHILE (v # NIL) DO
      Field.Split (v, fieldInfo);
      WITH z = p.map^[fieldNo] DO
        z.field := v;
        z.name  := fieldInfo.name;
        z.type  := fieldInfo.type;
        z.expr  := fieldInfo.dfault;
        z.done  := FALSE;
      END;
      v := v.next;
      INC (fieldNo);
    END;
    posOK := TRUE;
    EVAL Evaluate (p);  (* make sure that the everything that can be folded is *)

    FOR i := 0 TO LAST (p.args^) DO
      e := p.args[i];
      IF RangeExpr.Split (e, splitExpr, dfault) THEN
        Error.Msg ("range expressions not allowed in record constructors");
      END;

      IF KeywordExpr.Split (e, Id, splitExpr) THEN
        posOK := FALSE;
        fieldNo := 0;
        e := splitExpr;
        LOOP
          IF (fieldNo >= fieldCt) THEN
            Error.ID (Id, "unknown field name in record constructor");
            fieldNo := i;
            EXIT;
          END;
          IF (p.map^[fieldNo].name = Id) THEN EXIT END;
          INC (fieldNo);
        END;
      ELSE (* positional parameter *)
        IF (NOT posOK) THEN
          Error.Msg ("positional values must precede keyword values");
        END;
        IF (i >= fieldCt) THEN
            Error.Msg ("too many values in record constructor");
            fieldNo := fieldCt - 1;
          ELSE  fieldNo := i;
        END;
      END;

      IF (0 <= fieldNo) AND (fieldNo < fieldCt) THEN
        WITH z = p.map^[fieldNo] DO
          IF (z.done) THEN
            Error.ID (z.name, "record constructor field previously specified");
          END;
          z.done := TRUE;
          IF NOT Type.IsAssignable (z.type, Expr.TypeOf (e)) THEN
            Error.ID
              (z.name, "expression is not assignable to record constructor field");
          ELSE
            AssignStmt.Check (z.type, e, cs);
            z.expr := e;
          END;
        END;
      ELSE
        (* some other error, so don't even try *)
      END;
    END (*FOR*);

    FOR fieldNo := 0 TO fieldCt - 1 DO
      WITH z = p.map^[fieldNo] DO
        IF (NOT z.done) AND (z.expr = NIL) THEN
          Error.ID (z.name, "no value specified for record constructor field");
        END;
      END;
    END;
  END Check;

(* Externally dispatched-to: *)
PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
  VAR b: P;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => b := p;
    ELSE      RETURN FALSE;
    END;
    IF (NOT Type.IsEqual (a.tipe, b.tipe, x))
      OR ((a.args = NIL) # (b.args = NIL))
      OR ((a.args # NIL) AND (NUMBER (a.args^) # NUMBER (b.args^))) THEN
      RETURN FALSE;
    END;
    FOR i := 0 TO LAST (a.args^) DO
      IF NOT Expr.IsEqual (a.args[i], b.args[i], x) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END EqCheck;

(* Externally dispatched-to: *)
PROCEDURE NeedsAddress (<*UNUSED*> p: P) =
  BEGIN
    (* yep, all records get memory addresses *)
  END NeedsAddress;

(* Externally dispatched-to: *)
PROCEDURE UsesAssignProtocol (p: P): BOOLEAN =
(* PRE: p has been checked but not Prepped. *)
  BEGIN
    EVAL Evaluate (p);
    IF p.is_const THEN RETURN FALSE END;
    RETURN p.doDirectAssign
  END UsesAssignProtocol;

(* EXPORTED: *)
(* Externally dispatched-to: *)
PROCEDURE Prep (p: P) =
  BEGIN
    PrepLV (p, traced := FALSE);
  END Prep;

(* Externally dispatched-to: *)
PROCEDURE PrepLV (p: P;  traced: BOOLEAN) =
  BEGIN
    IF NOT UsesAssignProtocol (p)
    THEN InnerPrepLV (p, traced)
 (* ELSE postpone InnerPrepLV until Compile, when LHS will have been pushed. *)
    END;
  END PrepLV;

PROCEDURE InnerPrepLV (p: P;  traced: BOOLEAN) =
  (* PRE: IF UsesAssignProtocol (p), LHS address is compiled and on top of the
          CG stack, for us to use. *)
  VAR
    info: Type.Info;
    field: Field.Info;
    resultVar: CG.Var;
    usesAssignProtocol : BOOLEAN := UsesAssignProtocol (p);
  BEGIN
    INC (p.finalValUseCt);
    IF (p.finalVal # NIL) AND (p.finalValUseCt > 1)
    THEN (* We already did this before. *)
      <* ASSERT NOT usesAssignProtocol *>
      RETURN
    END;
    EVAL Type.CheckInfo (p.type, info);

    IF usesAssignProtocol THEN
      <* ASSERT NOT traced *>             (* CHECK ME? *)
      p.finalVal := CG.Pop ();
    ELSE
      resultVar := CG.Declare_temp (info.size, info.alignment,
                             CG.Type.Struct, in_memory := TRUE);
    END;

    FOR i := 0 TO LAST (p.map^) DO
      WITH z = p.map^[i] DO
        Field.Split (z.field, field);
        AssignStmt.PrepForEmit (field.type, z.expr, initializing := TRUE);
        IF usesAssignProtocol THEN
          CG.Push (p.finalVal);
          IF (field.offset # 0) THEN  CG.Add_offset (field.offset);  END;
        ELSE
          CG.Load_addr_of (resultVar, field.offset, info.alignment);
        END;
        AssignStmt.DoEmit (field.type, z.expr);
      END;
    END;

    IF usesAssignProtocol THEN
      (* result is already in p.finalVal *)
    ELSE
      CG.Load_addr_of_temp (resultVar, 0, info.alignment);
      p.finalVal := CG.Pop ();
    END;
  END InnerPrepLV;

(* Externally dispatched-to: *)
PROCEDURE Compile (p: P) =
  BEGIN
    CompileLV (p, traced := FALSE);
  END Compile;

(* Externally dispatched-to: *)
PROCEDURE CompileLV (p: P; traced: BOOLEAN) =
  VAR info: Type.Info;  offset: INTEGER;
  BEGIN
    IF UsesAssignProtocol (p)
    THEN (* InnerPrep was postponed until now. *)
      InnerPrepLV (p, traced)
    END;
    IF (p.is_const) THEN
      EVAL Type.CheckInfo (p.type, info);
      offset := Module.Allocate (info.size, info.alignment, TRUE, "*record*");
      PrepLiteral (p, p.tipe, TRUE);
      GenLiteral (p, offset, p.tipe, TRUE);
      CG.Load_addr_of (Module.GlobalData (TRUE), offset, info.alignment);
    ELSE
      CG.Push (p.finalVal);
      DEC (p.finalValUseCt);
      IF (p.finalValUseCt <= 0) THEN
        CG.Free (p.finalVal);
        p.finalVal := NIL;
      END;
    END;
  END CompileLV;

(* Externally dispatched-to: *)
PROCEDURE Evaluate (p: P): Expr.T =
(* Return a constant expr if p is constant, otherwise NIL. *)
(* NOTE: This will fold any constant argument in place, even if the
         whole constructor is not constant. *)
  VAR e: Expr.T;
  BEGIN
    IF (NOT p.evalAttempted) THEN
      p.evalAttempted   := TRUE;
      p.is_const := TRUE;
      FOR i := 0 TO LAST (p.args^) DO
        e := Expr.ConstValue (p.args[i]);
        IF (e = NIL) THEN p.is_const := FALSE; ELSE p.args[i] := e; END;
      END;
    END;
    IF p.is_const
      THEN RETURN p;
      ELSE RETURN NIL;
    END;
  END Evaluate;

(* Externally dispatched-to: *)
PROCEDURE IsZeroes (p: P;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    <* ASSERT p.map # NIL *> (* must already be checked *)
    FOR i := 0 TO LAST (p.map^) DO
      IF NOT Expr.IsZeroes (p.map^[i].expr) THEN RETURN FALSE END;
    END;
    RETURN TRUE;
  END IsZeroes;

(* Externally dispatched-to: *)
PROCEDURE GenFPLiteral (p: P;  buf: M3Buf.T) =
  BEGIN
    M3Buf.PutText (buf, "RECORD<");
    FOR i := 0 TO LAST (p.map^) DO
      IF (i > 0) THEN M3Buf.PutChar (buf, ',') END;
      Expr.GenFPLiteral (p.map^[i].expr, buf);
    END;
    M3Buf.PutChar (buf, '>');
  END GenFPLiteral;

(* Externally dispatched-to: *)
PROCEDURE PrepLiteral (p: P;   <*UNUSED*> type: Type.T;  is_const: BOOLEAN) =
  VAR e: Expr.T;  fieldInfo: Field.Info;
  BEGIN
    <* ASSERT p.map # NIL *> (* must already be checked *)
    FOR i := 0 TO LAST (p.map^) DO
      WITH z = p.map^[i] DO
        e := Expr.ConstValue (z.expr);  <* ASSERT e # NIL *>
        IF NOT Expr.IsZeroes (e) THEN
          Field.Split (z.field, fieldInfo);
          ArrayExpr.NoteTargetType (e, fieldInfo.type);
          Expr.PrepLiteral (e, fieldInfo.type, is_const);
        END;
      END;
    END;
  END PrepLiteral;

(* Externally dispatched-to: *)
PROCEDURE GenLiteral (p: P;  offset: INTEGER;  <*UNUSED*> type: Type.T;
                      is_const: BOOLEAN) =
  VAR e: Expr.T;  fieldInfo: Field.Info;
  BEGIN
    <* ASSERT p.map # NIL *> (* must already be checked *)
    FOR i := 0 TO LAST (p.map^) DO
      WITH z = p.map^[i] DO
        e := Expr.ConstValue (z.expr);  <* ASSERT e # NIL *>
        IF NOT Expr.IsZeroes (e) THEN
          Field.Split (z.field, fieldInfo);
          Expr.GenLiteral
            (e, offset + fieldInfo.offset, fieldInfo.type, is_const);
        END;
      END;
    END;
  END GenLiteral;

BEGIN
END RecordExpr.
