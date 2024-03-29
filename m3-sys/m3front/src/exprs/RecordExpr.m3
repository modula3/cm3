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
(* TODO: Eliminate p.type/p.tipe redundancy. *)
        args          : Expr.List := NIL;
        map           : REF ARRAY OF Info := NIL;
        constExpr     : Expr.T;
        finalVal      : CG.Val := NIL;
        finalValUseCt : INTEGER := 0;
        globalOffset  : INTEGER := FIRST (INTEGER) (* Means uninitialized. *);
        RTErrorMsg    : TEXT := NIL;
        RTErrorCode   := CG.RuntimeError.Unknown;
        evalAttempted : BOOLEAN := FALSE;
        (* TRUE even if Evaluate was called unsuccessfully. *)
        is_const      : BOOLEAN := FALSE;
        (* Meaningless if NOT evalAttempted.  Otherwise, Evaluate was successful. *)
        broken        : BOOLEAN := FALSE;
        prepped       : BOOLEAN := FALSE;
      OVERRIDES
        typeOf       := ExprRep.NoType;
        repTypeOf    := ExprRep.NoType;
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
        checkUseFailure := CheckUseFailure;
      END;

(* EXPORTED: *)
PROCEDURE New (type: Type.T;  args: Expr.List): Expr.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    ExprRep.Init (p);
    p.directAssignableType := TRUE;

    p.type           := type;
    p.repType        := Type.StripPacked (type);
    p.tipe           := type;
    p.args       := args;
    p.map            := NIL;
    p.constExpr      := NIL;
    p.finalVal       := NIL;
    p.finalValUseCt  := 0;
    p.evalAttempted  := FALSE;
    p.is_const       := FALSE;
    p.prepped        := FALSE;
    p.checked        := FALSE;
    p.RTErrorCode    := CG.RuntimeError.Unknown;
    p.RTErrorMsg     := NIL;
    RETURN p;
  END New;

PROCEDURE Copy (p: P): Expr.T =
  VAR new: P;
  BEGIN
    new := NEW (P);

    (* Fields inherited from Expr.T: *)
    new.origin := p.origin;
    new.type := p.type;
    new.align := p.align;
    new.checked := p.checked;
    new.directAssignableType := p.directAssignableType;
    new.doDirectAssign := p.doDirectAssign;
    new.isNamedConst := p.isNamedConst;

    (* Fields of P: *)
    new.directAssignableType := p.directAssignableType;
    new.type := p.type;
    new.repType := p.repType;
    new.tipe := p.tipe;
    new.args := p.args;
    new.map := p.map;
    new.constExpr := p.constExpr;
    new.finalVal := p.finalVal;
    new.finalValUseCt := p.finalValUseCt;
    new.evalAttempted := p.evalAttempted;
    new.is_const := p.is_const;
    new.prepped := p.prepped;
    new.checked := p.checked;
    new.RTErrorCode := p.RTErrorCode;
    new.RTErrorMsg := p.RTErrorMsg;
    new.broken := p.broken;
    new.globalOffset := p.globalOffset;
    RETURN p;
  END Copy;

(* EXPORTED: *)
PROCEDURE Is (e: Expr.T): BOOLEAN =
(* Purely syntactic. Will not look through a ConsExpr. *)
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
      argExpr := p.args[i]; (* Don't use a folded value of field. *)
      IF (KeywordExpr.Split (argExpr, argId, splitExpr)) THEN
        IF (argId = id) THEN result := splitExpr; RETURN TRUE END;
      ELSIF (i = fieldInfo.index) THEN
        result := argExpr;
        RETURN TRUE;
      END;
    END;
    IF fieldInfo.dfault # NIL THEN
      result := Expr.ConstValue (fieldInfo.dfault);
      RETURN result # NIL;
    END;
    RETURN FALSE;
  END Qualify;

PROCEDURE MergeRTError (p: P; Code: CG.RuntimeError; Msg: TEXT) =
  BEGIN
    IF p.RTErrorMsg = NIL THEN
      p.RTErrorCode := Code;
      p.RTErrorMsg := Msg;
    END;
  END MergeRTError;

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
    RTErrorCode: CG.RuntimeError;
    RTErrorMsg: TEXT;
  BEGIN
    p.tipe := Type.Check (p.tipe);
    FOR i := 0 TO LAST (p.args^) DO Expr.TypeCheck (p.args[i], cs) END;
    p.type := p.tipe;
    IF NOT RecordType.Split (p.tipe, fieldList) THEN
(* CHECK: Can't we ASSERT FALSE here? *)
      Error.Msg ("Record constructor must specify a record type (2.6.8)");
      p.broken := TRUE;
      p.checked := TRUE;
      RETURN;
    END;

    (* Count the fields of the record type. *)
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
    EVAL Evaluate (p); (* Fold arguments, if p is constant. *)

    FOR i := 0 TO LAST (p.args^) DO
      e := p.args[i];
      IF RangeExpr.Split (e, splitExpr, dfault) THEN
        Error.Msg
          ("Range expressions not allowed in record constructors (2.6.8).");
        p.broken := TRUE;
      END;

      IF KeywordExpr.Split (e, Id, splitExpr) THEN
        posOK := FALSE;
        fieldNo := 0;
        e := splitExpr;
        LOOP
          IF (fieldNo >= fieldCt) THEN
            Error.ID (Id, "Unknown field name in record constructor (2.6.8).");
            fieldNo := i;
            p.broken := TRUE;
            EXIT;
          END;
          IF (p.map^[fieldNo].name = Id) THEN EXIT END;
          INC (fieldNo);
        END;
      ELSE (* positional parameter *)
        IF (NOT posOK) THEN
          Error.Msg ("Positional values must precede keyword values (2.6.8).");
        END;
        IF (i >= fieldCt) THEN
            Error.Msg ("Too many values in record constructor (2.6.8).");
            fieldNo := fieldCt - 1;
            p.broken := TRUE;
          ELSE  fieldNo := i;
        END;
      END;

      IF (0 <= fieldNo) AND (fieldNo < fieldCt) THEN
        WITH z = p.map^[fieldNo] DO
          IF (z.done) THEN
            Error.ID
              (z.name, "Record constructor field previously specified (2.6.8).");
          END;
          z.done := TRUE;
          IF NOT Type.IsAssignable (z.type, Expr.TypeOf (e)) THEN
            Error.ID
              (z.name,
               "Expression is not assignable to record constructor field  (2.6.8).");
          ELSE
            ArrayExpr.NoteUseTargetVar (e);

            AssignStmt.CheckStaticRTErrExec
              (z.type, e, cs,
               (*VAR*)Code := RTErrorCode, (*VAR*)Msg := RTErrorMsg
              );
            MergeRTError (p, RTErrorCode, RTErrorMsg);
            z.expr := e;
          END;
        END;
      ELSE
        (* some other error, so don't even try *)
      END;
    END (*FOR args*);

    FOR fieldNo := 0 TO fieldCt - 1 DO
      WITH z = p.map^[fieldNo] DO
        IF (NOT z.done) AND (z.expr = NIL) THEN
          Error.ID
            (z.name, "No value specified for record constructor field (2.6.8).");
        END;
      END;
    END;
    p.checked := TRUE;
  END Check;

(* Externally dispatched-to: *)
PROCEDURE EqCheck (a: P;  e: Expr.T;  x: M3.EqAssumption): BOOLEAN =
(* TODO: This requires exact keyword/isPositional/positionalOrder/default match
         to return TRUE.  This is probably too picky. *)
  VAR b: P;
  BEGIN
    TYPECASE e OF
    | NULL => RETURN FALSE;
    | P(p) => b := p;
    ELSE      RETURN FALSE;
    END;
    IF (NOT Type.IsEqual (a.tipe, b.tipe, x))
      OR ((a.args = NIL) # (b.args = NIL))
      OR ((a.args # NIL) AND (NUMBER (a.args^) # NUMBER (b.args^)))
    THEN
      RETURN FALSE;
    END;
    FOR i := 0 TO LAST (a.args^) DO
      IF NOT Expr.IsEqual (a.args[i], b.args[i], x)
      THEN RETURN FALSE
      END;
    END;
    RETURN TRUE;
  END EqCheck;

(* Externally dispatched-to: *)
PROCEDURE NeedsAddress (<*UNUSED*> p: P) =
  BEGIN
    (* yep, we already know all records get memory addresses *)
  END NeedsAddress;

(* Externally dispatched-to: *)
PROCEDURE UsesAssignProtocol (p: P): BOOLEAN =
(* PRE: p has been checked but not Prepped. *)
  BEGIN
    <*ASSERT p.checked*>
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
    THEN InnerPrepLV (p, traced, usesAssignProtocol := FALSE)
 (* ELSE postpone InnerPrepLV until Compile, when LHS will have been pushed. *)
    END;
  END PrepLV;

PROCEDURE InnerPrepLV (p: P;  traced: BOOLEAN; usesAssignProtocol: BOOLEAN) =
  (* PRE: IF usesAssignProtocol (p), LHS address is compiled and on top of the
          CG stack, for us to use. *)
  VAR
    info: Type.Info;
    field: Field.Info;
    resultVar: CG.Var;
  BEGIN
    INC (p.finalValUseCt);
    IF (p.finalVal # NIL) AND (p.finalValUseCt > 1)
    THEN (* We already did this before. *)
      <* ASSERT NOT usesAssignProtocol *>
      p.prepped := TRUE;
      RETURN
    END;
    EVAL Type.CheckInfo (p.type, info);

    IF p.is_const THEN
      p.prepped := TRUE;
      RETURN
    END;
    IF usesAssignProtocol THEN (* lhs addr is on tos. *)
      <* ASSERT NOT traced *>             (* CHECKME? *)
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
        AssignStmt.DoEmit (field.type, z.expr, initializing := TRUE);
      END;
    END;

    IF usesAssignProtocol THEN
      (* result is already in p.finalVal *)
    ELSE
      CG.Load_addr_of_temp (resultVar, 0, info.alignment);
      p.finalVal := CG.Pop ();
    END;
    p.prepped := TRUE;
  END InnerPrepLV;

(* Externally dispatched-to: *)
PROCEDURE Compile (p: P; StaticOnly: BOOLEAN) =
  BEGIN
    CompileLV (p, traced := FALSE, StaticOnly := StaticOnly);
  END Compile;

(* Externally dispatched-to: *)
PROCEDURE CompileLV (p: P; traced: BOOLEAN; StaticOnly: BOOLEAN) =
  VAR info: Type.Info;
  BEGIN
    <* ASSERT NOT StaticOnly *>
(* NOTE^ If ever duplicate static records are removed as for arrays, this
    will need to change. *)
    IF UsesAssignProtocol (p)
    THEN (* InnerPrep was postponed until now. *)
      InnerPrepLV (p, traced, usesAssignProtocol := TRUE)
    END;
    IF (p.is_const) THEN
      EVAL Type.CheckInfo (p.type, info);
      p.globalOffset
        := Module.Allocate
             (info.size, info.alignment, TRUE, "*recordConstructor*");
      PrepLiteral (p, p.tipe, TRUE);
      GenLiteral (p, p.globalOffset, p.tipe, TRUE);
      CG.Load_addr_of
        (Module.GlobalData (TRUE), p.globalOffset, info.alignment);
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

  VAR new: P;
  VAR constArgs: Expr.List := NIL;
  VAR anArgFolded: BOOLEAN;
  BEGIN
    IF p.evalAttempted
    THEN RETURN p.constExpr;
    ELSE
      p.evalAttempted   := TRUE;
      constArgs := NEW(Expr.List, NUMBER(p.args^));
      p.is_const := TRUE;
      anArgFolded := FALSE;
      FOR i := 0 TO LAST (p.args^) DO
        WITH origArgi = p.args^[i], constArgi = constArgs^[i]
        DO
          constArgi := Expr.ConstValue (origArgi);
          IF constArgi = NIL
          THEN
            p.constExpr := NIL;
            p.is_const := FALSE;
            RETURN NIL;
          ELSIF constArgi # origArgi
          THEN anArgFolded := TRUE;
          END;
        END (*WITH*);
      END;
      IF anArgFolded THEN
        new := Copy (p);
        new.args := constArgs;
        p.constExpr := new;
        RETURN new;
      ELSE
        p.constExpr := p;
        RETURN p;
      END;
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

PROCEDURE RecConstrExpr (expr: Expr.T): P =
(* Look through a NamedExpr and then a ConsExpr, for a RecordExpr.T.
   NIL if no RecordExpr.T. *)

  VAR strippedExpr: Expr.T;
  BEGIN
    strippedExpr := Expr.StripNamedCons (expr);
    TYPECASE strippedExpr OF
    | NULL => RETURN NIL;
    | P (recordExpr) => RETURN recordExpr;
    ELSE RETURN NIL;
    END;
  END RecConstrExpr;

(*EXPORTED:*)
PROCEDURE CheckStaticRTErrEval
  (expr: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT) =
(* Set Code and Msg if they are not set and expr is known to produce a
   statically unconditional runtime error when evaluated. *)
(* Return the first-discovered error found and stored during Check. *)
  VAR constrExpr: P;
  BEGIN
    IF Code # CG.RuntimeError.Unknown THEN RETURN END;
    constrExpr := RecConstrExpr (expr);
    TYPECASE constrExpr OF
    | NULL =>
    | P(p) =>
      <* ASSERT p.checked *>
      IF p.RTErrorCode # CG.RuntimeError.Unknown THEN
        Code := p.RTErrorCode;
        Msg := p.RTErrorMsg;
      END;
    END;
  END CheckStaticRTErrEval;

(* Externally dispatched-to: *)
PROCEDURE CheckUseFailure (p: P): BOOLEAN =
  BEGIN
    <* ASSERT p.checked *>
    IF AssignStmt.DoGenRTAbort (p.RTErrorCode) AND Evaluate (p) # NIL THEN
      CG.Comment
        (p.globalOffset, TRUE,
         "Use of record constructor with statically detected runtime error: ",
         p.RTErrorMsg);
      CG.Abort (p.RTErrorCode);
      RETURN FALSE;
    ELSE RETURN TRUE;
    END;
  END CheckUseFailure;

BEGIN
END RecordExpr.
