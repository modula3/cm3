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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE M3CActualUtil;


IMPORT M3AST_LX, M3AST_AS, M3AST_SM;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_EXP, SeqM3AST_AS_CONS_ELEM, SeqM3AST_AS_Actual;

IMPORT M3Error, M3Assert, M3CTypeRelation, M3CTypeChkUtil, M3CExpsMisc,
    M3CDef, M3CSrcPos;


PROCEDURE Passable(
    formalType: M3AST_SM.TYPE_SPEC_UNSET;
    actual: M3AST_AS.EXP;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF formalType = NIL THEN RETURN TRUE END;
    IF ISTYPE(formalType, M3AST_AS.Procedure_type) THEN
      (* don't want the check to see if 'actual' is top level which comes
       with 'EXPAssignable' *)
      RETURN M3CTypeRelation.Assignable(
          formalType, actual.sm_exp_type_spec, safe);
    ELSE
      RETURN M3CTypeChkUtil.EXPAssignable(formalType, actual, safe);
    END; (* if *)
  END Passable;


PROCEDURE AddDefault(
    call: M3AST_AS.Call;
    formal: M3AST_AS.Formal_param)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF formal.as_default # NIL THEN
      (* assuming defaults on VAR parameters are caught elsewhere *)
      SeqM3AST_AS_EXP.AddRear(call.sm_actual_s, formal.as_default);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END AddDefault;


PROCEDURE CheckIsVARActual(actual: M3AST_AS.EXP) RAISES {}=
  VAR
    writeable: BOOLEAN;
  BEGIN
    IF NOT (M3CExpsMisc.IsDesignator(actual, writeable) AND writeable) THEN
      M3Error.Report(actual,
          "argument to VAR parameter is not writeable designator");
    END; (* if *)
  END CheckIsVARActual;


PROCEDURE TooFewArguments(call: M3AST_AS.Call) RAISES {}=
  BEGIN
    M3Error.Report(call, "too few arguments for procedure call")
  END TooFewArguments;


PROCEDURE TooManyArguments(call: M3AST_AS.Call) RAISES {}=
  BEGIN
    M3Error.Report(call, "too many arguments for procedure call")
  END TooManyArguments;


PROCEDURE ArgumentIsWrongType(exp: M3AST_AS.EXP) RAISES {}=
  BEGIN
    M3Error.Report(exp, "argument is wrong type");
  END ArgumentIsWrongType;


TYPE
  LotsOfActuals = [0..15];
  (* the procedures below cope with more than 15 elements, but they slow down
   a little *)

  SetOfLotsOfActuals = SET OF LotsOfActuals;

  ExpAndId = RECORD
    exp: M3AST_AS.EXP;
    id: M3AST_AS.Exp_used_id;
    hashId: M3AST_LX.Symbol_rep;
  END; (* record *)


REVEAL
  List = BRANDED REF RECORD
    next: List;
    count, positionals: INTEGER;
    matched: SetOfLotsOfActuals;
    array: ARRAY LotsOfActuals OF ExpAndId;
  END; (* record *)


TYPE
  BuildingRec = RECORD
    first, last: List := NIL;
    positional := TRUE;
  END; (* record *)


CONST
  AllMatched =
      SetOfLotsOfActuals{FIRST(LotsOfActuals)..LAST(LotsOfActuals)};


EXCEPTION
  FatalActualError;


PROCEDURE NewListRec(): List RAISES {}=
  VAR
    new := NEW(List);
  BEGIN
    new.next := NIL;
    new.count := 0;
    new.positionals := 0;
    new.matched := AllMatched;
    RETURN new;
  END NewListRec;


PROCEDURE AddExpAndId(
    exp: M3AST_AS.EXP;
    id: M3AST_AS.Exp_used_id;
    VAR b: BuildingRec)
    RAISES {}=
  BEGIN
    IF (b.first = NIL) OR (b.last.count > LAST(b.last.array)) THEN
      WITH new = NewListRec() DO
        IF b.first = NIL THEN b.first := new ELSE b.last.next := new END;
        b.last := new;
      END;
    END;
    VAR
      last := b.last;
    BEGIN
      WITH expAndId = last.array[last.count] DO
        expAndId.exp := exp;
        expAndId.id := id;
        IF id # NIL THEN
          expAndId.hashId := id.vUSED_ID.lx_symrep;
        ELSE
          expAndId.hashId := NIL;
        END;
      END;
      IF id = NIL THEN
        IF b.positional THEN
          INC(last.positionals);
          last.matched := last.matched - SetOfLotsOfActuals{last.count};
        ELSE
          M3Error.Report(exp,
              "positional items must precede those with keywords");
        END; (* if *)
      ELSE (* is not positional *)
        last.matched := last.matched - SetOfLotsOfActuals{last.count};
        b.positional := FALSE;
      END; (* if *)
      INC(last.count);
    END;
  END AddExpAndId;


PROCEDURE AddTypeSpec(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    pos: M3CSrcPos.T;
    VAR b: BuildingRec)
    RAISES {}=
  VAR
    t: M3AST_SM.TypeActual := NEW(M3AST_SM.TypeActual).init();
  BEGIN
    t.lx_srcpos := pos;
    t.sm_exp_type_spec := ts;
    AddExpAndId(t, NIL, b);
  END AddTypeSpec;


PROCEDURE AddActual(
    actual: M3AST_AS.Actual;
    VAR b: BuildingRec;
    typeOk := FALSE)
    RAISES {}=
  VAR
    id: M3AST_AS.Exp_used_id;
  BEGIN
    TYPECASE actual.as_id OF
    | NULL =>
        id := NIL;
    | M3AST_AS.Exp_used_id(expUsedId) =>
        id := expUsedId;
        typeOk := FALSE;
    ELSE
      M3Error.Report(actual.as_id, "expression not bound to valid keyword");
      id := NIL;
    END;
    TYPECASE actual.as_exp_type OF <*NOWARN*>
    | M3AST_AS.EXP(exp) =>
        WITH class =  M3CExpsMisc.Classify(exp) DO
          IF class = M3CExpsMisc.Class.Normal THEN
            AddExpAndId(exp, id, b);
          ELSIF typeOk AND class = M3CExpsMisc.Class.Type THEN
            VAR
              defId: M3AST_AS.DEF_ID;
            BEGIN
              M3Assert.Check(M3CExpsMisc.IsId(exp, defId) AND
                  ISTYPE(defId, M3AST_AS.Type_id));
              AddTypeSpec(NARROW(defId, M3AST_AS.Type_id).sm_type_spec,
                  exp.lx_srcpos, b);
            END;
          ELSE
            M3CExpsMisc.WrongClass(exp, class);
            AddExpAndId(NIL, id, b);
          END;
        END;
    | M3AST_AS.M3TYPE(m3Type) =>
        IF typeOk THEN
          TYPECASE m3Type OF <*NOWARN*>
          | M3AST_AS.Bad_M3TYPE =>
              AddTypeSpec(NIL, m3Type.lx_srcpos, b);
          | M3AST_AS.TYPE_SPEC(typeSpec) =>
              AddTypeSpec(typeSpec, typeSpec.lx_srcpos, b);
          END;
        ELSE
          M3CExpsMisc.WrongClass(m3Type, M3CExpsMisc.Class.Type);
          AddExpAndId(NIL, id, b);
        END;
    END; (* if *)
  END AddActual;


PROCEDURE ElementList(cons: M3AST_AS.Constructor): List RAISES {}=
  VAR
    b := BuildingRec{};
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(cons.as_element_s);
    element: M3AST_AS.CONS_ELEM;
  BEGIN
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, element) DO
      TYPECASE element OF <*NOWARN*>
      | M3AST_AS.Actual_elem(aElem) =>
          AddActual(aElem.as_actual, b);
      | M3AST_AS.RANGE_EXP_elem(rElem) =>
          TYPECASE rElem.as_range_exp OF
          | M3AST_AS.Range_EXP(rangeExp) =>
              AddExpAndId(rangeExp.as_exp, NIL, b);
          ELSE
            M3Error.Report(rElem, "range not allowed in record constructor");
          END;
      END;
    END;
    RETURN b.first;
  END ElementList;


PROCEDURE ActualList(call: M3AST_AS.Call; typeOk := FALSE): List RAISES {}=
  VAR
    b := BuildingRec{};
    s: SeqM3AST_AS_Actual.T := NIL;
    iter: SeqM3AST_AS_Actual.Iter;
    actual: M3AST_AS.Actual;
  BEGIN
    TYPECASE call OF
    | M3AST_AS.NEWCall(newcall) => s := newcall.sm_norm_actual_s;
    ELSE
    END;
    IF s = NIL THEN s := call.as_param_s END;
    iter := SeqM3AST_AS_Actual.NewIter(s);
    WHILE SeqM3AST_AS_Actual.Next(iter, actual) DO
      AddActual(actual, b, typeOk);
    END;
    RETURN b.first;
  END ActualList;


PROCEDURE TotalActuals(a: List): INTEGER RAISES {}=
  BEGIN
    IF a = NIL THEN
      RETURN 0;
    ELSIF a.next = NIL THEN
      RETURN a.count;
    ELSE
      RETURN NUMBER(LotsOfActuals) + TotalActuals(a.next);
    END; (* if *)
  END TotalActuals;


PROCEDURE PositionalActuals(a: List): INTEGER RAISES {}=
  BEGIN
    IF a = NIL THEN
      RETURN 0;
    ELSIF (a.count > a.positionals) OR (a.next = NIL) THEN
      RETURN a.positionals;
    ELSE
      RETURN NUMBER(LotsOfActuals) + PositionalActuals(a.next);
    END; (* if *)
  END PositionalActuals;


PROCEDURE FindByKeyword(
    keyword: M3AST_LX.Symbol_rep;
    VAR a: List;
    VAR pos: INTEGER)
    : BOOLEAN
    RAISES {}=
  <*FATAL FatalActualError*>
  BEGIN
    IF (a = NIL) OR (keyword = NIL) THEN
      RETURN FALSE;
    ELSE
      LOOP
        IF pos >= a.count THEN
          IF pos > a.count THEN RAISE FatalActualError END;
          a := a.next;
          IF a = NIL THEN RETURN FALSE END;
          pos := 0;
        END; (* if *)
        IF a.array[pos].hashId = keyword AND (* conjunct added MJJ 10/21/92 *)
           NOT(pos IN a.matched) THEN
          RETURN TRUE;
        ELSE
          INC(pos);
          (* and loop *)
        END; (* if *)
      END; (* loop *)
    END; (* if *)
  END FindByKeyword;


PROCEDURE MarkAsMatchedAndLookForDuplicates(
    keyword: M3AST_LX.Symbol_rep;
    VAR a: List;
    VAR pos: INTEGER)
    RAISES {}=
  BEGIN
    LOOP
      a.matched := a.matched + SetOfLotsOfActuals{pos};
      INC(pos);
      IF FindByKeyword(keyword, a, pos) THEN
        WITH expAndId = a.array[pos] DO
          M3Error.ReportWithId(expAndId.id,
            "\'%s\' has already been bound", expAndId.hashId);
        END;
      ELSE
        EXIT;
      END; (* if *)
    END; (* loop *)
  END MarkAsMatchedAndLookForDuplicates;


PROCEDURE ActualAt(
    a: List;
    pos: INTEGER;
    id: M3AST_LX.Symbol_rep)
    : M3AST_AS.EXP
    RAISES {}=
  <*FATAL FatalActualError*>
  BEGIN
    LOOP
      IF (pos < 0) OR (a = NIL) THEN
        RAISE FatalActualError;
      ELSIF pos < a.positionals THEN
        WITH expAndId = a.array[pos] DO
          MarkAsMatchedAndLookForDuplicates(id, a, pos);
          RETURN expAndId.exp;
        END;
      ELSE
        DEC(pos, NUMBER(LotsOfActuals));
        a := a.next;
      END; (* if *)
    END; (* loop *)
  END ActualAt;


PROCEDURE ActualByKeyword(
    a: List;
    typedId: M3AST_AS.TYPED_ID;
    VAR exp: M3AST_AS.EXP)
    : BOOLEAN
    RAISES {}=
  VAR
    position: INTEGER;
  BEGIN
    IF a = NIL THEN
      RETURN FALSE;
    ELSE
      position := a.positionals;
      IF FindByKeyword(typedId.lx_symrep, a, position) THEN
        WITH expAndId = a.array[position] DO
          exp := expAndId.exp;
          M3CDef.ResolveActualKeyword(expAndId.id, typedId);
          expAndId.id.sm_exp_type_spec := typedId.sm_type_spec;
        END;
        MarkAsMatchedAndLookForDuplicates(typedId.lx_symrep, a, position);
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END; (* if *)
    END; (* if *)
  END ActualByKeyword;


PROCEDURE FindUnmatched(a: List) RAISES {}=
  BEGIN
    WHILE a # NIL DO
      IF a.matched # AllMatched THEN
        FOR m := VAL(a.positionals, LotsOfActuals) TO LAST(LotsOfActuals) DO
          IF NOT (m IN a.matched) THEN
            WITH expAndId = a.array[m] DO
              M3Error.ReportWithId(expAndId.id,
                "no match found for keyword \'%s\'", expAndId.hashId);
            END;
          END; (* if *)
        END; (* for *)
      END; (* if *)
      a := a.next;
    END; (* while *)
  END FindUnmatched;


PROCEDURE OriginalActual(
    call: M3AST_AS.Call;
    pos: INTEGER)
    : M3AST_AS.Actual
    RAISES {}=
  VAR
    count := 0;
    iter := SeqM3AST_AS_Actual.NewIter(call.as_param_s);
    actual: M3AST_AS.Actual;
  BEGIN
    WHILE SeqM3AST_AS_Actual.Next(iter, actual) DO
      IF count = pos THEN RETURN actual ELSE INC(count) END;
    END; (* while *)
    M3Assert.Fail();
    <*ASSERT FALSE*>
  END OriginalActual;


BEGIN
END M3CActualUtil.
