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
(* Last modified on Fri Apr 19 17:25:31 PDT 1996 by detlefs    *)


MODULE M3CBackEnd_C  EXPORTS M3CBackEnd, M3CBackEnd_C;

(* This module defines the interface for the information required by
the compiler front-end from a back-end . *)

(* ToDo: all the bootstrap things like FIRST(REAL) *)

(* Version targeted to C back-end for a variety of machines *)

IMPORT M3CBackEnd;
IMPORT Fmt, Text, TextF, TextRd, TextWr, Rd, Wr, Word, Convert, RdExtras;
IMPORT M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_RANGE_EXP, SeqM3AST_AS_Fields;
 
IMPORT M3CId, M3CLiteral;
IMPORT M3Error, M3Assert, M3CSrcPos;
IMPORT M3CStdProcs, M3CWordProcs;
IMPORT M3CExpValue, M3CTypesMisc, M3ASTNext;
FROM M3CBackEnd_C_cc IMPORT a32, a64, a16, a8, minAlignment, recAlignment,
  arrayAlignment, ptrA, ptrS, realA, realS, longRealA, longRealS, intA, intS;

IMPORT M3CBackEnd_Float_Real, M3CBackEnd_Float_LongReal,
    M3CBackEnd_Float_Extended;

(* For FATAL *)
IMPORT Thread;

<*FATAL Thread.Alerted, Wr.Failure, Rd.Failure, Rd.EndOfFile, Convert.Failed *>


CONST
  NilValue = 0;
  False = ORD(FALSE);
  True = ORD(TRUE);

(* This does the alignments for ordinals. *)
PROCEDURE OrdAlign(size: INTEGER): INTEGER RAISES {}=
  BEGIN
    IF size = 0 THEN
      RETURN 0
    ELSIF size <= 8 THEN RETURN a8
    ELSIF size <= 16 THEN RETURN a16
    ELSIF size <= 32 THEN RETURN a32
    ELSE  RETURN a64 

    END; (* if *)
  END OrdAlign;

(* This does the alignments for sets. *)
PROCEDURE SetAlign(VAR (*inout*) size: INTEGER; exact: BOOLEAN): INTEGER =
  BEGIN
    IF NOT exact THEN (* represented as arrays of ints *)
      size := AlignTo(size, intS);
    END; (* if *)
    RETURN MAX(intA, recAlignment);
  END SetAlign;

PROCEDURE RegisterProcs() RAISES {}=
  BEGIN
    M3CBackEnd.LiteralValue:= LiteralValue_C;
    M3CBackEnd.ConstructorValue:= ConstructorValue_C;
    M3CBackEnd.ConstructorOriginal:= ConstructorOriginal_C;
    M3CBackEnd.IsOrdinal := IsOrdinal_C;
    M3CBackEnd.Ord:= Ord_C;
    M3CBackEnd.Val:= Val_C;
    M3CBackEnd.ConvertOrdinal := ConvertOrdinal_C;
    M3CBackEnd.BinaryOp:= BinaryOp_C;
    M3CBackEnd.InOp:= InOp_C;
    M3CBackEnd.UnaryOp:= UnaryOp_C;
    M3CBackEnd.StdUnaryOp:= StdUnaryOp_C;
    M3CBackEnd.StdBinaryOp:= StdBinaryOp_C;
    M3CBackEnd.StdUnaryTypeOp:= StdUnaryTypeOp_C;
    M3CBackEnd.WordOp := WordOp_C;
    M3CBackEnd.Compare:= Compare_C;
    M3CBackEnd.LoopholeOK:= LoopholeOK_C;
    M3CBackEnd.BitsOK:= BitsOK_C;
    M3CBackEnd.VarParamOK:= VarParamOK_C;
    M3CBackEnd.BitSizeAndAlign:= BitSizeAndAlign_C;
    M3CBackEnd.ExpValueToText := ExpValueToText_C;
    M3CBackEnd.TextToExpValue := TextToExpValue_C;
  END RegisterProcs;


<*INLINE*> PROCEDURE SimpleNewInteger_value(int: INTEGER): Integer_value RAISES {}=
  BEGIN
    RETURN NEW(Integer_value, sm_value := int);
  END SimpleNewInteger_value;

VAR
  smallIntegers_g := ARRAY [0..255] OF Integer_value {NIL, ..};
  firstInteger_g := SimpleNewInteger_value(FIRST(INTEGER));
  lastInteger_g := SimpleNewInteger_value(LAST(INTEGER));

PROCEDURE NewInteger_value(int: INTEGER): Integer_value RAISES {}=
  BEGIN
    IF FIRST(smallIntegers_g) <= int AND int <= LAST(smallIntegers_g) THEN
      WITH result = smallIntegers_g[int] DO
        IF result = NIL THEN result := SimpleNewInteger_value(int) END;
        RETURN result;
      END;
    ELSIF int = FIRST(INTEGER) THEN
      RETURN firstInteger_g;
    ELSIF int = LAST(INTEGER) THEN
      RETURN lastInteger_g;
    ELSE
      RETURN SimpleNewInteger_value(int);
    END;
  END NewInteger_value;

CONST
  BadBits = -1;

PROCEDURE SizeInBits(ts: M3AST_SM.TYPE_SPEC_UNSET;
    exact := FALSE): INTEGER RAISES {}=
  BEGIN
    IF ts = NIL THEN
      RETURN BadBits
    ELSE
      MayBeExactBitSizeAndAlign(ts, exact);
      RETURN NARROW(ts, M3AST_AS.TYPE_SPEC).sm_bitsize;
    END;
  END SizeInBits;

PROCEDURE BitSizeAndAlign_C(ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  BEGIN
    MayBeExactBitSizeAndAlign(ts);
  END BitSizeAndAlign_C;

PROCEDURE MayBeExactBitSizeAndAlign(ts: M3AST_AS.TYPE_SPEC;
    exact := FALSE) RAISES {}=
  VAR
    array_element, array_index: M3AST_SM.TYPE_SPEC_UNSET;
    size, align: INTEGER;
    s1, s2: INTEGER;
    isopen: BOOLEAN;
  BEGIN
    size := 0; align := 0;

    TYPECASE ts OF <*NOWARN*>
    | M3AST_AS.Ref_type,
      M3AST_AS.RefAny_type,
      M3AST_AS.Root_type,
      M3AST_AS.Address_type,
      M3AST_AS.Null_type,
      M3AST_AS.Opaque_type,
      M3AST_AS.Procedure_type =>
        size := ptrS;
        align := ptrA;

    | M3AST_AS.Integer_type =>
        size := intS;
        align := intA;

    | M3AST_AS.Real_type =>
        size := realS;
        align := realA;

    | M3AST_AS.LongReal_type, M3AST_AS.Extended_type =>
        size := longRealS;
        align := longRealA;

    | M3AST_AS.Enumeration_type(t) =>
        size := BitsForOrd(t.sm_num_elements-1, exact);
        align := OrdAlign(size);

    | M3AST_AS.Subrange_type(t) =>
        WITH range = t.as_range DO
          s1 := NARROW(range.as_exp1.sm_exp_value, Integer_value).sm_value;
          s2 := NARROW(range.as_exp2.sm_exp_value, Integer_value).sm_value;
        END;
        IF s1 <= s2 THEN
          IF s1 >= 0 THEN
            size := BitsForOrd(s2, exact);
          ELSE
            IF s2 < 0 THEN s2 := 0; END;
            size := 1 + MAX(BitsForOrd(-(s1+1), TRUE), BitsForOrd(s2, TRUE));
            IF NOT exact THEN size := ChooseByteHalfFull(size); END;
          END;
        END; (* if *)
        align := OrdAlign(size);

    | M3AST_AS.Set_type(t) =>
        GetSetBounds(t, s1, s2);
        IF s1 <= s2 THEN
          size := 1 + s2 - s1;
        END; (* if *)
        align := SetAlign(size, exact);

    | M3AST_AS.Packed_type(t) =>
        (* the check for legality is done elsewhere *)
        size := NARROW(t.as_exp.sm_exp_value, Integer_value).sm_value;
        align := 1;

    | M3AST_AS.Record_type(t) =>
        FieldsSizeAndAlign(t.as_fields_s, size, align);

    | M3AST_AS.Object_type(object_type) =>
        (* compute in place in case of self-reference *)
        size := ptrS; ts.sm_bitsize := size;
        align :=  ptrA; ts.sm_align := align;
        (* compute the referent size and alignment (for this component
        of the object; i.e. ignore supertypes). *)
        object_type.sm_rf_bitsize := 0;
        object_type.sm_rf_align := 0;
        FieldsSizeAndAlign(object_type.as_fields_s, 
            object_type.sm_rf_bitsize, object_type.sm_rf_align);

    | M3AST_AS.Array_type(t) =>
        EVAL M3ASTNext.Array(t, array_element, isopen, array_index);
        WITH elemTS = NARROW(array_element, M3AST_AS.TYPE_SPEC) DO
          align := MAX(elemTS.sm_align, arrayAlignment);
          IF isopen THEN
            (* leave size at 0 *)
          ELSE
            VAR
              low, high: M3AST_SM.Exp_value;
              s1, s2: INTEGER;
            BEGIN
              IF M3CExpValue.GetBounds(array_index, low, high) =
                    M3CBackEnd.NumStatus.Valid THEN
                s1 := NARROW(low, Integer_value).sm_value;
                s2 := NARROW(high, Integer_value).sm_value;
                IF s1 <= s2 THEN
                  size :=
                      AlignTo(elemTS.sm_bitsize, elemTS.sm_align) * (s2-s1+1);
                END;
              END;
            END;
          END;
        END;
    END; (* case *)

    ts.sm_bitsize := size;
    ts.sm_align := align;
  END MayBeExactBitSizeAndAlign;

PROCEDURE FieldsSizeAndAlign(fields: SeqM3AST_AS_Fields.T; 
    VAR (*inout*) size, align: INTEGER) RAISES {}=
  VAR
    field_id: M3AST_AS.Field_id;
    iter := M3ASTNext.NewIterField(fields);
  BEGIN
    WHILE M3ASTNext.Field(iter, field_id) DO
      size := AlignTo(size, field_id.sm_type_spec.sm_align);
      INC(size, field_id.sm_type_spec.sm_bitsize);
      align := MAX(align, field_id.sm_type_spec.sm_align);
    END;
    align := MAX(align, recAlignment);
  END FieldsSizeAndAlign;

PROCEDURE LiteralValue_C(lit: M3AST_AS.EXP;
    VAR (*out*) er: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {}=
  BEGIN
    TYPECASE lit OF <*NOWARN*>
    | M3AST_AS.Char_literal =>
        (* 'x' or '\x' or '\ddd' *)
        VAR
          cvi: INTEGER;
          t: TEXT := M3CLiteral.ToText(
              NARROW(lit, M3AST_AS.Char_literal).lx_litrep);
        BEGIN
          IF Text.GetChar(t, 1) = '\\' THEN
            (* escape *)
            CASE Text.GetChar(t, 2) OF
            | 'n' => cvi := ORD('\n');
            | 't' => cvi := ORD('\t');
            | 'r' => cvi := ORD('\r');
            | '\'' => cvi := ORD('\'');
            | '\"' => cvi := ORD('\"');
            | 'f' => cvi := ORD('\f');
            | '\\' => cvi := ORD('\\');
            ELSE  (* \ddd *)
              cvi := CHV(Text.GetChar(t, 2)) * 64 + 
                     CHV(Text.GetChar(t, 3)) * 8 + 
                     CHV(Text.GetChar(t, 4));
            END; (* case *)
          ELSE
            cvi := ORD(Text.GetChar(t, 1));
          END; (* if *)
          er := NewInteger_value(cvi);
        END;

    | M3AST_AS.Text_literal =>
        VAR tv := NEW(Text_value);
            t: TEXT := M3CLiteral.ToText(
                NARROW(lit, M3AST_AS.Text_literal).lx_litrep);
        BEGIN
          er := tv;
          tv.sm_value := Text.Sub(t, 1, Text.Length(t) - 2); 
        END;

    | M3AST_AS.Nil_literal =>
        er := NewInteger_value(NilValue);

    | M3AST_AS.Integer_literal =>
        VAR
          t: TEXT := M3CLiteral.ToText(
              NARROW(lit, M3AST_AS.Integer_literal).lx_litrep); 
          int: INTEGER;
        BEGIN
          IF NOT TextTo_Int(t, int) THEN
            RETURN M3CBackEnd.NumStatus.Overflow;
          END;
          er := NewInteger_value(int);
        END;

    | M3AST_AS.Real_literal =>
        VAR
	  real: REAL;
	  t := M3CLiteral.ToText(NARROW(lit, M3AST_AS.Real_literal).lx_litrep);
        BEGIN
          IF NOT TextTo_Real(t, real) THEN
            RETURN M3CBackEnd.NumStatus.Overflow;
          END;
          er := M3CBackEnd_Float_Real.New_value(real);
        END;

    | M3AST_AS.LongReal_literal =>
        VAR
	  longReal: LONGREAL;
	  t := M3CLiteral.ToText(
              NARROW(lit, M3AST_AS.LongReal_literal).lx_litrep);
        BEGIN
          IF NOT TextTo_LongReal(t, longReal) THEN
            RETURN M3CBackEnd.NumStatus.Overflow;
          END;
          er := M3CBackEnd_Float_LongReal.New_value(longReal)
        END;

    | M3AST_AS.Extended_literal =>
        VAR
	  extended: EXTENDED;
	  t := M3CLiteral.ToText(
              NARROW(lit, M3AST_AS.Extended_literal).lx_litrep);
        BEGIN
          IF NOT TextTo_Extended(t, extended) THEN
            RETURN M3CBackEnd.NumStatus.Overflow;
          END;
          er := M3CBackEnd_Float_Extended.New_value(extended)
        END;

    | M3AST_AS.Exp_used_id, M3AST_AS.Select =>
        (* procedure constants *)
        VAR
          idp, idu: M3AST_AS.Exp_used_id;
          pv := NEW(Proc_value);
        BEGIN
          er := pv;
          IF ISTYPE(lit, M3AST_AS.Select) THEN
            idp := NARROW(lit, M3AST_AS.Select).as_id;
            idu := NARROW(lit, M3AST_AS.Select).as_exp;
          ELSE
            idp := lit; idu := NIL;
          END; (* if *)
          pv.sm_value := M3CId.ToText(idp.vUSED_ID.lx_symrep);
          IF idu # NIL THEN
            pv.sm_value := M3CId.ToText(idu.vUSED_ID.lx_symrep) & Separator & 
              pv.sm_value;
          END; (* if *)        
        END;
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END LiteralValue_C;

<*INLINE*> PROCEDURE CHV(ch: CHAR): INTEGER RAISES {}=
  BEGIN
    RETURN ORD(ch) - ORD('0');
  END CHV;


PROCEDURE GetSetBounds(s: M3AST_AS.Set_type; VAR low, high: INTEGER) RAISES {}=
  VAR
    baseType: M3AST_SM.TYPE_SPEC_UNSET;
    l, h: M3AST_SM.Exp_value;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(s.as_type, baseType);
    M3Assert.Check(
        M3CExpValue.GetBounds(baseType, l, h) = M3CBackEnd.NumStatus.Valid);
    low := NARROW(l, Integer_value).sm_value;
    high := NARROW(h, Integer_value).sm_value;
  END GetSetBounds;


PROCEDURE ConstructorValue_C(
    cons: M3AST_AS.Constructor;
    VAR (*out*) er: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {}=
  BEGIN
    TYPECASE M3CTypesMisc.CheckedUnpack(cons.sm_exp_type_spec) OF <*NOWARN*>
    | M3AST_AS.Set_type(setType) =>
        VAR
          cv := NEW(Set_constructor_value);
          s1, s2: INTEGER;
        BEGIN
          GetSetBounds(setType, s1, s2);
          cv.sm_low := s1;
          cv.sm_value := NEW(REF ARRAY OF INTEGER, 
              (setType.sm_bitsize + Word.Size - 1) DIV Word.Size);
          FOR i := 0 TO LAST(cv.sm_value^) DO
            cv.sm_value[i] := 0;
          END;
          VAR
            iterActuals := SeqM3AST_AS_RANGE_EXP.NewIter(cons.sm_actual_s);
            actual: M3AST_AS.RANGE_EXP;
            lowBit, highBit: INTEGER;
          BEGIN
            WHILE SeqM3AST_AS_RANGE_EXP.Next(iterActuals, actual) DO
              TYPECASE actual OF <*NOWARN*>
              | M3AST_AS.Range_EXP(rExp) =>
                  lowBit := NARROW(
                      rExp.as_exp.sm_exp_value, Integer_value).sm_value;
                  highBit := lowBit;
              | M3AST_AS.Range(range) =>
                  lowBit := NARROW(
                      range.as_exp1.sm_exp_value, Integer_value).sm_value;
                  highBit := NARROW(
                      range.as_exp2.sm_exp_value, Integer_value).sm_value;
              END;
              DEC(lowBit, s1);
              DEC(highBit, s1);
              FOR setbit := lowBit TO highBit DO
                WITH w = cv.sm_value[setbit DIV Word.Size] DO
                  w := Word.Or(w, Word.Shift(1, setbit MOD Word.Size));
                END;
              END;
            END;
          END;
          er := cv;
        END;
    | M3AST_AS.Record_type, M3AST_AS.Array_type =>
        VAR
          cv := NEW(Array_or_record_constructor_value);
        BEGIN
          cv.sm_constructor := cons;
          er := cv;
        END;
    END;
    RETURN M3CBackEnd.NumStatus.Valid;
  END ConstructorValue_C;


PROCEDURE ConstructorOriginal_C(
    e: M3AST_SM.Exp_value)
    : M3AST_AS.Constructor
    RAISES {}=
  BEGIN
    TYPECASE e OF <*NOWARN*>
    | Array_or_record_constructor_value(cv) =>
        RETURN cv.sm_constructor;
    END; (* typecase *)
  END ConstructorOriginal_C;


PROCEDURE IsOrdinal_C(e: M3AST_SM.Exp_value): BOOLEAN RAISES {}=
  BEGIN
    RETURN e # NIL AND ISTYPE(e, Integer_value);
  END IsOrdinal_C;


PROCEDURE Val_C(n: INTEGER;
                <*UNUSED*> ts: M3AST_SM.TYPE_SPEC_UNSET;
    VAR (*out*) er: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {}=
  BEGIN
    (* ORD(n) = n for all types *)
    er := NewInteger_value(n);
    RETURN M3CBackEnd.NumStatus.Valid;
  END Val_C;


PROCEDURE Ord_C(
    e: M3AST_SM.Exp_value;
    VAR (*out*) i: INTEGER)
    : M3CBackEnd.NumStatus
    RAISES {}=
  BEGIN
    TYPECASE e OF <*NOWARN*>
    | Integer_value(iv) => 
        i := iv.sm_value; 
        RETURN M3CBackEnd.NumStatus.Valid;
    END;
  END Ord_C;


PROCEDURE ConvertOrdinal_C(
    e: M3AST_SM.Exp_value;
    <*UNUSED*> ts: M3AST_AS.TYPE_SPEC;
    VAR (* out *) er: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {}=
  BEGIN
    er := e;
    RETURN M3CBackEnd.NumStatus.Valid;
  END ConvertOrdinal_C;


PROCEDURE BinaryOp_C(op: M3AST_AS.BINARY; e1, e2: M3AST_SM.Exp_value;
    VAR (*out*) er: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {}=
  BEGIN
    (* Compatibility check *)
    IF TYPECODE(e1) # TYPECODE(e2) THEN
      RETURN M3CBackEnd.NumStatus.Unknown;
    END;
    (* Now the real work, computing values *)
    TYPECASE e2 OF
    | Integer_value(integerValue2) =>
        VAR
          integerValue1 := NARROW(e1, Integer_value);
          int1 := integerValue1.sm_value;
          int2 := integerValue2.sm_value;
          intr: INTEGER;
        BEGIN
          TYPECASE op OF <*NOWARN*>
          | M3AST_AS.Plus => intr := int1 + int2;
          | M3AST_AS.Minus => intr := int1 - int2;
          | M3AST_AS.Times => intr := int1 * int2;
          | M3AST_AS.Div =>
              IF int2 = 0 THEN RETURN M3CBackEnd.NumStatus.Overflow END;
              intr := int1 DIV int2;
          | M3AST_AS.Mod =>
              IF int2 = 0 THEN RETURN M3CBackEnd.NumStatus.Overflow END;
              intr := int1 MOD int2;
          | M3AST_AS.Eq => intr := ORD(int1 = int2);
          | M3AST_AS.Ne => intr := ORD(int1 # int2);
          | M3AST_AS.Gt => intr := ORD(int1 > int2);
          | M3AST_AS.Lt => intr := ORD(int1 < int2);
          | M3AST_AS.Ge => intr := ORD(int1 >= int2);
          | M3AST_AS.Le => intr := ORD(int1 <= int2);
          | M3AST_AS.And => 
              intr := ORD((int1 = True) AND (int2 = True));
          | M3AST_AS.Or => 
              intr := ORD((int1 = True) OR (int2 = True));
          ELSE RETURN M3CBackEnd.NumStatus.Unknown;
          END; (* case *)
          IF intr = int1 THEN
            er := integerValue1;
          ELSIF intr = int2 THEN
            er := integerValue2;
          ELSE
            er := NewInteger_value(intr);
          END;
        END;

    | Text_value(textValue2) =>
        VAR
          tv := NEW(Text_value);
        BEGIN
          IF ISTYPE(op, M3AST_AS.Textcat) THEN
            er := tv;
            tv.sm_value :=
                NARROW(e1, Text_value).sm_value & textValue2.sm_value;
          ELSE
            RETURN M3CBackEnd.NumStatus.Unknown;
          END;
        END;

    | Real_value(rv2) =>
        RETURN M3CBackEnd_Float_Real.BinaryOp(op,
                                              NARROW(e1, Real_value), rv2, er); 

    | LongReal_value(lrv2) =>
        RETURN M3CBackEnd_Float_LongReal.BinaryOp(op,
            NARROW(e1, LongReal_value), lrv2, er); 

    | Extended_value(ev2) =>
        RETURN M3CBackEnd_Float_Extended.BinaryOp(op,
            NARROW(e1, Extended_value), ev2, er); 

    | Set_constructor_value(c2) =>
        VAR
          c1 := NARROW(e1, Set_constructor_value);
        BEGIN
          IF NUMBER(c1.sm_value^) # NUMBER(c2.sm_value^) THEN
            RETURN M3CBackEnd.NumStatus.Unknown;
          END;
          TYPECASE op OF <*NOWARN*>
          | M3AST_AS.Plus,
            M3AST_AS.Minus,
            M3AST_AS.Times,
            M3AST_AS.Rdiv =>
              (* All return set as result *)
              VAR
                cv := NEW(Set_constructor_value);
              BEGIN
                cv.sm_value := NEW(REF ARRAY OF INTEGER, NUMBER(c1.sm_value^));
                er := cv;
                FOR i := 0 TO LAST(c1.sm_value^) DO
                  WITH w = cv.sm_value[i] DO
                    VAR
                      w1 := c1.sm_value[i];
                      w2 := c2.sm_value[i];
                    BEGIN
                      TYPECASE op OF <*NOWARN*>
                      | M3AST_AS.Plus =>
                          w := Word.Or(w1, w2);
                      | M3AST_AS.Minus =>
                         w := Word.And(w1, Word.Not(w2));
                      | M3AST_AS.Times =>
                          w := Word.And(w1, w2);
                      | M3AST_AS.Rdiv =>
                          w := Word.Xor(w1, w2);
                      END; (* case *)
                    END;
                  END; (* with *)
                END; (* for *)
              END;

          | M3AST_AS.Eq, M3AST_AS.Ne,
            M3AST_AS.Le, M3AST_AS.Ge,
            M3AST_AS.Lt, M3AST_AS.Gt =>
              (* All return booleans. The following is not very efficient but
               who cares? It won't get executed very often. *)
              VAR
                result: BOOLEAN;
              BEGIN
                IF Compare(c1, c2) = 0 THEN
                  (* Equal *)
                  TYPECASE op OF <*NOWARN*>
                  | M3AST_AS.Eq, M3AST_AS.Le, M3AST_AS.Ge =>
                      result := TRUE;
                  | M3AST_AS.Ne, M3AST_AS.Lt, M3AST_AS.Gt =>
                      result := FALSE;
                  END;
                ELSE
                  (* Not equal *)
                  TYPECASE op OF
                  | M3AST_AS.Eq =>
                      result := FALSE;
                  | M3AST_AS.Ne =>
                      result := TRUE;
                  ELSE
                    IF ISTYPE(op, M3AST_AS.Le) OR
		       ISTYPE(op, M3AST_AS.Lt) THEN
                      VAR
                        temp := c1;
                      BEGIN
                        c1 := c2; c2 := temp;
                      END;
                    END;
                    result := TRUE;
                    FOR i := 0 TO LAST(c1.sm_value^) DO
                      VAR
                        elem1 := c1.sm_value[i];
                      BEGIN
                        IF elem1 # Word.Or(elem1, c2.sm_value[i]) THEN
                          result := FALSE;
                          EXIT;
                        END;
                      END;
                    END;
                  END;
                END;
                er := NewInteger_value(ORD(result));
              END;
          END; (* case *)
        END; (* begin *)

    ELSE
      RETURN NotImplemented();
    END; (* case *)

    RETURN M3CBackEnd.NumStatus.Valid;
  END BinaryOp_C;

PROCEDURE InOp_C(e1, e2: M3AST_SM.Exp_value;
    VAR (*out*) er: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {}=
  VAR
    int := NARROW(e1, Integer_value).sm_value;
    set := NARROW(e2, Set_constructor_value);
    bool: BOOLEAN;
  BEGIN
    WITH i = int - set.sm_low, s = set.sm_value DO
      bool := Word.Extract(s[i DIV Word.Size], i MOD Word.Size, 1) # 0;
    END;
    er := NewInteger_value(ORD(bool));
    RETURN M3CBackEnd.NumStatus.Valid;    
  END InOp_C;

PROCEDURE UnaryOp_C(op: M3AST_AS.UNARY; e: M3AST_SM.Exp_value;
    VAR (*out*) er: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {}=
  BEGIN
    TYPECASE e OF
    | Integer_value(eiv) =>
        VAR
          intr: INTEGER;
        BEGIN
          TYPECASE op OF
          | M3AST_AS.Unaryplus =>
              intr := eiv.sm_value;
          | M3AST_AS.Unaryminus =>
              intr := -eiv.sm_value;
          | M3AST_AS.Not =>
              intr := ORD(eiv.sm_value = False);
          ELSE
            RETURN M3CBackEnd.NumStatus.Unknown;
          END; (* case *)
          er := NewInteger_value(intr);
        END;

    | Real_value(rv) =>
        RETURN M3CBackEnd_Float_Real.UnaryOp(op, rv, er);

    | LongReal_value(lrv) =>
        RETURN M3CBackEnd_Float_LongReal.UnaryOp(op, lrv, er);

    | Extended_value(ev) =>
        RETURN M3CBackEnd_Float_Extended.UnaryOp(op, ev, er);

    ELSE
      RETURN M3CBackEnd.NumStatus.Unknown;
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END UnaryOp_C;

PROCEDURE StdUnaryOp_C(f: M3CStdProcs.Func; 
    e: M3AST_SM.Exp_value;
    VAR (*out*) er: M3AST_SM.Exp_value;
    ft: M3AST_AS.FLOAT_TYPE := NIL): M3CBackEnd.NumStatus RAISES {}=
  BEGIN
    TYPECASE e OF
    | Integer_value(eiv) =>
        VAR
          eint: INTEGER := eiv.sm_value;
        BEGIN
          CASE f OF
          | M3CStdProcs.T.Abs =>
              IF eint < 0 THEN eint := -eint; END; (* if *)
              er := NewInteger_value(eint);
          | M3CStdProcs.T.Float =>
              TYPECASE ft OF <*NOWARN*>
              | M3AST_AS.Real_type =>
  	          er := M3CBackEnd_Float_Real.New_value(FLOAT(eint));
              | M3AST_AS.LongReal_type =>
                  er := M3CBackEnd_Float_LongReal.New_value(FLOAT(eint, LONGREAL));
              | M3AST_AS.Extended_type =>
                  er := M3CBackEnd_Float_Extended.New_value(FLOAT(eint, EXTENDED));
              END; (* typecase *)
	  ELSE
            RETURN NotImplemented();
          END; (* case *)
        END;

    | Real_value(rv) =>
        RETURN M3CBackEnd_Float_Real.StdUnaryOp(f, rv, er, ft);

    | LongReal_value(lrv) =>
        RETURN M3CBackEnd_Float_LongReal.StdUnaryOp(f, lrv, er, ft);

    | Extended_value(ev) =>
        RETURN M3CBackEnd_Float_Extended.StdUnaryOp(f, ev, er, ft);
    ELSE
      RETURN NotImplemented();
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END StdUnaryOp_C;

PROCEDURE StdBinaryOp_C(f: M3CStdProcs.Func; 
    e1, e2: M3AST_SM.Exp_value;
    VAR (*out*) er: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {}=
  BEGIN
    IF TYPECODE(e1) # TYPECODE(e2) THEN RETURN
      M3CBackEnd.NumStatus.Unknown;
    END;
    TYPECASE e1 OF
    | Integer_value(eiv1) =>
        VAR
          eiv2 := NARROW(e2, Integer_value);
        BEGIN
          CASE f OF <*NOWARN*>
          | M3CStdProcs.T.Min =>
              IF eiv1.sm_value < eiv2.sm_value THEN
                er := eiv1;
              ELSE
                er := eiv2;
              END;
          | M3CStdProcs.T.Max =>
              IF eiv1.sm_value > eiv2.sm_value THEN
                er := eiv1;
              ELSE
                er := eiv2;
              END;
          END; (* case *)
        END;

    | Real_value(rv1) =>
        RETURN M3CBackEnd_Float_Real.StdBinaryOp(f, rv1,
            NARROW(e2, Real_value), er);

    | LongReal_value(lrv1) =>
        RETURN M3CBackEnd_Float_LongReal.StdBinaryOp(f, lrv1,
            NARROW(e2, LongReal_value), er);

    | Extended_value(ev1) =>
        RETURN M3CBackEnd_Float_Extended.StdBinaryOp(f, ev1,
            NARROW(e2, Extended_value), er);

    ELSE
      RETURN NotImplemented();
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END StdBinaryOp_C;

PROCEDURE StdUnaryTypeOp_C(
    pf: M3CStdProcs.Func;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    VAR (*out*) er: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {}=
  VAR
    eint: INTEGER;
  BEGIN
    CASE pf OF
    | M3CStdProcs.T.BitSize =>
        eint := SizeInBits(ts);
    | M3CStdProcs.T.ByteSize,
      M3CStdProcs.T.AdrSize =>
        eint := RoundToByte(SizeInBits(ts));
    | M3CStdProcs.T.First =>
        TYPECASE ts OF <*NOWARN*>
        | M3AST_AS.Integer_type =>
            eint := FIRST(INTEGER);
        | M3AST_AS.Real_type =>
        | M3AST_AS.LongReal_type =>
        | M3AST_AS.Extended_type =>        
        END; (* typecase *)
    | M3CStdProcs.T.Last =>
        TYPECASE ts OF <*NOWARN*>
        | M3AST_AS.Integer_type =>
            eint := LAST(INTEGER);
        | M3AST_AS.Real_type =>
        | M3AST_AS.LongReal_type =>
        | M3AST_AS.Extended_type =>        
        END; (* typecase *)
    ELSE
      RETURN NotImplemented();        
    END; (* case *)
    er := NewInteger_value(eint);
    RETURN M3CBackEnd.NumStatus.Valid;
  END StdUnaryTypeOp_C;

PROCEDURE RoundToByte(n: INTEGER): INTEGER RAISES {}=
  CONST
    BitsInByte = 8;
  VAR er: INTEGER;
  BEGIN
    er := n DIV BitsInByte;
    IF (n MOD BitsInByte) # 0 THEN
      INC(er);
    END; (* if *)
    RETURN er;
  END RoundToByte;

PROCEDURE WordOp_C(
    w: M3CWordProcs.T;
    READONLY args: ARRAY OF M3AST_SM.Exp_value;
    VAR (* out *) er: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {}=
  VAR
    res: Word.T;
    arg0 := NARROW(args[0], Integer_value).sm_value;
  BEGIN
    IF w = M3CWordProcs.T.Not THEN
      res := Word.Not(arg0);
    ELSE
      VAR
        arg1 := NARROW(args[1], Integer_value).sm_value;
      BEGIN
        CASE w OF <*NOWARN*>
        | M3CWordProcs.T.Plus =>
            res := Word.Plus(arg0, arg1);
        | M3CWordProcs.T.Times =>
            res := Word.Times(arg0, arg1);
        | M3CWordProcs.T.Minus =>
            res := Word.Minus(arg0, arg1);
        | M3CWordProcs.T.Divide =>
            res := Word.Divide(arg0, arg1);
        | M3CWordProcs.T.Mod =>
            res := Word.Mod(arg0, arg1);
        | M3CWordProcs.T.LT =>
            res := ORD(Word.LT(arg0, arg1));
        | M3CWordProcs.T.LE =>
            res := ORD(Word.LE(arg0, arg1));
        | M3CWordProcs.T.GT =>
            res := ORD(Word.GT(arg0, arg1));
        | M3CWordProcs.T.GE =>
            res := ORD(Word.GE(arg0, arg1));
        | M3CWordProcs.T.And =>
            res := Word.And(arg0, arg1);
        | M3CWordProcs.T.Or =>
            res := Word.Or(arg0, arg1);
        | M3CWordProcs.T.Xor =>
            res := Word.Xor(arg0, arg1);
        | M3CWordProcs.T.Shift =>
            res := Word.Shift(arg0, arg1);
        | M3CWordProcs.T.RightShift =>
            res := Word.RightShift(arg0, arg1);
        | M3CWordProcs.T.Rotate =>
            res := Word.Rotate(arg0, arg1);
        | M3CWordProcs.T.RightRotate =>
            res := Word.RightRotate(arg0, arg1);
        | M3CWordProcs.T.Extract =>
            VAR
              arg2 := NARROW(args[2], Integer_value).sm_value;
            BEGIN
              IF arg1 < 0 OR arg2 < 0 OR arg1 + arg2 > Word.Size THEN
                RETURN M3CBackEnd.NumStatus.Unknown;
              END;
              res := Word.Extract(arg0, arg1, arg2);
            END;
        | M3CWordProcs.T.Insert =>
            VAR
              arg2 := NARROW(args[2], Integer_value).sm_value;
              arg3 := NARROW(args[3], Integer_value).sm_value;
            BEGIN
              IF arg2 < 0 OR arg3 < 0 OR arg2 + arg3 > Word.Size THEN
                RETURN M3CBackEnd.NumStatus.Unknown;
              END;
              res := Word.Insert(arg0, arg1, arg2, arg3);
            END;
        END;
      END;
    END;
    er := NewInteger_value(res);
    RETURN M3CBackEnd.NumStatus.Valid;
  END WordOp_C;

PROCEDURE LoopholeOK_C(<*UNUSED*> e: M3AST_AS.EXP; 
    <*UNUSED*> ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {}=
  BEGIN
    RETURN TRUE; (* no extra prohibitions *)
  END LoopholeOK_C;

PROCEDURE AlignTo(size, align: CARDINAL): INTEGER RAISES {}=
  VAR
    rem: CARDINAL;
  BEGIN
    IF align = 0 THEN RETURN size END;
    rem := size MOD align;
    IF rem = 0 THEN
      RETURN size
    ELSE
      RETURN size - rem + align;
    END; (* if *)
  END AlignTo;

PROCEDURE BitsForOrd(n: INTEGER; exact := FALSE): CARDINAL RAISES {}=
  VAR i, c: CARDINAL;
  BEGIN
    IF n <= 0 THEN RETURN 0 END;
    i := 0;
    c := n;
    WHILE c # 0 DO
      INC(i); c := c DIV 2;
    END; (* while *)
    IF NOT exact THEN
      i := ChooseByteHalfFull(i);
    END;
    RETURN i;
  END BitsForOrd;

PROCEDURE ChooseByteHalfFull(i: INTEGER): INTEGER=
  BEGIN
    IF i <= 8 THEN i := 8
    ELSIF i <= 16 THEN i := 16;
    ELSE i := AlignTo(i, 32);  
    END; (* if *)
    RETURN i;
  END ChooseByteHalfFull;

PROCEDURE BitsOK_C(e: M3AST_AS.EXP; 
    ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {}=
  VAR tts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    M3Assert.Check(ISTYPE(e.sm_exp_value, Integer_value));
    tts := ts;
    RETURN SizeInBits(tts, exact := TRUE) <=
           NARROW(e.sm_exp_value, Integer_value).sm_value;
  END BitsOK_C;

PROCEDURE VarParamOK_C(ts: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {}=
  BEGIN
    RETURN ts.sm_align MOD minAlignment = 0;
  END VarParamOK_C;

PROCEDURE Compare_C(e1, e2: M3AST_SM.Exp_value): INTEGER RAISES {}=
  BEGIN
    TYPECASE e1 OF
    | Integer_value(eiv1) =>
        TYPECASE e2 OF
        | Integer_value(eiv2) =>
            IF eiv1.sm_value = eiv2.sm_value THEN
              RETURN 0
            ELSIF eiv1.sm_value < eiv2.sm_value THEN
              RETURN -1
            ELSE
              RETURN 1
            END; (* if *)
        ELSE
          RETURN 1;
        END;

    | Set_constructor_value(cv1) =>
        TYPECASE e2 OF
        | Set_constructor_value(cv2) =>
            WITH s1 = cv1.sm_value, s2 = cv2.sm_value DO
              IF NUMBER(cv1.sm_value^) = NUMBER(cv2.sm_value^) THEN
                FOR i := 0 TO LAST(cv1.sm_value^) DO
                  IF s1[i] # s2[i] THEN RETURN 1 END;
                END;
              ELSE
                 RETURN 1;
              END;
              RETURN 0;
            END;
        ELSE
          RETURN 1;
        END;
         
    | Text_value(tv1) =>
        TYPECASE e2 OF
        | Text_value(tv2) =>
            RETURN Text.Compare(tv1.sm_value, tv2.sm_value);
        ELSE
          RETURN 1;
        END;

    | Proc_value(pv1) =>
        TYPECASE e2 OF
        | Proc_value(pv2) =>
            RETURN Text.Compare(pv1.sm_value, pv2.sm_value);
        ELSE
          RETURN 1;
        END;

    | Real_value(rv1) =>
        TYPECASE e2 OF
        | Real_value(rv2) =>
            IF rv1.sm_value = rv2.sm_value THEN RETURN 0
            ELSIF rv1.sm_value < rv2.sm_value THEN RETURN -1
            ELSE  RETURN 1
            END; (* if *)
        ELSE RETURN 1;
        END;

    | LongReal_value(rv1) =>
        TYPECASE e2 OF
        | LongReal_value(rv2) =>
            IF rv1.sm_value = rv2.sm_value THEN RETURN 0
            ELSIF rv1.sm_value < rv2.sm_value THEN RETURN -1
            ELSE RETURN 1
            END; (* if *)
        ELSE RETURN 1;
        END;

    | Extended_value(rv1) =>
        TYPECASE e2 OF
        | Extended_value(rv2) =>
            IF rv1.sm_value = rv2.sm_value THEN RETURN 0
            ELSIF rv1.sm_value < rv2.sm_value THEN RETURN -1
            ELSE RETURN 1
            END; (* if *)
        ELSE RETURN 1;
        END;

    ELSE
      EVAL NotImplemented();
      RETURN 0;
    END; (* case *)
  END Compare_C;

PROCEDURE NotImplemented(): M3CBackEnd.NumStatus RAISES {}=
  BEGIN
    M3Error.ReportAtPos(M3CSrcPos.Null, "back-end facility not implemented");
    RETURN M3CBackEnd.NumStatus.Unknown;
  END NotImplemented;


CONST
  SetCh = 's';
  ExtendedCh = 'x';  ExtendedText = "x";
  LongRealCh = 'l';  LongRealText = "l";
  RealCh = 'r';      RealText = "r";
  TextCh = 't';      TextText = "t";
  ProcCh = 'p';      ProcText = "p";
  (* Integer values, hopefully the most common, are just hex numbers *)

PROCEDURE ExpValueToText_C(e: M3AST_SM.Exp_value): TEXT RAISES {}=
  BEGIN
    TYPECASE e OF <*NOWARN*>
    | Integer_value(intValue) =>
        RETURN Fmt.Int(intValue.sm_value, 16);
    | Set_constructor_value(setValue) =>
        VAR
          s := TextWr.New();
          val := setValue.sm_value;
          result: TEXT;
        BEGIN
          Wr.PutChar(s, SetCh);
          Wr.PutText(s, Fmt.F("%s %s",
              Fmt.Int(setValue.sm_low, 16),
              Fmt.Int(NUMBER(val^), 16)));
          FOR i := 0 TO LAST(val^) DO
            Wr.PutText(s, Fmt.F(" %s", Fmt.Int(val[i], 16)));
          END;
          result := TextWr.ToText(s);
          Wr.Close(s);
          RETURN result;
        END;
    | Extended_value(extValue) =>
        RETURN ExtendedText & Fmt.Extended(extValue.sm_value);
    | LongReal_value(longValue) =>
        RETURN LongRealText & Fmt.LongReal(longValue.sm_value);
    | Real_value(realValue) =>
        RETURN RealText & Fmt.Real(realValue.sm_value);
    | Proc_value(procValue) =>
        RETURN ProcText & procValue.sm_value;
    | Text_value(textValue) =>
        RETURN TextText & textValue.sm_value;
    END;
  END ExpValueToText_C;


EXCEPTION
  Fatal;
<*FATAL Fatal*>

PROCEDURE TextToExpValue_C(t: TEXT): M3AST_SM.Exp_value RAISES {}=
  VAR
    length := Text.Length(t);
  BEGIN
    CASE Text.GetChar(t, 0) OF
    | SetCh =>
        RETURN SetTextToExpValue(t);
    | ExtendedCh =>
        VAR
	  new := NEW(Extended_value);
        BEGIN
          IF NOT TextTo_Extended(t, new.sm_value) THEN RAISE Fatal END;
          RETURN new;
        END;
    | LongRealCh =>
        VAR
	  new := NEW(LongReal_value);
        BEGIN
          IF NOT TextTo_LongReal(t, new.sm_value) THEN RAISE Fatal END;
          RETURN new;
        END;
    | RealCh =>
        VAR
	  new := NEW(Real_value);
        BEGIN
          IF NOT TextTo_Real(t, new.sm_value) THEN RAISE Fatal END;
          RETURN new;
        END;
    | ProcCh =>
        RETURN NEW(Proc_value, sm_value := Text.Sub(t, 1, length - 1));
    | TextCh =>
        RETURN NEW(Text_value, sm_value := Text.Sub(t, 1, length - 1));
    ELSE
      VAR
        new := NEW(Integer_value);
      BEGIN
        IF NOT TextTo_Int(t, new.sm_value, 16) THEN RAISE Fatal END;
        RETURN new;
      END;
    END
  END TextToExpValue_C;

PROCEDURE SetTextToExpValue(t: TEXT): Set_constructor_value RAISES {}=
  VAR
    new := NEW(Set_constructor_value);
    s := TextRd.New(t);
  BEGIN
    EVAL(Rd.GetChar(s));
    new.sm_low := GetInt(s, 16);
    new.sm_value := NEW(REF ARRAY OF INTEGER, GetInt(s, 16));
    FOR i := 0 TO LAST(new.sm_value^) DO
      new.sm_value[i] := GetInt(s, 16);
    END;
    Rd.Close(s);
    RETURN new;
  END SetTextToExpValue;

PROCEDURE GetInt(s: Rd.T; base: CARDINAL): INTEGER=
  VAR t := RdExtras.GetText(s); result: INTEGER;
  BEGIN
    EVAL TextTo_Int(t, result, base);
    RETURN result;
  END GetInt;

PROCEDURE TextTo_Int(t: Text.T;
    VAR i: INTEGER;
    <*UNUSED*> base: Fmt.Base := 10)
    : BOOLEAN=
  VAR used: INTEGER; l: INTEGER;
  BEGIN
    M3Assert.Check(t # NIL);
    l := Text.Length(t);
    IF l>2 AND Text.GetChar(t, 2) = '_' OR
       l>1 AND Text.GetChar(t, 1) = '_' THEN
      i := Convert.ToUnsigned(t^, used);
    ELSE
      i := Convert.ToInt(t^, used);
    END;
    RETURN used = l;
  END TextTo_Int;

PROCEDURE TextTo_Real(t: Text.T; VAR real: REAL): BOOLEAN=
  VAR used: INTEGER;
  BEGIN
    real := Convert.ToFloat(t^, used);
    RETURN used = Text.Length(t);
  END TextTo_Real;

PROCEDURE TextTo_LongReal(t: Text.T; VAR long: LONGREAL): BOOLEAN=
  VAR used: INTEGER;
  BEGIN
    long := Convert.ToLongFloat(t^, used);
    RETURN used = Text.Length(t);
  END TextTo_LongReal;

PROCEDURE TextTo_Extended(t: Text.T; VAR extended: EXTENDED): BOOLEAN=
  VAR used: INTEGER;
  BEGIN
    extended := Convert.ToExtended(t^, used);
    RETURN used = Text.Length(t);
  END TextTo_Extended;


BEGIN
  RegisterProcs();
END M3CBackEnd_C.
