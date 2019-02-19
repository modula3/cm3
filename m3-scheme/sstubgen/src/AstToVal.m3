(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AstToVal;

IMPORT AstToType, StubUtils, Value, M3AST_AS, M3CBackEnd_C;
IMPORT M3AST_SM_F, M3AST_AS_F, M3AST_TM_F;
IMPORT RTTypeSRC;
IMPORT Word;
IMPORT Debug;
IMPORT RTBrand;
IMPORT SeqM3AST_AS_CONS_ELEM;
IMPORT Atom;
IMPORT M3CId;

PROCEDURE ProcessExp(h: AstToType.Handle; exp: M3AST_AS.EXP): Value.T =
  BEGIN
    Debug.Out("AstToVal.ProcessExp : exp.sm_exp_value is " & 
      RTBrand.GetName(TYPECODE(exp.sm_exp_value)));

    TYPECASE exp.sm_exp_value OF
      |  M3CBackEnd_C.Integer_value (int) =>
           RETURN NEW(Value.Ordinal, ord := int.sm_value)
      |  M3CBackEnd_C.Text_value (txt) => 
           RETURN NEW(Value.Txt, val := txt.sm_value)
      |  M3CBackEnd_C.Real_value (real) => 
           RETURN NEW(Value.Float, val := real.sm_value);
      |  M3CBackEnd_C.LongReal_value (lreal) => 
           RETURN NEW(Value.LongFloat, val := lreal.sm_value);
      |  M3CBackEnd_C.Extended_value (ereal) => 
           RETURN NEW(Value.Extended, val := ereal.sm_value);
      
      |  M3CBackEnd_C.Set_constructor_value (set) => 
        VAR n := 0; BEGIN
          (* count how many bits are set *)
          FOR i := FIRST(set.sm_value^) TO LAST(set.sm_value^) DO
            FOR k := 0 TO BITSIZE(set.sm_value[i])-1 DO
              IF Word.Extract(set.sm_value[i],k,1) = 1 THEN
                INC(n)
              END
            END
          END;
          
          (* allocate array and make ordinal elements accordingly *)
          WITH elems = NEW(REF ARRAY OF Value.Ordinal, n) DO
            n := 0;
            FOR i := FIRST(set.sm_value^) TO LAST(set.sm_value^) DO
              FOR k := 0 TO BITSIZE(set.sm_value[i])-1 DO
                IF Word.Extract(set.sm_value[i],k,1) = 1 THEN
                  elems[n] := NEW(Value.Ordinal, ord := 
                                             set.sm_low + 
                                             i * BITSIZE(set.sm_value[0]) + k);
                  INC(n)
                END
              END
            END(*FOR*);
            RETURN NEW(Value.Set, elements := elems)
          END
        END;

      |  M3CBackEnd_C.Array_or_record_constructor_value(ar_constructor) => 
        WITH con = ar_constructor.sm_constructor DO
          VAR
            iter := SeqM3AST_AS_CONS_ELEM.NewIter(con.as_element_s);
            cons_elem : M3AST_AS.CONS_ELEM;
            elements : REF ARRAY OF Value.Element;
            p := 0;
          BEGIN
            IF con.as_propagate # NIL THEN
              Debug.Out("Propagate!",10);
              elements := NEW(REF ARRAY OF Value.Element, 
                      SeqM3AST_AS_CONS_ELEM.Length(con.as_element_s)+1);
              elements[LAST(elements^)] := NEW(Value.Propagate);
            ELSE
              elements := NEW(REF ARRAY OF Value.Element, 
                      SeqM3AST_AS_CONS_ELEM.Length(con.as_element_s))
            END;

            WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, cons_elem) DO
              Debug.Out(RTBrand.GetName(TYPECODE(cons_elem)),10);
              TYPECASE cons_elem OF
                M3AST_AS.RANGE_EXP_elem(re) => 
                elements[p] := NEW(Value.Range,
                                   val := ProcessExp(h,NARROW(re.as_range_exp,
                                                   M3AST_AS.Range_EXP).as_exp))
              |
                M3AST_AS.Actual_elem(ae) =>
                WITH fUsedId = NARROW(ae.as_actual.as_id, M3AST_AS_F.Exp_used_id),
                     fieldName = Atom.FromText(M3CId.ToText(fUsedId.vUSED_ID.lx_symrep)) DO
                  elements[p] := NEW(Value.Actual,
                                     field := fieldName,
                                     val := ProcessExp(h,
                                                  ae.as_actual.as_exp_type))
                END
              ELSE
                StubUtils.Die("AstToVal.ProcessExp: unknown element in Array_or_record_constructor_value of type: " & RTBrand.GetName(TYPECODE(cons_elem)))
              END;
              INC(p)
            END;
            
            RETURN NEW(Value.ArrayOrRecord, elements := elements)
          END

        END
      | M3CBackEnd_C.Proc_value => 

        PROCEDURE MakeValueProc(x : M3AST_AS_F.Exp_used_id) : Value.Proc =
          BEGIN
            RETURN NEW(Value.Proc, 
                       intf := Atom.FromText(M3CId.ToText(x.vUSED_ID.sm_def.tmp_unit_id.lx_symrep)),
                       item := Atom.FromText(M3CId.ToText(x.vUSED_ID.lx_symrep)))
          END MakeValueProc;
        BEGIN                       
          Debug.Out(RTBrand.GetName(TYPECODE(exp)),10);
          TYPECASE exp OF
            M3AST_AS.Select(s) => (* I.P *)
            RETURN MakeValueProc(s.as_id)
          ELSE
            (* just P *)
            RETURN MakeValueProc(exp)
          END
        END
      ELSE StubUtils.Die("AstToVal.ProcessExp: unsupported value: "
                         & RTTypeSRC.TypeName (exp.sm_exp_value));
        <*ASSERT FALSE*>
    END
  END ProcessExp;

BEGIN
END AstToVal.
