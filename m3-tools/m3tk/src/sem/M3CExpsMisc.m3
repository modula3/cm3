MODULE M3CExpsMisc;

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

IMPORT Text;

IMPORT M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_Actual;

IMPORT M3Error, M3Assert, M3CStdProcs;


PROCEDURE Classify(exp: M3AST_AS.EXP): Class RAISES {}=
  BEGIN
    TYPECASE exp OF
    | M3AST_AS.Exp_used_id(expUsedId) =>
        TYPECASE expUsedId.vUSED_ID.sm_def OF
        | NULL =>
            (* assume normal *)
        | M3AST_AS.Interface_id, M3AST_AS.Interface_AS_id =>
            RETURN Class.Interface;
        | M3AST_AS.Module_id =>
            M3Assert.Fail(); (* can't happen? *)
        | M3AST_AS.Type_id =>
            RETURN Class.Type;
        | M3AST_AS.Exc_id =>
            RETURN Class.Exception;
        | M3AST_AS.METHOD_OVERRIDE_ID =>
            RETURN Class.Method;
        ELSE
          (* normal *)
        END; (* case *)
    | M3AST_AS.Select(select) =>
          WITH class2 = Classify(select.as_id) DO
            IF class2 = Class.Method AND
                Classify(select.as_exp) = Class.Type THEN
              (* ObjectType.method *)
              RETURN Class.Normal;
            ELSE
              RETURN class2;
            END; (* if *)
          END;
    ELSE
      (* take the default *)
    END; (* case *)
    (* if we get here everything looks normal *)
    RETURN Class.Normal;
  END Classify;


PROCEDURE WrongClass(en: M3Error.ERROR_NODE; class: Class) RAISES {}=
  VAR
    text: Text.T;
  BEGIN
    CASE class OF
    | Class.Normal =>    text := "invalid use of expression";
    | Class.Type =>      text := "invalid use of type";
    | Class.Interface => text := "invalid use of interface";
    | Class.Method =>    text := "invalid use of method";
    | Class.Exception => text := "invalid use of exception";
    ELSE
      M3Assert.Fail();
    END; (* case *)
    M3Error.Report(en, text);
  END WrongClass;


PROCEDURE Index(
    index: M3AST_AS.Index;
    VAR writable: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  VAR
    array := index.as_array;
  BEGIN
    TYPECASE array.sm_exp_type_spec OF
    | NULL =>
    | M3AST_AS.Array_type =>
        RETURN IsDesignator(array, writable);
    ELSE
    END; (* if *)
    (* could validly be a ref type or could just be an error. In either
     case we go for: *)
    writable := TRUE;
    RETURN TRUE;
  END Index;


PROCEDURE FirstParam(
    params: SeqM3AST_AS_Actual.T;
    VAR writable: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  VAR
    actual: M3AST_AS.Actual;
    seqActual := SeqM3AST_AS_Actual.NewIter(params);
  BEGIN
    IF SeqM3AST_AS_Actual.Next(seqActual, actual) THEN
      TYPECASE actual.as_exp_type OF
      | M3AST_AS.EXP(exp) =>
          RETURN IsDesignator(exp, writable);
      ELSE
      END; (* if *)
    END; (* if *)
    (* we get here if there has been a previous error; the default is
     optimistic: *)
    writable := TRUE;
    RETURN TRUE;
  END FirstParam;


PROCEDURE Selection(
    b: M3AST_AS.Select;
    VAR writable: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  VAR
    exp1: M3AST_AS.EXP;
  BEGIN
    exp1 := b.as_exp;
    CASE Classify(exp1) OF
    | Class.Normal =>
        TYPECASE exp1.sm_exp_type_spec OF
        | NULL =>
            (* take the default *)
        | M3AST_AS.Record_type =>
            RETURN IsDesignator(exp1, writable);
        | M3AST_AS.Object_type =>
            VAR
              defId := b.as_id.vUSED_ID.sm_def;
            BEGIN
              IF defId # NIL AND
                 ISTYPE(defId, M3AST_AS.METHOD_OVERRIDE_ID) THEN
                RETURN FALSE;
              END;
            END;
        ELSE
          (* could validly be a ref type or it may be an error; in either
             case we take the default *)
        END; (* case *)
    | Class.Interface =>
        RETURN IsDesignator(b.as_id, writable);
    | Class.Type =>
        (* ObjectType.blah, Enumeration.blah *)
        RETURN FALSE;
    ELSE
      (* there has been a cockup; be optimistic and take the default *)
    END; (* case *)
    writable := TRUE;
    RETURN TRUE;
  END Selection;


PROCEDURE IsDesignator(
    exp: M3AST_AS.EXP;
    VAR writable: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  VAR
    def_id: M3AST_SM.DEF_ID_UNSET;
    pf: M3CStdProcs.T;
  BEGIN
    TYPECASE exp OF
    | M3AST_AS.Exp_used_id(exp_used_id) =>
        def_id := exp_used_id.vUSED_ID.sm_def;
        IF def_id = NIL THEN writable := TRUE; RETURN TRUE END;
        TYPECASE def_id OF
        | M3AST_AS.Var_id,
          M3AST_AS.F_Var_id,
          M3AST_AS.F_Value_id,
          M3AST_AS.Tcase_id,
          M3AST_AS.Handler_id =>
            writable := TRUE;
            RETURN TRUE;
        | M3AST_AS.F_Readonly_id,
          M3AST_AS.For_id =>
            writable := FALSE;
            RETURN TRUE;
        | M3AST_AS.With_id(with_id) =>
            WITH withExp = with_id.vINIT_ID.sm_init_exp DO
              IF withExp = NIL THEN
                writable := TRUE;
                RETURN TRUE;
              ELSE
                IF NOT IsDesignator(withExp, writable) THEN
                  writable := FALSE;
                END;
                RETURN TRUE;
              END;
            END;
        ELSE
          (* no *)
        END; (* case *)

    | M3AST_AS.Select(select) =>
        RETURN Selection(select, writable);

    | M3AST_AS.Deref(*unary*) =>
        writable := TRUE;
        RETURN TRUE;

    | M3AST_AS.Call(call) =>
        IF M3CStdProcs.IsStandardCall(exp, pf) AND
            ((pf = M3CStdProcs.T.Subarray) OR (pf = M3CStdProcs.T.Loophole)) THEN
          RETURN FirstParam(call.as_param_s, writable);
        END; (* if *)
    | M3AST_AS.Index(index) =>
        RETURN Index(index, writable);
    ELSE
      (* no way *)
    END; (* case *)
    (* if we get here then it doesn't look at all like a designator *)
    RETURN FALSE;
  END IsDesignator;
   

PROCEDURE IsId(
    exp: M3AST_AS.EXP;
    VAR defId: M3AST_AS.DEF_ID)
    : BOOLEAN
    RAISES {}=
  VAR
    id: M3AST_AS.Exp_used_id;
  BEGIN
    TYPECASE exp OF
    | M3AST_AS.Exp_used_id(expUsedId) =>
        id := expUsedId;
    | M3AST_AS.Select(select) =>
        IF Classify(select.as_exp) = Class.Interface THEN
          id := select.as_id;
        ELSE
          RETURN FALSE;
        END;
    ELSE
      RETURN FALSE
    END;
    (* If we get this far 'id' has been set up *)
    IF id.vUSED_ID.sm_def # NIL THEN
      defId := id.vUSED_ID.sm_def;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END IsId;


BEGIN
END M3CExpsMisc.
