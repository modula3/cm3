(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Aug 14 21:35:46 PDT 1993 by heydon                   *)

MODULE IndexedNF;

IMPORT JunoAST;

VAR
  avail := NEW(REF ARRAY OF T, 20);
  next := 0;
  (* The available "T"'s are in "avail[0..next-1]". *)

PROCEDURE New(var_cnt, conj_cnt: CARDINAL := 10): T =
  VAR res: T; BEGIN
    IF next > 0 THEN
      DEC(next);
      res := avail[next];
      res.v_cnt := 0; res.c_cnt := 0;
    ELSE
      res := NEW(T, var := NEW(REF JunoAST.Vars, var_cnt),
        conj := NEW(REF JunoAST.Formulas, conj_cnt))
    END;
    RETURN res
  END New;

PROCEDURE Dispose(inf: T) =
  BEGIN
    IF next > LAST(avail^) THEN
      VAR new := NEW(REF ARRAY OF T, 2 * NUMBER(avail^)); BEGIN
        SUBARRAY(new^, 0, NUMBER(avail^)) := avail^;
        avail := new
      END
    END;
    avail[next] := inf;
    INC(next)
  END Dispose;

PROCEDURE ToNF(inf: T): JunoAST.NormalForm =
  VAR res := NEW(JunoAST.NormalForm, bp := inf.bp); BEGIN
    res.var := NEW(REF JunoAST.Vars, inf.v_cnt);
    res.conj := NEW(REF JunoAST.Formulas, inf.c_cnt);
    res.var^ := SUBARRAY(inf.var^,  0, inf.v_cnt);
    res.conj^ := SUBARRAY(inf.conj^, 0, inf.c_cnt);
    RETURN res
  END ToNF;

PROCEDURE FromNF(nf: JunoAST.NormalForm; VAR (*INOUT*) res: T) =
  BEGIN
    res.bp := nf.bp;
    res.v_cnt := NUMBER(nf.var^);
    res.c_cnt := NUMBER(nf.conj^);
    IF NUMBER(res.var^) < res.v_cnt THEN
      res.var := NEW(REF JunoAST.Vars, 2 * res.v_cnt)
    END;
    SUBARRAY(res.var^, 0, res.v_cnt) := nf.var^;
    IF NUMBER(res.conj^) < res.c_cnt THEN
      res.conj := NEW(REF JunoAST.Formulas, 2 * res.c_cnt)
    END;
    SUBARRAY(res.conj^, 0, res.c_cnt) := nf.conj^
  END FromNF;

PROCEDURE AddVar(VAR (*INOUT*) res: T; var: JunoAST.NearVarLink) =
  BEGIN
    IF res.v_cnt > LAST(res.var^) THEN res.var := DoubleVar(res.var) END;
    res.var[res.v_cnt] := var;
    INC(res.v_cnt)
  END AddVar;

PROCEDURE AddConj(VAR (*INOUT*) res: T; conj: JunoAST.Formula) =
  BEGIN
    IF res.c_cnt > LAST(res.conj^) THEN res.conj := DoubleConj(res.conj) END;
    res.conj[res.c_cnt] := conj;
    INC(res.c_cnt)
  END AddConj;

PROCEDURE AddVarList(VAR (*INOUT*) res: T; vars: JunoAST.NearVarList) =
  BEGIN
    VAR tot := res.v_cnt + vars.size; BEGIN
      IF tot > NUMBER(res.var^) THEN
        res.var := DoubleVar(res.var, 1 + ((tot-1) DIV NUMBER(res.var^)));
        <* ASSERT NUMBER(res.var^) >= tot *>
      END
    END;
    VAR curr := vars.head; BEGIN
      WHILE curr # NIL DO
	res.var[res.v_cnt] := curr;
	INC(res.v_cnt);
	curr := curr.next
      END
    END
  END AddVarList;

PROCEDURE AddConjList(VAR (*INOUT*) res: T; forms: JunoAST.ExprList) =
  BEGIN
    VAR tot := res.c_cnt + forms.size; BEGIN
      IF tot > NUMBER(res.conj^) THEN
        res.conj := DoubleConj(res.conj, 1 + ((tot-1) DIV NUMBER(res.conj^)));
        <* ASSERT NUMBER(res.conj^) >= tot *>
      END
    END;
    VAR curr := forms.head; BEGIN
      WHILE curr # NIL DO
	res.conj[res.c_cnt] := curr.expr;
	INC(res.c_cnt);
	curr := curr.next
      END
    END
  END AddConjList;

PROCEDURE AddVarArray(VAR (*INOUT*) res: T; READONLY vars: JunoAST.Vars) =
  BEGIN
    VAR tot := res.v_cnt + NUMBER(vars); BEGIN
      IF tot > NUMBER(res.var^) THEN
        res.var := DoubleVar(res.var, 1 + ((tot-1) DIV NUMBER(res.var^)));
        <* ASSERT NUMBER(res.var^) >= tot *>
      END
    END;
    FOR i := FIRST(vars) TO LAST(vars) DO
      res.var[res.v_cnt] := vars[i];
      INC(res.v_cnt);
    END
  END AddVarArray;

PROCEDURE AddConjArray(VAR (*INOUT*) res: T; READONLY forms: JunoAST.Formulas)=
  BEGIN
    VAR tot := res.c_cnt + NUMBER(forms); BEGIN
      IF tot > NUMBER(res.conj^) THEN
        res.conj := DoubleConj(res.conj, 1 + ((tot-1) DIV NUMBER(res.conj^)));
        <* ASSERT NUMBER(res.conj^) >= tot *>
      END
    END;
    FOR i := FIRST(forms) TO LAST(forms) DO
      res.conj[res.c_cnt] := forms[i];
      INC(res.c_cnt);
    END
  END AddConjArray;

PROCEDURE DoubleVar(vars: REF JunoAST.Vars; factor := 2):REF JunoAST.Vars =
  VAR res := NEW(REF JunoAST.Vars, factor * NUMBER(vars^)); BEGIN
    SUBARRAY(res^, 0, NUMBER(vars^)) := vars^;
    RETURN res
  END DoubleVar;

PROCEDURE DoubleConj(forms: REF JunoAST.Formulas; factor := 2):
    REF JunoAST.Formulas =
  VAR res := NEW(REF JunoAST.Formulas, factor * NUMBER(forms^)); BEGIN
    SUBARRAY(res^, 0, NUMBER(forms^)) := forms^;
    RETURN res
  END DoubleConj;

BEGIN
END IndexedNF.
