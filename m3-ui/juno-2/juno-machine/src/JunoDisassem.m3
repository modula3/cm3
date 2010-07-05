(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun  8 18:44:29 PDT 1994 by heydon                   *)

UNSAFE MODULE JunoDisassem;

IMPORT JunoRT AS RT, JunoByteCode AS BC, JunoMarshal AS Marshal, JunoValue;
IMPORT Wr, Fmt, Thread;

PROCEDURE P(bs: RT.ByteStream; wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    a: UNTRACED REF RT.ByteCode := ADR(bs[0]);
    firstaddr := ADR(bs[0]);
    lastaddr := firstaddr + NUMBER(bs^);
    globalUsed := NEW(REF ARRAY OF BOOLEAN, 20);

  <* INLINE *>
  PROCEDURE Txt(t: TEXT) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN Wr.PutText(wr, t) END Txt;

  <* INLINE *>
  PROCEDURE Num(n: INTEGER) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN Txt(Fmt.Int(n)) END Num;

  <* INLINE *>
  PROCEDURE RealNum(r: JunoValue.Real) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN Txt(Fmt.Real(r)) END RealNum;

  <* INLINE *>
  PROCEDURE Label(offset: INTEGER) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN Txt("L"); Txt(Fmt.Int(offset + (a - firstaddr))) END Label;

  PROCEDURE Var(index: INTEGER) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      Wr.PutChar(wr, VAL(ORD('a') + index MOD 26, CHAR));
      IF index > 26 THEN Num(index DIV 26) END
    END Var;

  <* INLINE *>
  PROCEDURE Func(t: TEXT; v: INTEGER) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN Txt(t); Txt("("); Var(v); Txt(")") END Func;

  PROCEDURE MarkGlobal(index: CARDINAL) =
    BEGIN
      IF index > LAST(globalUsed^) THEN
        VAR new := NEW(REF ARRAY OF BOOLEAN, 2 * index); BEGIN
          SUBARRAY(new^, 0, NUMBER(globalUsed^)) := globalUsed^;
          globalUsed := new
        END
      END;
      globalUsed[index] := TRUE
    END MarkGlobal;

  PROCEDURE DoSolve (VAR a: UNTRACED REF RT.ByteCode)
    RAISES {Wr.Failure, Thread.Alerted} =
    VAR
      ins, inouts, nc: Marshal.UShort;
      c := 0;
    BEGIN
      ins := Marshal.ReadUShort(a);
      inouts := Marshal.ReadUShort(a);
      nc := Marshal.ReadUShort(a);
      Num(ins);
      Txt(", ");
      Num(inouts);
      Txt(", ");
      Num(nc);
      Txt(": \n");

      (* package constraints *)
      WHILE c < nc DO
        VAR n: BC.ConRange := a^; x, y, z: Marshal.UShort := 0; BEGIN
          INC(a);
          x := Marshal.ReadUShort(a);
          IF n < BC.REAL_C  THEN y := Marshal.ReadUShort(a) END;
          IF n < BC.EQUAL_C THEN z := Marshal.ReadUShort(a) END;
          Txt("\t  ");
          CASE n OF <* NOWARN *>
          | BC.CONS_C  => Var(x); Txt("=("); Var(y); Txt(","); Var(z); Txt(")")
          | BC.SUM_C   => Var(x); Txt("="); Var(y); Txt("+"); Var(z)
          | BC.PROD_C  => Var(x); Txt("="); Var(y); Txt("*"); Var(z)
          | BC.ATAN_C  => Var(x); Txt("=ATAN(");Var(y);Txt(",");Var(z);Txt(")")
          | BC.EQUAL_C => Var(x); Txt("="); Var(y)
          | BC.SIN_C   => Var(x); Func("=SIN", y)
          | BC.COS_C   => Var(x); Func("=COS", y)
          | BC.EXP_C   => Var(x); Func("=EXP", y)
          | BC.REAL_C  => Func("IS-REAL", x)
          | BC.TEXT_C  => Func("IS-TEXT", x)
          END;
          INC(c);
          IF c < nc THEN Txt("\n") END
        END
      END
    END DoSolve;

  (* P *)
  BEGIN
    FOR i := 0 TO LAST(globalUsed^) DO globalUsed[i] := FALSE END;
    WHILE a # lastaddr DO
      VAR bc := a^; BEGIN
        Txt("L");
        Num(a - firstaddr);
        Txt("\t");
        Txt(BC.names[bc]);
        Txt(" ");
        INC(a);
        CASE bc OF
        | BC.PUSHL, BC.POPL =>
            Num(Marshal.ReadShort(a))
        | BC.PUSHG, BC.POPG =>
            VAR index := Marshal.ReadULong(a); BEGIN
              Num(index); MarkGlobal(index)
            END
        | BC.INCSP, BC.DECSP, BC.PUSHM3NIL, BC.ERROR, BC.FERROR =>
            Num(a^); INC(a)
        | BC.PUSHNUM =>
            RealNum(Marshal.ReadReal(a))
        | BC.JUMP, BC.TJUMP, BC.FJUMP, BC.UJUMP, BC.ADD, BC.SUBTRACT,
          BC.MULTIPLY, BC.DIVIDE, BC.DIV_, BC.MOD_, BC.NEGATE, BC.ABS_,
          BC.FLOOR_, BC.CEILING_, BC.ROUND_, BC.MAX_, BC.MIN_, BC.ATAN,
          BC.SIN, BC.COS, BC.LN, BC.EXP, BC.REL, BC.CAR, BC.CDR, BC.CAR_CDR,
          BC.CONCAT =>
            Label(Marshal.ReadShort(a))
        | BC.CALL, BC.CALLEXT, BC.NEWCL, BC.NEWEXTCL =>
            Num(Marshal.ReadULong(a))
        | BC.LIST =>
            Num(Marshal.ReadUShort(a))
        | BC.CLOSE =>
            Num(Marshal.ReadUShort(a)); Txt(", ");
            Label(Marshal.ReadShort(a))
        | BC.APPLY =>
            Num(Marshal.ReadUShort(a)); Txt(", ");
            Num(Marshal.ReadUShort(a)); Txt(", ");
            Num(Marshal.ReadUShort(a)); Txt(", ");
            Label(Marshal.ReadShort(a))
        | BC.SOLVE => DoSolve(a)
        ELSE (* SKIP *)
        END;
        Txt("\n")
      END
    END;
    VAR first := TRUE; BEGIN
      FOR i := 0 TO LAST(globalUsed^) DO
        IF globalUsed[i] AND RT.value_tbl[i] # NIL THEN
          IF first THEN Txt("\n"); first := FALSE END;
          Txt("V"); Num(i); Txt("\t");
          JunoValue.Unparse(wr, RT.value_tbl[i]);
          Txt("\n")
        END
      END
    END
  END P;

BEGIN
END JunoDisassem.
