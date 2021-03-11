UNSAFE MODULE RefRecord;
(*IMPORT Stdio;*)

IMPORT Wr;
IMPORT Text;
IMPORT Fmt;
IMPORT TextWr;
IMPORT RTTipe;
IMPORT RTCollector;
IMPORT RTPacking;
FROM RTTipe IMPORT Kind;
IMPORT Thread;

<* FATAL Thread.Alerted, Wr.Failure *>

PROCEDURE FieldFormat(wr: Wr.T; x: ADDRESS;
                      field: RTTipe.Field; leadSpace := TRUE) =
  BEGIN
    IF field # NIL THEN
      IF leadSpace THEN Wr.PutChar(wr, ' '); END;
      TipeFormat(wr, x+(field.offset DIV 8), field.type);
      FieldFormat(wr, x, field.next);
    END;
  END FieldFormat;

PROCEDURE TipeFormat(wr: Wr.T; x: ADDRESS; tipe: RTTipe.T;
                     parenRec := TRUE) =
  VAR
    field: RTTipe.Field := NIL;
  PROCEDURE PutRange(min, max: INTEGER) =
    VAR
      val: INTEGER;
    BEGIN
      CASE tipe.size OF
      | 8 =>
        val := LOOPHOLE(x, UNTRACED REF [0..255])^;
        IF val > max THEN
          val := val - 256;
        END;
      | 16 => val := LOOPHOLE(x, UNTRACED REF [0..65535])^;
        IF val > max THEN
          val := val - 65536;
        END;
      | 32 => val := LOOPHOLE(x, UNTRACED REF INTEGER)^;
      ELSE
        <* ASSERT FALSE *>
      END;
      <* ASSERT val <= max *>
      <* ASSERT val >= min *>
      Wr.PutText(wr, Fmt.Int(val));
    END PutRange;
  BEGIN
    (* INC(x, tipe.align DIV 8); *)
    CASE tipe.kind OF
    | Kind.Cardinal, Kind.Integer =>
      Wr.PutText(wr, Fmt.Int(LOOPHOLE(x, UNTRACED REF INTEGER)^));
    | Kind.Enum => PutRange(0, NARROW(tipe, RTTipe.Enum).n_elts-1);
    | Kind.Subrange =>
      VAR
        sr: RTTipe.Subrange := tipe;
      BEGIN
        PutRange(sr.min, sr.max);
      END;
    | Kind.Boolean =>
      Wr.PutText(wr, Fmt.Bool(LOOPHOLE(x, UNTRACED REF BOOLEAN)^));
    | Kind.Char =>
      Wr.PutText(wr, Fmt.Char(LOOPHOLE(x, UNTRACED REF CHAR)^));
    | Kind.Real =>
      Wr.PutText(wr, Fmt.Real(LOOPHOLE(x, UNTRACED REF REAL)^));
    | Kind.Longreal =>
      Wr.PutText(wr, Fmt.LongReal(LOOPHOLE(x, UNTRACED REF LONGREAL)^));
    | Kind.Extended =>
      Wr.PutText(wr, Fmt.Extended(LOOPHOLE(x, UNTRACED REF EXTENDED)^));
    | Kind.Set =>
      VAR
        n := NARROW(tipe, RTTipe.Set).n_elts;
        s := LOOPHOLE(x, UNTRACED REF SET OF [0..1023]);
        c := ARRAY BOOLEAN OF CHAR{'0', '1'};
      BEGIN
        FOR i := 0 TO n-1 DO
          Wr.PutChar(wr, c[i IN s^]);
        END;
      END;
    | Kind.Array =>
      VAR
        a := NARROW(tipe, RTTipe.Array);
        n := a.n_elts;
        e := a.element;
        d := a.elt_pack DIV 8;
      BEGIN
        IF parenRec THEN Wr.PutChar(wr, '{'); END;
        FOR i := 0 TO n-1 DO
          IF i # 0 THEN Wr.PutChar(wr, ' '); END;
          TipeFormat(wr, x+d*i, e);
        END;
        IF parenRec THEN Wr.PutChar(wr, '}'); END;
      END;        
    | Kind.Record => field := NARROW(tipe, RTTipe.Record).fields;
    ELSE
    END;
    IF field # NIL THEN
      IF parenRec THEN Wr.PutChar(wr, '('); END;
      FieldFormat(wr, x, field, FALSE);
      IF parenRec THEN Wr.PutChar(wr, ')'); END;
    END;
  END TipeFormat;

PROCEDURE Format(x: REFANY): TEXT =
  VAR
    wr := TextWr.New();
    tc := TYPECODE(x);
    (* def := RTType.Get(tc); *)
    packing := RTPacking.Local();
    tipe := RTTipe.Get(tc, packing);
  BEGIN
    RTCollector.Disable();
    TipeFormat(wr, LOOPHOLE(x, ADDRESS), tipe, FALSE);
    RTCollector.Enable();
    RETURN TextWr.ToText(wr);
  END Format;

(*
PROCEDURE Test() =
  TYPE
    Enum = {Apple, Pear, Burger};
    RecType = RECORD
      a := 1.23456789E0;
      b := TRUE;
      c := 'c';
      d := 0.0;
      e := Enum.Pear;
      f := ARRAY [0..1] OF INTEGER{10, 20};
      i := -3;
      r := RECORD a := 'a'; b := 3.1D0; c := RECORD u := FALSE; END{}; END{};
      s := SET OF Enum {Enum.Pear};
      w := ARRAY [13..15] OF BOOLEAN{FALSE, TRUE, TRUE};
      x: [1..2000] := 100;
      y: [1..15] := 5;
      z: [-1000000..10000000] := 300;
    END;
  VAR
    u := NEW(REF RecType);
  BEGIN
    Wr.PutText(Stdio.stdout, Format(u) & "\n");
    (* result: 1.2345679 TRUE c 0 1 {10 20} -3 (a 3.1 (FALSE)) 010 {FALSE TRUE TRUE} 100 5 300 *)
  END Test;
*)
PROCEDURE Hash(a: T): INTEGER = BEGIN RETURN Text.Hash(Format(a)); END Hash;
PROCEDURE Equal(a,b:T): BOOLEAN = BEGIN RETURN Text.Equal(Format(a),Format(b));END Equal;

BEGIN
  (* Test(); *)
END RefRecord.
