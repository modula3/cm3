MODULE BigIntegerFmtLex;
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Rd, Thread, Fmt AS F, Lex AS L;
IMPORT Word, Text;
IMPORT BigIntegerRep AS BR, BigInteger AS BB;
FROM FmtLexSupport IMPORT Precedence;
IMPORT Arithmetic AS Arith;

(*
IMPORT IO;
*)


<* UNUSED *>
CONST
  Module = "BigIntegerFmtLex.";


(** Format the contents of the number data structure for debug purposes.
PROCEDURE Dump (READONLY x: T; ): TEXT =
  BEGIN
    RETURN
      "(" & F.Int(x.size) & ", " & ARRAY BOOLEAN OF TEXT{"+", "-"}[x.sign]
        & ", " & FastFmtU(x, 16, Word.Size DIV 4) & ")";
  END Dump;
*)

PROCEDURE FastFmtU (READONLY x: T; base: F.Base; pad: [1 .. Word.Size]; ):
  TEXT =
  VAR txt: TEXT;
  BEGIN
    IF x.size = 0 THEN
      RETURN "0";
    ELSE
      txt := F.Unsigned(x.data[x.size - 1], base);
      FOR k := x.size - 2 TO 0 BY -1 DO
        txt := txt & F.Pad(F.Unsigned(x.data[k], base), pad, '0');
      END;
      RETURN txt;
    END;
  END FastFmtU;

(* can be optimized with a division routine that is specialised to small
   divisors *)
PROCEDURE SlowFmtU (x: T; base: F.Base; ): TEXT =
  VAR
    qr                             := BB.QuotRem{x, BB.Zero};
    b    : T;
    txt                            := "";
    digit: [0 .. LAST(F.Base) - 1];
  BEGIN
    TRY
      b := BB.FromInteger(base);
      WHILE NOT BB.IsZero(qr.quot) DO
        (* IO.Put(Dump(qr.quot) & "\n"); *)
        qr := BR.DivModU(qr.quot, b);
        <* ASSERT qr.rem.size <= 1 *>
        digit := qr.rem.data[0];
        IF digit < 10 THEN
          txt := Text.FromChar(VAL(ORD('0') + digit, CHAR)) & txt;
        ELSE
          txt := Text.FromChar(VAL(ORD('a') + digit - 10, CHAR)) & txt;
        END;
      END;
    EXCEPT
    | Arith.Error (err) =>
        <* ASSERT NOT ISTYPE(err, Arith.ErrorDivisionByZero) *>
    END;
    IF Text.Empty(txt) THEN RETURN "0"; ELSE RETURN txt; END;
  END SlowFmtU;

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}; ): TEXT =
  VAR txt: TEXT;
  BEGIN
    CASE style.base OF
    | 2 => txt := FastFmtU(x, 2, Word.Size);
    | 4 => txt := FastFmtU(x, 4, Word.Size DIV 2);
    | 16 => txt := FastFmtU(x, 16, Word.Size DIV 4);
    ELSE
      txt := SlowFmtU(x, style.base);
    END;
    IF x.sign THEN RETURN "-" & txt; ELSE RETURN txt; END;
  END Fmt;

PROCEDURE Tex
  (x: T; READONLY style := TexStyle{}; <* UNUSED *> within: Precedence; ):
  TEXT =
  BEGIN
    IF style.base = 10 THEN
      RETURN Fmt(x, FmtStyle{base := style.base});
    ELSE
      RETURN Fmt(x, FmtStyle{base := style.base}) & "_{"
               & F.Int(style.base) & "}";
    END;
  END Tex;

PROCEDURE Lex (rd: Rd.T; <* UNUSED *> READONLY style: LexStyle; ): T
  RAISES {L.Error, Rd.Failure, Thread.Alerted} =
  VAR
    z            := BB.Zero;
    b            := BB.FromInteger(10);
    neg: BOOLEAN;
  BEGIN
    TRY
      neg := Rd.GetChar(rd) = '-';
      IF NOT neg THEN Rd.UnGetChar(rd); END;
    EXCEPT
    | Rd.EndOfFile => RAISE L.Error;
    END;
    TRY
      LOOP
        VAR c := Rd.GetChar(rd);
        BEGIN
          IF NOT c IN SET OF CHAR{'0' .. '9'} THEN EXIT END;
          z := BB.Add(BB.Mul(z, b), BB.FromInteger(ORD(c) - ORD('0')));
        END;
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;
    IF neg THEN RETURN BB.Neg(z); ELSE RETURN z; END;
  END Lex;

BEGIN
END BigIntegerFmtLex.
