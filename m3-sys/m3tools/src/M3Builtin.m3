(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Builtin;

IMPORT M3Const, M3Type, Target, TInt, TFloat;

TYPE
  Desc = RECORD
    proc     : Proc;
    min_args : CARDINAL;
    max_args : CARDINAL;
    is_const : BOOLEAN;
  END;

CONST
  PDesc = ARRAY Proc OF Desc {
    Desc{Proc.Abs,             1, 1, TRUE },
    Desc{Proc.Adr,             1, 1, FALSE },
    Desc{Proc.AdrSize,         1, 1, TRUE },
    Desc{Proc.BitSize,         1, 1, TRUE },
    Desc{Proc.ByteSize,        1, 1, TRUE },
    Desc{Proc.Ceiling,         1, 1, TRUE },
    Desc{Proc.Dec,             1, 2, FALSE },
    Desc{Proc.Dispose,         1, 1, FALSE },
    Desc{Proc.First,           1, 1, TRUE },
    Desc{Proc.Float,           1, 2, TRUE },
    Desc{Proc.Floor,           1, 1, TRUE },
    Desc{Proc.Inc,             1, 2, FALSE },
    Desc{Proc.IsType,          2, 2, TRUE },
    Desc{Proc.Last,            1, 1, TRUE },
    Desc{Proc.Loophole,        2, 2, FALSE },
    Desc{Proc.Max,             2, 2, TRUE },
    Desc{Proc.Min,             2, 2, TRUE },
    Desc{Proc.Narrow,          2, 2, TRUE },
    Desc{Proc.New,             1, 99999, FALSE },
    Desc{Proc.Number,          1, 1, TRUE },
    Desc{Proc.Ord,             1, 1, TRUE },
    Desc{Proc.Round,           1, 1, TRUE },
    Desc{Proc.Subarray,        3, 3, TRUE },
    Desc{Proc.Trunc,           1, 1, TRUE },
    Desc{Proc.Typecode,        1, 1, FALSE },
    Desc{Proc.Val,             2, 2, TRUE },
    Desc{Proc.WordPlus,        2, 2, TRUE },
    Desc{Proc.WordTimes,       2, 2, TRUE },
    Desc{Proc.WordMinus,       2, 2, TRUE },
    Desc{Proc.WordDivide,      2, 2, TRUE },
    Desc{Proc.WordMod,         2, 2, TRUE },
    Desc{Proc.WordLT,          2, 2, TRUE },
    Desc{Proc.WordLE,          2, 2, TRUE },
    Desc{Proc.WordGT,          2, 2, TRUE },
    Desc{Proc.WordGE,          2, 2, TRUE },
    Desc{Proc.WordAnd,         2, 2, TRUE },
    Desc{Proc.WordOr,          2, 2, TRUE },
    Desc{Proc.WordXor,         2, 2, TRUE },
    Desc{Proc.WordNot,         1, 1, TRUE },
    Desc{Proc.WordShift,       2, 2, TRUE },
    Desc{Proc.WordLeftShift,   2, 2, TRUE },
    Desc{Proc.WordRightShift,  2, 2, TRUE },
    Desc{Proc.WordRotate,      2, 2, TRUE },
    Desc{Proc.WordLeftRotate,  2, 2, TRUE },
    Desc{Proc.WordRightRotate, 2, 2, TRUE },
    Desc{Proc.WordExtract,     3, 3, TRUE },
    Desc{Proc.WordInsert,      4, 4, TRUE }
  };

PROCEDURE Eval (p: Proc;  READONLY args: ARRAY OF M3Const.T;
                VAR(*OUT*) val: M3Const.T)
  RAISES {M3Const.Error} =
  VAR n_args := NUMBER (args);  zero: Target.Float;
  BEGIN
    MustBe (PDesc[p].is_const);
    IF (n_args < PDesc[p].min_args) OR (PDesc[p].max_args < n_args) THEN
      RAISE M3Const.Error ("wrong number of arguments");
    END;

    CASE p OF
    | Proc.Abs =>
        WITH z = args[0] DO
          IF z.class = M3Const.Class.Integer THEN
            val.class := z.class;
            val.type  := M3Type.Integer;
            IF TInt.LT (z.int, TInt.Zero)
              THEN MustBe (TInt.Subtract (TInt.Zero, z.int, val.int));
              ELSE val.int := z.int;
            END;
          ELSIF z.class = M3Const.Class.Float THEN
            IF    (z.float.pre = Target.Precision.Short) THEN zero := TFloat.ZeroR;
            ELSIF (z.float.pre = Target.Precision.Long) THEN  zero := TFloat.ZeroL;
            ELSE                                              zero := TFloat.ZeroX;
            END;
            val.class := z.class;
            val.type  := z.type;
            IF TFloat.LT (z.float, zero)
              THEN MustBe (TFloat.Subtract (zero, z.float, val.float));
              ELSE val.float := z.float;
            END;
          ELSE
            MustBe (FALSE);
          END;
        END;

    | Proc.AdrSize => NotImpl ("ADRSIZE");
    | Proc.BitSize => NotImpl ("BITSIZE");

    | Proc.ByteSize =>
        WITH z = args[0] DO
          IF z.class = M3Const.Class.Type THEN
            VAR info: M3Type.Info; BEGIN
              M3Type.GetInfo (z.type, info);
              val.type  := M3Type.Integer;
              val.class := M3Const.Class.Integer;
              MustBe (info.size >= 0);
              MustBe (TInt.FromInt (info.size, val.int));
            END;
          ELSE
            NotImpl ("BYTESIZE(expr)");
          END;
        END;

    | Proc.Ceiling => NotImpl ("CEILING");
    | Proc.First => NotImpl ("FIRST");
    | Proc.Float => NotImpl ("FLOAT");
    | Proc.Floor => NotImpl ("FLOOR");
    | Proc.IsType => NotImpl ("ISTYPE");
    | Proc.Last => NotImpl ("LAST");

    | Proc.Max =>
        WITH a = args[0], b = args[0] DO
          MustBe (a.class = b.class);
          IF a.class = M3Const.Class.Integer THEN
            val.class := a.class;
            val.type  := M3Type.Integer;
            IF TInt.LT (a.int, b.int)
              THEN val.int := b.int
              ELSE val.int := a.int;
            END;
          ELSIF a.class = M3Const.Class.Enum THEN
            val.class := a.class;
            val.info  := MAX (a.info, b.info);
            val.type  := a.type;
          ELSIF a.class = M3Const.Class.Float THEN
            MustBe (a.float.pre = b.float.pre);
            val.class := a.class;
            val.type  := a.type;
            IF TFloat.LT (a.float, b.float)
              THEN val.float := b.float;
              ELSE val.float := a.float;
            END;
          ELSE
            MustBe (FALSE);
          END;
        END;

    | Proc.Min =>
        WITH a = args[0], b = args[0] DO
          MustBe (a.class = b.class);
          IF a.class = M3Const.Class.Integer THEN
            val.class := a.class;
            val.type  := M3Type.Integer;
            IF TInt.LT (a.int, b.int)
              THEN val.int := a.int
              ELSE val.int := b.int;
            END;
          ELSIF a.class = M3Const.Class.Enum THEN
            val.class := a.class;
            val.info  := MIN (a.info, b.info);
            val.type  := a.type;
          ELSIF a.class = M3Const.Class.Float THEN
            MustBe (a.float.pre = b.float.pre);
            val.class := a.class;
            val.type  := a.type;
            IF TFloat.LT (a.float, b.float)
              THEN val.float := a.float;
              ELSE val.float := b.float;
            END;
          ELSE
            MustBe (FALSE);
          END;
        END;

    | Proc.Narrow => NotImpl ("NARROW");
    | Proc.Number => NotImpl ("NUMBER");
    | Proc.Ord => NotImpl ("ORD");
    | Proc.Round => NotImpl ("ROUND");
    | Proc.Subarray => NotImpl ("SUBARRAY");
    | Proc.Trunc => NotImpl ("TRUNC");
    | Proc.Val => NotImpl ("VAL");
    | Proc.WordPlus => NotImpl ("Word.Plus");
    | Proc.WordTimes => NotImpl ("Word.Times");
    | Proc.WordMinus => NotImpl ("Word.Minus");
    | Proc.WordDivide => NotImpl ("Word.Divide");
    | Proc.WordMod => NotImpl ("Word.Mod");
    | Proc.WordLT => NotImpl ("Word.LT");
    | Proc.WordLE => NotImpl ("Word.LE");
    | Proc.WordGT => NotImpl ("Word.GT");
    | Proc.WordGE => NotImpl ("Word.GE");
    | Proc.WordAnd => NotImpl ("Word.And");
    | Proc.WordOr => NotImpl ("Word.Or");
    | Proc.WordXor => NotImpl ("Word.Xor");
    | Proc.WordNot => NotImpl ("Word.Not");
    | Proc.WordShift => NotImpl ("Word.Shift");
    | Proc.WordLeftShift => NotImpl ("Word.LeftShift");
    | Proc.WordRightShift => NotImpl ("Word.RightShift");
    | Proc.WordRotate => NotImpl ("Word.Rotate");
    | Proc.WordLeftRotate => NotImpl ("Word.LeftRotate");
    | Proc.WordRightRotate => NotImpl ("Word.RightRotate");
    | Proc.WordExtract => NotImpl ("Word.Extract");
    | Proc.WordInsert => NotImpl ("Word.Insert");
    ELSE <* ASSERT FALSE *>  (* ==> NOT PDesc[i].is_const *)
    END;
  END Eval;

PROCEDURE NotImpl (func: TEXT) RAISES {M3Const.Error} =
  BEGIN
    RAISE M3Const.Error ("cannot evaluate builtin function " & func & " yet.");
  END NotImpl;

PROCEDURE MustBe (b: BOOLEAN) RAISES {M3Const.Error} =
  BEGIN
    IF NOT b THEN RAISE M3Const.Error ("not a constant") END;
  END MustBe;

BEGIN
  FOR p := FIRST (PDesc) TO LAST (PDesc) DO
    <*ASSERT PDesc[p].proc = p *>
  END;
END M3Builtin.
