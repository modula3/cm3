(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Brand;

IMPORT M3, Expr, M3String, M3WString, M3Buf, Scanner, Module;
IMPORT TextExpr, Target, Type, Token, M3ID, Error, CG, Word;

REVEAL
  T = BRANDED "Brand.T" REF RECORD
    origin: INTEGER     := 0;
    type  : Type.T      := NIL;
    expr  : Expr.T      := NIL;
    val8  : M3String.T  := NIL;
    val32 : M3WString.T := NIL;
    next  : T           := NIL;
    error : BOOLEAN     := FALSE;
  END;

VAR brand_buf  : M3Buf.T := NIL;
VAR all_brands : ARRAY [0..97] OF T;

PROCEDURE Reset () =
  VAR t, u: T;
  BEGIN
    FOR i := FIRST (all_brands) TO LAST (all_brands) DO
      t := all_brands[i];
      WHILE (t # NIL) DO
        u := t.next;
        t.next  := NIL;
        t.error := FALSE;
        t := u;
      END;
      all_brands[i] := NIL;
    END;
  END Reset;

PROCEDURE Parse (): T =
  VAR t: T := NIL;
  BEGIN
    IF (Scanner.cur.token = Token.T.tBRANDED) THEN
      t := NEW (T, origin := Scanner.offset);
      Scanner.GetToken (); (* BRANDED *)
      IF (Scanner.cur.token IN Token.ExprStart)
        THEN t.expr := Expr.Parse ();
        ELSE t.expr := GenerateBrand ();
      END;
    END;
    RETURN t;
  END Parse;

PROCEDURE New (txt: TEXT): T =
  VAR t := NEW (T);
  BEGIN
    t.origin := Scanner.offset;
    t.expr   := TextExpr.New8 (M3String.Add (txt));
    RETURN t;
  END New;

PROCEDURE GenerateBrand (): Expr.T =
  CONST Suffix = ARRAY BOOLEAN OF CHAR { 'M', 'I' };
  VAR counter: ARRAY [0..4] OF CHAR;
  BEGIN
    Module.GetNextCounter (counter);

    (* build the string *)
    IF (brand_buf = NIL) THEN brand_buf := M3Buf.New (); END;
    M3ID.Put      (brand_buf, Module.Name (NIL));
    M3Buf.PutText (brand_buf, " # AuTo-BrAnD # ");
    M3Buf.PutSub  (brand_buf, counter);
    M3Buf.PutChar (brand_buf, Suffix [Module.IsInterface ()]);

    RETURN TextExpr.New8 (M3String.Add (M3Buf.ToText (brand_buf)));
  END GenerateBrand;

PROCEDURE Check (t: T;  holder: Type.T;
                 VAR hash: INTEGER;  VAR cs: Expr.CheckState) =
  VAR e: Expr.T;  xx: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN; END;
    t.type := holder;
    Expr.TypeCheck (t.expr, cs);
    e := Expr.ConstValue (t.expr);
    IF (e = NIL) THEN
      Error.Msg ("brand is not a constant");
    ELSIF TextExpr.Split8 (e, t.val8) THEN
      t.expr := e;
      xx := M3String.Hash (t.val8);
      hash := Word.Plus (Word.Times (hash, 37), xx);
      CheckDuplicate (t, xx);
    ELSIF TextExpr.Split32 (e, t.val32) THEN
      t.expr := e;
      xx := M3WString.Hash (t.val32);
      hash := Word.Plus (Word.Times (hash, 37), xx);
      CheckDuplicate (t, xx);
    ELSE
      Error.Msg ("brand is not a TEXT constant");
    END;
  END Check;

PROCEDURE CheckDuplicate (t: T;  hash: INTEGER) =
  VAR cell : INTEGER := hash MOD NUMBER (all_brands);
  VAR node : T       := all_brands[cell];
  BEGIN
    LOOP
      IF (node = NIL) THEN
        (* add an entry to the table *)
        t.next := all_brands[cell];
        all_brands[cell] := t;
        RETURN;
      ELSIF (node = t) OR (node.type = t.type) THEN
        (* ok, this is a repeated check of an existing brand *)
        RETURN;
      ELSIF (node.val8 = t.val8) AND (node.val32 = t.val32) THEN
        IF (node.type.origin # t.type.origin) THEN
          (* error, duplicate brand *)
          DuplicateError (t);
          DuplicateError (node);
        END;
        RETURN;
      END;
      node := node.next;
    END;
  END CheckDuplicate;

PROCEDURE DuplicateError (t: T) =
  VAR save := Scanner.offset;
  BEGIN
    IF NOT t.error THEN
      t.error := TRUE;
      Scanner.offset := t.origin;
      Error.Txt (ToText (t), "duplicate brand");
      Scanner.offset := save;
    END;
  END DuplicateError;

PROCEDURE Compile (t: T): INTEGER =
  VAR offset := -1;  len, n_bytes: INTEGER;
  BEGIN
    IF (t = NIL) THEN
      (* no brand *)
    ELSIF (t.val8 # NIL) THEN
      len := Target.Char.size * (M3String.Length (t.val8) + 1);
      n_bytes := (len - Target.Char.size) DIV Target.Int8.size;
      offset := Module.Allocate (Target.Integer.size + len,
                                 Target.Integer.align, TRUE, "brand");
      CG.Init_intt (offset, Target.Integer.size, n_bytes, TRUE);
      M3String.Init_chars (offset + Target.Integer.size, t.val8, TRUE);
    ELSIF (t.val32 # NIL) THEN
      len := Target.Word32.size * (M3WString.Length (t.val32) + 1);
      n_bytes := (len - Target.Word32.size) DIV Target.Int8.size;
      offset := Module.Allocate (Target.Integer.size + len,
                                 Target.Integer.align, TRUE, "brand");
      CG.Init_intt (offset, Target.Integer.size, n_bytes, TRUE);
      M3WString.Init_chars (offset + Target.Integer.size, t.val32, TRUE);
    END;
    RETURN offset;
  END Compile;

PROCEDURE GenFPrint (t: T;  VAR x: M3.FPInfo) =
  BEGIN
    IF (t = NIL) THEN
      (* no brand info *)
    ELSIF (t.val8 # NIL) THEN
      M3Buf.PutText (x.buf, "-BRAND8 ");
      M3Buf.PutInt  (x.buf, M3String.Length (t.val8));
      M3Buf.PutChar (x.buf, ' ');
      M3String.Put  (x.buf, t.val8);
    ELSIF (t.val32 # NIL) THEN
      M3Buf.PutText (x.buf, "-BRAND16 ");
    (* Even though the characters of this brand now occupy 32 bits, let's keep
       this string "-BRAND16", to avoid altering fingerprints and undermining
       pickles written earlier. *) 
      M3Buf.PutInt  (x.buf, M3WString.Length (t.val32));
      M3Buf.PutChar (x.buf, ' ');
      M3WString.PutLiteral (x.buf, t.val32);
    END;
  END GenFPrint;

PROCEDURE Equal (a, b: T): BOOLEAN =
  BEGIN
    IF    (a = NIL)       OR  (b = NIL)       THEN  RETURN (a = b);
    ELSIF (a.val8  # NIL) AND (b.val8 # NIL)  THEN  RETURN (a.val8 = b.val8);
    ELSIF (a.val32 # NIL) AND (b.val32 # NIL) THEN  RETURN (a.val32 = b.val32);
    ELSIF (a.val8  # NIL) AND (b.val32 # NIL) THEN  RETURN FALSE;
    ELSIF (a.val32 # NIL) AND (b.val8 # NIL)  THEN  RETURN FALSE;
    ELSE  RETURN Expr.IsEqual (a.expr, b.expr, NIL);
    END;
  END Equal;

PROCEDURE ToText (t: T): TEXT =
  VAR txt: TEXT := NIL;
  BEGIN
    IF    (t = NIL)       THEN txt := NIL;
    ELSIF (t.val8  # NIL) THEN txt := M3String.ToText (t.val8);
    ELSIF (t.val32 # NIL) THEN txt := M3WString.ToLiteral (t.val32);
    END;
    RETURN txt;
  END ToText;

BEGIN
END Brand.

