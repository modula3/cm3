(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

(* "RegExpr" provides regular expression matching of text strings *)

MODULE RegExpr;

IMPORT Text;

REVEAL
  T = BRANDED "RegExpr.T" REF RECORD
    body : TEXT;
    root : CARDINAL;
    ops  : REF ARRAY OF Desc;
  END;

TYPE
  Op = { Or, And, Concat, AnyString, ThisString };
  StrLen = [0..16_ffff];

  Desc = RECORD
    a, b: CARDINAL;  (* operands: (left, right) or string: (start, len) *)
    min, max: StrLen;
    ch: CHAR;
    op: Op;
  END;

(*------------------------------------------------------------ compiling ---*)

TYPE
  ParseState = RECORD
    body     : TEXT;
    ops      : REF ARRAY OF Desc;
    ch       : CHAR;
    len      : CARDINAL;
    next_ch  : CARDINAL;
    next_buf : CARDINAL;
    next_op  : CARDINAL;
  END;

PROCEDURE Compile (pattern: TEXT): T  RAISES {Error} =
  VAR t: T;  s: ParseState;
  BEGIN
    IF (pattern = NIL) THEN RAISE Error ("NIL pattern"); END;
    t := NEW (T, body := pattern);
    s.len := Text.Length (pattern);
    IF (s.len < 1) THEN
      s.next_op := 0;
      t.ops  := NEW (REF ARRAY OF Desc, 1);
      t.root := EmptyString (s);
    ELSE
      s.body     := pattern;
      s.ops      := NEW (REF ARRAY OF Desc, s.len + s.len);
      s.ch       := Text.GetChar (pattern, 0);
      s.next_ch  := 1;
      s.next_buf := 0;
      s.next_op  := 0;
      t.root     := ParseExpr (s);
      t.ops      := s.ops;
    END;
    RETURN t;
  END Compile;

PROCEDURE ParseExpr (VAR s: ParseState): CARDINAL  RAISES {Error} =
  VAR a, b: CARDINAL;
  BEGIN
    a := ParseTerm (s);
    WHILE (s.ch = '|') DO
      NextCh (s);
      b := ParseTerm (s);
      WITH z = s.ops [s.next_op] DO
        z.op := Op.Or;  z.a := a;  z.b := b;
        z.min := MIN (s.ops[a].min, s.ops[b].min);
        z.max := MAX (s.ops[b].max, s.ops[b].max);
      END;
      a := s.next_op;
      INC (s.next_op);
    END;
    RETURN a;
  END ParseExpr;

PROCEDURE ParseTerm (VAR s: ParseState): CARDINAL  RAISES {Error} =
  VAR a, b: CARDINAL;
  BEGIN
    a := ParseFactor (s);
    WHILE (s.ch = '&') DO
      NextCh (s);
      b := ParseFactor (s);
      WITH z = s.ops [s.next_op] DO
        z.op := Op.And;  z.a := a;  z.b := b;
        z.min := MAX (s.ops[a].min, s.ops[b].min);
        z.max := MIN (s.ops[b].max, s.ops[b].max);
      END;
      a := s.next_op;
      INC (s.next_op);
    END;
    RETURN a;
  END ParseTerm;

PROCEDURE ParseFactor (VAR s: ParseState): CARDINAL  RAISES {Error} =
  VAR a, b: CARDINAL;
  BEGIN
    a := ParsePrimary (s);
    WHILE (s.next_ch <= s.len)
      AND (s.ch # '|') AND (s.ch # '&') AND (s.ch # ')') DO
      b := ParsePrimary (s);
      WITH z = s.ops [s.next_op] DO
        z.op := Op.Concat;  z.a := a;  z.b := b;
        z.min := MIN (s.ops[a].min + s.ops[b].min, LAST (StrLen));
        z.max := MIN (s.ops[a].max + s.ops[b].max, LAST (StrLen));
      END;
      a := s.next_op;
      INC (s.next_op);
    END;
    RETURN a;
  END ParseFactor;

PROCEDURE ParsePrimary (VAR s: ParseState): CARDINAL  RAISES {Error} =
  VAR x := s.next_op;
  BEGIN
    CASE s.ch OF
    | '&', '|', ')' => 
        RETURN EmptyString (s);

    | '*' =>
        NextCh (s);
        WITH z = s.ops [x] DO
          z.op  := Op.AnyString;
          z.min := 0;
          z.max := LAST (StrLen);
        END;
        INC (s.next_op);
        RETURN x;

    | '@' =>
        NextCh (s);
        WITH z = s.ops [x] DO
          z.op  := Op.AnyString;
          z.min := 1;
          z.max := 1;
        END;
        INC (s.next_op);
        RETURN x;

    | '(' =>
        NextCh (s);
        x := ParseExpr (s);
        IF (s.ch = ')') THEN
          NextCh (s);  (* ok *)
        ELSE
          RAISE Error ("unmatched parenthesis");
        END;
        RETURN x;

    ELSE
        RETURN ParseString (s);
    END;
  END ParsePrimary;

PROCEDURE ParseString (VAR s: ParseState): CARDINAL =
  VAR x := s.next_op;
  BEGIN
    INC (s.next_op);
    WITH z = s.ops [x] DO
      z.op  := Op.ThisString;
      z.a   := s.next_buf;
      z.b   := 0;

      WHILE (s.next_ch <= s.len)
        AND (s.ch # '|') AND (s.ch # '&') AND (s.ch # '*')
        AND (s.ch # '@') AND (s.ch # '(') AND (s.ch # ')') DO
        IF (s.ch = '\134') AND (s.next_ch < s.len) THEN
          NextCh (s); (* eat the backslash escape *)
        END;
        s.ops[s.next_buf].ch := s.ch;  INC (s.next_buf);
        INC (z.b);
        NextCh (s);
      END;

      z.min := z.b;
      z.max := z.b
    END;

    RETURN x;
  END ParseString;

PROCEDURE EmptyString (VAR s: ParseState): CARDINAL =
  VAR x := s.next_op;
  BEGIN
    WITH z = s.ops[x] DO
      z.op  := Op.ThisString;
      z.a   := 0;
      z.b   := 0;
      z.min := 0;
      z.max := 0;
    END;
    INC (s.next_op);
    RETURN x;
  END EmptyString;

PROCEDURE NextCh (VAR s: ParseState) =
  BEGIN
    IF (s.next_ch < s.len) THEN
      s.ch := Text.GetChar (s.body, s.next_ch);
      INC (s.next_ch);
    ELSE
      s.ch := '\000';
      INC (s.next_ch);
    END;
  END NextCh;

(*------------------------------------------------------------- matching ---*)

PROCEDURE Match (t: T;  txt: TEXT): BOOLEAN =
  BEGIN
    RETURN MatchSubstring (t, txt, 0, 0);
  END Match;

PROCEDURE MatchSubstring (t: T;  txt: TEXT;  pre, post: CARDINAL): BOOLEAN =
  VAR len: INTEGER;  buf: ARRAY [0..255] OF CHAR;  ref: REF ARRAY OF CHAR;
  BEGIN
    IF (t   = NIL) THEN RETURN TRUE;  END;
    IF (txt = NIL) THEN RETURN FALSE; END;
    len := Text.Length (txt) - pre - post;
    IF (len <= NUMBER (buf)) THEN
      Text.SetChars (buf, txt, pre);
      RETURN MatchSub (t, SUBARRAY (buf, 0, len));
    ELSE
      ref := NEW (REF ARRAY OF CHAR, len);
      Text.SetChars (buf, txt, pre);
      RETURN MatchSub (t, ref^);
    END;
  END MatchSubstring;

PROCEDURE MatchSub (t: T;  READONLY str: ARRAY OF CHAR): BOOLEAN =
  BEGIN
    IF (t   = NIL)         THEN RETURN TRUE;  END;
    IF (NUMBER (str) <= 0) THEN RETURN FALSE; END;
    RETURN DoMatch (t.ops, t.root, str, 0, NUMBER (str));
  END MatchSub;

PROCEDURE DoMatch (ops: REF ARRAY OF Desc;  x: CARDINAL;
                   READONLY txt: ARRAY OF CHAR;
                   start, len: INTEGER): BOOLEAN =
  BEGIN
    WITH z = ops[x] DO
      IF (len < z.min) OR (z.max < len) THEN RETURN FALSE; END;

      CASE z.op OF

      | Op.Or =>
          RETURN DoMatch (ops, z.a, txt, start, len)
              OR DoMatch (ops, z.b, txt, start, len);

      | Op.And =>
          RETURN DoMatch (ops, z.a, txt, start, len)
             AND DoMatch (ops, z.b, txt, start, len);

      | Op.AnyString =>
          RETURN TRUE;

      | Op.ThisString =>
          FOR i := 0 TO z.b - 1 DO
            IF (ops[z.a + i].ch # txt [start + i]) THEN RETURN FALSE; END;
          END;
          RETURN TRUE;

      | Op.Concat =>
          WITH za = ops[z.a],  zb = ops[z.b] DO
            VAR max_a := MIN (za.max, len - zb.min);
                min_a := MAX (za.min, len - zb.max);
            BEGIN
              FOR i := max_a TO min_a BY -1 DO
                IF    DoMatch (ops, z.a, txt, start,   i    )
                  AND DoMatch (ops, z.b, txt, start+i, len-i) THEN
                  RETURN TRUE;
                END;
              END;
              RETURN FALSE;
            END;
          END;

      END; (* CASE*)
    END; (* WITH *)
  END DoMatch; 

(*----------------------------------------------------------------- misc ---*)

PROCEDURE SimpleString (t: T): TEXT =
  BEGIN
    IF (t = NIL) THEN  RETURN NIL;  END;
    WITH z = t.ops [t.root] DO
      IF (z.op = Op.ThisString)
        AND (z.a = 0)
        AND (z.b = Text.Length (t.body)) THEN
        RETURN t.body;
      END;
    END;
    RETURN NIL;
  END SimpleString;

BEGIN
END RegExpr.


