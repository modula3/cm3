(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE M3Lexer;

IMPORT Rd, Target, TInt, TFloat, TWord;
IMPORT M3ID, M3Scanner, Text;

FROM M3Scanner IMPORT TK_Comment, TK_Error, TK_EOF,
  TK_Ident, TK_Card_const, TK_Real_const, TK_Longreal_const,
  TK_Extended_const, TK_Char_const, TK_Text_const,
  TK_Begin_pragma, TK_End_pragma, TK_Comma;

CONST
  Backslash = '\134';  (* ASCII / Unicode value *)

TYPE
  SUPER = M3Scanner.Default;

REVEAL
  T = T_ BRANDED "M3Lexer.T" OBJECT
    saved        : BOOLEAN := FALSE;
    next_token   : CARDINAL  := TK_Comment;
    next_start   : CARDINAL  := 0;
    next_length  : CARDINAL  := 0;
    next_offset  : CARDINAL  := 0;
    next_line    : CARDINAL  := 0;
    next_column  : CARDINAL  := 0;
    next_msg     : TEXT      := NIL;
  OVERRIDES
    next        := NextToken;
    className   := ClassName;
    initFromRd  := InitFromRd;
    initFromBuf := InitFromBuf;
  END;

TYPE
  Pragma = REF RECORD
    id   : M3ID.T;
    val  : INTEGER;
    next : Pragma;
  END;

VAR
  pragmas: Pragma := NIL;

CONST
  CNames = ARRAY [TK_Inline .. TK_Fatal] OF TEXT {
    "<*INLINE*>", "<*EXTERNAL*>", "<*ASSERT*>", "<*UNUSED*>",
    "<*OBSOLETE*>", "<*TRACE*>", "<* calling convention *>",
    "<*FATAL*>"
  };

PROCEDURE ClassName (t: T;  tk: INTEGER): TEXT =
  BEGIN
    IF (FIRST (CNames) <= tk) AND (tk <= LAST (CNames))
      THEN RETURN CNames [tk];
      ELSE RETURN SUPER.className (t, tk);
    END;
  END ClassName;

PROCEDURE InitFromRd (t: T;  rd: Rd.T): T =
  BEGIN
    pragmas := NIL;
    RETURN SUPER.initFromRd (t, rd,
             skip_comments := TRUE, split_pragmas := TRUE);
  END InitFromRd;

PROCEDURE InitFromBuf (t: T;  buf: Buf): T =
  BEGIN
    pragmas := NIL;
    RETURN SUPER.initFromBuf (t, buf,
             skip_comments := TRUE, split_pragmas := TRUE);
  END InitFromBuf;

PROCEDURE NextToken (t: T) =
  BEGIN
    IF (t.saved) THEN
      t.saved  := FALSE;
      t.token  := t.next_token;
      t.start  := t.next_start;
      t.length := t.next_length;
      t.offset := t.next_offset;
      t.line   := t.next_line;
      t.column := t.next_column;
      t.msg    := t.next_msg;
    ELSE
      REPEAT
        SUPER.next (t);
        CASE t.token OF
        | TK_Ident          => FixID (t);
        | TK_Card_const     => FixInt (t);
        | TK_Real_const     => FixFloat (t, Target.Precision.Short);
        | TK_Longreal_const => FixFloat (t, Target.Precision.Long);
        | TK_Extended_const => FixFloat (t, Target.Precision.Extended);
        | TK_Char_const     => FixChar (t);
        | TK_Text_const     => FixText (t);
        | TK_Begin_pragma   => FixPragma (t);
        ELSE (* let the token through... *)
        END;
      UNTIL (t.token # TK_Comment);
    END;
  END NextToken;

PROCEDURE FixID (t: T) =
  BEGIN
    t.id := M3ID.FromStr (SUBARRAY (t.buffer^, t.offset, t.length));
  END FixID;

PROCEDURE FixInt (t: T) =
  VAR break := -1;  base: INTEGER;
  BEGIN
    FOR i := t.offset TO t.offset + t.length - 1 DO
      IF (t.buffer [i] = '_') THEN
        (* we have a based integer... *)
        break := i;
        EXIT;
      END;
    END;

    IF (break < 0) THEN (* scan a simple integer *)
      IF NOT TInt.New (SUBARRAY (t.buffer^, t.offset, t.length), t.int) THEN
        Err (t, "illegal integer literal");
      END;
    ELSIF NOT TInt.New (SUBARRAY (t.buffer^, t.offset, break - t.offset), t.int)
       OR NOT TInt.ToInt (t.int, base)
       OR (base < 2) OR (16 < base) THEN
      Err (t, "illegal base for integer literal");
    ELSIF NOT TWord.New (SUBARRAY (t.buffer^, break+1,
                                   t.offset + t.length - break - 1),
                         base, t.int) THEN
      Err (t, "illegal based integer literal");
    END;
  END FixInt;

PROCEDURE FixFloat (t: T;  prec: Target.Precision) =
  BEGIN
    IF NOT TFloat.New (SUBARRAY (t.buffer^, t.offset, t.length),
                       prec, t.float) THEN
      Err (t, "illegal floating-point literal");
    END;
  END FixFloat;

PROCEDURE FixChar (t: T) =
  VAR ch: CHAR;
  BEGIN
    ch := t.buffer [t.offset+1];
    IF (t.length = 3) THEN (* ok *)
    ELSIF ConvertEscape (t, t.offset+1, t.length-2, ch) THEN (* ok *)
    ELSE Err (t, "illegal character literal");
    END;
    t.char := ORD (ch);
  END FixChar;

PROCEDURE FixText (t: T) =
  VAR
    len  := 0;
    cur  := t.offset + 1;        (* ignore the surrounding quotes *)
    stop := cur + t.length - 2;  (* ignore the surrounding quotes *)
    txt  := "";
    ch   : CHAR;
    buf  : ARRAY [0..255] OF CHAR;
  BEGIN
    WHILE (cur < stop) DO
      ch := t.buffer [cur];
      IF (ch = Backslash) THEN
        IF (cur+1 < stop) AND ConvertEscape (t, cur, 2, ch) THEN
          INC (cur);
        ELSIF (cur+3 < stop) AND ConvertEscape (t, cur, 4, ch) THEN
          INC (cur, 3);
        ELSE
          Err (t, "unrecognized escape sequence in text literal");
          RETURN;
        END;
      END;
      IF (len >= NUMBER (buf)) THEN
        txt := txt & Text.FromChars (buf);
        len := 0;
      END;
      buf [len] := ch;  INC (len);
      INC (cur);
    END;
    IF (len > 0) THEN
      txt := txt & Text.FromChars (SUBARRAY (buf, 0, len));
    END;
    t.text :=txt;
  END FixText;

PROCEDURE ConvertEscape (t: T;  offs, len: INTEGER;  VAR ch: CHAR): BOOLEAN =
  VAR c0, c1, c2: CHAR;  
  BEGIN
    IF t.buffer[offs] # Backslash THEN RETURN FALSE; END;

    IF (len = 2) THEN
      c0 := t.buffer [offs+1];
      IF    (c0 = 'n')       THEN ch := '\n';       RETURN TRUE;
      ELSIF (c0 = 't')       THEN ch := '\t';       RETURN TRUE;
      ELSIF (c0 = 'r')       THEN ch := '\r';       RETURN TRUE;
      ELSIF (c0 = 'f')       THEN ch := '\f';       RETURN TRUE;
      ELSIF (c0 = '\'')      THEN ch := '\'';       RETURN TRUE;
      ELSIF (c0 = '\"')      THEN ch := '\"';       RETURN TRUE;
      ELSIF (c0 = Backslash) THEN ch := Backslash;  RETURN TRUE;
      END;

    ELSIF (len = 4) THEN
      c0 := t.buffer [offs+1];
      c1 := t.buffer [offs+2];
      c2 := t.buffer [offs+3];
      IF    ('0' <= c0) AND (c0 <= '7')
        AND ('0' <= c1) AND (c1 <= '7')
        AND ('0' <= c2) AND (c2 <= '7') THEN
        ch := VAL ( (ORD (c0) - ORD ('0')) * 64
                  + (ORD (c1) - ORD ('0')) * 8
                  + (ORD (c2) - ORD ('0')), CHAR);
        RETURN TRUE;
      END;
    END;

    RETURN FALSE;
  END ConvertEscape;

PROCEDURE FixPragma (t: T) =
  VAR
    p: Pragma;
    save_start   : CARDINAL;
    save_length  : CARDINAL;
    save_offset  : CARDINAL;
    save_line    : CARDINAL;
    save_column  : CARDINAL;
  BEGIN
    (* just in case we need to back up, save the current token *)
    <*ASSERT t.token = TK_Begin_pragma *>
    save_start  := t.start;
    save_length := t.length;
    save_offset := t.offset;
    save_line   := t.line;
    save_column := t.column;

    (* get the ID that defines the pragma *)
    SUPER.next (t);

    IF t.token # TK_Ident THEN
      (* oops, it's a badly formed pragma *)
      t.saved       := TRUE;
      t.next_token  := t.token;
      t.next_start  := t.start;
      t.next_length := t.length;
      t.next_offset := t.offset;
      t.next_line   := t.line;
      t.next_column := t.column;
      t.next_msg    := t.msg;

      t.token  := TK_Begin_pragma;
      t.start  := save_start;
      t.length := save_length;
      t.offset := save_offset;
      t.line   := save_line;
      t.column := save_column;
      t.msg    := NIL;
      RETURN;
    END;

    t.id := M3ID.FromStr (SUBARRAY (t.buffer^, t.offset, t.length));

    p := LookUpPragma (t.id);

    IF (p = NIL) AND Target.FindConvention (M3ID.ToText (t.id)) # NIL THEN
      (* it's a legit calling convention on the current target *)
      t.token := TK_CallConv;
      AddPragma (M3ID.ToText (t.id), TK_CallConv);

    ELSIF (p = NIL) THEN
      (* unknown pragma, restore the begin_pragma/id token sequence... *)
      t.saved       := TRUE;
      t.next_token  := t.token;
      t.next_start  := t.start;
      t.next_length := t.length;
      t.next_offset := t.offset;
      t.next_line   := t.line;
      t.next_column := t.column;
      t.next_msg    := t.msg;

      t.token  := TK_Begin_pragma;
      t.start  := save_start;
      t.length := save_length;
      t.offset := save_offset;
      t.line   := save_line;
      t.column := save_column;
      t.msg    := NIL;

    ELSIF (p.val > 0) THEN
      (* it's a recognized pragma to pass through *)
      t.token := p.val;

    ELSIF (p.val = IGNORE_PRAGMA) THEN
      EatPragma (t);

    ELSIF (p.val = LINE_PRAGMA) THEN
      EatPragma (t);  (* until we need something better... *)

    ELSIF (p.val = PRAGMA_PRAGMA) THEN
      SUPER.next (t);  (* PRAGMA *)
      WHILE (t.token = TK_Ident)
        OR ((M3Scanner.First_Keyword <= t.token)
            AND (t.token <= M3Scanner.Last_Keyword)) DO
        FixID (t);
        IF LookUpPragma (t.id) = NIL THEN
          AddPragma (M3ID.ToText (t.id), IGNORE_PRAGMA);
        END;
        SUPER.next (t);  (* Ident *)
        IF (t.token # TK_Comma) THEN EXIT END;
        SUPER.next (t); (* , *)
      END;
      IF (t.token # TK_End_pragma) THEN
        t.token := TK_Error;
        t.msg   := "missing '*>' on <*PRAGMA*> pragma";
      ELSE
        t.token := TK_Comment; (* fetch the next real token in the outer loop *)
      END;

    ELSE <*ASSERT FALSE*>
    END;

  END FixPragma;

PROCEDURE LookUpPragma (id: M3ID.T): Pragma =
  VAR p: Pragma;
  BEGIN
    IF (pragmas = NIL) THEN InitPragmas () END;
    p := pragmas;
    WHILE (p # NIL) AND (p.id # id) DO p := p.next; END;
    RETURN p;
  END LookUpPragma;

PROCEDURE EatPragma (t: T) =
  VAR key := t.id;  line := t.line;  offs := t.offset;  col := t.column;
  BEGIN
    LOOP
      IF (t.token = TK_End_pragma) THEN
        t.token := TK_Comment; (* so the top-level NextToken() cycles *)
        EXIT;
      ELSIF (t.token = TK_Error) THEN
        EXIT;
      ELSIF (t.token = TK_EOF) THEN
        t.token  := TK_Error;
        t.line   := line;
        t.offset := offs;
        t.column := col;
        t.msg := "<*" & M3ID.ToText(key) & "*> pragma is missing its closing '*>'";
        EXIT;
      ELSE
        SUPER.next (t);
      END;
    END;
  END EatPragma;

CONST
  IGNORE_PRAGMA = -1;
  PRAGMA_PRAGMA = -2;
  LINE_PRAGMA   = -3;

PROCEDURE InitPragmas () = 
  BEGIN
    AddPragma ("TRACE",     TK_Trace);
    AddPragma ("OBSOLETE",  TK_Obsolete);
    AddPragma ("LINE",      LINE_PRAGMA);
    AddPragma ("PRAGMA",    PRAGMA_PRAGMA);
    AddPragma ("NOWARN",    IGNORE_PRAGMA);
    AddPragma ("INLINE",    TK_Inline);
    AddPragma ("UNUSED",    TK_Unused);
    AddPragma ("FATAL",     TK_Fatal);
    AddPragma ("EXTERNAL",  TK_External);
    AddPragma ("ASSERT",    TK_Assert);
  END InitPragmas;

PROCEDURE AddPragma (txt: TEXT;  val: INTEGER) =
  BEGIN
    pragmas := NEW (Pragma, id := M3ID.Add (txt), val := val, next := pragmas);
  END AddPragma;

PROCEDURE Err (t: T;  msg: TEXT) =
  BEGIN
    t.msg := msg & ": " & t.toText ();
    t.token := TK_Error;
  END Err;

BEGIN
END M3Lexer.
