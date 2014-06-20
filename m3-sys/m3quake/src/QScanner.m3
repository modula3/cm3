(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Feb 21 11:20:03 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

MODULE QScanner;

IMPORT File, OSError, Text;
IMPORT M3File, Quake, QToken, QIdent;
(** IMPORT Stdio, Wr, Fmt; **)

TYPE
  TK = QToken.T;

REVEAL
  T = T_ BRANDED OBJECT
    map     : Quake.IDMap := NIL;
    char    : CHAR;
    curLine : INTEGER := 0;
    cursor  : INTEGER := 0;
    buflen  : INTEGER := 0;
    buffer  : REF ARRAY OF CHAR := NIL;
  OVERRIDES
    init     := Init;
    initText := InitText;
    next     := Next;
  END;

CONST
  EOFChar = '\000';

VAR
  init_done    := FALSE;
  AlphaNumeric := ARRAY CHAR OF BOOLEAN { FALSE, .. };

PROCEDURE Init (t: T;  f: File.T;  map: Quake.IDMap): T =
  VAR size: INTEGER;
  BEGIN
    IF NOT init_done THEN InitTables () END;

    (* slurp the source into memory *)
    TRY
     WITH stat = f.status () 
     DO 
      size := VAL(stat.size, INTEGER);
      t.buffer := NEW (REF ARRAY OF CHAR, MAX (0, size) + 1);
      t.buflen := M3File.Read (f, t.buffer^, size);
      IF (t.buflen # size) THEN RETURN NIL; END;
      t.buffer [t.buflen] := EOFChar;
     END;
    EXCEPT OSError.E => RETURN NIL;
    END;

    t.map     := map;
    t.token   := TK.Unknown;
    t.line    := 0;
    t.char    := ' ';
    t.curLine := 1;
    t.cursor  := 0;
    t.start   := 0;
    t.length  := 0;
    t.buflen  := NUMBER (t.buffer^);
    
    RETURN t;
  END Init;

PROCEDURE InitText (t: T;  txt: TEXT;  map: Quake.IDMap): T =
  BEGIN
    IF NOT init_done THEN InitTables () END;

    t.buflen := Text.Length (txt);
    t.buffer := NEW (REF ARRAY OF CHAR, t.buflen + 1);
    Text.SetChars (t.buffer^, txt);
    t.buffer [t.buflen] := EOFChar;

    t.map     := map;
    t.token   := TK.Unknown;
    t.line    := 0;
    t.char    := ' ';
    t.curLine := 1;
    t.cursor  := 0;
    t.start   := 0;
    t.length  := 0;
    t.buflen  := NUMBER (t.buffer^);
    
    RETURN t;
  END InitText;

PROCEDURE InitTables () =
  BEGIN
    WhiteSpace [' ']  := TRUE;
    WhiteSpace ['\n'] := TRUE;
    WhiteSpace ['\t'] := TRUE;
    WhiteSpace ['\r'] := TRUE;
    WhiteSpace ['\f'] := TRUE;

    AlphaNumeric ['_'] := TRUE;
    FOR c := 'a' TO 'z' DO AlphaNumeric [c] := TRUE END;
    FOR c := 'A' TO 'Z' DO AlphaNumeric [c] := TRUE END;
    FOR c := '0' TO '9' DO AlphaNumeric [c] := TRUE END;

    init_done := TRUE;
  END InitTables;

(**
PROCEDURE Next (t: T) =
  <*FATAL ANY*>
  BEGIN
    NextXX (t);
    Wr.PutText (Stdio.stdout, "tok ");
    Wr.PutText (Stdio.stdout, Fmt.Int (t.line));
    Wr.PutText (Stdio.stdout, " => ");
    Wr.PutText (Stdio.stdout, QToken.Name [t.token]);
    Wr.PutText (Stdio.stdout, "\n");
  END Next;
**)

PROCEDURE Next (t: T) =
  BEGIN
    LOOP
      (* skip white space *)
      WHILE WhiteSpace [t.char] DO NextChar (t) END;

      (* remember where this token starts *)
      t.line := t.curLine;
      t.start := t.cursor - 1;

      CASE t.char OF
      | '%' => (* Single-line comment *)
          NextChar (t);
          WHILE (t.char # '\n') AND (t.char # EOFChar) DO NextChar (t); END;

      | '/' => (* C-style comment *)
          NextChar (t);
          IF (t.char # '*') THEN  t.token := TK.Unknown;  EXIT;  END;
          NextChar (t);
          SkipComment (t);

      | '"'                         => ReadString (t);    EXIT;
      | '0' .. '9'                  => ReadCardinal (t);  EXIT;
      | 'a' .. 'z', 'A' .. 'Z', '_' => ReadName (t);      EXIT;

        (* It's punctuation *)
      | '$'     => t.token := TK.Dollar;     NextChar (t);  EXIT;
      | '&'     => t.token := TK.Ampersand;  NextChar (t);  EXIT;
      | '('     => t.token := TK.LParen;     NextChar (t);  EXIT;
      | ')'     => t.token := TK.RParen;     NextChar (t);  EXIT;
      | '+'     => t.token := TK.Plus;       NextChar (t);  EXIT;
      | ','     => t.token := TK.Comma;      NextChar (t);  EXIT;
      | ':'     => t.token := TK.Colon;      NextChar (t);  EXIT;
      | '<'     => t.token := TK.Less;       NextChar (t);  EXIT;
      | '='     => t.token := TK.Equal;      NextChar (t);  EXIT;
      | '>'     => t.token := TK.Greater;    NextChar (t);  EXIT;
      | '@'     => t.token := TK.At;         NextChar (t);  EXIT;
      | '['     => t.token := TK.LSquare;    NextChar (t);  EXIT;
      | ']'     => t.token := TK.RSquare;    NextChar (t);  EXIT;
      | '{'     => t.token := TK.LBrace;     NextChar (t);  EXIT;
      | '}'     => t.token := TK.RBrace;     NextChar (t);  EXIT;
      | EOFChar => t.token := TK.EOF;                       EXIT;

      ELSE  t.token := TK.Unknown; EXIT;
      END;
    END; (* LOOP *)

    t.length := t.cursor - t.start - 1;
  END Next;

PROCEDURE NextChar (t: T) =
  BEGIN
    IF (t.cursor <= t.buflen) THEN
      t.char := t.buffer [t.cursor];
      INC (t.cursor);
      IF (t.char = '\n') THEN INC (t.curLine) END;
    ELSE
      t.char := EOFChar;
    END;
  END NextChar;

PROCEDURE SkipComment (t: T) =
  VAR c0 := ' ';  c1 := t.char;
  BEGIN
    WHILE (c1 # EOFChar) AND ((c1 # '/') OR (c0 # '*')) DO
      c0 := c1;
      NextChar (t);
      c1 := t.char;
    END;
    NextChar (t);
  END SkipComment;

PROCEDURE ReadCardinal (t: T) =
  VAR i: CARDINAL := 0;
  BEGIN
    WHILE ('0' <= t.char) AND (t.char <= '9') DO
      i := i * 10 + ORD(t.char) - ORD('0');
      NextChar(t);
    END;
    t.cardinal := i;
    t.token    := TK.Cardinal;
  END ReadCardinal;

PROCEDURE ReadString (t: T) =
  VAR start, next: INTEGER;
  BEGIN
    start := t.cursor; (* first character after quote *)
    next  := t.cursor;
    NextChar (t);
    LOOP
      CASE t.char OF
      | EOFChar => EXIT;
      | '"'     => NextChar (t); EXIT;
      | '\\'    =>
        NextChar (t);
        CASE t.char OF
        | '\n' => (* ignore quoted new-line in strings *)
        | '\\' => t.buffer[next] := '\\';  INC (next);     
        | 'n'  => t.buffer[next] := '\n';  INC (next);
        | 'r'  => t.buffer[next] := '\r';  INC (next);
        | 't'  => t.buffer[next] := '\t';  INC (next);
        | 'f'  => t.buffer[next] := '\f';  INC (next);
        | '"'  => t.buffer[next] := '"';   INC (next);
        | EOFChar => EXIT;
        ELSE t.buffer[next] := t.char;  INC (next);
        END;
        NextChar (t);
      ELSE
        t.buffer[next] := t.char;  INC (next);
        NextChar (t);
      END;
    END;

    t.string := t.map.str2id (SUBARRAY (t.buffer^, start, next - start));
    t.token  := TK.String;
  END ReadString;

PROCEDURE ReadName (t: T) =
  VAR start := t.cursor;  id: Quake.ID;
  BEGIN
    WHILE AlphaNumeric [t.char] DO NextChar (t); END;
    id := t.map.str2id (SUBARRAY (t.buffer^, start-1, t.cursor-start));
    t.string := id;
    t.token  := TK.Name;
    IF (t.map.min_keyword <= id) AND (id <= t.map.max_keyword) THEN
      t.token := t.map.keywords [id - t.map.min_keyword];
    END;
  END ReadName;

BEGIN
  InitTables ();
END QScanner.
