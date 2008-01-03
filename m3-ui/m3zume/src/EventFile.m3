(* Copyright (C) 1995, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Aug 21 15:07:51 PDT 1996 by najork  *)
(*      modified on Tue Feb  7 14:51:54 PST 1995 by kalsow  *)

MODULE EventFile;

IMPORT Text, Rd, Atom, AtomList, Thread, Fmt, TextTextTbl, TextWr, Wr;

EXCEPTION Error (TEXT);

CONST
  EOF = '\000';

TYPE (* token type *)
  TK = { EOF, Id, Digit,
         Import, AlgData,
         Output, Procedure, Update, Feedback,
         Value, Readonly,
         Comma, Semi, Colon, R_paren, L_paren, R_bracket, L_bracket };

CONST
  TKName = ARRAY TK OF TEXT {
    "<EOF>", "<identifier>", "<digit>",
    "IMPORT", "ALGDATA",
    "OUTPUT", "PROCEDURE", "UPDATE", "FEEDBACK",
    "VALUE", "READONLY",
    "','", "';'", "':'", "')'", "'('", "']'", "'['"
  };

TYPE
  State = RECORD
    rd              : Rd.T;
    ch              : CHAR;
    at_eof          : BOOLEAN := FALSE;
    tok             : TK      := TK.EOF;
    tok_line        : INTEGER := 0;
    cur_line        : INTEGER := 0;
    cur_id          : TEXT    := NIL;
    alg_data        : TEXT    := NIL;
    import_head     : Import  := NIL;
    import_tail     : Import  := NIL;
    event_head      : Event   := NIL;
    event_tail      : Event   := NIL;
  END;

TYPE
  CC = { WhiteSpace, Letter, Illegal };

VAR
  init_done := FALSE;
  CharClass : ARRAY CHAR OF CC;
  BuiltinPrinters: TextTextTbl.T := NIL;

PROCEDURE Parse (rd: Rd.T;  VAR(*OUT*) t: T): TEXT =
  VAR s: State;
  BEGIN
    Init ();
    t.imports  := NIL;
    t.alg_data := NIL;
    t.events   := NIL;
    TRY
      s.rd := rd;
      s.ch := ' ';
      s.cur_line := 1;
      GetToken (s);
      ParseImports (s);
      ParseAlgData (s);
      ParseEvents (s);
      IF (s.tok # TK.EOF) THEN
        IF (s.event_head # NIL) OR (s.alg_data # NIL) THEN
          ParseErr (s, "OUTPUT, PROCEDURE, UPDATE, or FEEDBACK");
        ELSE
          ParseErr (s, "IMPORT, ALGDATA, OUTPUT, PROCEDURE, UPDATE, or FEEDBACK");
        END;
      END;
    EXCEPT Error (msg) =>
      RETURN msg;
    END;
    t.imports  := s.import_head;
    t.alg_data := s.alg_data;
    t.events   := s.event_head;
    RETURN NIL;
  END Parse;

PROCEDURE ParseImports (VAR s: State) RAISES {Error} =
  VAR im: Import;
  BEGIN
    WHILE (s.tok = TK.Import) DO
      GetToken (s); (* IMPORT *)
      WHILE (s.tok = TK.Id) DO
        im := NEW (Import, next := NIL, interface := s.cur_id);
        IF (s.import_head = NIL)
          THEN s.import_head := im;
          ELSE s.import_tail.next := im;
        END;
        s.import_tail := im;
        GetToken (s); (* ID *)
        IF (s.tok # TK.Comma) THEN EXIT; END;
        GetToken (s); (* "," *)
      END;
      IF (s.tok = TK.Semi) THEN
        GetToken (s);  (* ";" *)
      ELSE
        ParseErr (s, "',' or ';' in IMPORT list");
      END;
    END;
  END ParseImports;


PROCEDURE ParseAlgData (VAR s: State) RAISES {Error} =
  BEGIN
    IF (s.tok = TK.AlgData) THEN
      s.alg_data := GetSection (s);
      GetToken (s); (* load the token following the AlgData section *)
    END;
  END ParseAlgData;

PROCEDURE ParseEvents (VAR s: State) RAISES {Error} =
  VAR e: Event;
  BEGIN
    LOOP
      e := NEW (Event, next := NIL, name := NIL, args := NIL);
      IF    (s.tok = TK.Output)    THEN  e.kind := EventKind.Output;
      ELSIF (s.tok = TK.Procedure) THEN  e.kind := EventKind.Output;
      ELSIF (s.tok = TK.Update)    THEN  e.kind := EventKind.Update;
      ELSIF (s.tok = TK.Feedback)  THEN  e.kind := EventKind.Feedback;
      ELSE EXIT;
      END;
      IF (s.event_head = NIL)
        THEN s.event_head := e;
        ELSE s.event_tail.next := e;
      END;
      s.event_tail := e;
      GetToken (s);  (* OUTPUT, PROCEDURE, UPDATE, or FEEDBACK *)

      IF (s.tok = TK.L_bracket) THEN
        GetToken (s); (* '[' *)
        IF (s.tok # TK.Id)
          OR (Text.Length (s.cur_id) # 1)
          OR (Text.GetChar (s.cur_id, 0) < '0')
          OR (Text.GetChar (s.cur_id, 0) > '9') THEN
          ParseErr (s, "event priority [0..9]");
        END;
        e.priority := s.cur_id;
        GetToken (s); (* <id> *)
        IF (s.tok # TK.R_bracket) THEN ParseErr (s, "closing ']'"); END;
        GetToken (s); (* ']' *)
      ELSE
        e.priority := "1";
      END;

      IF (s.tok # TK.Id) THEN ParseErr (s, "event name"); END;
      e.name := s.cur_id;
      GetToken (s);  (* <ident> *)

      IF (s.tok = TK.L_paren) THEN
        GetToken (s); (* '(' *)
        ParseArgs (s, e);
        IF (s.tok # TK.R_paren) THEN ParseErr (s, "closing ')'"); END;
        GetToken (s); (* ')' *)
      END;

      IF (s.tok # TK.Semi) THEN ParseErr (s, "';'"); END;
      GetToken (s);
    END;
  END ParseEvents;

PROCEDURE ParseArgs (VAR s: State;  e: Event) RAISES {Error} =
  VAR a, b, c: EventArg;   m: ArgMode;  printer: TEXT;
  BEGIN
    LOOP
      m := ArgMode.Value;
      IF    (s.tok = TK.Value)    THEN m := ArgMode.Value;     GetToken (s);
      ELSIF (s.tok = TK.Readonly) THEN m := ArgMode.Readonly;  GetToken (s);
      ELSIF (s.tok = TK.R_paren)  THEN EXIT;
      END;

      IF (s.tok # TK.Id) THEN ParseErr (s, "argument name"); END;
      c := e.args; (* first old arg *)
      WHILE (s.tok = TK.Id) DO
        a := NEW (EventArg, next := e.args, mode := m,
                name := s.cur_id, type := NIL, printer := NIL);
        e.args := a;
        GetToken (s); (* <id> *)
        IF (s.tok # TK.Comma) THEN EXIT END;
        GetToken (s); (* ',' *)
      END;

      IF (s.tok # TK.Colon) THEN ParseErr (s, "':' type name"); END;
      GetToken (s); (* ":" *)

      IF (s.tok # TK.Id) THEN ParseErr (s, "type name"); END;
      b := a; REPEAT b.type := s.cur_id; b := b.next; UNTIL b = c;
      GetToken (s); (* <id> *)

      IF (s.tok = TK.L_bracket) THEN
        GetToken (s); (* "[" *)
        IF (s.tok =  TK.Id) THEN
          printer := s.cur_id;
          GetToken (s); (* <id> *)
        ELSE
          printer := "";
        END;
        IF (s.tok # TK.R_bracket) THEN ParseErr (s, "closing ']'"); END;
        GetToken (s); (* "]" *)
      ELSIF NOT BuiltinPrinters.get (a.type, printer) THEN
        ParseErr (s, "print procedure for type " & a.type);
      END;
      b := a; REPEAT b.printer := printer; b := b.next; UNTIL b = c;

      IF    (s.tok = TK.Semi)    THEN GetToken (s); (* ";" *)
      ELSIF (s.tok = TK.R_paren) THEN EXIT;
      ELSE ParseErr (s, "';' or ')'");
      END;
    END;

    (* reverse the arg list *)
    b := NIL;  a := e.args;
    WHILE (a # NIL) DO
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;
    e.args := b;
    
  END ParseArgs;

PROCEDURE ParseErr (VAR s: State;  expected: TEXT) RAISES {Error} =
  VAR tok := TKName [s.tok];
  BEGIN
    IF (s.tok = TK.Id) OR (s.tok = TK.Digit) THEN
      tok := tok & " = \"" & s.cur_id & "\"";
    END;
    Err (s, "syntax error, expected " & expected & ", but found " & tok);
  END ParseErr;


PROCEDURE GetSection (VAR s: State): TEXT RAISES {Error} =
  (* read lines up to a blank line. *)
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := TextWr.New ();  blank_line : BOOLEAN;
  BEGIN
      (* skip white space *)
      WHILE (CharClass [s.ch] = CC.WhiteSpace) AND (s.ch # '\n') DO
        GetCh (s);
      END;
      IF (s.ch = '\n') THEN GetCh (s); END;

      (* accumulate bytes *)
      blank_line := TRUE;
      LOOP
        IF (s.ch = EOF) THEN RETURN TextWr.ToText (wr); END;
        Wr.PutChar (wr, s.ch);
        IF (s.ch = '\n') THEN
          IF (blank_line) THEN RETURN TextWr.ToText (wr); END;
          blank_line := TRUE;
        END;
        blank_line := blank_line AND (CharClass[s.ch] = CC.WhiteSpace);
        GetCh (s);
      END;
  END GetSection;

PROCEDURE GetToken (VAR s: State) RAISES {Error} =
  VAR len: INTEGER;  buf: ARRAY [0..255] OF CHAR;
  BEGIN
    LOOP
      (* skip white space *)
      WHILE (CharClass [s.ch] = CC.WhiteSpace) DO GetCh (s); END;

      (* remember where the token started *)
      s.tok_line := s.cur_line;

      CASE s.ch OF
      | 'a'..'z', 'A'..'Z' => (* identifier *)
           len := 0;
           WHILE (CharClass [s.ch] = CC.Letter) DO
             IF (len > LAST (buf)) THEN Err (s, "identifier too long"); END;
             buf[len] := s.ch;  INC (len);
             GetCh (s);
           END;
           s.tok := TK.Id;
           s.cur_id := Text.FromChars (SUBARRAY (buf, 0, len));
           ClassifyID (s, len);
           EXIT;

      | '0' .. '9' => (* digit *)
           s.tok := TK.Digit;
           s.cur_id := Text.FromChar (s.ch);
           EXIT;

      | '(' =>
           GetCh (s);
           IF (s.ch = '*')
             THEN SkipComment (s);
             ELSE s.tok := TK.L_paren; EXIT;
           END;
         
      | ')' => s.tok := TK.R_paren;    GetCh (s);  EXIT;
      | '[' => s.tok := TK.L_bracket;  GetCh (s);  EXIT;
      | ']' => s.tok := TK.R_bracket;  GetCh (s);  EXIT;
      | ',' => s.tok := TK.Comma;      GetCh (s);  EXIT;
      | ';' => s.tok := TK.Semi;       GetCh (s);  EXIT;
      | ':' => s.tok := TK.Colon;      GetCh (s);  EXIT;
      | EOF => s.tok := TK.EOF;  EXIT;
      ELSE Err (s, "illegal character: 8_" & Fmt.Int (ORD (s.ch), 8)); EXIT;
      END; (* case *)
    END; (*loop*)
  END GetToken;

PROCEDURE SkipComment (VAR s: State) RAISES {Error} =
  VAR nest: INTEGER;
  BEGIN
    GetCh (s); (* initial '*' *)
    nest := 1;
    WHILE (nest > 0) DO
      IF (s.ch = '*') THEN
        GetCh (s);  IF (s.ch = ')') THEN DEC (nest); GetCh (s);  END;
      ELSIF (s.ch = '(') THEN
        GetCh (s);  IF (s.ch = '*') THEN INC (nest); GetCh (s);  END;
      ELSIF (s.ch = EOF) THEN
        Err (s, "EOF encountered in comment");
        nest := 0;
      ELSE
        GetCh (s);
      END;
    END;
  END SkipComment;

PROCEDURE ClassifyID (VAR s: State;  len: INTEGER) =
  BEGIN
    CASE len OF
    | 5 => IF    Text.Equal (s.cur_id, "VALUE")     THEN s.tok := TK.Value;
           END;
    | 6 => IF    Text.Equal (s.cur_id, "OUTPUT")    THEN s.tok := TK.Output;
           ELSIF Text.Equal (s.cur_id, "IMPORT")    THEN s.tok := TK.Import;
           ELSIF Text.Equal (s.cur_id, "UPDATE")    THEN s.tok := TK.Update;
           END;
    | 7 => IF    Text.Equal (s.cur_id, "ALGDATA")   THEN s.tok := TK.AlgData;
           END;
    | 8 => IF    Text.Equal (s.cur_id, "FEEDBACK")  THEN s.tok := TK.Feedback;
           ELSIF Text.Equal (s.cur_id, "READONLY")  THEN s.tok := TK.Readonly;
           END;
    | 9 => IF    Text.Equal (s.cur_id, "PROCEDURE") THEN s.tok := TK.Procedure;
           END;
    ELSE (* skip *)
    END;
  END ClassifyID;

PROCEDURE GetCh (VAR s: State) RAISES {Error} =
  BEGIN
    IF s.at_eof THEN  s.ch := EOF; RETURN;  END;
    TRY
      s.ch := Rd.GetChar (s.rd);
      IF (s.ch = '\n') THEN INC (s.cur_line); END;
    EXCEPT
    | Rd.EndOfFile   => s.ch := EOF;  s.at_eof := TRUE;
    | Rd.Failure(ec) => Err (s, "problem reading event file" & OSErr (ec));
    | Thread.Alerted => Err (s, "interrupted while reading event file");
    END;
  END GetCh;

PROCEDURE Err (READONLY s: State;  msg: TEXT) RAISES {Error} =
  BEGIN
    msg := "line " & Fmt.Int (s.tok_line) & ": " & msg;
    RAISE Error (msg);
  END Err;

PROCEDURE OSErr (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    IF (msg = NIL) THEN msg := "" END;
    RETURN msg;
  END OSErr;

PROCEDURE Init () =
  BEGIN
    IF init_done THEN RETURN END;
    FOR c := FIRST (CHAR) TO LAST (CHAR) DO CharClass[c] := CC.Illegal; END;
    CharClass[' ']  := CC.WhiteSpace;
    CharClass['\t'] := CC.WhiteSpace;
    CharClass['\n'] := CC.WhiteSpace;
    CharClass['\r'] := CC.WhiteSpace;
    CharClass['.']  := CC.Letter;
    CharClass['_']  := CC.Letter;
    FOR c := 'a' TO 'z' DO CharClass[c] := CC.Letter; END;
    FOR c := 'A' TO 'Z' DO CharClass[c] := CC.Letter; END;
    FOR c := '0' TO '9' DO CharClass[c] := CC.Letter; END;
    BuiltinPrinters := NEW (TextTextTbl.Default).init ();
    EVAL BuiltinPrinters.put ("INTEGER",  "Fmt.Int");
    EVAL BuiltinPrinters.put ("BOOLEAN",  "OblFmt.Bool");
    EVAL BuiltinPrinters.put ("CARDINAL", "Fmt.Int");
    EVAL BuiltinPrinters.put ("REAL",     "OblFmt.Real");
    EVAL BuiltinPrinters.put ("LONGREAL", "Fmt.LongReal");
    EVAL BuiltinPrinters.put ("EXTENDED", "Fmt.Extended");
    EVAL BuiltinPrinters.put ("Text.T",   "TextConv.Encode");
    EVAL BuiltinPrinters.put ("TEXT",     "TextConv.Encode");
    init_done := TRUE;
  END Init;

BEGIN
END EventFile.
