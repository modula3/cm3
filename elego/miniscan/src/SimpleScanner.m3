(*---------------------------------------------------------------------------*)
MODULE SimpleScanner;

IMPORT ASCII, Rd, TextSetDef, Text, Thread, Fmt;
IMPORT ScanToken, ScanTokenSeq, SMsg AS Msg;

(*---------------------------------------------------------------------------*)
CONST Backslash = '\\';

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "SimpleScanner Type 0.0" OBJECT
    (* public variables 
       skipComments    : BOOLEAN;         default TRUE
       nestingComments : BOOLEAN;         default TRUE
       oneLineComments : BOOLEAN;         default TRUE
       commentOpenSym  : TEXT;            default `/*'
       commentCloseSym : TEXT;            default `*/'
       lineCommentSym  : TEXT;            default '#' 
       stringOpenSym   : TEXT;            default `"' 
       stringCloseSym  : TEXT;            default `"' 
       identSymbols    : ASCII.Set;       default ASCII.AlphaNumerics + {'_'}
       whiteSpace      : ASCII.Set;       default ASCII.Spaces
       specialSymbols  : ASCII.Set;       default DefaultSpecialChars
       compoundToken   : TextSet.T;       default {`:=', `<=', `>=', `->'}
       keywordToken    : TextSet.T;       default {}
    *)
    endOfFile : BOOLEAN;
    rd        : Rd.T;
    line, col : CARDINAL;
    actLen    : CARDINAL;
    actLine   : TEXT;
    actData   : TEXT;
    stack     : ScanTokenSeq.T;
  METHODS
    nextLine() RAISES {Error} := NextLine;
    nextChar() : CHAR RAISES {Error} := NextChar;
    actChar() : CHAR RAISES {Error} := ActChar;
    pos() : TEXT := Pos;
    errorState() : TEXT := ErrorState;
  OVERRIDES
    init := Init;
    nextToken := NextToken;
    pushBack := PushBack;
    eof := Eof;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; inputStream : Rd.T) : T =
  BEGIN
    self.skipComments    := TRUE;
    self.nestingComments := TRUE;
    self.oneLineComments := TRUE;
    self.commentOpenSym  := "/*";
    self.commentCloseSym := "*/";
    self.lineCommentSym  := "#";
    self.stringOpenSym   := "\"";
    self.stringCloseSym  := "\"";
    self.identSymbols    := ASCII.AlphaNumerics + ASCII.Set{'_'};
    self.whiteSpace      := ASCII.Spaces;
    self.compoundToken   := NEW(TextSetDef.T).init();
    self.keywordToken    := NEW(TextSetDef.T).init();
    self.rd              := inputStream;
    self.specialSymbols  := DefaultSpecialChars;
    EVAL self.compoundToken.insert(":=");
    EVAL self.compoundToken.insert("<=");
    EVAL self.compoundToken.insert(">=");
    EVAL self.compoundToken.insert("->");
    self.line := 0;
    self.col  := 0;
    self.actLine := "";
    self.actLen  := 0;
    self.actData := "";
    self.stack := NIL; (* allocate stack only on need *)
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE Pos(self : T) : TEXT =
  BEGIN
    RETURN "line " & Fmt.Int(self.line) & " column " & Fmt.Int(self.col);
  END Pos;

(*---------------------------------------------------------------------------*)
PROCEDURE ErrorState(self : T) : TEXT =
  VAR c : CHAR; cs : TEXT;
  BEGIN
    TRY
      c  := self.actChar();
      cs := Text.FromChar(c);
    EXCEPT ELSE
      cs := "undefined";
    END;
    RETURN "\n  current symbol:   `" & cs & "'" &
           "\n  current line:     `" & self.actLine & "'" &
           "\n  accumulated data: `" & self.actData & "'";
  END ErrorState;

(*---------------------------------------------------------------------------*)
PROCEDURE NextLine(self : T) RAISES {Error} =
  BEGIN
    IF self.col >= Text.Length(self.actLine) THEN
      TRY
        self.actLine := Rd.GetLine(self.rd) & "\n";
        self.col := 0;
        self.actLen := Text.Length(self.actLine);
        INC(self.line);
      EXCEPT
        Rd.EndOfFile   => self.endOfFile := TRUE;
      | Rd.Failure     => RAISE Error("error reading input stream");
      | Thread.Alerted => RAISE Error("interrupted while scanning");
      END;
    END;
  END NextLine;

(*---------------------------------------------------------------------------*)
PROCEDURE ActChar(self : T) : CHAR  RAISES {Error} =
  BEGIN
    IF self.col < self.actLen THEN
      RETURN Text.GetChar(self.actLine, self.col);
    ELSE
      RAISE Error(NIL);
    END;
  END ActChar;

(*---------------------------------------------------------------------------*)
PROCEDURE NextChar(self : T) : CHAR  RAISES {Error} =
  VAR c : CHAR;
  BEGIN
    WHILE (NOT self.endOfFile) AND (self.col >= self.actLen) DO
      NextLine(self);
    END;
    IF self.endOfFile THEN
      c := '?';
    ELSE
      c := Text.GetChar(self.actLine, self.col);
      INC(self.col);
    END;
    RETURN c;
  END NextChar;

(*---------------------------------------------------------------------------*)
PROCEDURE SkipWhiteSpace(self : T) RAISES {Error} =
  BEGIN
    WHILE (NOT self.endOfFile) AND (self.nextChar() IN self.whiteSpace) DO
      (* nothing *)
    END;
    IF self.col > 0 THEN
      DEC(self.col);
    END;
  END SkipWhiteSpace;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadSymbols(self : T; set : ASCII.Set) RAISES {Error} =
  VAR c := self.nextChar();
  BEGIN
    WHILE (NOT self.endOfFile) AND (c IN set) DO
      self.actData := self.actData & Text.FromChar(c);
      c := self.nextChar();
    END;
    IF self.col > 0 THEN
      DEC(self.col);
    ELSE
      RAISE Error("ReadSymbols: cannot push back character");
    END;
  END ReadSymbols;

(*---------------------------------------------------------------------------*)
PROCEDURE Adjust(self : T; token : TEXT) RAISES {Error} =
  BEGIN
    WITH sub = Text.Length(self.actData) - Text.Length(token) DO
      IF self.col >= sub THEN
        DEC(self.col, sub);
      ELSE
        RAISE Error("Adjust: cannot push back characters");
      END;
    END;
  END Adjust;

(*---------------------------------------------------------------------------*)
PROCEDURE Matches(self : T; token : TEXT) : BOOLEAN =
  BEGIN
    WITH tlen = Text.Length(token) DO
      WITH dlen = Text.Length(self.actData) DO
        IF tlen > dlen THEN 
          RETURN FALSE;
        END;
        RETURN Text.Equal(Text.Sub(self.actData, 0, tlen), token);
      END;
    END;
  END Matches;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadString(self : T) RAISES {Error} =
  CONST OctalSet = ASCII.Set{'0'..'7'};
  VAR 
    s := ASCII.All;
    t := ASCII.Set{};
    d :  TEXT;
  BEGIN
    WITH slen = Text.Length(self.stringOpenSym) DO
      self.actData := Text.Sub(self.actData, slen);
    END;
    (* string open symbol stripped off *)
    IF Text.Length(self.actData) > 0 THEN
      (* check if the string end has already been read *)
      (* lots of special symbols may have been accumulated in actData
         by ReadSymbols, eg. "\\" or "\\\" *)
      (* We just throw them away (to be read again), because there is no
         other way to interpret the escape sequences in strings *)
      Adjust(self, "");
      self.actData := "";
    END;
    FOR i := 0 TO Text.Length(self.stringCloseSym) - 1 DO
      t := t + ASCII.Set{Text.GetChar(self.stringCloseSym, i)};
    END;
    s := s - ASCII.Set{Backslash};
    s := s - t;
    LOOP
      ReadSymbols(self, s);
      IF self.actChar() = Backslash THEN
        EVAL self.nextChar();
        VAR n := self.nextChar(); BEGIN
          IF n = 'n' THEN
            n := ASCII.NL;
          ELSIF n = 'r' THEN
            n := ASCII.CR;
          ELSIF n = 't' THEN
            n := ASCII.HT;
          ELSIF n = 'h' THEN
            n := ASCII.BS;
          ELSIF n = 'g' THEN
            n := ASCII.BEL;
          ELSIF n = '[' THEN
            n := ASCII.ESC;
          ELSIF n IN OctalSet THEN
            VAR val := ORD(n) - ORD('0'); numOct := 1; BEGIN
              n := self.nextChar();
              WHILE n IN OctalSet AND numOct < 3 DO
                INC(numOct);
                val := val * 8 + ORD(n) - ORD('0');
                n := self.nextChar();
              END;
              DEC(self.col); (* last char must be read again *)
              val := val MOD 256;
              n := VAL(val, CHAR);
            END;
          END;
          self.actData := self.actData & Text.FromChar(n);
        END;
      ELSE
        d := self.actData;
        self.actData := "";
        ReadSymbols(self, t);
        IF Matches(self, self.stringCloseSym) THEN
          Adjust(self, self.stringCloseSym);
          self.actData := d;
          RETURN;
        ELSE
          self.actData := d & Text.Sub(self.actData, 0, 1);
          Adjust(self, "x");
        END;
      END;
    END;
  END ReadString;

(*---------------------------------------------------------------------------*)
PROCEDURE ReadOneLineComment(self : T) =
  BEGIN
    self.actData := Text.Sub(self.actLine, self.col);
    self.col := self.actLen + 1;
  END ReadOneLineComment; 

(*---------------------------------------------------------------------------*)
PROCEDURE ReadMultiLineComment(self : T) RAISES {Error} =
  VAR 
    s := ASCII.All;
    t := ASCII.Set{};
    d :  TEXT;
    level := 1;
  BEGIN
    WITH slen = Text.Length(self.commentOpenSym) DO
      self.actData := Text.Sub(self.actData, slen);
    END;
    FOR i := 0 TO Text.Length(self.commentOpenSym) - 1 DO
      t := t + ASCII.Set{Text.GetChar(self.commentOpenSym, i)};
    END;
    FOR i := 0 TO Text.Length(self.commentCloseSym) - 1 DO
      t := t + ASCII.Set{Text.GetChar(self.commentCloseSym, i)};
    END;
    s := s - ASCII.Set{Backslash};
    s := s - t;
    LOOP
      ReadSymbols(self, s);
      IF self.actChar() = Backslash THEN
        EVAL self.nextChar();
        self.actData := self.actData & Text.FromChar(self.nextChar());
      ELSE
        d := self.actData;
        self.actData := "";
        ReadSymbols(self, t);
        IF Matches(self, self.commentCloseSym) THEN
          DEC(level);
          IF NOT self.nestingComments OR level = 0 THEN
            Adjust(self, self.commentCloseSym);
            self.actData := d;
            RETURN;
          ELSE
            Adjust(self, "x");
            self.actData := d & Text.Sub(self.actData, 0, 1);
          END;
        ELSIF Matches(self, self.commentOpenSym) THEN
          INC(level);
          Adjust(self, self.commentOpenSym);
          self.actData := d & self.commentOpenSym;
        ELSE
          Adjust(self, "x");
          self.actData := d & Text.Sub(self.actData, 0, 1);
        END;
      END;
    END;
  END ReadMultiLineComment; 

(*---------------------------------------------------------------------------*)
PROCEDURE NextToken(self : T) : Token RAISES {Error} =
  VAR token : Token;
  BEGIN
    Debug("scanner.nextToken");
    (* if there are tokens on the stack, return the top one *)
    IF self.stack # NIL AND self.stack.size() > 0 THEN
      token := self.stack.remlo();
      DebugToken(token);
      RETURN token;
    END;
    (* stack is empty *)
    token := NEW(Token);
    LOOP (* read until a complete syntactical element has been recognized *)
      SkipWhiteSpace(self);
      self.actData := "";
      token.line := self.line;
      token.col  := self.col;
      (* new token refers to correct position *)
      IF self.endOfFile THEN
        (* first check for end of file *)
        token.kind := ScanToken.Kind.EndOfFile;
        token.repr := "";
        EXIT;
      (* no end of file *)
      ELSIF self.actChar() IN self.identSymbols THEN
        (* keyword or identifier found *)
        ReadSymbols(self, self.identSymbols);
        token.repr := self.actData;
        IF self.keywordToken.member(token.repr) THEN
          token.kind := ScanToken.Kind.Keyword;
        ELSE
          token.kind := ScanToken.Kind.Ident;
        END;
        EXIT;
      (* no end of file, no keyword, no identifier *)
      ELSIF self.actChar() IN self.specialSymbols THEN
        (* last chance: anything consisting of special symbols *)
        ReadSymbols(self, self.specialSymbols);
        IF Matches(self, self.lineCommentSym) AND self.oneLineComments THEN
          (* one line comment found *)
          ReadOneLineComment(self);
          IF NOT self.skipComments THEN
            (* only return it if comments are not to be skipped *)
            token.kind := ScanToken.Kind.Comment;
            token.repr := self.actData;
            EXIT;
          END;
          (* continue at start of loop *)
        ELSIF Matches(self, self.commentOpenSym) THEN
          (* multi line comment found *)
          ReadMultiLineComment(self);
          IF NOT self.skipComments THEN
            (* only return it if comments are not to be skipped *)
            token.kind := ScanToken.Kind.Comment;
            token.repr := self.actData;
            EXIT;
          END;
          (* continue at start of loop *)
        ELSE
          (* check for compound symbol *)
          VAR iter  := self.compoundToken.iterate(); 
              tok   :  TEXT; 
              found := FALSE;
          BEGIN
            WHILE iter.next(tok) DO
              IF Matches(self, tok) THEN
                token.repr := tok;
                token.kind := ScanToken.Kind.CompoundSymbol;
                Adjust(self, tok);
                found := TRUE;
                EXIT;
              END;
            END;
            IF found THEN EXIT END;
            (* no compound token but special symbol *)
            IF Matches(self, self.stringOpenSym) THEN
              (* string found *)
              ReadString(self);
              token.repr := self.actData;
              token.kind := ScanToken.Kind.String;
            ELSE
              (* single special character found *)
              token.repr := Text.Sub(self.actData, 0, 1);
              token.kind := ScanToken.Kind.Other;
              Adjust(self, token.repr);
            END;
            EXIT;
          END;
        END;
      ELSE
        (* what we found does not belong to any defined lexical class *)
        VAR err := "Unexpected symbol at " & self.pos() & ": " &
              self.errorState(); 
        BEGIN
          INC(self.col);
          RAISE Error(err);
        END;
      END;
    END;
    DebugToken(token);
    RETURN token;
  END NextToken;

(*---------------------------------------------------------------------------*)
PROCEDURE PushBack(self : T; t : Token) =
  BEGIN
    Debug("scanner.pushBack");
    DebugToken(t);
    IF self.stack = NIL THEN
      self.stack := NEW(ScanTokenSeq.T).init(5);
    END;
    self.stack.addlo(t);
  END PushBack;

(*---------------------------------------------------------------------------*)
PROCEDURE Eof(self : T) : BOOLEAN =
  BEGIN
    RETURN self.endOfFile;
  END Eof;

(*---------------------------------------------------------------------------*)
PROCEDURE Debug(msg : TEXT) =
  BEGIN
    IF debugScanner THEN
      IF Msg.dFlag THEN
        Msg.D(msg);
      ELSIF Msg.tFlag THEN
        Msg.T(msg);
      ELSE
        Msg.Debug(msg);
      END;
    END;
  END Debug;

(*---------------------------------------------------------------------------*)
PROCEDURE DebugToken(token : ScanToken.T) =
  BEGIN
    IF debugScanner THEN
      ScanToken.DebugToken(token);
    END;
  END DebugToken;

(*---------------------------------------------------------------------------*)
BEGIN
  debugScanner := FALSE;
END SimpleScanner.
