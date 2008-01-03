MODULE SGMLCS EXPORTS SGMLC, SGMLCScanner;

IMPORT ASCII, Text, Rd, Thread, RefSeq, IntSeq, TextIntTbl, Wr, Stdio;

CONST
  Debug = FALSE;

REVEAL
  Scanner = PublicScanner BRANDED OBJECT
      err: ErrHandler ;
      lastCh: CHAR ;
      lastLineLen: CARDINAL;
      ch: CHAR ;
      buf: REF ARRAY OF CHAR;
      bufLen: CARDINAL;
      state: State;
      stateStack: IntSeq.T;
      singleQuote: BOOLEAN;
      resolver: EntityResolver;
      inMarkup: BOOLEAN;
    OVERRIDES
      initSimple := Init;
      get := Get;
      setEntityResolver := SetEntityResolver;
      pushState := PushState;
      popState := PopState;
      pushFile := PushFile;
      pushNextFile := PushNextFile;
      inMarkupDecl := InMarkupDecl;
    END;

CONST
  (* States in which parameter entity replacement should be done *)

  PEReplaceState = SET OF State{State.AttList, State.Element, State.Entity, 
      State.StartCSect, State.Notation, State.ElementTag, 
      State.DocType };


  WhiteChars = SET OF CHAR{' ', '\t', '\r', '\n'};
  NameChars = ASCII.Letters + ASCII.Set{'_'};
  NameMiddleChars = ASCII.Letters + ASCII.Digits + ASCII.Set{'.','-','_'};
  NmTokenChars = ASCII.Letters + ASCII.Digits + ASCII.Set{'.','-','_','/'};
  DigitChars = ASCII.Digits;
  HexDigitChars = ASCII.Digits + ASCII.Set{'a'..'f', 'A'..'F'};

  (* ARRAY OF CHAR with some of the reserved keywords, used for
     fast comparison with the characters stored in the input buffer. *)

  DashImplied = ARRAY OF CHAR{'#','I','M','P','L','I','E','D'};
  DashPCData = ARRAY OF CHAR{'#','P','C','D','A','T','A'};
  DashRequired = ARRAY OF CHAR{'#','R','E','Q','U','I','R','E','D'};
  DashFixed = ARRAY OF CHAR{'#','F','I','X','E','D'};
  CDataTag = ARRAY OF CHAR{'<','!','[','C','D','A','T','A'};
  DocTypeTag = ARRAY OF CHAR{'<','!','D','O','C','T','Y','P','E'};
  ElementTag = ARRAY OF CHAR{'<','!','E','L','E','M','E','N','T'};
  AttListTag = ARRAY OF CHAR{'<','!','A','T','T','L','I','S','T'};
  EntityTag = ARRAY OF CHAR{'<','!','E','N','T','I','T','Y'};
  NotationTag = ARRAY OF CHAR{'<','!','N','O','T','A','T','I','O','N'};

(* Actually Wr is only used with Debug = TRUE *)

<*FATAL Wr.Failure, Thread.Alerted*>

(* We will be processing or not markup declarations *)

PROCEDURE InMarkupDecl(self: Scanner; b: BOOLEAN) =
  BEGIN
    self.inMarkup := b;
  END InMarkupDecl;

PROCEDURE SetEntityResolver(self: Scanner; r: EntityResolver) =
  BEGIN
    self.resolver := r;
  END SetEntityResolver;

(* Case insensitive character array comparison, where "a" is a buffer
   longer than length and b is a character array. *)

PROCEDURE CIEqual(READONLY a: ARRAY OF CHAR; length: CARDINAL;
    READONLY b: ARRAY OF CHAR): BOOLEAN =
  BEGIN
    IF length # NUMBER(b) THEN RETURN FALSE; END;

    FOR i := 0 TO length - 1 DO 
      IF ASCII.Upper[a[i]] # b[i] THEN RETURN FALSE; END;
    END;

    RETURN TRUE;
  END CIEqual;

(* Get the next character, update the input state, remember enough of
   the current state in case PrevCh is called, and insure that the input
   buffer is large enough. *)

PROCEDURE NextCh(s: Scanner; record := TRUE) 
    RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  VAR 
    newBuf : REF ARRAY OF CHAR ;
  BEGIN
    IF s.input.rd = NIL THEN RAISE Rd.EndOfFile; END;
    s.lastCh := s.ch ;
    s.ch := Rd.GetChar(s.input.rd) ;
    INC(s.input.offset);
    IF (s.ch = '\n') THEN
      INC(s.input.currentLine) ;
      s.lastLineLen := s.input.currentCol ;
      s.input.currentCol := 0
    ELSE
      INC(s.input.currentCol)
    END ;
    IF (record) THEN
      IF (s.bufLen = NUMBER(s.buf^)) THEN
        (* double the buffer size *)
        newBuf := NEW(REF ARRAY OF CHAR, NUMBER(s.buf^) * 2) ;
        SUBARRAY(newBuf^, 0, NUMBER(s.buf^)) := s.buf^ ;
        s.buf := newBuf
      END ;
      s.buf[s.bufLen] := s.ch;
      INC(s.bufLen);
    END;
  END NextCh;

(* Unget the current character *)

PROCEDURE PrevCh(s : Scanner) =
  BEGIN
    Rd.UnGetChar(s.input.rd) ;
    s.ch := s.lastCh ;
    DEC(s.input.offset) ;
    DEC(s.bufLen);

    IF (s.input.currentCol = 0) THEN
      s.input.currentCol := s.lastLineLen ;
      DEC(s.input.currentLine)
    ELSE
      DEC(s.input.currentCol)
    END
  END PrevCh ;

(* Get the next token *)

PROCEDURE Get(s : Scanner ; VAR ss : ScanSymbol) 
    RAISES { Rd.Failure, Thread.Alerted } =
  VAR
    tmp: INTEGER;
    rd: Rd.T;
  BEGIN
    TRY
      NextCh(s);

      (* Within tags and markup declarations, white space and comments
         are skipped *)

      IF s.state IN SET OF State{State.StartCSect, State.ElementTag, 
          State.DocType, State.Element, State.AttList, State.Entity, 
          State.Notation, State.Catalog} OR
          (s.inMarkup AND s.state IN SET OF State{State.ContentCSect, 
          State.PCData}) THEN
        LOOP
          (* Skip white space *)

          IF s.ch IN WhiteChars THEN
            NextCh(s)

          (* Skip comments: -- comment -- *)

          ELSIF s.ch = '-' THEN
            NextCh(s);
            IF s.ch = '-' THEN 
              SkipComment(s);
              NextCh(s);
            ELSE
              PrevCh(s);
              EXIT;
            END;
          ELSE EXIT;
          END;
        END;
      END;

      (* This is where the token really starts *)

      s.buf[0] := s.ch;
      s.bufLen := 1;
      ss.offset := s.input.offset - 1 ;
      ss.line   := s.input.currentLine ;
      ss.column := s.input.currentCol ;

      (* Scan the token and extract it from the input buffer *)

      ss.sym    := GetSym(s);
      ss.length := s.bufLen;
      ss.string := Text.FromChars(SUBARRAY(s.buf^, 0, ss.length)) ;

      (* It is a parameter reference, scan the replacement text *)

      IF ss.sym = Symbol.PEReference AND 
          ((s.state IN PEReplaceState) OR 
           (s.state IN SET OF State{State.PCData, State.ContentCSect} AND
            s.inMarkup)
          ) THEN
        rd := s.resolver.resolve(ss.string);

        IF Debug THEN
          IF rd = NIL THEN Wr.PutText(Stdio.stdout,"Undefined "); END;
          Wr.Flush(Stdio.stdout);
        END;

        IF rd = NIL THEN
          s.err.error(s.input.currentLine, s.input.currentCol,
              "Undefined parameter reference: " & ss.string);
          ss.sym := Symbol.Undef;
          RETURN;
        END;

        (* Insert the replacement text in the input stack, and get the token
           from there. *)

        PushFile(s,ss.string,rd);
        s.get(ss);
        RETURN;
      END;

      (* Many things in SGML are case insentitive *)

      IF ss.sym IN SET OF Symbol{Symbol.NonKeywordName, Symbol.NmToken,
          Symbol.EntityRef} THEN
        VAR tmp := NEW(REF ARRAY OF CHAR, s.bufLen) ;
        BEGIN
          FOR i := 0 TO s.bufLen - 1 DO
            tmp[i] := ASCII.Upper[s.buf[i]]
          END ;
          ss.name := Text.FromChars(tmp^)
        END
      ELSE
        ss.name := ss.string
      END;

      (* Check for reserved words *)

      IF ss.sym = Symbol.NonKeywordName THEN
        IF keywords.get(ss.name,tmp) THEN ss.sym := VAL(tmp,Symbol); END;
      END;

      IF Debug THEN
        Wr.PutText(Stdio.stdout,SymbolName[ss.sym] & ": " & ss.string & "\n");
        Wr.Flush(Stdio.stdout);
      END;
    EXCEPT
      (* The current input was exhausted. Get the next token from the
         next input source in the stack. *)

      Rd.EndOfFile =>
        IF s.input.rd # NIL THEN Rd.Close(s.input.rd); END;
        s.input.rd := NIL;

        (* All input is exhausted, return an end of file token *)

        IF s.inputStack.size() = 0 THEN
          ss.line := s.input.currentLine;
          ss.column := s.input.currentCol;
          ss.sym    := Symbol.Eof;
          ss.string := NIL;
          ss.name   := NIL;
          IF Debug THEN
            Wr.PutText(Stdio.stdout,SymbolName[ss.sym] & ": NIL\n");
            Wr.Flush(Stdio.stdout);
          END;

        (* Continue scanning with the next input source in the stack. *)

        ELSE
          IF Debug THEN
            Wr.PutText(Stdio.stdout,"PEReference end " & 
                s.input.name & "\n");
            Wr.Flush(Stdio.stdout);
          END;

          s.input := s.inputStack.remhi();
          s.get(ss);
        END;
    END
  END Get;

(* Initialize the scanner. The input file is specified later with pushFile. *)

PROCEDURE Init(s: Scanner; e: ErrHandler) : Scanner =
  BEGIN
    s.input := NEW(Input);
    s.err := e;
    s.lastLineLen := 0 ;
    s.lastCh := ASCII.NUL;
    s.buf := NEW(REF ARRAY OF CHAR, 128) ;
    s.bufLen := 0;
    s.inputStack := NEW(RefSeq.T).init();
    s.state := State.PCData;
    s.stateStack := NEW(IntSeq.T).init();
    s.resolver := NIL;
    s.inMarkup := FALSE;
    RETURN s
  END Init ;

(* Get the symbol starting with the current character. *)

PROCEDURE GetSym(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  VAR
  BEGIN
    CASE s.state OF
    | State.Catalog =>
        (* Catalogs are very simple files with names and strings *)

        CASE s.ch OF
        | '"' => 
            s.singleQuote := FALSE;
            RETURN GetCatalogString(s);
        | '\'' =>
            s.singleQuote := TRUE;
            RETURN GetCatalogString(s);
        | 'A'..'Z', 'a'..'z', '_' => RETURN GetName(s);
        | '0'..'9', '.', '/' => RETURN GetNmToken(s);
        ELSE
          RETURN Symbol.Undef;
        END;

    | State.AttList, State.Element, State.Entity, State.StartCSect, 
          State.Notation, State.ElementTag =>

        (* Within tags, the input is relatively context free. Look for
           end tag, operators, names, strings and parameter references. *)

        CASE s.ch OF
        | '>' => 
            PopState(s);
            RETURN Symbol.RB;
        | '(' => RETURN Symbol.LP;
        | ')' => RETURN Symbol.RP;
        | '?' => RETURN Symbol.IntMark;
        | '*' => RETURN Symbol.Star;
        | '+' => 
            NextCh(s);
            IF s.ch = '(' THEN 
              RETURN Symbol.PlusLP;
            ELSE 
              PrevCh(s);
              RETURN Symbol.Plus;
            END;
        | '|' => RETURN Symbol.Vertical;
        | ',' => RETURN Symbol.Coma;
        | '"' => 
            s.singleQuote := FALSE;
            IF s.state = State.Entity THEN PushState(s,State.EntityValue);
            ELSE PushState(s,State.AttValue);
            END;
            RETURN Symbol.DQuote;
        | '\'' =>
            s.singleQuote := TRUE;
            IF s.state = State.Entity THEN PushState(s,State.EntityValue);
            ELSE PushState(s,State.AttValue);
            END;
            RETURN Symbol.Quote;
        | '=' => RETURN Symbol.Equal;
        | '#' => RETURN GetDashName(s);
        | '%' => RETURN GetPEReference(s);
        | '/' => 
            NextCh(s);
            IF s.ch = '>' THEN
              PopState(s);
              RETURN Symbol.EmptyElementEndTag;
            ELSE
              PrevCh(s);
              RETURN GetNmToken(s);
            END;
        | '[' => 
            IF s.state = State.StartCSect THEN 
              s.state := State.ContentCSect;
            END;
            RETURN Symbol.LSB;
        | '-' =>
            IF s.state = State.Element THEN RETURN Symbol.Minus;
            ELSE RETURN GetNmToken(s);
            END;
        | 'A'..'Z', 'a'..'z', '_' => RETURN GetName(s);
        | '0'..'9', '.' => RETURN GetNmToken(s);
        ELSE
          RETURN Symbol.Undef;
        END;
    | State.PCData =>

        (* Within the element content, you find tags, entity references,
           or parsed character data. *)

        IF s.ch = '<' THEN RETURN GetTag(s);
        ELSIF s.inMarkup AND s.ch = '%' THEN RETURN GetPEReference(s);
        ELSIF s.ch = '&' THEN RETURN GetReference(s);
        ELSE RETURN GetPCData(s);
        END;
    | State.AttValue =>

        (* Attribute values may contain characters and entity references. *)
        IF s.ch = '&' THEN RETURN GetReference(s);
        ELSIF s.ch = '\'' AND s.singleQuote THEN 
          PopState(s);
          RETURN Symbol.Quote;
        ELSIF s.ch = '"' AND NOT s.singleQuote THEN 
          PopState(s);
          RETURN Symbol.DQuote;
        ELSIF s.ch = '<' THEN RETURN Symbol.Undef;
        ELSE RETURN GetAttValue(s);
        END;
    | State.EntityValue =>

        (* Entity values may contain characters and emtity references. *)

        IF s.ch = '%' THEN RETURN GetPEReference(s);
        ELSIF s.ch = '&' THEN RETURN GetReference(s);
        ELSIF s.ch = '\'' AND s.singleQuote THEN 
          PopState(s);
          RETURN Symbol.Quote;
        ELSIF s.ch = '"' AND NOT s.singleQuote THEN 
          PopState(s);
          RETURN Symbol.DQuote;
        ELSE RETURN GetEntityValue(s);
        END;
    | State.DocType =>

        (* Within a document type declaration you may find names, strings
           and square brackets around markup declarations. *)

        IF s.ch = '%' THEN RETURN GetPEReference(s);
        ELSIF s.ch = '<' THEN RETURN GetTag(s);
        ELSIF s.ch = '[' THEN RETURN Symbol.LSB;
        ELSIF s.ch = ']' THEN RETURN Symbol.RSB;
        ELSIF s.ch = '\'' THEN
          s.singleQuote := TRUE;
          PushState(s, State.EntityValue);
          RETURN Symbol.Quote;
        ELSIF s.ch = '"' THEN
          s.singleQuote := FALSE;
          PushState(s, State.EntityValue);
          RETURN Symbol.DQuote;
        ELSIF s.ch = '>' THEN
          PopState(s);
          RETURN Symbol.RB;
        ELSIF s.ch IN NameChars THEN RETURN GetName(s);
        ELSE RETURN Symbol.Undef;
        END;
    | State.ContentCSect =>
        (* Within a conditional section, watch for the terminating
           right square bracket. Parameter references are processed
           if within markup. *)

        IF s.ch = ']' THEN RETURN GetEndCSect(s);
        ELSIF s.inMarkup AND s.ch = '%' THEN RETURN GetPEReference(s);
        ELSIF s.ch = '<' THEN RETURN GetTag(s);
        ELSIF s.ch = '&' THEN RETURN GetReference(s);
        ELSE RETURN GetPCData(s);
        END;
    END;
  END GetSym ;

(* Keep a state stack *)

PROCEDURE PopState(s: Scanner) =
  BEGIN
    s.state := VAL(s.stateStack.remhi(),State);
  END PopState;

PROCEDURE PushState(s: Scanner; state: State) =
  BEGIN
    s.stateStack.addhi(ORD(s.state));
    s.state := state;
  END PushState;

(* Keep a file stack *)

PROCEDURE PushFile(s: Scanner; name: TEXT; rd: Rd.T) =
  BEGIN
    IF Debug THEN
      Wr.PutText(Stdio.stdout,"Push file: " & name & "\n");
      Wr.Flush(Stdio.stdout);
    END;

    IF s.input # NIL AND s.input.rd # NIL THEN 
      s.inputStack.addhi(s.input); 
    END;

    s.input := NEW(Input);
    s.input.name := name;
    s.input.rd := rd;
  END PushFile;

PROCEDURE PushNextFile(s: Scanner; name: TEXT; rd: Rd.T) =
  BEGIN
    IF Debug THEN
      Wr.PutText(Stdio.stdout,"Push file next: " & name & "\n");
      Wr.Flush(Stdio.stdout);
    END;

    s.inputStack.addhi(NEW(Input, name := name, rd := rd));
  END PushNextFile;

(* Look for one of the keywords starting with "#". *)

PROCEDURE GetDashName(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    WHILE NOT Rd.EOF(s.input.rd) DO
      NextCh(s);
      IF NOT s.ch IN NameMiddleChars THEN
        PrevCh(s);
        EXIT;
      END;
    END;
    (* IMPLIED PCDATA REQUIRED FIXED *)
    IF CIEqual(s.buf^, s.bufLen, DashImplied) THEN RETURN Symbol.DashImplied;
    ELSIF CIEqual(s.buf^, s.bufLen, DashPCData) THEN RETURN Symbol.DashPCData;
    ELSIF CIEqual(s.buf^, s.bufLen, DashRequired) THEN 
      RETURN Symbol.DashRequired;
    ELSIF CIEqual(s.buf^, s.bufLen, DashFixed) THEN RETURN Symbol.DashFixed;
    ELSE RETURN Symbol.Undef;
    END;
  END GetDashName;

(* The following "get" procedures are called once the first character has
   been read. They process the following characters in the token, if any. *)

(* Get a name *)

PROCEDURE GetName(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    WHILE NOT Rd.EOF(s.input.rd) DO
      NextCh(s);
      IF NOT s.ch IN NameMiddleChars THEN
        PrevCh(s);
        EXIT;
      END;
    END;
    RETURN Symbol.NonKeywordName;
  END GetName;

(* Get a name token (may start with a digit) *)

PROCEDURE GetNmToken(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    WHILE NOT Rd.EOF(s.input.rd) DO
      NextCh(s);
      IF NOT s.ch IN NmTokenChars THEN
        PrevCh(s);
        EXIT;
      END;
    END;
    RETURN Symbol.NmToken;
  END GetNmToken;

(* A start tag was found. Determine the type of tag *)

PROCEDURE GetTag(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    NextCh(s);

    (* Start Element Tag, read just the < *)

    IF s.ch IN NameChars THEN
      PrevCh(s);
      PushState(s,State.ElementTag);
      RETURN Symbol.StartElementTag;

    (* A Processing Instruction (PI) tag, read it all *)

    ELSIF s.ch = '?' THEN
      NextCh(s);

      (* Remove <? *)
      s.buf[0] := s.ch;
      s.bufLen := 1;

      NextCh(s);
      WHILE s.buf[s.bufLen - 2] # '?' OR s.buf[s.bufLen - 1] # '>' DO
        NextCh(s);
      END;

      (* Remove the ?> *)
      DEC(s.bufLen,2);
      RETURN Symbol.PItk;

    (* End Element Tag, read </ *)

    ELSIF s.ch = '/' THEN
      PushState(s,State.ElementTag);
      RETURN Symbol.EndElementTag;

    ELSIF s.ch = '!' THEN 
      NextCh(s);

      (* Comment, read it all *)

      IF s.ch = '-' THEN
        NextCh(s);
        IF s.ch # '-' THEN RETURN Symbol.Undef; END;
        NextCh(s);

        (* Skip the <-- *)
        s.buf[0] := s.ch;
        s.bufLen := 1;

        NextCh(s);
        WHILE s.buf[s.bufLen - 2] # '-' OR s.buf[s.bufLen - 1] # '-' DO
          NextCh(s);
        END;
        NextCh(s);
        IF s.ch # '>' THEN RETURN Symbol.Undef; END;

        (* Remove the trailing --> *)
        DEC(s.bufLen,3);
        RETURN Symbol.Commenttk;
      ELSIF s.ch = '[' THEN
        NextCh(s);

        (* CDATA section, read it all *)

        IF ASCII.Upper[s.ch] = 'C' THEN
          LOOP
            NextCh(s);
            IF NOT s.ch IN NameChars THEN
              EXIT;
            END;
          END;
          IF NOT CIEqual(s.buf^, s.bufLen, CDataTag) THEN 
            RETURN Symbol.Undef; 
          END;
          NextCh(s);

          (* Remove the <[CDATA[ *)
          s.buf[0] := s.ch;
          s.bufLen := 1;

          NextCh(s);
          NextCh(s);
          WHILE s.buf[s.bufLen - 3] # ']' OR s.buf[s.bufLen - 2] # ']' OR
              s.buf[s.bufLen - 1] # '>' DO
            NextCh(s);
          END;

          (* Remove the ]]> *)
          DEC(s.bufLen,3);
          RETURN Symbol.CData;

        (* Conditional section start, read <[ *)

        ELSE
          PrevCh(s);
          PushState(s,State.StartCSect);
          RETURN Symbol.StartCSect;
        END;

      (* We have <! followed by a char, check for <!ELEMENT, <!ATTRIBUTE... *)

      ELSIF s.ch IN NameChars THEN
        WHILE NOT Rd.EOF(s.input.rd) DO
          NextCh(s);
          IF NOT s.ch IN NameChars THEN
            PrevCh(s);
            EXIT;
          END;
        END;

        IF CIEqual(s.buf^, s.bufLen, DocTypeTag) THEN
          PushState(s, State.DocType);
          RETURN Symbol.DocTypeTag;
        ELSIF CIEqual(s.buf^, s.bufLen, ElementTag) THEN
          PushState(s, State.Element);
          RETURN Symbol.ElementTag;
        ELSIF CIEqual(s.buf^, s.bufLen, AttListTag) THEN
          PushState(s, State.AttList);
          RETURN Symbol.AttListTag;
        ELSIF CIEqual(s.buf^, s.bufLen, EntityTag) THEN
          PushState(s, State.Entity);
          RETURN Symbol.EntityTag;
        ELSIF CIEqual(s.buf^, s.bufLen, NotationTag) THEN
          PushState(s, State.Notation);
          RETURN Symbol.NotationTag;
        ELSE
          RETURN Symbol.Undef;
        END;
      ELSE
        RETURN Symbol.Undef;
      END;
    ELSE
      RETURN Symbol.Undef;
    END;
  END GetTag;

(* Read an entity reference, possibly a character or Hex character 
   reference. *)

PROCEDURE GetReference(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted }    =
  BEGIN
    NextCh(s);
    s.buf[0] := s.ch;
    s.bufLen := 1;

    IF s.ch = '#' THEN
      NextCh(s);
      IF ASCII.Upper[s.ch] = 'X' THEN
        NextCh(s);
        WHILE s.ch IN HexDigitChars DO NextCh(s); END;
        DEC(s.bufLen);
        IF s.ch # ';' THEN RETURN Symbol.Undef;
        ELSE RETURN Symbol.HexCharRef;
        END;
      ELSE
        WHILE s.ch IN DigitChars DO NextCh(s); END;
        DEC(s.bufLen);
        IF s.ch # ';' THEN RETURN Symbol.Undef;
        ELSE RETURN Symbol.CharRef;
        END;
      END;
    END;

    IF NOT s.ch IN NameChars THEN RETURN Symbol.Undef; END;
    WHILE s.ch IN NameMiddleChars DO
      NextCh(s);
    END;
    DEC(s.bufLen);
    IF s.ch # ';' THEN RETURN Symbol.Undef;
    ELSE RETURN Symbol.EntityRef;
    END;
  END GetReference;

(* Read parsed character data. Check if only white space is present;
   there are places where only white space is accepted. *)

PROCEDURE GetPCData(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  VAR
    allWhite := s.ch IN WhiteChars;
  BEGIN
    WHILE NOT Rd.EOF(s.input.rd) DO
      NextCh(s);
      IF s.ch = '&' OR s.ch = '<' THEN
        PrevCh(s);
        EXIT;
      END;
      IF NOT s.ch IN WhiteChars THEN allWhite := FALSE; END;
    END;
    IF allWhite THEN RETURN Symbol.White;
    ELSE RETURN Symbol.PCDataChunk;
    END;
  END GetPCData;

(* Get an attribute value. *)

PROCEDURE GetAttValue(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    WHILE NOT Rd.EOF(s.input.rd) DO
      NextCh(s);
      IF s.ch = '&' OR s.ch = '<' OR (s.ch = '\'' AND s.singleQuote) OR
         (s.ch = '"' AND NOT s.singleQuote) THEN
        PrevCh(s);
        EXIT;
      END;
    END;
    RETURN Symbol.AttValueData;
  END GetAttValue;

(* Get a % alone or a parameter entity reference. *)

PROCEDURE GetPEReference(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    NextCh(s);

    IF s.ch IN WhiteChars THEN
      PrevCh(s);
      RETURN Symbol.Percent;
    END;

    IF NOT s.ch IN NameChars THEN RETURN Symbol.Undef; END;

    s.buf[0] := s.ch;
    s.bufLen := 1;

    WHILE s.ch IN NameMiddleChars DO
      IF Rd.EOF(s.input.rd) THEN RETURN Symbol.PEReference; END;
      NextCh(s);
    END;

    IF s.ch # ';' THEN PrevCh(s);
    ELSE DEC(s.bufLen);
    END;

    RETURN Symbol.PEReference;
  END GetPEReference;

(* Get an entity value. *)

PROCEDURE GetEntityValue(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    WHILE NOT Rd.EOF(s.input.rd) DO
      NextCh(s);
      IF s.ch = '&' OR s.ch = '%' OR (s.ch = '\'' AND s.singleQuote) OR
          (s.ch = '"' AND NOT s.singleQuote) THEN
        PrevCh(s);
        EXIT;
      END;
    END;
    RETURN Symbol.EntityValueData;
  END GetEntityValue;

(* Get a simple string without checking for entity references. *)

PROCEDURE GetCatalogString(s: Scanner): Symbol RAISES 
    {Rd.Failure, Rd.EndOfFile, Thread.Alerted } =
  BEGIN
    NextCh(s);
    s.buf[0] := s.ch;
    s.bufLen := 1;

    WHILE (s.ch # '\'' OR NOT s.singleQuote) AND
          (s.ch # '"' OR s.singleQuote) DO
      NextCh(s);
    END;

    DEC(s.bufLen);
    RETURN Symbol.NmToken
  END GetCatalogString;

(* Get a conditional section end ]]> *)
PROCEDURE GetEndCSect(s: Scanner): Symbol RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    NextCh(s);
    IF s.ch # ']' THEN RETURN Symbol.Undef; END;
    NextCh(s);
    IF s.ch # '>' THEN RETURN Symbol.Undef; END;
    PopState(s);
    RETURN Symbol.EndCSect;
  END GetEndCSect;

(* Skip until the end of a comment, marked by "--". *)

PROCEDURE SkipComment(s: Scanner) RAISES {Rd.Failure, Rd.EndOfFile,
    Thread.Alerted } =
  BEGIN
    NextCh(s);
    s.buf[0] := s.ch;
    s.bufLen := 1;
    NextCh(s);

    WHILE s.buf[s.bufLen - 2] # '-' OR s.buf[s.bufLen - 1] # '-' DO
      NextCh(s);
    END;
    DEC(s.bufLen,2);
  END SkipComment;

(* Fill in the keywords table *)

VAR
  keywords := NEW(TextIntTbl.Default).init();
BEGIN
  EVAL keywords.put("IGNORE",ORD(Symbol.IGNOREkw));
  EVAL keywords.put("INCLUDE",ORD(Symbol.INCLUDEkw));
  EVAL keywords.put("EMPTY",ORD(Symbol.EMPTYkw));
  EVAL keywords.put("ANY",ORD(Symbol.ANYkw));
  EVAL keywords.put("CDATA",ORD(Symbol.CDATAkw));
  EVAL keywords.put("ID",ORD(Symbol.IDkw));
  EVAL keywords.put("IDREF",ORD(Symbol.IDREFkw));
  EVAL keywords.put("IDREFS",ORD(Symbol.IDREFSkw));
  EVAL keywords.put("ENTITY",ORD(Symbol.ENTITYkw));
  EVAL keywords.put("ENTITIES",ORD(Symbol.ENTITIESkw));
  EVAL keywords.put("NAME",ORD(Symbol.NAMEkw));
  EVAL keywords.put("NAMES",ORD(Symbol.NAMESkw));
  EVAL keywords.put("NMTOKEN",ORD(Symbol.NMTOKENkw));
  EVAL keywords.put("NMTOKENS",ORD(Symbol.NMTOKENSkw));
  EVAL keywords.put("NUMBER",ORD(Symbol.NUMBERkw));
  EVAL keywords.put("NUMBERS",ORD(Symbol.NUMBERSkw));
  EVAL keywords.put("NOTATION",ORD(Symbol.NOTATIONkw));
  EVAL keywords.put("SYSTEM",ORD(Symbol.SYSTEMkw));
  EVAL keywords.put("PUBLIC",ORD(Symbol.PUBLICkw));
  EVAL keywords.put("SGMLDECL",ORD(Symbol.SGMLDECLkw));
  EVAL keywords.put("CATALOG",ORD(Symbol.CATALOGkw));
  EVAL keywords.put("DOCTYPE",ORD(Symbol.DOCTYPEkw));
  EVAL keywords.put("NDATA",ORD(Symbol.NDATAkw));
  EVAL keywords.put("O",ORD(Symbol.Okw));
END SGMLCS.
