(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Created by luca                               *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue May 26 15:46:59 1998
 *)

MODULE SynScan;
IMPORT Wr, TextRefTbl, Text, TextConv, Rd, SynWr, SynLocation, 
       Fmt, Lex, FloatMode, TextRd, Thread, SynScan;

REVEAL
  Keyword =  BRANDED "SynScan.Keyword" OBJECT name: TEXT; keyword: BOOLEAN END;
  KeywordSet = BRANDED "SynScan.KeywordSet" OBJECT table: TextRefTbl.T END;

TYPE
  Symbol = Keyword;

  TokenClass =
    {IdeCase, InfixCase, CharCase, IntCase, RealCase, StringCase, DelimCase,
     EofCase };

  InputList = REF InputListBase;
  InputListBase = RECORD
    rd: Rd.T;
    fileName: TEXT;
    acceptedCharPos, acceptedLinePos, acceptedLineCharPos: INTEGER;
    acceptedTokenBegPos, acceptedTokenEndPos: INTEGER;
    generateEOF: BOOLEAN;
    closeReader: BOOLEAN;
    rest: InputList;
  END;

CONST EofChar = VAL(255, CHAR);

REVEAL T = 
  BRANDED "SynScan.T" REF RECORD
    swr: SynWr.T;
    scanBuffer: REF ARRAY OF CHAR;
    scanBufferSize: INTEGER;
    charTable: ARRAY CHAR OF CharacterClass;
    keySet: KeywordSet;
    lookAheadReady: BOOLEAN;
    lookAheadChar: CHAR;
    tokenReady: BOOLEAN;
    tokenClass: TokenClass;
    tokenBegPos: INTEGER;
    tokenEndPos: INTEGER;
    tokenChar: CHAR;
    tokenInt: INTEGER;
    tokenReal: LONGREAL;
    tokenString: TEXT;
    tokenDelim: CHAR;
    tokenSym: Symbol;
    input: InputList;
    scanPoint: INTEGER;
    firstPrompt, nextPrompt: TEXT;
    isFirstPrompt: BOOLEAN;
    (* The following variables are copies of "input^" fields, so
       that they can survive a PopInput. *)
      fileName: TEXT;
      acceptedCharPos, acceptedLinePos, acceptedLineCharPos: INTEGER;
      acceptedTokenBegPos, acceptedTokenEndPos: INTEGER;
  END;

PROCEDURE NewKeywordSet(): KeywordSet =
  BEGIN
    RETURN NEW(KeywordSet, table:=NEW(TextRefTbl.Default).init(10));
  END NewKeywordSet;

PROCEDURE CopyKeywordSet(keywordSet: KeywordSet): KeywordSet =
  VAR newKeySet: KeywordSet; key: TEXT; value: REFANY;
    iterator: TextRefTbl.Iterator; symbol: Symbol;
  BEGIN
    newKeySet := NEW(KeywordSet, 
      table:=NEW(TextRefTbl.Default).init(keywordSet.table.size()));
    iterator := keywordSet.table.iterate();
    WHILE iterator.next((*out*)key, (*out*)value) DO
      symbol := NARROW(value, Symbol);
      IF symbol.keyword THEN
        EVAL newKeySet.table.put(key, symbol);
      END;
    END;
    RETURN newKeySet;
  END CopyKeywordSet;

PROCEDURE GetKeywordSet(sc: T): KeywordSet =
  BEGIN
    RETURN sc^.keySet;
  END GetKeywordSet;

PROCEDURE UseKeywordSet(sc: T; keywordSet: KeywordSet) =
  BEGIN
    sc^.keySet := keywordSet;
  END UseKeywordSet;

PROCEDURE BeKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword =
  VAR value: REFANY; symbol: Symbol;
  BEGIN
    IF keywordSet.table.get(ide, (*VAR OUT*) value) THEN
      symbol := NARROW(value, Symbol);
      symbol.keyword := TRUE;
    ELSE
      symbol := 
        NEW(Symbol, name:=Text.Sub(ide,0,Text.Length(ide)), keyword:=TRUE);
      EVAL keywordSet.table.put(ide, symbol);
    END;
    RETURN symbol;
  END BeKeyword;

PROCEDURE GetKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword =
  VAR value: REFANY; symbol: Symbol;
  BEGIN
    IF keywordSet.table.get(ide, (*VAR OUT*) value) THEN
      symbol := NARROW(value, Symbol);
      IF symbol.keyword THEN RETURN symbol ELSE RETURN NIL END;
    ELSE RETURN NIL;
    END;
  END GetKeyword;

PROCEDURE IsDelimiter(sc: T; char: CHAR): BOOLEAN =
  BEGIN
    RETURN sc^.charTable[char] = CharacterClass.DelimCharCase;
  END IsDelimiter;

PROCEDURE IsIdentifier(sc: T; string: TEXT): BOOLEAN =
  VAR class: CharacterClass; length: INTEGER; 
  BEGIN
    length := Text.Length(string);
    IF length=0 THEN RETURN FALSE END;
    IF sc^.charTable[Text.GetChar(string,0)]=CharacterClass.LetterCharCase THEN
      FOR i:=0 TO length-1 DO 
	class := sc^.charTable[Text.GetChar(string,i)];     
        IF (class # CharacterClass.LetterCharCase) AND 
	    (class # CharacterClass.DigitCharCase) THEN RETURN FALSE END;
      END;
      RETURN TRUE;
    ELSIF sc^.charTable[Text.GetChar(string,0)]=CharacterClass.SpecialCharCase THEN
      FOR i:=0 TO length-1 DO 
	class := sc^.charTable[Text.GetChar(string,i)];     
        IF (class # CharacterClass.SpecialCharCase) THEN RETURN FALSE END;
      END;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsIdentifier;

PROCEDURE ScanPoint(sc: T): INTEGER =
  BEGIN
    RETURN sc.scanPoint;
  END ScanPoint;

PROCEDURE GetInputState(sc: T) =
  BEGIN
    WITH z = sc^ DO
      IF z.input = NIL THEN
        z.fileName := "<no input>";
        z.acceptedCharPos := 0;
        z.acceptedLinePos := 0;
        z.acceptedLineCharPos := 0;
        z.acceptedTokenBegPos := 0;
        z.acceptedTokenEndPos := 0;
      ELSE
        z.fileName := z.input^.fileName;
        z.acceptedCharPos := z.input^.acceptedCharPos;
        z.acceptedLinePos := z.input^.acceptedLinePos;
        z.acceptedLineCharPos := z.input^.acceptedLineCharPos;
        z.acceptedTokenBegPos := z.input^.acceptedTokenBegPos;
        z.acceptedTokenEndPos := z.input^.acceptedTokenEndPos;
     END;
   END;
  END GetInputState;

PROCEDURE SetInputState(sc: T) =
  BEGIN
    WITH z = sc^ DO
      IF z.input # NIL THEN
        z.input^.acceptedCharPos := z.acceptedCharPos;
        z.input^.acceptedLinePos := z.acceptedLinePos;
        z.input^.acceptedLineCharPos := z.acceptedLineCharPos;
        z.input^.acceptedTokenBegPos := z.acceptedTokenBegPos;
        z.input^.acceptedTokenEndPos := z.acceptedTokenEndPos;
       END;
     END;
   END SetInputState;

PROCEDURE NewInput(fileName: TEXT; rd: Rd.T; rest: InputList;
    closeReader: BOOLEAN; generateEOF: BOOLEAN): InputList =
  VAR res: InputList;
  BEGIN
    res := NEW(InputList);
    res^.fileName := fileName;
    res^.rd := rd;
    res^.acceptedCharPos := 0;
    res^.acceptedLinePos := 1;
    res^.acceptedLineCharPos := 0;
    res^.acceptedTokenBegPos := 0;
    res^.acceptedTokenEndPos := 0;
    res^.generateEOF := generateEOF;
    res^.closeReader := closeReader;
    res^.rest := rest;
    RETURN res;
  END NewInput;

PROCEDURE PushInput(sc: T; fileName: TEXT; rd: Rd.T; closeReader: BOOLEAN;
    generateEOF: BOOLEAN := TRUE) =
  BEGIN
    SetInputState(sc);
    sc^.input := NewInput(fileName, rd, sc^.input, closeReader, generateEOF);
    GetInputState(sc);
  END PushInput;

PROCEDURE PopInput(sc: T) RAISES {NoReader} =
  BEGIN
    IF sc^.input = NIL THEN RAISE NoReader
    ELSE
      TRY
        TRY
          IF sc^.input^.closeReader THEN Rd.Close(sc^.input^.rd) END;
        FINALLY
          sc^.input := sc^.input^.rest;
          GetInputState(sc);
        END;
      EXCEPT Rd.Failure, Thread.Alerted => END;
    END;
  END PopInput;

PROCEDURE CurrentLocationInfo(sc: T; VAR(*out*) info: SynLocation.Info) =
  BEGIN
    info.fileName:=sc^.fileName;
    info.char := sc^.acceptedCharPos;
    info.line := sc^.acceptedLinePos;
    info.lineChar := sc^.acceptedLineCharPos;
  END CurrentLocationInfo;

PROCEDURE SetCharNo(sc: T; charNo, lineNo, lineCharNo: INTEGER) =
  BEGIN
    WITH z = sc^ DO
      z.acceptedCharPos := charNo;
      z.acceptedLinePos := lineNo;
      z.acceptedLineCharPos := lineCharNo;
      z.acceptedTokenBegPos := charNo;
      z.acceptedTokenEndPos := charNo;
    END;
  END SetCharNo;

PROCEDURE PrintPrompt(sc: T) =
  BEGIN
    WITH z = sc^,
         pwr = SynWr.UnderlyingWr(sc.swr) DO
      IF z.isFirstPrompt THEN
        TRY
          Wr.PutText(pwr, z.firstPrompt);
          Wr.Flush(pwr);
          INC(z.acceptedCharPos, Text.Length(z.firstPrompt));
          INC(z.acceptedLineCharPos, Text.Length(z.firstPrompt));
        EXCEPT Wr.Failure, Thread.Alerted => END;
        z.isFirstPrompt := FALSE;
      ELSE
        TRY
          Wr.PutText(pwr, z.nextPrompt);
          Wr.Flush(pwr);
          INC(z.acceptedCharPos, Text.Length(z.nextPrompt));
          INC(z.acceptedLineCharPos, Text.Length(z.nextPrompt));
        EXCEPT Wr.Failure, Thread.Alerted => END;
      END;
    END;
  END PrintPrompt;
  
PROCEDURE LookChar(sc: T): CHAR RAISES {NoReader} =
  VAR char: CHAR;
  BEGIN
    WITH z = sc^ DO
      IF z.lookAheadReady THEN RETURN z.lookAheadChar END;
      IF z.input=NIL THEN RAISE NoReader END;
      TRY
        IF Rd.CharsReady(z.input^.rd) = 0 THEN PrintPrompt(sc); END;
      EXCEPT Rd.Failure => RAISE NoReader END;
      TRY 
        char := Rd.GetChar(z.input^.rd);
        z.lookAheadChar := char;
        z.lookAheadReady := TRUE;
        RETURN char;
      EXCEPT
      |  Rd.EndOfFile => 
        IF z.input^.generateEOF THEN
	  PopInput(sc);
	  z.lookAheadChar := EofChar;
	  z.lookAheadReady := TRUE;
	  RETURN EofChar;
        ELSE
	  PopInput(sc);
	  RETURN LookChar(sc);
        END;
      | Rd.Failure, Thread.Alerted => RAISE NoReader;
      END;
    END;
  END LookChar;
    
PROCEDURE GetChar(sc: T): CHAR RAISES {NoReader} =
  BEGIN
    WITH z = sc^ DO
      IF NOT z.lookAheadReady THEN EVAL LookChar(sc) END;
      z.lookAheadReady := FALSE;
      INC(z.acceptedCharPos);
      INC(z.acceptedLineCharPos);
      IF z.lookAheadChar='\n' THEN
        INC(z.acceptedLinePos);
        z.acceptedLineCharPos := 0;
      END;
      RETURN z.lookAheadChar;
    END;
  END GetChar;

PROCEDURE HaveChar(sc: T; char: CHAR): BOOLEAN RAISES {NoReader} =
  BEGIN
    IF char = LookChar(sc) THEN
      char := GetChar(sc);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END;
  END HaveChar;

PROCEDURE ScanNumber(sc: T): TokenClass RAISES {NoReader, Fail} =
  VAR integer: BOOLEAN; char: CHAR; buf: REF ARRAY OF CHAR;
  BEGIN
    WITH z = sc^ DO
      IF HaveChar(sc, '~') THEN
        z.scanBuffer[z.scanBufferSize]:= '-';
        INC(z.scanBufferSize);
      END;
      integer := TRUE;
      LOOP
        char := LookChar(sc);
        IF (z.charTable[char] # CharacterClass.DigitCharCase) AND
            (char # '.') AND (char # 'e') AND (char # 'd') AND 
            (char # '~') THEN
          EXIT
        END;
        char := GetChar(sc);
        IF (char = '.') OR (char = 'e') OR (char = 'd') THEN 
          integer := FALSE;
        END;
        IF (NOT integer) AND (char = '~') THEN char := '-' END;
        z.scanBuffer[z.scanBufferSize]:= char;
        INC(z.scanBufferSize);
        IF z.scanBufferSize = NUMBER(z.scanBuffer^) THEN
          buf := NEW(REF ARRAY OF CHAR, 2*z.scanBufferSize);
          SUBARRAY(buf^, 0, z.scanBufferSize) := z.scanBuffer^;
          z.scanBuffer := buf;
        END;
      END;
      TRY
        IF integer THEN
          z.tokenInt := 
            Lex.Int(
              TextRd.New(
                Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize))));
          z.scanBufferSize := 0;
          RETURN TokenClass.IntCase
        ELSE
          z.tokenReal :=
            Lex.LongReal(
              TextRd.New(
                Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize))));
          z.scanBufferSize := 0;
          RETURN TokenClass.RealCase;
        END;
      EXCEPT Lex.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted =>
        SyntaxMsg(sc, "Illegal numerical constant", "");
        RAISE Fail;
      END;
    END;
  END ScanNumber;

PROCEDURE DecodeCharFromProducer(
    sc: T;
    VAR (*out*)charOut: CHAR)
    RAISES {TextConv.Fail} =
  CONST Octal01 = TextConv.CharSet{'0', '1'};
  VAR charsIn: ARRAY[0..3] OF CHAR; availIn: INTEGER;
  BEGIN
    TRY
      charsIn[0] := GetChar(sc);
      availIn := 1;
      IF charsIn[0] = TextConv.Escape THEN
        charsIn[1] := GetChar(sc);
        INC(availIn);
        IF charsIn[1] IN Octal01 THEN
          charsIn[2] := GetChar(sc);
          charsIn[3] := GetChar(sc);
          INC(availIn, 2);
        END;
      END;
    EXCEPT ELSE (* p failure *) RAISE TextConv.Fail;
    END;
    EVAL TextConv.DecodeChar(charsIn, availIn, (*out*)charOut);
  END DecodeCharFromProducer;

PROCEDURE DecodeChar(sc: T): CHAR RAISES {Fail} =
  VAR char: CHAR;
  BEGIN
    TRY
      DecodeCharFromProducer(sc, (*out*)char);
    EXCEPT
    | TextConv.Fail => 
        SyntaxMsg(sc, "ill-formed character escape sequence", "");
        RAISE Fail;
    END;
    RETURN char;
  END DecodeChar;

PROCEDURE ScanChar(sc: T): CHAR RAISES {NoReader, Fail} =
  VAR char: CHAR;
  BEGIN
    char := GetChar(sc);
    char := DecodeChar(sc);
    IF GetChar(sc) # '\'' THEN
      SyntaxMsg(sc, "closing \' expected", "");
      RAISE Fail;
    END;
    RETURN char;
  END ScanChar;

PROCEDURE ScanString(sc: T): TEXT RAISES {NoReader, Fail} =
  VAR char: CHAR; string: TEXT; buf: REF ARRAY OF CHAR;
  BEGIN
    WITH z = sc^ DO
      char := GetChar(sc);
      WHILE LookChar(sc) # '\"' DO
        char := DecodeChar(sc);
        z.scanBuffer[z.scanBufferSize]:= char;
        INC(z.scanBufferSize);
        IF z.scanBufferSize = NUMBER(z.scanBuffer^) THEN
          buf := NEW(REF ARRAY OF CHAR, 2*z.scanBufferSize);
          SUBARRAY(buf^, 0, z.scanBufferSize) := z.scanBuffer^;
          z.scanBuffer := buf;
        END;
      END;
      char := GetChar(sc);
      string := Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize));
      z.scanBufferSize := 0;
      RETURN string;
    END;
  END ScanString;

PROCEDURE ScanAlphaNumIde(sc: T) RAISES {NoReader} =
  VAR class: CharacterClass;
  BEGIN
    WITH z = sc^ DO
      LOOP
        class := z.charTable[LookChar(sc)];
        IF (class # CharacterClass.LetterCharCase) 
            AND (class # CharacterClass.DigitCharCase) 
        THEN EXIT 
        END;
        z.scanBuffer[z.scanBufferSize]:=GetChar(sc);
        INC(z.scanBufferSize);
      END;
    END;
  END ScanAlphaNumIde;

PROCEDURE ScanSpecialIde(sc: T) RAISES {NoReader} =
  BEGIN
    WITH z = sc^ DO
      WHILE z.charTable[LookChar(sc)] = CharacterClass.SpecialCharCase DO
        z.scanBuffer[z.scanBufferSize]:=GetChar(sc);
        INC(z.scanBufferSize);
      END;
    END;
  END ScanSpecialIde;

PROCEDURE ScanComment(sc: T) RAISES {NoReader, Fail} =
  VAR level: CARDINAL; char: CHAR;
  BEGIN
    level := 1;
    WHILE 0 < level DO
      char := GetChar(sc);
      IF char=EofChar THEN
        SyntaxMsg(sc, "Open comment at end of file", "");
        RAISE Fail;
      END;
      IF char = '*' THEN
        IF LookChar(sc) = ')' THEN char := GetChar(sc); level := level - 1 END;
      ELSIF char = '(' THEN
        IF LookChar(sc) = '*' THEN char := GetChar(sc); level := level + 1 END;
      END;
    END
  END ScanComment;

PROCEDURE NextToken(sc: T): TokenClass RAISES {NoReader, Fail} =
  VAR char: CHAR; class: CharacterClass; tokenClass1: TokenClass;
  BEGIN
    WITH z = sc^ DO
      WHILE z.charTable[LookChar(sc)] = CharacterClass.BlankCharCase DO 
        char := GetChar(sc); 
      END;
      z.tokenBegPos := z.acceptedCharPos;
      char := LookChar(sc);
      class := z.charTable[char];
      IF class = CharacterClass.LetterCharCase THEN
        ScanAlphaNumIde(sc);
        tokenClass1 := TokenClass.IdeCase;
      ELSIF class = CharacterClass.DelimCharCase THEN
        z.tokenDelim := GetChar(sc);
        IF (char = '(') AND (LookChar(sc) = '*') THEN
          char := GetChar(sc);
          ScanComment(sc);
          tokenClass1 := NextToken(sc);
        ELSE
          tokenClass1 := TokenClass.DelimCase;
        END;
      ELSIF class = CharacterClass.SpecialCharCase THEN
        ScanSpecialIde(sc);
        tokenClass1 := TokenClass.InfixCase;
      ELSIF (char = '~') OR (class = CharacterClass.DigitCharCase) THEN
        tokenClass1 := ScanNumber(sc);
      ELSIF char = '\'' THEN
        z.tokenChar := ScanChar(sc);
        tokenClass1 := TokenClass.CharCase;
      ELSIF char = '\"' THEN
        z.tokenString := ScanString(sc);
        tokenClass1 := TokenClass.StringCase;
      ELSIF class = CharacterClass.EofCase THEN 
        char := GetChar(sc);
        tokenClass1 := TokenClass.EofCase;
      ELSE
        char := GetChar(sc);
        SyntaxMsg(sc, "Illegal Char", "");
        RAISE Fail;
      END;
      z.tokenEndPos := z.acceptedCharPos;
      RETURN tokenClass1;
    END;
  END NextToken;

PROCEDURE LookToken(sc: T): TokenClass RAISES {NoReader, Fail} =
  BEGIN
    WITH z = sc^ DO
      IF z.tokenReady THEN RETURN z.tokenClass END;
      z.tokenClass := NextToken(sc);
      z.tokenReady := TRUE;
      RETURN z.tokenClass;
    END;
  END LookToken;

PROCEDURE GetToken(sc: T): TokenClass RAISES {NoReader, Fail} =
  VAR class: TokenClass;
  BEGIN
    WITH z = sc^ DO
      IF z.tokenReady THEN
        z.tokenReady := FALSE;
        class := z.tokenClass;
      ELSE
        class := NextToken(sc);
      END;
      INC(z.scanPoint);
      z.acceptedTokenBegPos := z.tokenBegPos;
      z.acceptedTokenEndPos := z.tokenEndPos;
      RETURN class;
    END;
  END GetToken;

PROCEDURE PrintContext(sc: T) =
  VAR string: TEXT;
  BEGIN
    WITH z = sc^ DO
      SynWr.Text(sc.swr, " just before: ", loud:=TRUE);
      CASE z.tokenClass OF
      | TokenClass.IdeCase, TokenClass.InfixCase =>
          string := 
            Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize));
          z.scanBufferSize := 0;
          SynWr.Text(sc.swr, string, loud:=TRUE);
      | TokenClass.CharCase => SynWr.Char(sc.swr, z.tokenChar, loud:=TRUE);
      | TokenClass.IntCase => 
          SynWr.Text(sc.swr, Fmt.Int(z.tokenInt), loud:=TRUE);
      | TokenClass.RealCase => 
          SynWr.Text(sc.swr, Fmt.LongReal(z.tokenReal), loud:=TRUE);
      | TokenClass.StringCase =>
          SynWr.Char(sc.swr, '\"', loud:=TRUE);
          SynWr.Text(sc.swr, z.tokenString, loud:=TRUE);
          SynWr.Char(sc.swr, '\"', loud:=TRUE);
      | TokenClass.DelimCase => SynWr.Char(sc.swr, z.tokenDelim, loud:=TRUE);
      | TokenClass.EofCase => SynWr.Text(sc.swr, "<EndOfInput>", loud:=TRUE);
      END;
      SynWr.Flush(sc.swr, loud:=TRUE);
    END;
  END PrintContext;

PROCEDURE PrintSequel(sc: T) =
  VAR n: INTEGER; ch: CHAR;
  BEGIN
    IF sc^.input = NIL THEN RETURN END;
    n := 40;
    TRY
      WHILE (0 < n) AND (Rd.CharsReady(sc^.input^.rd) > 0) DO
        ch:=GetChar(sc); (* NOWARN *)
        IF sc^.input = NIL THEN RETURN END;
        IF (Rd.CharsReady(sc^.input^.rd)>0) OR (ch#'\n') THEN
          SynWr.Char(sc.swr, ch, loud:=TRUE);
        END;
        n := n - 1;
      END;
      IF Rd.CharsReady(sc^.input^.rd) > 0 THEN
        SynWr.Text(sc.swr, " ...", loud:=TRUE);
      END;
    EXCEPT Rd.Failure, SynScan.NoReader => END;
  END PrintSequel;

PROCEDURE FlushInput(sc: T) =
  BEGIN
    IF sc^.input = NIL THEN RETURN END;
    TRY
      WHILE Rd.CharsReady(sc^.input^.rd) > 0 DO 
        EVAL GetChar(sc); (* NOWARN *)
        IF sc^.input = NIL THEN RETURN END;
      END; 
    EXCEPT Rd.Failure, SynScan.NoReader => END;
  END FlushInput;

PROCEDURE ErrorMsg(sc: T; msg: TEXT := "") =
  BEGIN
    IF NOT Text.Empty(msg) THEN
      SynWr.Text(sc.swr, msg, loud:=TRUE);
      SynWr.Char(sc.swr, '\n', loud:=TRUE);
      SynWr.Flush(sc.swr, loud:=TRUE);
    END;
    Reset(sc);
  END ErrorMsg;

PROCEDURE SyntaxMsg(sc: T; cause: TEXT := ""; culprit: TEXT := "";
    errorReportStyle: ErrorReportStyle := ErrorReportStyle.LinePlusChar) =
  VAR info: SynLocation.Info;
  BEGIN
    CurrentLocationInfo(sc, (*out*)info);
    SynWr.Text(sc.swr, "Syntax error just before: ", loud:=TRUE);
    PrintSequel(sc);
    IF (NOT Text.Empty(cause)) OR (NOT Text.Empty(culprit)) THEN
      SynWr.Char(sc.swr, '\n', loud:=TRUE);
      SynWr.Text(sc.swr, "  ", loud:=TRUE);
      SynWr.Text(sc.swr, cause, loud:=TRUE);
      SynWr.Char(sc.swr, ' ', loud:=TRUE);
      SynWr.Text(sc.swr, culprit, loud:=TRUE);
    END;
    SynWr.Char(sc.swr, '\n', loud:=TRUE);
    SynWr.Text(sc.swr, "Error detected ", loud:=TRUE);
    CASE errorReportStyle OF
    | ErrorReportStyle.LinePlusChar =>
        IF Text.Empty(info.fileName) THEN
          SynLocation.PrintLineDifference(sc.swr,
            SynLocation.NewLineLocation(info), info.line);
        ELSE
          SynLocation.PrintLocation(sc.swr,
            SynLocation.NewLineLocation(info));
        END;
    | ErrorReportStyle.CharRange =>
        SynLocation.PrintLocation(sc.swr,
            SynLocation.NewCharLocation(info, info));
    END;
    SynWr.Char(sc.swr, '\n', loud:=TRUE);
    SynWr.Flush(sc.swr, loud:=TRUE);
    Reset(sc);
  END SyntaxMsg;

PROCEDURE GetTokenChar(sc: T; VAR (*out*) char: CHAR): BOOLEAN 
RAISES {NoReader, Fail} =
  BEGIN
    IF LookToken(sc) = TokenClass.CharCase THEN
      EVAL(GetToken(sc));
      char := sc^.tokenChar; 
      RETURN TRUE;
    ELSE RETURN FALSE; 
    END;
  END GetTokenChar;

PROCEDURE GetTokenNat(sc: T; VAR (*out*) nat: CARDINAL): BOOLEAN 
RAISES {NoReader, Fail} =
  BEGIN
    IF LookToken(sc) = TokenClass.IntCase THEN
      IF sc^.tokenInt >= 0 THEN
	EVAL(GetToken(sc));
        nat := sc^.tokenInt; 
	RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    ELSE RETURN FALSE;
    END;
  END GetTokenNat;

PROCEDURE GetTokenInt(sc: T; VAR (*out*) int: INTEGER): BOOLEAN 
RAISES {NoReader, Fail} =
  BEGIN
    IF LookToken(sc) = TokenClass.IntCase THEN 
      EVAL(GetToken(sc));
      int := sc^.tokenInt;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END GetTokenInt;

PROCEDURE GetTokenReal(sc: T; VAR (*ou*) real: LONGREAL): BOOLEAN 
RAISES {NoReader, Fail} =
  BEGIN
    IF LookToken(sc) = TokenClass.RealCase THEN 
      EVAL(GetToken(sc));
      real := sc^.tokenReal;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END GetTokenReal;

PROCEDURE GetTokenString(sc: T; VAR (*out*) string: TEXT): BOOLEAN 
RAISES {NoReader, Fail} =
  BEGIN
    IF LookToken(sc) = TokenClass.StringCase THEN 
      EVAL(GetToken(sc));
      string := sc^.tokenString;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END GetTokenString;

PROCEDURE GetTokenIde(sc: T; VAR (*ou*) ide: TEXT): BOOLEAN 
RAISES {NoReader, Fail} =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    WITH z = sc^ DO
      class := LookToken(sc);
      IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
        IF z.scanBufferSize # 0 THEN
          name := Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize));
	  IF z.keySet.table.get(name, (*VAR OUT*) value) THEN
	    z.tokenSym := NARROW(value, Symbol);
	  ELSE
	    z.tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	    EVAL z.keySet.table.put(name, z.tokenSym);
	  END;
          z.scanBufferSize := 0;
        END;
        IF z.tokenSym.keyword THEN RETURN FALSE END;
        EVAL(GetToken(sc));
        ide := z.tokenSym.name;
        RETURN TRUE;
      ELSE RETURN FALSE;
      END;
    END;
  END GetTokenIde;

PROCEDURE GetTokenName(sc: T; VAR (*ou*) text: TEXT): BOOLEAN 
RAISES {NoReader, Fail} =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    WITH z = sc^ DO
      class := LookToken(sc);
      IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
        IF z.scanBufferSize # 0 THEN
	  name := Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize));
	  IF z.keySet.table.get(name, (*VAR OUT*) value) THEN
	    z.tokenSym := NARROW(value, Symbol);
	  ELSE
	    z.tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	    EVAL z.keySet.table.put(name, z.tokenSym);
	  END;
          z.scanBufferSize := 0;
        END;
        EVAL(GetToken(sc));
        text := z.tokenSym.name;
        RETURN TRUE;
      ELSE RETURN FALSE;
      END;
    END;
  END GetTokenName;

PROCEDURE GetTokenEof(sc: T): BOOLEAN 
RAISES {NoReader, Fail} =
  VAR class : TokenClass ;
  BEGIN
    class := LookToken(sc);
    IF class = TokenClass.EofCase THEN
      EVAL GetToken(sc);
      RETURN TRUE;
    ELSE
      RETURN FALSE ;
    END;
  END GetTokenEof;

PROCEDURE HaveTokenIde(sc: T; ide: TEXT): BOOLEAN 
RAISES {NoReader, Fail} =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    WITH z = sc^ DO
      class := LookToken(sc);
      IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
        IF z.scanBufferSize # 0 THEN
	  name := Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize));
	  IF z.keySet.table.get(name, (*VAR OUT*) value) THEN
	    z.tokenSym := NARROW(value, Symbol);
	  ELSE
	    z.tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	    EVAL z.keySet.table.put(name, z.tokenSym);
	  END;
          z.scanBufferSize := 0;
        END;
        IF z.tokenSym.keyword THEN RETURN FALSE END;
        IF NOT Text.Equal(ide, z.tokenSym.name) THEN RETURN FALSE END;
        EVAL(GetToken(sc));
        RETURN TRUE;
      ELSE RETURN FALSE;
      END;
    END;
  END HaveTokenIde;

PROCEDURE HaveTokenName(sc: T; text: TEXT): BOOLEAN 
RAISES {NoReader, Fail} =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    WITH z = sc^ DO
      class := LookToken(sc);
      IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
        IF z.scanBufferSize # 0 THEN
	  name := Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize));
	  IF z.keySet.table.get(name, (*VAR OUT*) value) THEN
	    z.tokenSym := NARROW(value, Symbol);
	  ELSE
	    z.tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	    EVAL z.keySet.table.put(name, z.tokenSym);
	  END;
          z.scanBufferSize := 0;
        END;
        IF NOT Text.Equal(text, z.tokenSym.name) THEN RETURN FALSE END;
        EVAL(GetToken(sc));
        RETURN TRUE;
     ELSE RETURN FALSE;
     END;
    END;
  END HaveTokenName;

PROCEDURE HaveTokenKey(sc: T; key: TEXT): BOOLEAN RAISES {NoReader, Fail} =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    WITH z = sc^ DO
      class := LookToken(sc);
      IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
        IF z.scanBufferSize # 0 THEN
	  name := Text.FromChars(SUBARRAY(z.scanBuffer^, 0, z.scanBufferSize));
	  IF z.keySet.table.get(name, (*VAR OUT*) value) THEN
	    z.tokenSym := NARROW(value, Symbol);
	  ELSE
	    z.tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	    EVAL z.keySet.table.put(name, z.tokenSym);
	  END;
          z.scanBufferSize := 0;
        END;
        IF NOT z.tokenSym.keyword THEN RETURN FALSE END;
        IF NOT Text.Equal(key, z.tokenSym.name) THEN RETURN FALSE END;
        EVAL(GetToken(sc));
        RETURN TRUE;
      ELSE RETURN FALSE;
      END;
    END;
  END HaveTokenKey;

PROCEDURE HaveTokenDelim(sc: T; delim: CHAR): BOOLEAN 
RAISES {NoReader, Fail} =
  BEGIN
    IF LookToken(sc) = TokenClass.DelimCase THEN
      IF delim # sc^.tokenDelim THEN RETURN FALSE END;
      EVAL(GetToken(sc));
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END HaveTokenDelim;

PROCEDURE SetChar(sc: T; n: CHAR; class: CharacterClass) =
  BEGIN sc^.charTable[n] := class; END SetChar;

PROCEDURE SetRange(sc: T; n: CHAR; m: CHAR; class: CharacterClass) =
  BEGIN
    FOR i := ORD(n) TO ORD(m) DO sc^.charTable[VAL(i, CHAR)] := class; END;
  END SetRange;

PROCEDURE TopLevel(sc: T): BOOLEAN =
  BEGIN RETURN Text.Empty(sc^.fileName); END TopLevel;

PROCEDURE SetPrompt(sc: T; newFirstPrompt, newNextPrompt: TEXT) =
  BEGIN
    sc^.firstPrompt := newFirstPrompt;
    sc^.nextPrompt := newNextPrompt;
    sc^.isFirstPrompt := TRUE;
  END SetPrompt;

PROCEDURE FirstPrompt(sc: T) = BEGIN sc^.isFirstPrompt := TRUE; END FirstPrompt;

PROCEDURE Clear(sc: T) =
  VAR ch: CHAR; class: TokenClass;
  BEGIN
    IF TopLevel(sc) THEN FlushInput(sc) END;
    TRY
      IF sc^.lookAheadReady THEN ch:=GetChar(sc) END; (* NOWARN *)
    EXCEPT SynScan.NoReader => END;
    TRY
      IF sc^.tokenReady THEN class:=GetToken(sc) END; (* NOWARN *)
    EXCEPT SynScan.NoReader, SynScan.Fail => END;
    sc^.scanBufferSize := 0;
  END Clear;

PROCEDURE Reset(sc: T) =
  BEGIN
    Clear(sc);
    WHILE (sc^.input#NIL) AND NOT(Text.Empty(sc^.input^.fileName)) DO 
      TRY
        PopInput(sc); (* NOWARN *)
      EXCEPT SynScan.NoReader => END;
    END;
  END Reset;

PROCEDURE GetKeywordName(key : Keyword): TEXT =
BEGIN
 RETURN key.name ;
END GetKeywordName;

PROCEDURE GetWriter(sc: T): SynWr.T =
BEGIN
  RETURN sc.swr;
END GetWriter;

PROCEDURE New(swr: SynWr.T): T =
  VAR sc: T;
  BEGIN
    sc := NEW(T);
    
    sc.swr := swr;
    
    sc^.scanPoint := 0;
    sc^.scanBuffer := NEW(REF ARRAY OF CHAR, 256);

    sc^.keySet := NewKeywordSet();

    sc^.lookAheadReady := FALSE;
    sc^.tokenReady := FALSE;
    sc^.scanBufferSize := 0;
    sc^.input := NIL;

    SetPrompt(sc, "", "");

    SetRange(sc, VAL(9, CHAR), VAL(10, CHAR), CharacterClass.BlankCharCase);
    SetRange(sc, VAL(12, CHAR), VAL(13, CHAR), CharacterClass.BlankCharCase);
    SetChar(sc, ' ', CharacterClass.BlankCharCase);
    SetChar(sc, '!', CharacterClass.DelimCharCase);
    SetChar(sc, '\"', CharacterClass.ReservedCharCase);
    SetRange(sc, '#', '&', CharacterClass.SpecialCharCase);
    SetChar(sc, '\'', CharacterClass.ReservedCharCase);
    SetRange(sc, '(', ')', CharacterClass.DelimCharCase);
    SetRange(sc, '*', '+', CharacterClass.SpecialCharCase);
    SetChar(sc, ',', CharacterClass.DelimCharCase);
    SetChar(sc, '-', CharacterClass.SpecialCharCase);
    SetChar(sc, '.', CharacterClass.DelimCharCase);
    SetChar(sc, '/', CharacterClass.SpecialCharCase);
    SetRange(sc, '0', '9', CharacterClass.DigitCharCase);
    SetChar(sc, ':', CharacterClass.SpecialCharCase);
    SetChar(sc, ';', CharacterClass.DelimCharCase);
    SetChar(sc, '<', CharacterClass.SpecialCharCase);
    SetChar(sc, '=', CharacterClass.SpecialCharCase);
    SetChar(sc, '>', CharacterClass.SpecialCharCase);
    SetChar(sc, '?', CharacterClass.DelimCharCase);
    SetChar(sc, '@', CharacterClass.SpecialCharCase);
    SetRange(sc, 'A', 'Z', CharacterClass.LetterCharCase);
    SetChar(sc, '[', CharacterClass.DelimCharCase);
    SetChar(sc, '\\', CharacterClass.SpecialCharCase);
    SetChar(sc, ']', CharacterClass.DelimCharCase);
    SetChar(sc, '^', CharacterClass.SpecialCharCase);
    SetChar(sc, '_', CharacterClass.DelimCharCase);
    SetChar(sc, '`', CharacterClass.LetterCharCase);
    SetRange(sc, 'a', 'z', CharacterClass.LetterCharCase);
    SetChar(sc, '{', CharacterClass.DelimCharCase);
    SetChar(sc, '|', CharacterClass.DelimCharCase);
    SetChar(sc, '}', CharacterClass.DelimCharCase);
    SetChar(sc, '~', CharacterClass.ReservedCharCase);
    SetChar(sc, EofChar, CharacterClass.EofCase);

    RETURN sc;

  END New;


PROCEDURE Setup() =
  BEGIN
  END Setup;

BEGIN
END SynScan.




(* An old version of ScanNumber that does not depend on Lex.

PROCEDURE ScanNat(sc: T): CARDINAL RAISES {NoReader, Fail} =
  VAR nat: INTEGER;
  BEGIN
    nat := 0;
    WHILE sc^.charTable[LookChar(sc)] = CharacterClass.DigitCharCase DO
      nat := (10 * nat) + (ORD(GetChar(sc)) - ORD('0'));
      IF (nat < 0) OR (nat > LAST(CARDINAL)) THEN
        SyntaxMsg(sc, "Integer constant is too big", "");
        RAISE Fail;
      END;
    END;
    RETURN nat;
  END ScanNat;

PROCEDURE ScanNeg(sc: T): INTEGER RAISES {NoReader, Fail} =
  VAR neg: INTEGER;
  BEGIN
    neg := 0;
    WHILE sc^.charTable[LookChar(sc)] = CharacterClass.DigitCharCase DO
      neg := (10 * neg) - (ORD(GetChar(sc)) - ORD('0'));
      IF neg > 0 THEN
        SyntaxMsg(sc, "Integer constant is too big", "");
        RAISE Fail;
      END;
    END;
    RETURN neg;
  END ScanNeg;

PROCEDURE ScanInt(): INTEGER RAISES {NoReader, Fail} =
  BEGIN
    IF HaveChar('~') THEN RETURN ScanNeg() ELSE RETURN ScanNat() END;
  END ScanInt;

PROCEDURE ScanNumber(sc: T): TokenClass RAISES {NoReader, Fail} =
  VAR negative: BOOLEAN; int: INTEGER; real, quot: LONGREAL; ch: CHAR;
  BEGIN
    WITH z = sc^ DO
      negative := HaveChar(sc, '~');
      IF negative THEN int := ScanNeg(sc) ELSE int := ScanNat(sc) END;
      IF LookChar(sc) = '.' THEN
        ch := GetChar(sc);
        real := FLOAT(int, LONGREAL);
        quot := 1.0d0;
        WHILE z.charTable[LookChar(sc)] = CharacterClass.DigitCharCase DO
          quot := quot * 10.0d0;
          IF negative THEN
            real := real - 
              (FLOAT((ORD(GetChar(sc)) - ORD('0')), LONGREAL) / quot);
          ELSE
            real := real + 
              (FLOAT((ORD(GetChar(sc)) - ORD('0')), LONGREAL) / quot);
          END;
        END;
        z.tokenReal := real;
        RETURN TokenClass.RealCase;
      ELSE
        z.tokenInt := int;
        RETURN TokenClass.IntCase
      END;
    END;
  END ScanNumber;

*)
