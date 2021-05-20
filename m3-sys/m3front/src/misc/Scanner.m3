(* Copyright (C) 1992, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)

(* portions Copyright 1996-2000, Critical Mass, Inc.             *)

UNSAFE MODULE Scanner;

IMPORT Text, Fmt, File, OSError, TextIntTbl, Word;
IMPORT M3, M3ID, Error, M3String, M3WString, Token;
IMPORT Target, TInt, TWord, TFloat, Host, M3Buf;
IMPORT CharSeq, CharSeqRep;

CONST
  MaxStack  = 40;
  MaxLines  = 100000;
  MaxString = 4095;
  MaxBuffer = 4096;
  UndoPad   = 4;
  EOFChar   = '\000';
  MaxRsrvd  = 250;  (* => only the first few ids may be reserved *)

TYPE
  (* CharSet = SET OF CHAR; *)
  TK = Token.T;
  IDList = UNTRACED REF RECORD id: M3ID.T;  next: IDList END;
  CharMap = ARRAY CHAR OF BOOLEAN;

VAR (* CONST *)
  WhiteSpace    : CharMap := CharMap { FALSE, .. };
  AlphaNumerics : CharMap := CharMap { FALSE, .. };
  Digits        : CharMap := CharMap { FALSE, .. };
  OctalDigits   : CharMap := CharMap { FALSE, .. };
  HexDigits     : CharMap := CharMap { FALSE, .. };
  CommentAlert  : CharMap := CharMap { FALSE, .. };
  missing       : M3ID.T;
  LINE          : M3ID.T;
  NOWARN        : M3ID.T;
  PRAGMA        : M3ID.T;

TYPE
  InputBufferIndex = [-UndoPad .. MaxBuffer+1];
  InputBuffer = REF ARRAY InputBufferIndex OF File.Byte;
  (* Note: to avoid a range check in GetCh (and ScanComment), we don't
     use "MaxBuffer-1" as the upper bound of the input buffer. *)

TYPE
  StringBufferIndex =  [0..MaxString];
  StringBuffer =  ARRAY StringBufferIndex OF CHAR;

TYPE
  FileState = RECORD
    ch      : CHAR;
    offs    : INTEGER;  (* fileno * MaxLines + lineno *)
    input   : File.T;
    buf     : InputBuffer;
    buf_ptr : InputBufferIndex;
    buf_len : INTEGER;
    sym     : Symbol;
    ignore  : IDList;  (* pragmas to ignore *)
    accepted: INTEGER;
    is_main : BOOLEAN;
  END;

TYPE
  FileNames = REF ARRAY OF TEXT;

VAR (* never reset *)
  nFiles      : INTEGER;
  file_map    := NEW (TextIntTbl.Default).init ();
  files       := NEW (FileNames, 200);
  local_files := NEW (FileNames, 200);
  reserved    : ARRAY [0..MaxRsrvd] OF M3.Value;

VAR (* explicitly reset *)
  input       : File.T;
  input_buf   : InputBuffer;
  input_ptr   : InputBufferIndex;
  input_len   : INTEGER;
  ch          : CHAR;
  ignore      : IDList;
  accepted    : INTEGER := 2;
  tos         : INTEGER;
  stack       : ARRAY [0..MaxStack] OF FileState;
  buf         : StringBuffer;

PROCEDURE Initialize () =
  BEGIN
    missing    := M3ID.Add ("<missing id>");
    LINE       := M3ID.Add ("LINE");
    NOWARN     := M3ID.Add ("NOWARN");
    PRAGMA     := M3ID.Add ("PRAGMA");

    nFiles     := 0;
    tos        := 0;

    WhiteSpace [' ']  := TRUE;
    WhiteSpace ['\n'] := TRUE;
    WhiteSpace ['\t'] := TRUE;
    WhiteSpace ['\r'] := TRUE;
    WhiteSpace ['\f'] := TRUE;

    AlphaNumerics ['_'] := TRUE;
    FOR c := 'a' TO 'z' DO AlphaNumerics [c] := TRUE END;
    FOR c := 'A' TO 'Z' DO AlphaNumerics [c] := TRUE END;
    FOR c := '0' TO '9' DO AlphaNumerics [c] := TRUE END;

    FOR c := '0' TO '9' DO Digits [c] := TRUE END;

    FOR c := '0' TO '7' DO OctalDigits [c] := TRUE END;

    FOR c := '0' TO '9' DO HexDigits [c] := TRUE END;
    FOR c := 'a' TO 'f' DO HexDigits [c] := TRUE END;
    FOR c := 'A' TO 'F' DO HexDigits [c] := TRUE END;

    CommentAlert ['*'] := TRUE;
    CommentAlert ['('] := TRUE;
    CommentAlert [EOFChar] := TRUE;
    CommentAlert ['\n'] := TRUE;
    CommentAlert ['@'] := TRUE;
  END Initialize;

PROCEDURE Reset () =
  BEGIN
    WHILE (tos > 0) DO
      Pop ();
      Host.CloseFile (input);
    END;
    input_buf  := NIL;
    input_len  := -1; (* not 0 ==> EOF *)
    input_ptr  := 0;
    ch         := ' ';
    ignore     := NIL;
    accepted   := 2;

    (* interface variables *)
    offset     := 0;
    nLines     := 0;
    nPushed    := 0;
    cur.token  := TK.tEOF;
  END Reset;

PROCEDURE Push (name: TEXT;  file: File.T;  is_main: BOOLEAN) =
  BEGIN
    INC (nPushed);
    WITH z = stack[tos] DO
      z.ch      := ch;
      z.offs    := offset;
      z.sym     := cur;
      z.input   := input;
      z.buf     := input_buf;
      z.buf_len := input_len;
      z.buf_ptr := input_ptr;
      z.ignore  := ignore;
      z.accepted:= accepted;
      z.is_main := in_main;
    END;
    INC (tos);
    in_main   := is_main;
    offset    := FileNumber (name) * MaxLines + 1;
    ch        := ' ';
    ignore    := NIL;
    accepted  := 2;
    input     := file;
    input_ptr := 0;
    input_len := -1;  (* not 0 ==> EOF *)
    input_buf := stack[tos].buf;
    IF (input_buf = NIL) THEN
      input_buf := NEW (InputBuffer);
      stack[tos].buf := input_buf;
    END;
    IF (file # NIL) THEN GetToken (); (* prime the input stream *) END;
  END Push;

PROCEDURE Pop () =
  BEGIN
    DEC (tos);
    WITH z = stack[tos] DO
      ch        := z.ch;
      offset    := z.offs;
      cur       := z.sym;
      input     := z.input;
      input_buf := z.buf;
      input_ptr := z.buf_ptr;
      input_len := z.buf_len;
      ignore    := z.ignore;
      accepted  := z.accepted;
      in_main   := z.is_main;
    END;
  END Pop;

PROCEDURE FileNumber (filename: TEXT): INTEGER =
   (* returns index into files of filename, adding it if it doesn't exist *)
  VAR index := offset DIV MaxLines;
  BEGIN
    (* often we'll hit the current file *)
    IF (index < NUMBER (files^))
      AND (files[index] # NIL)
      AND Text.Equal (files[index], filename) THEN
      RETURN index;
    END;

    IF file_map.get (filename, index) THEN RETURN index; END;

    (* add a new one *)
    IF (nFiles >= NUMBER (files^)) THEN ExpandFiles(); END;
    EVAL file_map.put (filename, nFiles);
    files[nFiles] := filename;
    local_files[nFiles] := NIL;
    INC(nFiles);
    RETURN nFiles-1;
  END FileNumber;

PROCEDURE ExpandFiles () =
  VAR
    n := NUMBER (files^);
    new_global := NEW (FileNames, n + n);
    new_local  := NEW (FileNames, n + n);
  BEGIN
    SUBARRAY (new_global^, 0, n) := files^;
    SUBARRAY (new_local^, 0, n)  := local_files^;
    files       := new_global;
    local_files := new_local;
  END ExpandFiles;

PROCEDURE Here (VAR file: TEXT;  VAR line: INTEGER) =
  BEGIN
    file := files [offset DIV MaxLines];
    line := offset MOD MaxLines;
  END Here;

PROCEDURE LocalHere (VAR file: TEXT;  VAR line: INTEGER) =
  VAR fnum := offset DIV MaxLines;
  BEGIN
    IF (local_files[fnum] = NIL) THEN
      local_files[fnum] := files[fnum];
    END;
    file := local_files [fnum];
    line := offset MOD MaxLines;
  END LocalHere;

PROCEDURE SameFile (a, b: INTEGER): BOOLEAN =
  BEGIN
    RETURN (a DIV MaxLines) = (b DIV MaxLines);
  END SameFile;

PROCEDURE Match (t: Token.T) =
  BEGIN
    IF (cur.token = t) THEN
      GetToken ();
    ELSE
      DoFail ("missing \'" & M3ID.ToText (Token.name [t]) & "\'", t);
      IF (cur.token = t) THEN GetToken () END;
    END;
  END Match;

PROCEDURE MatchID (): M3ID.T =
  VAR id: M3ID.T;
  BEGIN
    IF (cur.token = TK.tIDENT) THEN
      id := cur.id;
      GetToken ();
    ELSE
      DoFail ("missing identifier", TK.tIDENT);
      IF (cur.token = TK.tIDENT)
        THEN  id := cur.id;  GetToken ();
        ELSE  id := missing;
      END;
    END;
    RETURN id;
  END MatchID;

PROCEDURE Fail (msg: TEXT) =
  BEGIN
    DoFail (msg, TK.tEOF);
  END Fail;

PROCEDURE DoFail (msg: TEXT;  stop: TK) =
  VAR t: TEXT;  i: INTEGER;
  BEGIN
    IF (accepted > 1) THEN
      t := "syntax error: " & msg;
      CASE cur.token OF
      | TK.tIDENT =>
          Error.ID (cur.id, t);
      | TK.tTEXTCONST =>
          Error.Txt ("\"" & M3String.ToText (cur.str) & "\"", t);
      | TK.tWTEXTCONST =>
          Error.Txt (M3WString.ToLiteral (cur.wstr), t);
      | TK.tREALCONST, TK.tLONGREALCONST, TK.tEXTENDEDCONST =>
          Error.Txt ("<float>", t);
      | TK.tCARDCONST, TK.tLONGCARDCONST =>
          IF TInt.ToInt (cur.int, i)
            THEN Error.Int (i, t);
            ELSE Error.Txt ("<integer>", t);
          END;
      | TK.tCHARCONST =>
          IF TInt.ToInt (cur.int, i)
            THEN Error.Txt (CharLiteral (i), t);
            ELSE Error.Txt ("<char>", t);
          END;
      | TK.tWCHARCONST =>
          IF TInt.ToInt (cur.int, i)
            THEN Error.Txt (CharLiteral (i), t);
            ELSE Error.Txt ("<wide-char>", t);
          END;
      ELSE (* no extra info *)
          Error.Msg (t);
      END;
    END;
    (* skip forward to a major token... *)
    WHILE (cur.token # stop) AND NOT (cur.token IN Restart) DO
      GetToken ();
    END;
    accepted := 0;
    IF (cur.token = stop) THEN accepted := 1; END;
  END DoFail;

PROCEDURE CharLiteral (c: INTEGER): TEXT =
  VAR lit: TEXT := "";
  BEGIN
    IF    (c = ORD ('\n')) THEN  lit := "'\\n' = ";
    ELSIF (c = ORD ('\t')) THEN  lit := "'\\t' = ";
    ELSIF (c = ORD ('\r')) THEN  lit := "'\\r' = ";
    ELSIF (c = ORD ('\f')) THEN  lit := "'\\f' = ";
    ELSIF (c = ORD ('\\')) THEN  lit := "'\\\\' = ";
    ELSIF (c = ORD ('\'')) THEN  lit := "'\\\'' = ";
    ELSIF (c = ORD ('\"')) THEN  lit := "'\\\"' = ";
    ELSIF (ORD (' ') <= c) AND (c <= ORD ('~')) THEN
      lit := "'" & Text.FromChar (VAL (c, CHAR)) & "' = ";
    END;
    RETURN lit & "16_" & Fmt.Int (c, 16);
  END CharLiteral;

CONST
  Restart = Token.Set {
    TK.tEOF, TK.tSEMI, TK.tINLINE, TK.tEXTERNAL, TK.tASSERT,
    TK.tUNUSED, TK.tOBSOLETE, TK.tTRACE, TK.tCALLCONV,
    TK.tFATAL,  TK.tIMPLICIT, TK.tDEBUG, TK.tBEGIN, TK.tCASE, TK.tCONST,
    TK.tELSE, TK.tELSIF, TK.tEVAL, TK.tEXCEPT, TK.tEXCEPTION,
    TK.tEXIT, TK.tEXPORTS, TK.tFINALLY, TK.tFOR, TK.tFROM,
    TK.tGENERIC, TK.tIF, TK.tIMPORT, TK.tINTERFACE,
    TK.tLOCK, TK.tLOOP, TK.tMODULE, TK.tPROCEDURE,
    TK.tRAISE, TK.tREADONLY, TK.tREPEAT, TK.tRETURN, TK.tREVEAL,
    TK.tTHEN, TK.tTRY, TK.tTYPE, TK.tTYPECASE, TK.tUNSAFE, TK.tUNTIL,
    TK.tVALUE, TK.tVAR, TK.tWHILE, TK.tWITH };

PROCEDURE NoteReserved (name: M3ID.T;  value: M3.Value) =
  BEGIN
    <* ASSERT M3ID.GetClass (name) = 0 *>
    <* ASSERT reserved[name] = NIL *>
    reserved [name] := value;
  END NoteReserved;

<*INLINE*> PROCEDURE GetCh () =
  <*FATAL OSError.E*>
  BEGIN
    LOOP
      IF (input_ptr < input_len) THEN
        ch := VAL (input_buf[input_ptr], CHAR);

        IF capturingText THEN
          capturedText.addhi(ch);
        END;

        INC (input_ptr);
        RETURN;
      ELSIF (input_len = 0) THEN
        ch := EOFChar;
        RETURN;
      ELSE
        input_len := input.read (SUBARRAY (input_buf^, UndoPad, MaxBuffer));
        input_ptr := 0;
        input_buf[input_len] := ORD('@'); (* => in CommentAlert *)
        (* loop around and try again *)
      END;
    END;
  END GetCh;

PROCEDURE GetToken () =
  VAR len: StringBufferIndex;
  BEGIN
    INC (accepted);
    LOOP
      (* skip white space *)
      WHILE (WhiteSpace[ch]) DO
        IF (ch = '\n') THEN INC (offset);  INC (nLines)  END;
        GetCh ();
      END;
      (* remember where this token starts *)
      cur.offset := offset;

      CASE ch OF

      | 'a'..'z', 'A'..'Z' =>
          (* scan an identifier *)
          len := 0;
          IF (ch = 'w') OR (ch = 'W') THEN
            (* check for a wide-char or wide-text literal *)
            buf [len] := ch;  INC (len);
            GetCh ();
            IF    (ch = '\'') THEN  ScanChar (wide := TRUE);  RETURN;
            ELSIF (ch = '\"') THEN  ScanWideText ();          RETURN;
            END;
          END;
          WHILE (AlphaNumerics[ch]) DO
            buf [len] := ch;  INC (len);
            GetCh ();
          END;
          cur.id    := M3ID.FromStr (buf, len);
          cur.token := TK.tIDENT;
          cur.defn  := NIL;
          VAR i := M3ID.GetClass (cur.id); BEGIN
            IF (ORD (Token.First_Keyword) <= i)
              AND (i <= ORD (Token.Last_Keyword)) THEN
              cur.token := VAL (i, TK);
            END;
          END;
          IF (cur.id <= LAST(reserved)) THEN
            cur.defn := reserved[cur.id];
          END;
          RETURN;

      | '0'..'9' => ScanNumber ();                          RETURN;
      | '\''     => ScanChar (wide := FALSE);               RETURN;
      | '\"'     => ScanText ();                            RETURN;
      | '+'      => cur.token := TK.tPLUS;       GetCh ();  RETURN;
      | '-'      => cur.token := TK.tMINUS;      GetCh ();  RETURN;
      | '/'      => cur.token := TK.tSLASH;      GetCh ();  RETURN;
      | '&'      => cur.token := TK.tAMPERSAND;  GetCh ();  RETURN;
      | ','      => cur.token := TK.tCOMMA;      GetCh ();  RETURN;
      | ';'      => cur.token := TK.tSEMI;       GetCh ();  RETURN;
      | '['      => cur.token := TK.tLBRACKET;   GetCh ();  RETURN;
      | '{'      => cur.token := TK.tLBRACE;     GetCh ();  RETURN;
      | '^'      => cur.token := TK.tARROW;      GetCh ();  RETURN;
      | '#'      => cur.token := TK.tSHARP;      GetCh ();  RETURN;
      | ')'      => cur.token := TK.tRPAREN;     GetCh ();  RETURN;
      | ']'      => cur.token := TK.tRBRACKET;   GetCh ();  RETURN;
      | '}'      => cur.token := TK.tRBRACE;     GetCh ();  RETURN;
      | '|'      => cur.token := TK.tBAR;        GetCh ();  RETURN;
      | EOFChar  => cur.token := TK.tEOF;                   RETURN;

      | '*' => (* '*>' '*' *)
            GetCh ();
            IF (ch = '>')
              THEN  cur.token := TK.tENDPRAGMA;  GetCh ();
              ELSE  cur.token := TK.tASTERISK;
            END;
            RETURN;
      | '=' => (*  '='  '=>'  *)
            GetCh ();
            IF (ch = '>')
              THEN  cur.token := TK.tIMPLIES;  GetCh ();
              ELSE  cur.token := TK.tEQUAL;
            END;
            RETURN;
      | ':' => (*  ':'  ':='  *)
            GetCh ();
            IF (ch = '=')
              THEN  cur.token := TK.tASSIGN;  GetCh ();
              ELSE  cur.token := TK.tCOLON;
            END;
            RETURN;
      | '.' => (*  '.'  '..'  *)
            GetCh ();
            IF (ch = '.')
              THEN  cur.token := TK.tDOTDOT;  GetCh ();
              ELSE  cur.token := TK.tDOT;
            END;
            RETURN;
      | '(' => (*  '('*'  '('  *)
            GetCh ();
            IF (ch = '*')
              THEN  ScanComment ();
              ELSE  cur.token := TK.tLPAREN;  RETURN;
            END;
      | '>' => (*  '>'  '>='  *)
            GetCh ();
            IF (ch = '=')
              THEN  cur.token := TK.tGREQUAL;  GetCh ();
              ELSE  cur.token := TK.tGREATER;
            END;
            RETURN;
      | '<' => (*  '<'  '<='  '<:'  '<*' *)
            GetCh ();
            IF    (ch = '=') THEN  cur.token := TK.tLSEQUAL;  GetCh ();
            ELSIF (ch = ':') THEN  cur.token := TK.tSUBTYPE;  GetCh ();
            ELSIF (ch = '*') THEN  ScanPragma ();
            ELSE                   cur.token := TK.tLESS;
            END;
            RETURN;

      ELSE
        Error.Int (ORD (ch), "Illegal character");
        GetCh ();

      END; (*case*)
    END; (*loop*)
  END GetToken; 

PROCEDURE HexDigitValue ( ch : CHAR ) : INTEGER =  
  BEGIN 
    IF ORD ( '0' ) <= ORD ( ch ) AND ORD ( ch ) <= ORD ( '9' ) 
    THEN RETURN ORD ( ch ) - ORD ( '0' ) 
    ELSIF ORD ( 'A' ) <= ORD ( ch ) AND ORD ( ch ) <= ORD ( 'F' ) 
    THEN RETURN ORD ( ch ) - ORD ( 'A' ) + 10  
    ELSIF ORD ( 'a' ) <= ORD ( ch ) AND ORD ( ch ) <= ORD ( 'f' ) 
    THEN RETURN ORD ( ch ) - ORD ( 'a' ) + 10  
    ELSE RETURN FIRST ( INTEGER ) (* Shouldn't happen. *) 
    END; 
  END HexDigitValue; 

PROCEDURE ScanNumber () =
  VAR
    base: INTEGER;
    val: Target.Int;
    pre: Target.Precision;
    len: StringBufferIndex;
  BEGIN
    (* scan the decimal digits *)
    len := 0;
    WHILE (Digits[ch]) DO
      buf[len] := ch;  INC (len);
      GetCh ();
    END;

    IF (ch = '_') THEN
      (* scan a based integer *)
      IF    NOT TInt.New (SUBARRAY (buf, 0, len), val)
         OR NOT TInt.ToInt (val, base)
         OR (base < 2)
         OR (16 < base) THEN
        Error.Int (base, "illegal base for based literal, 10 used");
        base := 10;
      END;
      len := 0;
      LOOP
        GetCh ();
        IF NOT (HexDigits[ch]) THEN EXIT END;
        IF HexDigitValue (ch) >= base 
        THEN
          Error.Msg ("digit in based literal with value not less than base.");
        END; 
        buf [len] := ch;  INC (len);
      END;
      IF (ch = 'l') OR (ch = 'L') THEN
        GetCh (); (* eat the precision character *)
        IF (len = 0)
          OR NOT (TWord.New (SUBARRAY (buf, 0, len), base, val))
          OR TWord.LT (Target.Long.max, val) THEN
          Error.Msg ("illegal based LONGINT literal, zero used");
          val := TInt.Zero;
        END;
        cur.token := TK.tLONGCARDCONST;
        EVAL TInt.Extend (val, Target.Longint.bytes, cur.int);
      ELSE
        IF (len = 0)
          OR NOT TWord.New (SUBARRAY (buf, 0, len), base, val)
          OR TWord.LT (Target.Word.max, val) THEN
          Error.Msg ("illegal based INTEGER literal, zero used");
          val := TInt.Zero;
        END;
        cur.token := TK.tCARDCONST;
        EVAL TInt.Extend (val, Target.Integer.bytes, cur.int);
      END;

    ELSIF (ch = '.') THEN
      (* scan a floating point number *)
      buf[len] := '.';  INC (len);
      GetCh (); (* eat the '.' *)
      IF (ch = '.') THEN
        (* we saw  "dddd.." *)

        (*****  Rd.UnGetChar (input);  *****)
        DEC (input_ptr);  input_buf[input_ptr] := ORD ('.');

        IF NOT TInt.New (SUBARRAY (buf, 0, len-1), val)
          OR TInt.LT (val, Target.Integer.min)
          OR TInt.LT (Target.Integer.max, val) THEN
          Error.Msg ("illegal INTEGER literal, zero used");
          val := TInt.Zero;
        END;
        cur.token := TK.tCARDCONST;
        cur.int   := val;
        RETURN;
      END;

      (* scan the fractional digits *)
      IF NOT (Digits[ch]) THEN
        Error.Msg ("missing digits in real fraction");
        buf[len] := '0';  INC (len);
      END;
      WHILE (Digits[ch]) DO  buf[len] := ch; INC (len); GetCh ()  END;

      (* check for the exponent *)
      IF (ch = 'e') OR (ch = 'E') THEN
        buf[len] := 'e';  INC (len);
        cur.token := TK.tREALCONST;
        pre := Target.Precision.Short;
      ELSIF (ch = 'd') OR (ch = 'D') THEN
        buf[len] := 'e';  INC (len);
        cur.token := TK.tLONGREALCONST;
        pre := Target.Precision.Long;
      ELSIF (ch = 'x') OR (ch = 'X') THEN
        buf[len] := 'e';  INC (len);
        cur.token := TK.tEXTENDEDCONST;
        pre := Target.Precision.Extended;
      ELSE (* real constant with no exponent *)
        IF NOT TFloat.New (SUBARRAY (buf, 0, len), pre, cur.float) THEN
          Error.Msg ("illegal floating-point literal");
        END;
        cur.token := TK.tREALCONST;
        pre := Target.Precision.Short;
        RETURN;
      END;
      GetCh (); (* eat the exponent entry char *)

      (* get the exponent sign *)
      IF (ch = '+') THEN
        buf[len] := '+';  INC (len);
        GetCh ();
      ELSIF (ch = '-') THEN
        buf[len] := '-';  INC (len);
        GetCh ();
      ELSE
        buf[len] := '+';
      END;

      (* finally, get the exponent digits *)
      IF NOT (Digits[ch]) THEN
        Error.Msg ("missing digits in real exponent");
        buf[len] := '0';  INC (len);
      END;
      WHILE (Digits[ch]) DO  buf[len] := ch; INC (len); GetCh ();  END;

      IF NOT TFloat.New (SUBARRAY (buf, 0, len), pre, cur.float) THEN
        Error.Msg ("illegal floating-point literal");
      END;

    ELSE
      (* already scanned a decimal integer *)
      IF (ch = 'l') OR (ch = 'L') THEN
        GetCh (); (* eat the precision character *)
        IF NOT TInt.New (SUBARRAY (buf, 0, len), val)
          OR TInt.LT (val, Target.Longint.min)
          OR TInt.LT (Target.Longint.max, val) THEN
          Error.Msg ("illegal LONGINT literal, zero used");
          val := TInt.Zero;
        END;
        cur.token := TK.tLONGCARDCONST;
        cur.int   := val;
      ELSE
        IF NOT TInt.New (SUBARRAY (buf, 0, len), val)
          OR TInt.LT (val, Target.Integer.min)
          OR TInt.LT (Target.Integer.max, val) THEN
          Error.Msg ("illegal INTEGER literal, zero used");
          val := TInt.Zero;
        END;
        cur.token := TK.tCARDCONST;
        cur.int   := val;
      END;
    END;

  END ScanNumber;

PROCEDURE ScanChar (wide: BOOLEAN) =
  CONST Tok = ARRAY BOOLEAN OF TK { TK.tCHARCONST, TK.tWCHARCONST };
  CONST Bytes = ARRAY BOOLEAN OF [1..3] {1, 2 };
  VAR val := 0;
  BEGIN
    cur.token := Tok[wide];
    cur.int   := TInt.Zero;
    GetCh ();
    IF (ch = '\'') THEN
      Error.Msg ("missing character in character literal");
      GetCh ();
      RETURN;
    ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') THEN
      Error.Msg ("end-of-line encountered in character literal");
      RETURN;
    ELSIF (ch = '\\') THEN
      GetCh ();
      IF    (ch = 'n')  THEN  val := ORD ('\n');   GetCh ();
      ELSIF (ch = 't')  THEN  val := ORD ('\t');   GetCh ();
      ELSIF (ch = 'r')  THEN  val := ORD ('\r');   GetCh ();
      ELSIF (ch = 'f')  THEN  val := ORD ('\f');   GetCh ();
      ELSIF (ch = '\\') THEN  val := ORD ('\\');   GetCh ();
      ELSIF (ch = '\'') THEN  val := ORD ('\'');   GetCh ();
      ELSIF (ch = '\"') THEN  val := ORD ('\"');   GetCh ();
      ELSIF (ch = 'x') OR (ch = 'X')  
      THEN  
        GetCh ();  
        val := GetHexChar (bytes:=Bytes[wide]);
      ELSIF (OctalDigits[ch]) THEN  val := GetOctalChar (wide);
      ELSIF wide AND ch = 'U' 
      THEN 
        GetCh ();  
        val := GetHexChar (bytes:=3); 
      ELSE  Error.Msg ("unknown escape sequence in character literal");
      END;
    ELSIF (ch = EOFChar) THEN
      Error.Msg ("EOF encountered in character literal");
      RETURN ;
    ELSE (* a simple character literal *)
      val := ORD (ch);
      GetCh ();
    END;
    IF (ch # '\'')
      THEN Error.Msg ("missing closing quote on character literal");
      ELSE GetCh ();
    END;
    IF NOT TInt.FromInt (val, cur.int) THEN
      Error.Msg ("illegal character literal");
    END;
  END ScanChar;

PROCEDURE ScanText () =
  VAR i: INTEGER;  mbuf: M3Buf.T := NIL;
  PROCEDURE Stuff (c: CHAR) =
    BEGIN
      IF (i < NUMBER (buf)) THEN
        buf [i] := c;  INC (i);
      ELSIF (i = NUMBER (buf)) THEN
        mbuf := M3Buf.New ();
        M3Buf.PutSub (mbuf, buf);
        M3Buf.PutChar (mbuf, c);
        INC (i);
      ELSE
        M3Buf.PutChar (mbuf, c);
        INC (i);
      END;
    END Stuff;
  BEGIN
    i := 0;
    cur.token := TK.tTEXTCONST;
    GetCh ();
    LOOP
      IF (ch = '\"') THEN
        GetCh ();
        EXIT;
      ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') THEN
        Error.Msg ("end-of-line encountered in text literal");
        EXIT;
      ELSIF (ch = '\\') THEN
        GetCh ();
        IF    (ch = 'n') THEN  Stuff ('\n');  GetCh ();
        ELSIF (ch = 't') THEN  Stuff ('\t');  GetCh ();
        ELSIF (ch = 'r') THEN  Stuff ('\r');  GetCh ();
        ELSIF (ch = 'f') THEN  Stuff ('\f');  GetCh ();
        ELSIF (ch = '\\') THEN Stuff ('\\');  GetCh ();
        ELSIF (ch = '\'') THEN Stuff ('\'');  GetCh ();
        ELSIF (ch = '\"') THEN Stuff ('\"');  GetCh ();
        ELSIF (ch = 'x')
           OR (ch = 'X') 
        THEN GetCh ();  Stuff (VAL (GetHexChar (bytes:=1), CHAR));
        ELSIF (OctalDigits[ch]) 
        THEN Stuff (VAL (GetOctalChar (wide:=FALSE), CHAR));
        ELSE  Error.Msg ("unknown escape sequence in text literal");
        END;
      ELSIF (ch = EOFChar) THEN
        Error.Msg ("EOF encountered in text literal");
        EXIT;
      ELSE (* a simple character *)
        Stuff (ch);
        GetCh ();
      END;
    END;

    IF (mbuf = NIL)
      THEN cur.str := M3String.FromStr (buf, i);
      ELSE cur.str := M3String.Add (M3Buf.ToText (mbuf));
    END;
  END ScanText;

PROCEDURE ScanWideText () =
  VAR
    res: M3WString.T := NIL;
    i: INTEGER := 0;
    wbuf: ARRAY [0..127] OF M3WString.Char;
  PROCEDURE Stuff (c: INTEGER) =
    BEGIN
      c := Word.And (c, 16_1fffff);
      IF (i = NUMBER (wbuf)) THEN
        IF (res = NIL)
          THEN res := M3WString.FromStr (wbuf);
          ELSE res := M3WString.Concat (res, M3WString.FromStr (wbuf));
        END;
        i := 0;
      END;
      wbuf[i] := c;  INC (i);
    END Stuff;
  BEGIN
    cur.token := TK.tWTEXTCONST;
    GetCh (); (* opening quote *)
    LOOP
      IF (ch = '\"') THEN
        GetCh ();
        EXIT;
      ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') THEN
        Error.Msg ("end-of-line encountered in wide text literal");
        EXIT;
      ELSIF (ch = '\\') THEN
        GetCh ();
        IF    (ch = 'n') THEN  Stuff (ORD ('\n'));  GetCh ();
        ELSIF (ch = 't') THEN  Stuff (ORD ('\t'));  GetCh ();
        ELSIF (ch = 'r') THEN  Stuff (ORD ('\r'));  GetCh ();
        ELSIF (ch = 'f') THEN  Stuff (ORD ('\f'));  GetCh ();
        ELSIF (ch = '\\') THEN Stuff (ORD ('\\'));  GetCh ();
        ELSIF (ch = '\'') THEN Stuff (ORD ('\''));  GetCh ();
        ELSIF (ch = '\"') THEN Stuff (ORD ('\"'));  GetCh ();
        ELSIF (ch = 'x')
           OR (ch = 'X') THEN GetCh ();  Stuff (GetHexChar (bytes:=2));
        ELSIF (OctalDigits[ch]) THEN Stuff (GetOctalChar (wide := TRUE));
        ELSIF ch = 'U' 
        THEN GetCh ();  Stuff (GetHexChar (bytes:=3)); 
        ELSE  Error.Msg ("unknown escape sequence in wide text literal");
        END;
      ELSIF (ch = EOFChar) THEN
        Error.Msg ("EOF encountered in wide text literal");
        EXIT;
      ELSE (* a simple character *)
        Stuff (ORD (ch));
        GetCh ();
      END;
    END;
    IF (res = NIL)
      THEN cur.wstr := M3WString.FromStr (wbuf, i);
      ELSE cur.wstr := M3WString.Concat (res, M3WString.FromStr (wbuf, i));
    END;
  END ScanWideText;

PROCEDURE GetOctalChar (wide: BOOLEAN): INTEGER =
  VAR value: INTEGER := 0;
  BEGIN
    IF NOT GetOctalDigit (wide, value) THEN RETURN value; END;
    IF NOT GetOctalDigit (wide, value) THEN RETURN value; END;
    IF NOT GetOctalDigit (wide, value) THEN RETURN value; END;
    IF wide THEN
      IF NOT GetOctalDigit (wide, value) THEN RETURN value; END;
      IF NOT GetOctalDigit (wide, value) THEN RETURN value; END;
      IF NOT GetOctalDigit (wide, value) THEN RETURN value; END;
      IF value > Target.WideChar16Max THEN
        Error.Msg ("Octal escaped WIDECHAR value out of range");
        value := 0; 
      END; 
    ELSE 
      IF value > 16_FF THEN 
        Error.Msg ("Octal escaped CHAR value out of range");
        value := 0; 
      END; 
    END;
    RETURN value;
  END GetOctalChar;

PROCEDURE GetOctalDigit (wide: BOOLEAN;  VAR value: INTEGER): BOOLEAN =
  CONST Bad = ARRAY BOOLEAN OF TEXT {
     "octal character escape must have 3 digits",
     "wide octal character escape must have 6 digits" };
  BEGIN
    IF OctalDigits[ch] THEN
      value := value * 8 + ORD (ch) - ORD ('0');
      GetCh ();
      RETURN TRUE;
    ELSE
      Error.Msg (Bad[wide]);
      RETURN FALSE;
    END;
  END GetOctalDigit;

(* CONST ReplacementWt = UniCodec.ReplacementWt; *) 
(* We don't have UniCodec when compiling this with an earlier compiler. *) 
CONST ReplacementWt = 16_FFFD;   

PROCEDURE GetHexChar (bytes: [1..3]): INTEGER =
  VAR value: INTEGER := 0;
  BEGIN
    IF NOT GetHexDigit (bytes, value) THEN RETURN value; END;
    IF NOT GetHexDigit (bytes, value) THEN RETURN value; END;
    IF bytes > 1 
    THEN 
      IF NOT GetHexDigit (bytes, value) THEN RETURN value; END;
      IF NOT GetHexDigit (bytes, value) THEN RETURN value; END;
      IF bytes = 3  
      THEN 
        (* Only get here for a Unicode escape, which is always 6 hex digits. *) 
        IF NOT GetHexDigit (bytes, value) THEN RETURN value; END;
        IF NOT GetHexDigit (bytes, value) THEN RETURN value; END;
        IF value > Target.WideChar32Max THEN
          Error.Msg ("Unicode escaped character outside of Unicode range");
          value := 0; 
        ELSIF value > Target.WideCharMax()
        THEN 
          Error.Warn
            (2, "Character outside WIDECHAR range, replaced by Unicode replacement character."); 
          value := ReplacementWt; 
        END; 
      END; 
    END;
    RETURN value;
  END GetHexChar;

PROCEDURE GetHexDigit (bytes: [1..3];  VAR value: INTEGER): BOOLEAN =
  CONST Bad = ARRAY [1..3]OF TEXT {
     "hex character escape must have 2 digits",
     "wide hex character escape must have 4 digits",
     "Unicode character escape must have 6 digits" };
  VAR x: INTEGER;
  BEGIN
    IF  NOT (HexDigits[ch]) THEN
      Error.Msg (Bad[bytes]);  RETURN FALSE;
    ELSIF ('0' <= ch) AND (ch <= '9') THEN
      x := ORD (ch) - ORD ('0');
    ELSIF ('a' <= ch) AND (ch <= 'f') THEN
      x := ORD (ch) - ORD ('a') + 10;
    ELSIF ('A' <= ch) AND (ch <= 'F') THEN
      x := ORD (ch) - ORD ('A') + 10;
    END;
    value := value * 16 + x;
    GetCh ();
    RETURN TRUE;
  END GetHexDigit;

PROCEDURE ScanComment () =
  VAR nest, save: INTEGER; start: INTEGER;
  BEGIN
    start := cur.offset;
    GetCh ();
    nest := 1;
    WHILE (nest > 0) DO
      WHILE (NOT CommentAlert[ch]) DO
        (* INLINE GetCh (); *)
        ch := VAL (input_buf[input_ptr], CHAR);
        INC (input_ptr);
      END;
      IF (ch = '*') THEN
        GetCh ();  IF (ch = ')') THEN DEC (nest); GetCh ();  END;
      ELSIF (ch = '(') THEN
        GetCh ();  IF (ch = '*') THEN INC (nest); GetCh ();  END;
      ELSIF (ch = EOFChar) THEN
        save := offset;
        offset := start;
        Error.Msg ("EOF encountered in comment");
        offset := save;
        nest := 0;
      ELSIF (ch = '\n') THEN
        INC (offset);  INC (nLines);
        GetCh ();
      ELSE
        GetCh ();
      END;
    END;
  END ScanComment;

PROCEDURE ScanPragma () =
  VAR nest, save, start, i, lineno, fileno: INTEGER;  ss: IDList;
  BEGIN
    start := cur.offset;
    GetCh();  (* '*' *)

    (* skip white space *)
    WHILE (WhiteSpace[ch]) DO
      IF (ch = '\n') THEN INC (offset);  INC (nLines);  END;
      GetCh();
    END;

    (* scan an identifier *)
    i := 0;
    WHILE (AlphaNumerics[ch]) DO
      buf [i] := ch;  INC (i);
      GetCh ();
    END;
    cur.id    := M3ID.FromStr (buf, i);
    cur.token := VAL (M3ID.GetClass (cur.id), TK);

    IF (Token.First_Pragma<=cur.token) AND (cur.token<=Token.Last_Pragma) THEN
      RETURN;
    END;

    IF (cur.id = LINE) THEN
      GetToken (); (* LINE *)
      IF (cur.token # TK.tCARDCONST) THEN
        Error.Msg ("missing line number on LINE pragma; skipping to \'*>\'");
        WHILE (cur.token # TK.tENDPRAGMA) AND (cur.token # TK.tEOF) DO
          GetToken ();
        END;
        IF (cur.token = TK.tENDPRAGMA) THEN GetToken () END;
        RETURN;
      END;
      IF NOT TInt.ToInt (cur.int, lineno) THEN
        Error.Msg ("illegal line number, ignored");
        lineno := offset MOD MaxLines;
      END;
      fileno := offset DIV MaxLines;
      GetToken (); (* CARD "line number" *)
      IF (cur.token = TK.tTEXTCONST) THEN
        fileno := FileNumber (M3String.ToText (cur.str));
        GetToken(); (* TEXT "filename" *)
      END;
      offset := fileno * MaxLines + lineno - 1;
      IF (cur.token # TK.tENDPRAGMA)
        THEN Error.Msg ("missing \'*>\' on LINE pragma");
        ELSE GetToken (); (* fetch the next one *)
      END;
      RETURN;
    ELSIF (cur.id = NOWARN) THEN
      Error.IgnoreWarning (cur.offset);
      GetToken ();  (* NOWARN *)
      IF (cur.token # TK.tENDPRAGMA)
        THEN Error.Msg ("missing \'*>\' on NOWARN pragma");
        ELSE GetToken (); (* fetch the next one *)
      END;
      RETURN;
    ELSIF (cur.id = PRAGMA) THEN
      GetToken (); (* PRAGMA *)
      WHILE (cur.token = TK.tIDENT) 
      OR ((Token.First_Pragma<=cur.token) AND (cur.token<=Token.Last_Pragma))
      OR ((Token.First_Keyword<=cur.token) AND (cur.token<=Token.Last_Keyword))
      DO
        ignore := NEW (IDList, id := cur.id, next := ignore);
        GetToken ();  (* IDENT *)
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken ();  (* COMMA *)
      END;
      IF (cur.token # TK.tENDPRAGMA)
        THEN Error.Msg ("missing \'*>\' on PRAGMA pragma");
        ELSE GetToken ();  (* fetch the next real token *)
      END;
      RETURN;
    ELSIF Target.FindConvention (M3ID.ToText (cur.id)) # NIL THEN
      cur.token := TK.tCALLCONV;
      RETURN;
    ELSE (* scan and ignore the list *)
      ss := ignore;
      WHILE (ss # NIL) AND (ss.id # cur.id) DO  ss := ss.next  END;
      IF (ss = NIL) THEN
        Error.WarnID (2, cur.id, "unrecognized pragma (ignored)");
      END;
    END;


    (* scan over and ignore the offending pragma *)
    nest := 1;
    WHILE (nest > 0) DO
      IF (ch = '*') THEN
        GetCh();  IF (ch = '>') THEN DEC(nest); GetCh(); END;
      ELSIF (ch = '<') THEN
        GetCh();  IF (ch = '*') THEN INC(nest); GetCh(); END;
      ELSIF (ch = EOFChar) THEN
        save := offset;
        offset := start;
        Error.Msg ("EOF encountered in pragma");
        offset := save;
        nest := 0;
      ELSIF (ch = '\n') THEN
        INC (offset);  INC (nLines);
        GetCh();
      ELSE
        GetCh();
      END;
    END;

    GetToken (); (* get the next token *)
  END ScanPragma;

(* Support capturing underlying text while scanning.
 * This is used for assertion messages.
 * Consider a different implementation:
 *  - Turn on full buffering
 *  - Use positions within the buffer.
 *
 * Or consider mmap, really.
 *)
VAR capturingText: BOOLEAN;
VAR capturedText: CharSeq.T;

PROCEDURE EnableTextCapture() =
BEGIN
  IF capturedText = NIL THEN
    capturedText := NEW(CharSeq.T);
  END;
  EVAL capturedText.init(99);
  capturingText := TRUE;
END EnableTextCapture;

PROCEDURE DisableTextCapture(VAR text: REF ARRAY OF CHAR; VAR size: INTEGER) =
BEGIN
  IF capturingText THEN
    capturingText := FALSE;
    text := capturedText.elem;
    size := capturedText.sz;
  END;
END DisableTextCapture;

BEGIN
END Scanner.
