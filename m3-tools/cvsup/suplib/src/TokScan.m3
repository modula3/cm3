(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: TokScan.m3,v 1.1.1.1 2009-04-09 17:02:01 jkrell Exp $ *)

MODULE TokScan;

IMPORT
  ASCII, Fmt, IP, MD5Digest, RCSDate, RCSError, SupMisc, Text, Time,
  Word;

(*****************************************************************************)
(* Code common to all subtypes. *)
(*****************************************************************************)

TYPE
  Common = T OBJECT
    text: TEXT;
    sep: SET OF CHAR;
    len: CARDINAL;
    pos: CARDINAL;
    emptyTokens: BOOLEAN;
    onEmptyLastField := FALSE;
  METHODS
    init(t: TEXT;
	 READONLY separators: SET OF CHAR := Blanks;
	 emptyTokens := FALSE): T := Init;
  OVERRIDES
    getToken := GetToken;
    getChar := GetChar;
    getInt := GetInt;
    getTime := GetTime;
    getRCSDate := GetRCSDate;
    getMD5 := GetMD5;
    getEndpoint := GetEndpoint;
    getLiteral := GetLiteral;
    getFolded := GetFolded;
    getEnd := GetEnd;
  END;

PROCEDURE GetChar(self: Common; what: TEXT := "single-character token"): CHAR
  RAISES {Error} =
  VAR
    t: TEXT;
  BEGIN
    t := self.getToken(what);
    IF Text.Length(t) # 1 THEN
      RAISE Error("Invalid " & what);
    END;
    RETURN Text.GetChar(t, 0);
  END GetChar;

PROCEDURE GetEnd(self: Common; what: TEXT := "end")
  RAISES {Error} =
  VAR
    tok: TEXT;
  BEGIN
    IF self.next(tok) THEN
      RAISE Error("Expected " & what & ", got something else");
    END;
  END GetEnd;

PROCEDURE GetFolded(self: Common; what: TEXT)
  RAISES {Error} =
  BEGIN
    WITH qWhat = "\"" & what & "\"", t = self.getToken(qWhat) DO
      IF NOT EqualFolded(t, what) THEN
	RAISE Error("Expected " & qWhat & ", got something else");
      END;
    END;
  END GetFolded;

PROCEDURE GetEndpoint(self: Common; what: TEXT := "IP endpoint"): IP.Endpoint
  RAISES {Error} =
  VAR
    ep: IP.Endpoint;
  BEGIN
    TRY
      ep.addr.a[0] := self.getInt();
      ep.addr.a[1] := self.getInt();
      ep.addr.a[2] := self.getInt();
      ep.addr.a[3] := self.getInt();
      ep.port := self.getInt();
      RETURN ep;
    EXCEPT Error =>
      RAISE Error("Invalid " & what);
    END;
  END GetEndpoint;

PROCEDURE GetInt(self: Common;
                 what: TEXT := "integer";
		 radix: [2..16] := 10): Word.T
  RAISES {Error} =
  BEGIN
    RETURN AtoI(self.getToken(what), what, radix);
  END GetInt;

PROCEDURE GetLiteral(self: Common; what: TEXT)
  RAISES {Error} =
  BEGIN
    WITH qWhat = "\"" & what & "\"", t = self.getToken(qWhat) DO
      IF NOT Text.Equal(t, what) THEN
	RAISE Error("Expected " & qWhat & ", got something else");
      END;
    END;
  END GetLiteral;

PROCEDURE GetMD5(self: Common; what: TEXT := "MD5 checksum"): MD5Digest.T
  RAISES {Error} =
  BEGIN
    TRY
      RETURN MD5Digest.FromText(self.getToken(what));
    EXCEPT MD5Digest.Malformed =>
      RAISE Error("Invalid " & what);
    END;
  END GetMD5;

PROCEDURE GetRCSDate(self: Common; what: TEXT := "RCS date"): Time.T
  RAISES {Error} =
  BEGIN
    TRY
      RETURN RCSDate.ToTime(self.getToken(what));
    EXCEPT RCSError.E(msg) =>
      RAISE Error("Invalid " & what & ": " & msg);
    END;
  END GetRCSDate;

PROCEDURE GetTime(self: Common; what: TEXT := "time"): Time.T
  RAISES {Error} =
  BEGIN
    RETURN DecodeTime(self.getToken(what));
  END GetTime;

PROCEDURE GetToken(self: Common; what: TEXT := "token"): TEXT
  RAISES {Error} =
  VAR
    t: TEXT;
  BEGIN
    IF NOT self.next(t) THEN
      RAISE Error("Missing " & what);
    END;
    RETURN t;
  END GetToken;

PROCEDURE Init(self: Common;
               t: TEXT;
               READONLY separators: SET OF CHAR := Blanks;
	       emptyTokens := FALSE): T =
  BEGIN
    self.text := t;
    self.sep := separators;
    self.emptyTokens := emptyTokens;
    self.len := Text.Length(t);
    self.pos := 0;
    IF NOT self.emptyTokens THEN  (* Skip a leading string of separators. *)
      WHILE self.pos < self.len
      AND Text.GetChar(self.text, self.pos) IN self.sep DO
	INC(self.pos);
      END;
    END;
    RETURN self;
  END Init;

(*****************************************************************************)
(* Subclass for basic token scanning. *)
(*****************************************************************************)

TYPE
  Raw = Common OBJECT METHODS
    init(t: TEXT;
	 READONLY separators: SET OF CHAR := Blanks;
	 emptyTokens := FALSE): T := RawInit;
  OVERRIDES
    next := RawNext;
    getRest := RawGetRest;
  END;

PROCEDURE New(t: TEXT;
	      READONLY separators: SET OF CHAR := Blanks;
	      emptyTokens := FALSE): T =
  BEGIN
    RETURN NEW(Raw).init(t, separators, emptyTokens);
  END New;

PROCEDURE RawGetRest(self: Raw): TEXT =
  BEGIN
    WHILE self.pos < self.len AND
	  Text.GetChar(self.text, self.pos) IN self.sep DO
      INC(self.pos);
    END;
    RETURN Text.Sub(self.text, self.pos);
  END RawGetRest;

PROCEDURE RawInit(self: Raw;
                  t: TEXT;
                  READONLY separators: SET OF CHAR := Blanks;
		  emptyTokens := FALSE): T =
  BEGIN
    EVAL Common.init(self, t, separators, emptyTokens);
    RETURN self;
  END RawInit;

PROCEDURE RawNext(self: Raw; VAR tok: TEXT): BOOLEAN =
  VAR
    start: CARDINAL;
  BEGIN
    (* Upon entry we are positioned at the beginning of the new token. *)
    IF self.pos >= self.len THEN
      IF self.onEmptyLastField THEN
	self.onEmptyLastField := FALSE;
      ELSE
	RETURN FALSE;
      END;
    END;

    (* Scan the token. *)
    start := self.pos;
    WHILE self.pos < self.len
    AND NOT Text.GetChar(self.text, self.pos) IN self.sep DO
      INC(self.pos);
    END;
    tok := Text.Sub(self.text, start, self.pos - start);

    (* Skip the separator(s). *)
    IF self.pos < self.len THEN
      INC(self.pos);
      IF self.emptyTokens THEN
	IF self.pos = self.len THEN
	  self.onEmptyLastField := TRUE;
	END;
      ELSE  (* Skip a string of separators. *)
	WHILE self.pos < self.len
	AND Text.GetChar(self.text, self.pos) IN self.sep DO
	  INC(self.pos);
	END;
      END;
    END;
    RETURN TRUE;
  END RawNext;

(*****************************************************************************)
(* Subclass for scanning and decoding escaped text. *)
(*****************************************************************************)

TYPE
  Dec = Raw OBJECT METHODS
    init(t: TEXT): T := DecInit;
  OVERRIDES
    next := DecNext;
    getRest := DecGetRest;
  END;

PROCEDURE DecGetRest(self: Dec): TEXT
  RAISES {Error} =
  BEGIN
    TRY
      RETURN SupMisc.DecodeWS(Raw.getRest(self));
    EXCEPT SupMisc.InvalidEscape =>
      RAISE Error("Invalid escape sequence");
    END;
  END DecGetRest;

PROCEDURE DecInit(self: Dec; t: TEXT): T =
  BEGIN
    EVAL Raw.init(self, t);
    RETURN self;
  END DecInit;

PROCEDURE DecNext(self: Dec; VAR tok: TEXT): BOOLEAN
  RAISES {Error} =
  BEGIN
    TRY
      IF NOT Raw.next(self, tok) THEN RETURN FALSE END;
      tok := SupMisc.DecodeWS(tok);
      RETURN TRUE;
    EXCEPT SupMisc.InvalidEscape =>
      RAISE Error("Invalid escape sequence");
    END;
  END DecNext;

PROCEDURE NewDec(t: TEXT): T =
  BEGIN
    RETURN NEW(Dec).init(t);
  END NewDec;

(*****************************************************************************)
(* Handy utility procedures. *)
(*****************************************************************************)

PROCEDURE AtoI(t: TEXT; what: TEXT := "integer"; radix: [2..16] := 10): Word.T
  RAISES {Error} =
  VAR
    len := Text.Length(t);
    val: Word.T := 0;
    digit: INTEGER;
  BEGIN
    IF len = 0 THEN RAISE
      Error("Invalid " & what);
    END;
    FOR i := 0 TO len-1 DO
      WITH ch = Text.GetChar(t, i) DO
	CASE ch OF
	| '0'..'9' => digit := ORD(ch) - ORD('0');
	| 'a'..'f' => digit := ORD(ch) - ORD('a') + 10;
	| 'A'..'F' => digit := ORD(ch) - ORD('A') + 10;
	ELSE
	  digit := radix;
	END;
	IF digit >= radix THEN
	  RAISE Error("Invalid " & what);
	END;
	val := Word.Plus(Word.Times(val, radix), digit);
      END;
    END;
    RETURN val;
  END AtoI;

PROCEDURE DecodeTime(text: TEXT): Time.T
  RAISES {Error} =
  VAR
    negative := FALSE;
    time: Time.T;
  BEGIN
    IF Text.Length(text) > 0 AND Text.GetChar(text, 0) = '-' THEN
      negative := TRUE;
      text := Text.Sub(text, 1);
    END;
    time := FLOAT(AtoI(text), Time.T);
    IF negative THEN
      time := -time;
    END;
    RETURN time;
  END DecodeTime;

PROCEDURE EncodeEndpoint(READONLY ep: IP.Endpoint;
                         VAR toks: ARRAY [0..4] OF TEXT) =
  BEGIN
    toks[0] := Fmt.Int(ep.addr.a[0]);
    toks[1] := Fmt.Int(ep.addr.a[1]);
    toks[2] := Fmt.Int(ep.addr.a[2]);
    toks[3] := Fmt.Int(ep.addr.a[3]);
    toks[4] := Fmt.Int(ep.port);
  END EncodeEndpoint;

PROCEDURE EncodeTime(time: Time.T): TEXT =
  VAR
    absTime: Word.T;
  BEGIN
    absTime := ROUND(ABS(time));
    IF time < 0.0d0 AND absTime # 0 THEN
      RETURN "-" & Fmt.Unsigned(absTime, 10);
    ELSE
      RETURN Fmt.Unsigned(absTime, 10);
    END;
  END EncodeTime;

PROCEDURE EqualFolded(a, b: TEXT): BOOLEAN =
  VAR
    len := Text.Length(a);
  BEGIN
    IF Text.Length(b) # len THEN RETURN FALSE END;
    FOR i := 0 TO len-1 DO
      IF ASCII.Upper[Text.GetChar(a, i)] # ASCII.Upper[Text.GetChar(b, i)] THEN
	RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END EqualFolded;

PROCEDURE ScanLeadingInt(t: TEXT; VAR pos: CARDINAL): Word.T =
  VAR
    tLen := Text.Length(t);
    val: Word.T := 0;
  BEGIN
    WHILE pos < tLen DO
      WITH ch = Text.GetChar(t, pos) DO
	IF ch < '0' OR ch > '9' THEN EXIT END;
	val := Word.Plus(Word.Times(val, 10), ORD(ch) - ORD('0'));
      END;
      INC(pos);
    END;
    RETURN val;
  END ScanLeadingInt;

PROCEDURE Trim(t: TEXT): TEXT =
  CONST
    WhiteSpace = SET OF CHAR{' ', '\t'};
  VAR
    start := 0;
    limit := Text.Length(t);
  BEGIN
    WHILE start < limit AND Text.GetChar(t, start) IN WhiteSpace DO
      INC(start);
    END;
    WHILE start < limit AND Text.GetChar(t, limit-1) IN WhiteSpace DO
      DEC(limit);
    END;
    RETURN Text.Sub(t, start, limit-start);
  END Trim;

BEGIN
END TokScan.
