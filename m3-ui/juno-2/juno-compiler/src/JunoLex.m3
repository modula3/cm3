(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Mar 17 14:27:29 PST 1995 by heydon                   *)
(*      modified on Wed Feb 15 16:27:30 PST 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:54:04 PDT 1992 by myers                    *)

MODULE JunoLex;

IMPORT JunoValue;
IMPORT JunoToken;
IMPORT Atom, Rd, Lex, FloatMode, Text, Thread;

<* FATAL Thread.Alerted *>

(* The procedures in this module that raise "Rd.Failure" do so when they
   encounter a problem reading from their "Stream" argument. *)

REVEAL
  Private = MUTEX BRANDED "JunoLex.Private" OBJECT
    c: CHAR;				 (* next character *)
    rd: Rd.T;                            (* input stream *)
    token: JunoToken.T;			 (* most recent token *)
    buff: REF ARRAY OF CHAR;		 (* character buffer *)
    sz: CARDINAL;			 (* buffer size *)
  END;

  Stream = Public BRANDED "JunoLex.Stream" OBJECT OVERRIDES
    next := Next;
  END;

(* Conceptually, a Stream "s" contains a stream of characters that can be
   thought of as a text. This implementation maintains the following
   invariant:

|    The character "s.c" concatenated with the characters remaining on
|    "s.rd" yields the stream of tokens that have yet to be returned by a
|    call to "s.next()".

   Hence, "s.c" is a one-character look-ahead buffer. In the case where the
   previous token ended with the last character in "s.rd", "s.c" is set to
   the special value ' ' (space). *)

CONST InitBuffSize = 40; Space = ' ';

PROCEDURE New(rd: Rd.T): Stream RAISES {Rd.Failure} =
  <* FATAL Rd.EndOfFile *>
  VAR result := NEW(Stream, c := Space, rd := rd, sz := InitBuffSize); BEGIN
    result.token := NEW(JunoToken.T);
    result.buff := NEW(REF ARRAY OF CHAR, result.sz);
    IF NOT Rd.EOF(result.rd) THEN result.c := Rd.GetChar(result.rd) END;
    RETURN result
  END New;

PROCEDURE Close(s: Stream): TEXT =
  BEGIN
    RETURN Text.FromChar(s.c)
  END Close;

PROCEDURE Next(s: Stream): JunoToken.T RAISES {Error, Rd.Failure} =
  BEGIN
    LOCK s DO
      TRY GetNextToken(s) EXCEPT
        Rd.EndOfFile => s.token.kind := JunoToken.Kind.EndMarker
      END;
      RETURN s.token
    END
  END Next;

PROCEDURE GetNextToken(s: Stream) RAISES {Error, Rd.EndOfFile, Rd.Failure} =
(* Assuming "s" is not a closed stream, read the next token from "s" after
   skipping any initial whitespace. Raises Error(ErrorKind.BadInitialChar) if
   the first character appearing after any whitespace and nested comments is
   an illegal starting character for a token; this exception may also be
   raised by procedures called by this procedure. Raises Rd.EndOfFile if no
   token was found on "s" after any initial whitespace. *)

  PROCEDURE GetReal() RAISES {Error, Rd.Failure} =
  (* Read a real number, and store its kind and value in "s.token". *)
    BEGIN
      s.token.kind := JunoToken.Kind.LitReal;
      s.token.num := FLOAT(ReadReal(s), JunoValue.Real)
    END GetReal;

  (* GetNextToken *)
  BEGIN
    (* skip whitespace *)
    WHILE s.c IN Lex.Blanks DO s.c := Rd.GetChar(s.rd) END;
    s.lastPos := Rd.Index(s.rd) - 1;

    TRY
      CASE s.c OF
      | '0'..'9' =>                      (* numeric literal *)
          GetReal()
      | '.' =>				 (* "." operator *)
          TRY s.c := Rd.GetChar(s.rd) EXCEPT
            Rd.EndOfFile =>
              s.token.kind := JunoToken.Kind.Dot; RAISE Rd.EndOfFile
          END;
          IF '0' <= s.c AND s.c <= '9' THEN
            Rd.UnGetChar(s.rd); GetReal()
          ELSE
            s.token.kind := JunoToken.Kind.Dot
          END
      | '\"' =>                          (* text literal *)
          s.token.kind := JunoToken.Kind.LitText;
          s.token.val := ReadText(s);
          s.c := Rd.GetChar(s.rd)
      | 'a'..'z', 'A'..'Z', '_' =>       (* identifier or keyword *)
          s.token.kind := ReadId(s, s.token.val)
      | ';' =>				 (* ";" operator *)
          s.token.kind := JunoToken.Kind.Semi;
          s.c := Rd.GetChar(s.rd)
      | ',' =>				 (* "," operator *)
          s.token.kind := JunoToken.Kind.Comma;
          s.c := Rd.GetChar(s.rd)
      | ')' =>				 (* ")" operator *)
          s.token.kind := JunoToken.Kind.RPren;
          s.c := Rd.GetChar(s.rd)
      | '[' =>				 (* "[" operator *)
          s.token.kind := JunoToken.Kind.LBracket;
          s.c := Rd.GetChar(s.rd)
      | ']' =>				 (* "]" operator *)
          s.token.kind := JunoToken.Kind.RBracket;
          s.c := Rd.GetChar(s.rd)
      | '{' =>				 (* "{" operator *)
          s.token.kind := JunoToken.Kind.LBrace;
          s.c := Rd.GetChar(s.rd)
      | '}' =>				 (* "}" operator *)
          s.token.kind := JunoToken.Kind.RBrace;
          s.c := Rd.GetChar(s.rd)
      | '|' =>				 (* "|" operator *)
          s.token.kind := JunoToken.Kind.Else;
          s.c := Rd.GetChar(s.rd)
      | '~' =>				 (* "~" operator *)
          s.token.kind := JunoToken.Kind.Near;
          s.c := Rd.GetChar(s.rd)
      | '=' =>				 (* "=" operator *)
          s.token.kind := JunoToken.Kind.Equals;
          s.c := Rd.GetChar(s.rd)
      | '#' =>				 (* "#" operator *)
          s.token.kind := JunoToken.Kind.Differs;
          s.c := Rd.GetChar(s.rd)
      | '+' =>				 (* "+" operator *)
          s.token.kind := JunoToken.Kind.Plus;
          s.c := Rd.GetChar(s.rd)
      | '*' =>				 (* "*" operator *)
          s.token.kind := JunoToken.Kind.Times;
          s.c := Rd.GetChar(s.rd)
      | '&' =>				 (* "&" operator *)
          s.token.kind := JunoToken.Kind.Concat;
          s.c := Rd.GetChar(s.rd)
      | '/' =>				 (* "/" operator *)
          TRY s.c := Rd.GetChar(s.rd) EXCEPT
            Rd.EndOfFile =>
              s.token.kind := JunoToken.Kind.Divide; RAISE Rd.EndOfFile
          END;
          IF s.c = '*' THEN
            s.token.kind := JunoToken.Kind.Comment;
            s.token.val := ReadComment(s, private := TRUE);
            s.c := Rd.GetChar(s.rd)
          ELSE
            s.token.kind := JunoToken.Kind.Divide
          END
      | '(' =>				 (* "(" operator or comment-start *)
          TRY s.c := Rd.GetChar(s.rd) EXCEPT
            Rd.EndOfFile =>
              s.token.kind := JunoToken.Kind.LPren; RAISE Rd.EndOfFile
          END;
          IF s.c = '*' THEN
            s.token.kind := JunoToken.Kind.Comment;
            s.token.val := ReadComment(s, private := FALSE);
            s.c := Rd.GetChar(s.rd)
          ELSE
            s.token.kind := JunoToken.Kind.LPren
          END
      | ':' =>				 (* ":" or ":=" operator *)
          TRY s.c := Rd.GetChar(s.rd) EXCEPT
            Rd.EndOfFile =>
              s.token.kind := JunoToken.Kind.Colon; RAISE Rd.EndOfFile
          END;
          IF s.c = '=' THEN
            s.token.kind := JunoToken.Kind.Assign;
            s.c := Rd.GetChar(s.rd)
          ELSIF s.c = ':' THEN
            s.token.kind := JunoToken.Kind.SuchThat;
            s.c := Rd.GetChar(s.rd)
          ELSE
            s.token.kind := JunoToken.Kind.Colon;
          END
      | '-' =>				 (* "-" or "->" operator *)
          TRY s.c := Rd.GetChar(s.rd) EXCEPT
            Rd.EndOfFile =>
              s.token.kind := JunoToken.Kind.Minus; RAISE Rd.EndOfFile
          END;
          IF s.c = '>' THEN
            s.token.kind := JunoToken.Kind.Guard;
            s.c := Rd.GetChar(s.rd)
          ELSE
            s.token.kind := JunoToken.Kind.Minus;
          END
      | '<' =>				 (* "<" or "<=" operator *)
          TRY s.c := Rd.GetChar(s.rd) EXCEPT
            Rd.EndOfFile =>
              s.token.kind := JunoToken.Kind.Less; RAISE Rd.EndOfFile
          END;
          IF s.c = '=' THEN
            s.token.kind := JunoToken.Kind.AtMost;
            s.c := Rd.GetChar(s.rd)
          ELSE
            s.token.kind := JunoToken.Kind.Less;
          END
      | '>' =>				 (* ">" or ">=" operator *)
          TRY s.c := Rd.GetChar(s.rd) EXCEPT
            Rd.EndOfFile =>
              s.token.kind := JunoToken.Kind.Greater; RAISE Rd.EndOfFile
          END;
          IF s.c = '=' THEN
            s.token.kind := JunoToken.Kind.AtLeast;
            s.c := Rd.GetChar(s.rd)
          ELSE
            s.token.kind := JunoToken.Kind.Greater;
          END
      ELSE
	  Rd.UnGetChar(s.rd);
	  RaiseError(ErrorKind.BadInitialChar, "")
      END
    EXCEPT
      Rd.EndOfFile => s.c := Space (* EOF encountered after a legal token *)
    END
  END GetNextToken;
  
PROCEDURE FlushBuf(
    prefix: TEXT;
    READONLY buf: ARRAY OF CHAR;
    VAR (*INOUT*) pos: CARDINAL) 
  : TEXT =
(* Return "Text.FromChars(SUBARRAY(buf, 0, pos))" appended to "prefix", and
   set "pos" to 0. *)
  VAR txt := Text.FromChars(SUBARRAY(buf, 0, pos)); res: TEXT; BEGIN
    IF prefix = NIL
      THEN res := txt
      ELSE res := prefix & txt
    END;
    pos := 0;
    RETURN res
  END FlushBuf;

CONST BufSize = 100;
TYPE Buffer = ARRAY [0..BufSize-1] OF CHAR;

PROCEDURE ReadText(s: Stream): TEXT RAISES {Error, Rd.Failure} =
(* Assuming "s.c" is the opening double-quote of a text string, return the
   text string on "s", processing all escape Modula-3 character escape
   sequences.

   Raises Error(ErrorKind.UnclosedText) if end-of-file occurs before the
   terminating double-quote; RAISES Error(ErrorKind.BadEscapeChar) if an
   illegal escape suffix follows the escape character "\" in the literal.

   Note: Since a text may be followed by end-of-file, and since we must
   return a valid result in that case, this procedure does *not* maintain the
   invariant that "s.c" contains the next unprocessed character; that
   character will be the first character on "s.rd". *)
  VAR buf: Buffer; pos: CARDINAL := 0; res := "";

  <* INLINE *>
  PROCEDURE AppendChar(c: CHAR) =
    BEGIN
      IF pos = BufSize THEN res := FlushBuf(res, buf, pos) END;
      buf[pos] := c; INC(pos)
    END AppendChar;

  PROCEDURE ReadOctEscape(VAR (*INOUT*) pos: CARDINAL): CHAR
    RAISES {Rd.EndOfFile, Rd.Failure} =
  (* Requires "s.c" is one of the characters '0'..'7'. *)
    CONST OctDigits = SET OF CHAR{'0'..'7'}; Zero = ORD('0');
    VAR val := ORD(s.c) - Zero; BEGIN
      buf[pos] := s.c; INC(pos);
      FOR i := 1 TO 2 DO
        s.c := Rd.GetChar(s.rd);
        IF NOT s.c IN OctDigits THEN
          Rd.UnGetChar(s.rd); RAISE Rd.EndOfFile
        END;
        buf[pos] := s.c; INC(pos);
        val := (val * 8) + (ORD(s.c) - Zero)
      END;
      IF val > ORD(LAST(CHAR)) THEN RAISE Rd.EndOfFile END;
      RETURN VAL(val, CHAR)
    END ReadOctEscape;

  PROCEDURE ReadEscape(): CHAR RAISES {Error, Rd.Failure} =
    VAR escPos: CARDINAL; escChar: CHAR; BEGIN
      (* guarantee that buffer will not be flushed in this procedure *)
      IF pos + 4 >= BufSize THEN res := FlushBuf(res, buf, pos) END;
      escPos := pos;
      buf[escPos] := s.c; INC(escPos);	 (* save '\\' *)
      TRY
      	s.c := Rd.GetChar(s.rd);
      	CASE s.c OF
      	| 'n'  => escChar := '\n'
      	| 't'  => escChar := '\t'
      	| 'r'  => escChar := '\r'
      	| 'f'  => escChar := '\f'
      	| '\\' => escChar := '\\'
      	| '\"' => escChar := '\"'
      	| '0'..'7' => escChar := ReadOctEscape(escPos)
      	ELSE Rd.UnGetChar(s.rd); RAISE Rd.EndOfFile
      	END
      EXCEPT
        Rd.EndOfFile =>
          res := FlushBuf(res, buf, escPos);
          RaiseError(ErrorKind.BadEscapeChar, "\"" & res)
      END;
      RETURN escChar
    END ReadEscape;

  (* ReadText *)
  BEGIN
    TRY
      TRY
      	LOOP
          s.c := Rd.GetChar(s.rd);
      	  CASE s.c OF
      	  | '\\' => AppendChar(ReadEscape())
      	  | '\"' => EXIT
      	  ELSE      AppendChar(s.c)
      	  END;
      	END
      FINALLY
        res := FlushBuf(res, buf, pos)
      END
    EXCEPT
      Rd.EndOfFile => RaiseError(ErrorKind.UnclosedText, "\"" & res)
    END;
    RETURN res
  END ReadText;

PROCEDURE PrefixChars(rd: Rd.T; start: CARDINAL): TEXT RAISES {Rd.Failure} =
  <* FATAL Rd.EndOfFile *>
  VAR curr := Rd.Index(rd); buf := NEW(REF ARRAY OF CHAR, curr - start); BEGIN
    Rd.Seek(rd, start);
    FOR i := 0 TO curr - start - 1 DO buf[i] := Rd.GetChar(rd) END;
    RETURN Text.FromChars(buf^)
  END PrefixChars;

PROCEDURE ReadReal(s: Stream): REAL RAISES {Error, Rd.Failure} =
(* This procedure assumes that "s.rd" is positioned one character *past* the
   first character of the real number. *)
  VAR res: REAL; start: CARDINAL; BEGIN
    start := Rd.Index(s.rd) - 1;
    Rd.Seek(s.rd, start);
    TRY
      res := Lex.Real(s.rd);
      s.c := Rd.GetChar(s.rd)
    EXCEPT
      Lex.Error, FloatMode.Trap =>
        RaiseError(ErrorKind.BadReal, PrefixChars(s.rd, start))
    | Rd.EndOfFile => s.c := Space
    END;
    RETURN res
  END ReadReal;

PROCEDURE ReadId(s: Stream; VAR (*OUT*) id: REFANY): JunoToken.Kind
    RAISES {Rd.Failure} =
(* Parse the identifier appearing on "s". If it is a true identifier, return
   "JunoToken.Kind.Id", and set "id" to the "Atom.T" corresponding to the
   parsed identifier. Otherwise, it must be a keyword or a reserved
   identifier; in that case, return the proper JunoToken.Kind and leave "id"
   unchanged. *)
  VAR sz: CARDINAL := 0;

  PROCEDURE DoubleSBuf() =
  (* Double the size of "s.buff". *)
    VAR newSz := s.sz * 2; buff := NEW(REF ARRAY OF CHAR, newSz); BEGIN
      SUBARRAY(buff^, 0, sz) := SUBARRAY(s.buff^, 0, sz);
      s.buff := buff; s.sz := newSz;
    END DoubleSBuf;

  PROCEDURE ReadIdToBuf() RAISES {Rd.Failure} =
  (* Read identifier off "s" into "s.buff". *)
    CONST IdChars = SET OF CHAR{'a'..'z', 'A'..'Z', '0'..'9', '_'}; BEGIN
      TRY
    	WHILE s.c IN IdChars DO
    	  IF sz = s.sz THEN DoubleSBuf() END;
    	  s.buff[sz] := s.c; INC(sz);
    	  s.c := Rd.GetChar(s.rd)
    	END
      EXCEPT
    	Rd.EndOfFile => s.c := Space
      END
    END ReadIdToBuf;

  PROCEDURE MatchTail(tail: TEXT): BOOLEAN =
  (* Return "TRUE" iff the characters in "s.buff[1..sz-1]" match the text
     "Text.Sub(tail, 1)". *)
    CONST Start = 1; BEGIN
      IF sz # Text.Length(tail) THEN RETURN FALSE END;
      WITH buf = s.buff DO
      	FOR i := Start TO sz - 1 DO
	  IF buf[i] # Text.GetChar(tail, i) THEN RETURN FALSE END
      	END
      END;
      RETURN TRUE
    END MatchTail;

  PROCEDURE Keyword(): JunoToken.Kind =
  (* If the characters "SUBARRAY(s.buff^, 0, sz)" are a keyword, then return
     the corresponding token kind. Otherwise, return "JunoToken.Kind.Id". *)
    BEGIN
      CASE s.buff[0] OF
      | 'A' =>
    	IF MatchTail("AND") THEN RETURN JunoToken.Kind.And
    	ELSIF MatchTail("ABS") THEN RETURN JunoToken.Kind.Abs
    	ELSIF MatchTail("ATAN") THEN RETURN JunoToken.Kind.Atan
    	ELSIF MatchTail("ABORT") THEN RETURN JunoToken.Kind.Abort
    	END
      | 'C' =>
    	IF MatchTail("CAR") THEN RETURN JunoToken.Kind.Car
    	ELSIF MatchTail("CDR") THEN RETURN JunoToken.Kind.Cdr
    	ELSIF MatchTail("CONG") THEN RETURN JunoToken.Kind.Cong
    	ELSIF MatchTail("CONST") THEN RETURN JunoToken.Kind.Const
    	ELSIF MatchTail("COS") THEN RETURN JunoToken.Kind.Cos
    	ELSIF MatchTail("CEILING") THEN RETURN JunoToken.Kind.Ceiling
    	END
      | 'D' =>
    	IF MatchTail("DO") THEN RETURN JunoToken.Kind.Do
    	ELSIF MatchTail("DIV") THEN RETURN JunoToken.Kind.Div
    	END
      | 'E' =>
    	IF sz = 1 THEN RETURN JunoToken.Kind.Exists
    	ELSIF MatchTail("END") THEN RETURN JunoToken.Kind.End
    	ELSIF MatchTail("EXP") THEN RETURN JunoToken.Kind.Exp
    	END
      | 'F' =>
    	IF MatchTail("FI") THEN RETURN JunoToken.Kind.Fi
    	ELSIF MatchTail("FALSE") THEN RETURN JunoToken.Kind.False
    	ELSIF MatchTail("FLOOR") THEN RETURN JunoToken.Kind.Floor
    	ELSIF MatchTail("FUNC") THEN RETURN JunoToken.Kind.Func
    	END
      | 'H' =>
    	IF MatchTail("HOR") THEN RETURN JunoToken.Kind.Hor END
      | 'I' =>
    	IF MatchTail("IF") THEN RETURN JunoToken.Kind.If
    	ELSIF MatchTail("IN") THEN RETURN JunoToken.Kind.In
    	ELSIF MatchTail("IS") THEN RETURN JunoToken.Kind.Is
    	ELSIF MatchTail("INT") THEN RETURN JunoToken.Kind.Int
    	ELSIF MatchTail("IMPORT") THEN RETURN JunoToken.Kind.Import
    	END
      | 'L' =>
    	IF sz = 2 AND s.buff[1] = 'N' THEN RETURN JunoToken.Kind.Ln END
      | 'M' =>
    	IF MatchTail("MAX") THEN RETURN JunoToken.Kind.Max
    	ELSIF MatchTail("MIN") THEN RETURN JunoToken.Kind.Min
    	ELSIF MatchTail("MOD") THEN RETURN JunoToken.Kind.Mod
    	ELSIF MatchTail("MODULE") THEN RETURN JunoToken.Kind.Module
    	END
      | 'N' =>
        IF sz = 3 THEN
    	  IF MatchTail("NOT") THEN RETURN JunoToken.Kind.Not
    	  ELSIF MatchTail("NIL") THEN RETURN JunoToken.Kind.Nil
    	  END
        END
      | 'O' =>
    	IF sz = 2 THEN
    	  IF s.buff[1] = 'D' THEN RETURN JunoToken.Kind.Od
    	  ELSIF s.buff[1] = 'R' THEN RETURN JunoToken.Kind.Or END
    	END
      | 'P' =>
    	IF MatchTail("PARA") THEN RETURN JunoToken.Kind.Para
    	ELSIF MatchTail("PAIR") THEN RETURN JunoToken.Kind.Pair
    	ELSIF MatchTail("PRED") THEN RETURN JunoToken.Kind.Pred
    	ELSIF MatchTail("PRIVATE") THEN RETURN JunoToken.Kind.Private
    	ELSIF MatchTail("PROC") THEN RETURN JunoToken.Kind.Proc
    	END
      | 'R' =>
    	IF MatchTail("REAL") THEN RETURN JunoToken.Kind.Real
    	ELSIF MatchTail("REL") THEN RETURN JunoToken.Kind.Rel
    	ELSIF MatchTail("ROUND") THEN RETURN JunoToken.Kind.Round
    	END
      | 'S' =>
    	IF MatchTail("SKIP") THEN RETURN JunoToken.Kind.Skip
    	ELSIF MatchTail("SAVE") THEN RETURN JunoToken.Kind.Save
    	ELSIF MatchTail("SIN") THEN RETURN JunoToken.Kind.Sin
    	END
      | 'T' =>
        IF sz = 4 THEN
    	  IF MatchTail("TEXT") THEN RETURN JunoToken.Kind.Text
    	  ELSIF MatchTail("TRUE") THEN RETURN JunoToken.Kind.True
    	  END
        END
      | 'U' =>
        IF sz = 2 AND MatchTail("UI") THEN RETURN JunoToken.Kind.UI END
      | 'V' =>
        IF sz = 3 THEN
    	  IF MatchTail("VER") THEN RETURN JunoToken.Kind.Ver
    	  ELSIF MatchTail("VAR") THEN RETURN JunoToken.Kind.Var
    	  END
        END
      ELSE (* SKIP *)
      END;
      RETURN JunoToken.Kind.Id
    END Keyword;

  (* ReadId *)
  BEGIN
    ReadIdToBuf();
    IF 'A' <= s.buff[0] AND s.buff[0] <= 'Z' THEN
      VAR res := Keyword(); BEGIN
        IF res # JunoToken.Kind.Id THEN RETURN res END
      END
    END;
    id := Atom.FromText(Text.FromChars(SUBARRAY(s.buff^, 0, sz)));
    RETURN JunoToken.Kind.Id
  END ReadId;

PROCEDURE ReadComment(s: Stream; private: BOOLEAN): TEXT
    RAISES {Error, Rd.Failure} =
(* Assuming "s.c" contains the '*' character at the start of the comment,
   return the TEXT of the full comment including the start- and end-of-comment
   markers and any nested comments. Raises "Error(ErrorKind.UnclosedComment)"
   if the file ends before the final end-of-comment characters. If "private =
   FALSE", then the end-of-comment characters are Modula-3 style, otherwise,
   they are C style.

   Note: Since a comment may be followed by end-of-file, and since we must
   return a valid result in that case, this procedure does *not* maintain the
   invariant that "s.c" contains the next unprocessed character; that
   character will be the first character on "s.rd". *)
  VAR buf: Buffer; i: CARDINAL; res := "";

  <* INLINE *>
  PROCEDURE AppendChar(c: CHAR) =
    BEGIN
      IF i = BufSize THEN res := FlushBuf(res, buf, i) END;
      buf[i] := c; INC(i)
    END AppendChar;

  (* ReadComment *)
  VAR first, last: CHAR; BEGIN
    <* ASSERT s.c = '*' *>
    res := "";
    IF private
      THEN buf[0] := '/'; last := '/'
      ELSE buf[0] := '('; last := ')'
    END;
    buf[1] := '*'; i := 2;
    TRY
      TRY
      	s.c := Rd.GetChar(s.rd);
      	LOOP
      	  first := s.c;
          CASE first OF
      	    '(', '/' =>
      	      s.c := Rd.GetChar(s.rd);
      	      IF s.c = '*' THEN
            	res := FlushBuf(res, buf, i);
      	    	res := res & ReadComment(s, private := (first = '/'));
                s.c := Rd.GetChar(s.rd)
      	      ELSE
            	AppendChar(first)
      	      END
      	  | '*' =>
              AppendChar(first);
      	      s.c := Rd.GetChar(s.rd);
      	      IF s.c = last THEN
                AppendChar(last);
      	    	EXIT
      	      END
      	  ELSE
              AppendChar(first);
              s.c := Rd.GetChar(s.rd)
      	  END
      	END
      FINALLY
        res := FlushBuf(res, buf, i)
      END
    EXCEPT
      Rd.EndOfFile => RaiseError(ErrorKind.UnclosedComment, res)
    END;
    RETURN res
  END ReadComment;

PROCEDURE RaiseError(kind: ErrorKind; t: TEXT) RAISES {Error} =
  BEGIN
    RAISE Error(NEW(ErrorRec, kind := kind, initialChars := t))
  END RaiseError;

PROCEDURE ErrorText(e: ErrorKind): TEXT =
  BEGIN
    CASE e OF
    | ErrorKind.BadInitialChar  => RETURN "Illegal initial character"
    | ErrorKind.BadEscapeChar   => RETURN "Illegal \\ escape"
    | ErrorKind.BadReal         => RETURN "Illegal number"
    | ErrorKind.UnclosedComment => RETURN "Unclosed comment"
    | ErrorKind.UnclosedText    => RETURN "Unclosed text literal"
    END
  END ErrorText;

BEGIN END JunoLex.
