(* Copyright 1997-2003 John D. Polstra.
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
 *)

(* This module implements the most performance critical text
   manipulation procedures in the "SupMisc" interface.  It uses
   the "UnsafeWr" interface for speed. *)

MODULE SupMiscText EXPORTS SupMisc;

IMPORT Pathname, Text, Thread, UnsafeWr, Wr;

PROCEDURE Cat3(a, b, c: TEXT): TEXT =
  BEGIN
    RETURN a & b & c;
  END Cat3;

PROCEDURE CatN(READONLY a: ARRAY OF TEXT): TEXT =
  VAR
    t: TEXT;
  BEGIN
    t := a[0];
    FOR i := 1 TO LAST(a) DO
      t := t & a[i];
    END;
    RETURN t;
  END CatN;

PROCEDURE CommonPathLength(a, b: Pathname.T): CARDINAL =
  VAR
    aLen := Text.Length(a);
    bLen := Text.Length(b);
    minLen := MIN(aLen, bLen);
    aCh, bCh: CHAR;
    lastSlash: CARDINAL := 0;
  BEGIN
    FOR i := 0 TO minLen-1 DO
      aCh := Text.GetChar(a, i);
      bCh := Text.GetChar(b, i);
      IF aCh # bCh THEN RETURN lastSlash END;
      IF aCh = SlashChar THEN
	IF i = 0 THEN  (* Include the leading slash. *)
	  lastSlash := 1;
	ELSE
	  lastSlash := i;
	END;
      END;
    END;

    (* One path is a prefix of the other. *)
    IF aLen > minLen THEN  (* Path "b" is a prefix of "a". *)
      IF Text.GetChar(a, minLen) = SlashChar THEN
	RETURN minLen;
      ELSE
	RETURN lastSlash;
      END;
    ELSIF bLen > minLen THEN  (* Path "a" is a prefix of "b". *)
      IF Text.GetChar(b, minLen) = SlashChar THEN
	RETURN minLen;
      ELSE
	RETURN lastSlash;
      END;
    ELSE  (* The paths are identical. *)
      RETURN minLen;
    END;
  END CommonPathLength;

CONST EscapedChars = SET OF CHAR{' ', '\t', '\n', '\r', '\\'};

PROCEDURE DecodeWS(t: TEXT): TEXT
  RAISES {InvalidEscape} =
  VAR
    len := Text.Length(t);
    startPos := 0;
    slashPos: INTEGER;
    nt := "";
    ch: CHAR := '\000';
  BEGIN
    WHILE startPos < len DO
      slashPos := Text.FindChar(t, '\\', startPos);
      IF slashPos = -1 THEN
	nt := nt & Text.Sub(t, startPos);
	EXIT;
      END;
      IF slashPos + 1 >= len THEN RAISE InvalidEscape END;
      nt := nt & Text.Sub(t, startPos, slashPos - startPos);
      CASE Text.GetChar(t, slashPos + 1) OF
      | '_' => ch := ' ';
      | 't' => ch := '\t';
      | 'n' => ch := '\n';
      | 'r' => ch := '\r';
      | '\\' => ch := '\\';
      ELSE
	RAISE InvalidEscape;
      END;
      nt := nt & Text.FromChar(ch);
      startPos := slashPos + 2;
    END;
    RETURN nt;
  END DecodeWS;

PROCEDURE EncodeWS(t: TEXT): TEXT =
  VAR
    len := Text.Length(t);
    runStart: CARDINAL;
    nt: TEXT := NIL;
    ch: CHAR;
  BEGIN
    runStart:= 0;
    FOR pos := 0 TO len - 1 DO
      ch := Text.GetChar(t, pos);
      IF ch IN EscapedChars THEN
	IF nt = NIL THEN
	  nt := Text.Sub(t, runStart, pos - runStart);
	ELSE
	  nt := nt & Text.Sub(t, runStart, pos - runStart);
	END;
	CASE ch OF
	| ' '  => nt := nt & "\\_";
	| '\t' => nt := nt & "\\t";
	| '\n' => nt := nt & "\\n";
	| '\r' => nt := nt & "\\r";
	| '\\' => nt := nt & "\\\\";
	ELSE
	  <* ASSERT FALSE *>
	END;
	runStart := pos + 1;
      END;
    END;
    IF nt = NIL THEN
      nt := t;
    ELSE
      nt := nt & Text.Sub(t, runStart, len - runStart);
    END;
    RETURN nt;
  END EncodeWS;

PROCEDURE PathCompare(a, b: Pathname.T): [-1..1] =
  VAR
    aLen := Text.Length(a);
    bLen := Text.Length(b);
    aCh, bCh: CHAR;
  BEGIN
    FOR i := 0 TO MIN(aLen, bLen) - 1 DO
      aCh := Text.GetChar(a, i);
      IF aCh = SlashChar THEN aCh := FIRST(CHAR) END;
      bCh := Text.GetChar(b, i);
      IF bCh = SlashChar THEN bCh := FIRST(CHAR) END;
      IF aCh < bCh THEN RETURN -1 END;
      IF aCh > bCh THEN RETURN +1 END;
    END;
    IF aLen < bLen THEN RETURN -1 END;
    IF aLen > bLen THEN RETURN +1 END;
    RETURN 0;
  END PathCompare;

PROCEDURE PutCmd(wr: Wr.T;
                 cmd: TEXT;
                 f0, f1, f2, f3, f4, f5, f6, f7, f8, f9: TEXT := NIL;
		 more := FALSE;
		 encode := FALSE)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    a := ARRAY [0..9] OF TEXT{f0, f1, f2, f3, f4, f5, f6, f7, f8, f9};
    n := NUMBER(a);
  BEGIN
    WHILE n > 0 AND a[n-1] = NIL DO DEC(n) END;
    LOCK wr DO
      IF cmd # NIL THEN
	UnsafeWr.FastPutText(wr, cmd);
      END;
      FOR i := 0 TO n-1 DO
	UnsafeWr.FastPutChar(wr, ' ');
	IF encode THEN
	  UnsafeWr.FastPutText(wr, EncodeWS(a[i]));
	ELSE
	  UnsafeWr.FastPutText(wr, a[i]);
	END;
      END;
      IF NOT more THEN
	UnsafeWr.FastPutChar(wr, '\n');
      END;
    END;
  END PutCmd;

BEGIN
END SupMiscText.
