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
 *
 * $Id: SupMiscText.m3,v 1.1.1.1 2009-04-09 17:02:03 jkrell Exp $ *)

(* This module implements the most performance critical text
   manipulation procedures in the "SupMisc" interface.  It uses
   the "TextF" and "UnsafeWr" interfaces for speed. *)

MODULE SupMiscText EXPORTS SupMisc;

IMPORT Pathname, Text, TextF, Thread, UnsafeWr, Wr;

PROCEDURE Cat3(a, b, c: TEXT): TEXT =
  VAR
    t: TEXT;
  BEGIN
    WITH n1 = NUMBER(a^), n2 = NUMBER(b^), n3 = NUMBER(c^) DO
      t := NEW(TEXT, n1 + n2 + n3 - 2);
      SUBARRAY(t^, 0, n1) := a^;
      SUBARRAY(t^, n1 - 1, n2) := b^;
      SUBARRAY(t^, n1 + n2 - 2, n3) := c^;
    END;
    RETURN t;
  END Cat3;

PROCEDURE CatN(READONLY a: ARRAY OF TEXT): TEXT =
  VAR
    len: CARDINAL := 0;
    t: TEXT;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      INC(len, NUMBER(a[i]^) - 1);
    END;
    t := NEW(TEXT, len + 1);
    len := 0;
    FOR i := FIRST(a) TO LAST(a) DO
      WITH n = NUMBER(a[i]^) DO
	SUBARRAY(t^, len, n) := a[i]^;
	INC(len, n - 1);
      END;
    END;
    RETURN t;
  END CatN;

PROCEDURE CommonPathLength(a, b: Pathname.T): CARDINAL =
  VAR
    aLen := NUMBER(a^) - 1;
    bLen := NUMBER(b^) - 1;
    minLen := MIN(aLen, bLen);
    aCh, bCh: CHAR;
    lastSlash: CARDINAL := 0;
  BEGIN
    FOR i := 0 TO minLen-1 DO
      aCh := a[i];
      bCh := b[i];
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
      IF a[minLen] = SlashChar THEN
	RETURN minLen;
      ELSE
	RETURN lastSlash;
      END;
    ELSIF bLen > minLen THEN  (* Path "a" is a prefix of "b". *)
      IF b[minLen] = SlashChar THEN
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
    oldLen := NUMBER(t^) - 1;
    newLen := oldLen;
    i: CARDINAL;
    nt: TEXT;
  BEGIN
    i := 0;
    WHILE i < oldLen DO
      IF t[i] = '\\' THEN  (* Skip second character of escape. *)
	INC(i);
	IF i >= oldLen THEN RAISE InvalidEscape END;
	DEC(newLen);
      END;
      INC(i);
    END;
    IF newLen = oldLen THEN RETURN t END;

    nt := NEW(TEXT, newLen + 1);
    nt[newLen] := '\000';
    i := 0;
    FOR ni := 0 TO newLen - 1 DO
      IF t[i] = '\\' THEN
	(* Note that we don't have to check for passing the end of the text,
	   because there is always an extra NUL byte on the end. *)
	CASE t[i+1] OF
	| '_' => nt[ni] := ' ';
	| 't' => nt[ni] := '\t';
	| 'n' => nt[ni] := '\n';
	| 'r' => nt[ni] := '\r';
	| '\\' => nt[ni] := '\\';
	ELSE
	  RAISE InvalidEscape;
	END;
	INC(i, 2);
      ELSE
	nt[ni] := t[i];
	INC(i);
      END;
    END;
    RETURN nt;
  END DecodeWS;

PROCEDURE EncodeWS(t: TEXT): TEXT =
  VAR
    oldLen := NUMBER(t^) - 1;
    newLen := oldLen;
    nt: TEXT;
    ni: CARDINAL;
  BEGIN
    FOR i := 0 TO oldLen - 1 DO
      IF t[i] IN EscapedChars THEN INC(newLen) END;
    END;
    IF newLen = oldLen THEN RETURN t END;

    nt := NEW(TEXT, newLen + 1);
    nt[newLen] := '\000';
    ni := 0;
    FOR i := 0 TO oldLen - 1 DO
      WITH ch = t[i] DO
	CASE ch OF
	| ' '  => nt[ni] := '\\';  nt[ni+1] := '_';  INC(ni, 2);
	| '\t' => nt[ni] := '\\';  nt[ni+1] := 't';  INC(ni, 2);
	| '\n' => nt[ni] := '\\';  nt[ni+1] := 'n';  INC(ni, 2);
	| '\r' => nt[ni] := '\\';  nt[ni+1] := 'r';  INC(ni, 2);
	| '\\' => nt[ni] := '\\';  nt[ni+1] := '\\';  INC(ni, 2);
	ELSE
	  nt[ni] := ch;
	  INC(ni);
	END;
      END;
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
      aCh := a[i];
      IF aCh = SlashChar THEN aCh := FIRST(CHAR) END;
      bCh := b[i];
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
