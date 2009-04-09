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
 * $Id: RCSRevNum.m3,v 1.1.1.1 2009-04-09 17:01:58 jkrell Exp $ *)

MODULE RCSRevNum;

IMPORT Text, Word;

PROCEDURE Cat(r1, r2: T): T =
  BEGIN
    RETURN r1 & "." & r2;
  END Cat;

PROCEDURE Compare(r1, r2: T): [-1..1] =
  VAR
    l1 := Text.Length(r1);
    l2 := Text.Length(r2);
    lMin := MIN(l1, l2);
    ch1, ch2: CHAR;
    dot1, dot2: INTEGER;
  BEGIN
    FOR i := 0 TO lMin-1 DO
      ch1 := Text.GetChar(r1, i);
      ch2 := Text.GetChar(r2, i);
      IF ch1 # ch2 THEN
	(* We have found the first non-matching characters of the revision
	   numbers.  Find the distance to the next dot in each. *)
	dot1 := Text.FindChar(r1, '.', i);
	IF dot1 = -1 THEN dot1 := l1 END;
	dot2 := Text.FindChar(r2, '.', i);
	IF dot2 = -1 THEN dot2 := l2 END;
	IF dot1 # dot2 THEN
	  (* The non-matching components have different lengths.  The
	     longer one is greater. *)
	  IF dot1 < dot2 THEN RETURN -1 ELSE RETURN 1 END;
	ELSE
	  (* The non-matching components have the same length.  The first
	     non-matching digits provide the comparison. *)
	  IF ch1 < ch2 THEN RETURN -1 ELSE RETURN 1 END;
	END;
      END;
    END;

    (* One revision number is a prefix of the other.  The longer one is
       greater. *)
    IF l1 # l2 THEN
      IF l1 < l2 THEN RETURN -1 ELSE RETURN 1 END;
    END;
    RETURN 0;
  END Compare;

PROCEDURE Equal(r1, r2: T): BOOLEAN =
  BEGIN
    RETURN Text.Equal(r1, r2);
  END Equal;

PROCEDURE Hash(r: T): Word.T =
  BEGIN
    RETURN Text.Hash(r);
  END Hash;

PROCEDURE IsTrunk(r: T): BOOLEAN =
  BEGIN
    WITH n = Text.FindChar(r, '.') DO
      RETURN n >= 0 AND Text.FindChar(r, '.', n+1) = -1;
    END;
  END IsTrunk;

PROCEDURE Last(r: T): T =
  BEGIN
    WITH pos = Text.FindCharR(r, '.') DO
      IF pos = -1 THEN
	RETURN r;
      ELSE
	RETURN Text.Sub(r, pos+1);
      END;
    END;
  END Last;

PROCEDURE NumParts(r: T): CARDINAL =
  VAR
    np := 1;
    pos := Text.FindChar(r, '.');
  BEGIN
    WHILE pos >= 0 DO
      INC(np);
      pos := Text.FindChar(r, '.', pos+1);
    END;
    RETURN np;
  END NumParts;

PROCEDURE Prefix(r: T): T =
  BEGIN
    WITH pos = Text.FindCharR(r, '.') DO
      IF pos = -1 THEN
	RETURN "";
      ELSE
	RETURN Text.Sub(r, 0, pos);
      END;
    END;
  END Prefix;

PROCEDURE IsCVSBranch(r: T): BOOLEAN =
  (* <=> n > 2 AND n MOD 2 = 1 OR 
         n > 2 AND n MOD 2 = 0 AND Last(Prefix(r)) = 0
         for n = NumParts(r) *)
  VAR n := NumParts(r);
  BEGIN
    RETURN n > 2 AND n MOD 2 = 1 OR
           n > 2 AND n MOD 2 = 0 AND Text.Equal(Last(Prefix(r)), "0");
  END IsCVSBranch;

BEGIN
END RCSRevNum.
