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
 * $Id$ *)

UNSAFE MODULE CText;

IMPORT Cstring, Ctypes, M3toC, Text8;

PROCEDURE SharedTtoS(t: TEXT): Ctypes.char_star =
  BEGIN
    RETURN M3toC.SharedTtoS(t);
  END SharedTtoS;

PROCEDURE FreeSharedS(t: TEXT; s: Ctypes.char_star) =
  BEGIN
    M3toC.FreeSharedS(t, s);
  END FreeSharedS;

PROCEDURE CopyMtoT(mem: Ctypes.void_star; len: CARDINAL): TEXT =
  VAR
    t := Text8.Create(len);
  BEGIN
    EVAL Cstring.memcpy(ADR(t.contents[0]), mem, len);
    RETURN t;
  END CopyMtoT;

PROCEDURE CopyQuotedMtoT(mem: Ctypes.void_star; len: CARDINAL): TEXT =
  VAR
    t: Text8.T;
    p: UNTRACED REF CHAR := mem;
    lim: UNTRACED REF CHAR := mem + len;
    tLen := len;
  BEGIN
    (* Calculate the text length, accounting for doubled '@' characters. *)
    WHILE p < lim DO
      IF p^ = '@' THEN
	INC(p);
	<* ASSERT p < lim *>
	<* ASSERT p^ = '@' *>
	DEC(tLen);
      END;
      INC(p);
    END;

    (* Copy the string into the text, eliminating extra '@' characters. *)
    t := Text8.Create(tLen);
    p := mem;
    FOR i := 0 TO tLen-1 DO
      t.contents[i] := p^;
      IF p^ = '@' THEN INC(p) END;
      INC(p);
    END;
    RETURN t;
  END CopyQuotedMtoT;

BEGIN
END CText.
