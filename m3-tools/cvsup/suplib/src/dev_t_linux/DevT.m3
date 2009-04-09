(* Copyright 1999-2003 John D. Polstra.
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

UNSAFE MODULE DevT;

IMPORT
  CText, Ctypes, DevTLinux, Fmt, OSError, OSErrorPosix, Text, TokScan,
  Utypes, Word;

PROCEDURE Decode(t: TEXT): T
  RAISES {TokScan.Error} =
  VAR
    dev: T;
    len := Text.Length(t);
    chunkLen := 2 * BYTESIZE(dev[0]);
  BEGIN
    IF len > chunkLen * NUMBER(dev) THEN
      RAISE TokScan.Error("encoded dev_t too long");
    END;
    (* XXX - This is specific to little-endian machines. *)
    FOR i := FIRST(dev) TO LAST(dev) DO
      IF len > 0 THEN
	WITH start = MAX(len - chunkLen, 0) DO
	  dev[i] := TokScan.AtoI(Text.Sub(t, start, len - start),
	    "dev_t chunk", 16);
	  len := start;
	END;
      ELSE
	dev[i] := 0;
      END;
    END;
    RETURN dev;
  END Decode;

PROCEDURE Encode(READONLY dev: T): TEXT =
  VAR
    i: INTEGER;
    t: TEXT;
    chunkLen := 2 * BYTESIZE(dev[0]);
  BEGIN
    (* XXX - This is specific to little-endian machines. *)
    i := LAST(dev);
    WHILE i > FIRST(dev) AND dev[i] = 0 DO
      DEC(i);
    END;
    t := Fmt.Unsigned(dev[i], 16);
    WHILE i > FIRST(dev) DO
      DEC(i);
      t := t & Fmt.Pad(Fmt.Unsigned(dev[i], 16), chunkLen, '0');
    END;
    RETURN t;
  END Encode;

PROCEDURE Equal(READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN a = b;
  END Equal;

PROCEDURE Hash(READONLY dev: T): Word.T =
  VAR
    h: Word.T := 0;
  BEGIN
    FOR i := FIRST(dev) TO LAST(dev) DO
      h := Word.Xor(h, dev[i]);
    END;
    RETURN h;
  END Hash;

PROCEDURE Mknod(path: TEXT;
                mode: Utypes.mode_t;
		READONLY dev: T) RAISES {OSError.E} =
  VAR
    pathStr: Ctypes.char_star;
    r: Ctypes.int;
  BEGIN
    pathStr := CText.SharedTtoS(path);
    r := DevTLinux.mknod(pathStr, mode, dev);
    CText.FreeSharedS(path, pathStr);
    IF r = -1 THEN
      OSErrorPosix.Raise();
    END;
  END Mknod;

BEGIN
END DevT.
