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
 * $Id: DevT.m3,v 1.2 2009-04-12 04:55:36 jkrell Exp $ *)

UNSAFE MODULE DevT;

IMPORT
  CText, Ctypes, Fmt, OSError, OSErrorPosix, TokScan, Unix, Utypes, Word, Long;

PROCEDURE Decode(t: TEXT): T
  RAISES {TokScan.Error} =
  BEGIN
    RETURN TokScan.AtoL(t, "dev_t", 16);
  END Decode;

PROCEDURE Encode(READONLY dev: T): TEXT =
  BEGIN
    RETURN Fmt.LongUnsigned(dev, 16);
  END Encode;

PROCEDURE Equal(READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN a = b;
  END Equal;

PROCEDURE Hash(READONLY dev: T): Word.T =
  BEGIN
    RETURN Word.Xor(ORD(Long.RightShift(dev, 32)), ORD(Long.And(dev, 16_FFFFFFFFL)));
  END Hash;

PROCEDURE Mknod(path: TEXT;
                mode: Utypes.mode_t;
		READONLY dev: T) RAISES {OSError.E} =
  VAR
    pathStr: Ctypes.char_star;
    r: Ctypes.int;
  BEGIN
    pathStr := CText.SharedTtoS(path);
    r := Unix.mknod(pathStr, mode, dev);
    CText.FreeSharedS(path, pathStr);
    IF r = -1 THEN
      OSErrorPosix.Raise();
    END;
  END Mknod;

BEGIN
END DevT.
