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
 * $Id: Umd5.i3,v 1.1.1.1 2009-04-09 17:02:01 jkrell Exp $ *)

(* The "Umd5" interface provides low-level access to the MD5 library. *)

INTERFACE Umd5;

FROM Ctypes IMPORT char_star, unsigned_char_star, const_unsigned_char_star,
  unsigned_char, unsigned_int, unsigned_long;

TYPE
  MD5_CTX = RECORD
    state: ARRAY [0..3] OF unsigned_long;
    count: ARRAY [0..1] OF unsigned_long;
    buffer: ARRAY [0..63] OF unsigned_char;
  END;

  MD5_CTX_star = UNTRACED REF MD5_CTX;

<*EXTERNAL*>
PROCEDURE MD5Init(ctx: MD5_CTX_star);

<*EXTERNAL*>
PROCEDURE MD5Update(ctx: MD5_CTX_star;
                    data: const_unsigned_char_star;
		    len: unsigned_int);

<*EXTERNAL*>
PROCEDURE MD5Final(rslt: unsigned_char_star;
                   ctx: MD5_CTX_star);

<*EXTERNAL*>
PROCEDURE MD5End(ctx: MD5_CTX_star;
                 buf: char_star): char_star;

<*EXTERNAL*>
PROCEDURE MD5File(filename: char_star;
                  buf: char_star): char_star;

<*EXTERNAL*>
PROCEDURE MD5Data(data: const_unsigned_char_star;
                  len: unsigned_int;
		  buf: char_star): char_star;

END Umd5.
