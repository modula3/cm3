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
 *)

(* From John D. Polstra's Ugzip.m3. *)

UNSAFE MODULE zlib;

IMPORT Cstdlib;
FROM Ctypes IMPORT int, unsigned, void_star, unsigned_char_star;

VAR ZLIB_VERSION_CHAR_ARRAY :=
        ARRAY [0..5] OF CHAR {'1', '.', '2', '.', '3', '\000'};
VAR ZLIB_VERSION := ADR(ZLIB_VERSION_CHAR_ARRAY);

PROCEDURE deflateInit(strm: z_streamp; level: int): int =
  BEGIN
    RETURN deflateInit_(strm, level, ZLIB_VERSION, BYTESIZE(z_stream));
  END deflateInit;

PROCEDURE inflateInit(strm: z_streamp): int =
  BEGIN
    RETURN inflateInit_(strm, ZLIB_VERSION, BYTESIZE(z_stream));
  END inflateInit;

PROCEDURE deflateInit2(strm: z_streamp;
                       level: int;
                       method: int;
                       windowBits: int;
                       memLevel: int;
                       strategy: int): int =
  BEGIN
    RETURN deflateInit2_(strm, level, method, windowBits, memLevel,
                         strategy, ZLIB_VERSION, BYTESIZE(z_stream));
  END deflateInit2;

PROCEDURE inflateInit2(strm: z_streamp;
                       windowBits: int): int =
  BEGIN
    RETURN inflateInit2_(strm, windowBits,
                         ZLIB_VERSION, BYTESIZE(z_stream));
  END inflateInit2;

PROCEDURE inflateBackInit(strm: z_streamp;
                          windowBits: int;
                          window: unsigned_char_star): int =
  BEGIN
    RETURN inflateBackInit_(strm, windowBits, window,
                            ZLIB_VERSION, BYTESIZE(z_stream));
  END inflateBackInit;

PROCEDURE malloc(<*UNUSED*> opaque: void_star;
                 items: unsigned;
                 size: unsigned): void_star =
  BEGIN
    RETURN Cstdlib.malloc(items * size);
  END malloc;

PROCEDURE free(<*UNUSED*> opaque: void_star; address: void_star) =
  BEGIN
    Cstdlib.free(address);
  END free;

BEGIN
END zlib.
