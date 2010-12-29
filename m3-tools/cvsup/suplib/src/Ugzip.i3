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

(* The "Ugzip" interface provides low-level access to the "zlib"
   compression library. *)

INTERFACE Ugzip;

FROM Ctypes IMPORT char_star, const_char_star, unsigned_char_star,
  int, unsigned_int, unsigned_long, void_star;

CONST
  ZLIB_VERSION = "1.0.4";

  (* Flush values *)
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  (* Return codes *)
  Z_OK            =  0;
  Z_STREAM_END    =  1;
  Z_NEED_DICT     =  2;
  Z_ERRNO         = -1;
  Z_STREAM_ERROR  = -2;
  Z_DATA_ERROR    = -3;
  Z_MEM_ERROR     = -4;
  Z_BUF_ERROR     = -5;
  Z_VERSION_ERROR = -6;

  (* Compression levels *)
  Z_NO_COMPRESSION      =  0;
  Z_BEST_SPEED          =  1;
  Z_BEST_COMPRESSION    =  9;
  Z_DEFAULT_COMPRESSION = -1;

  (* Data type values *)
  Z_BINARY  = 0;
  Z_ASCII   = 1;
  Z_UNKNOWN = 2;

TYPE
  AllocFunc = PROCEDURE(opaque: void_star;
		        items: unsigned_int;
			size: unsigned_int): void_star;

  FreeFunc = PROCEDURE(opaque: void_star;
		       address: void_star);

  z_stream = RECORD
    next_in: unsigned_char_star;
    avail_in: unsigned_int;
    total_in: unsigned_long;

    next_out: unsigned_char_star;
    avail_out: unsigned_int;
    total_out: unsigned_long;

    msg: char_star;
    state: void_star;

    zalloc: AllocFunc;
    zfree: FreeFunc;
    opaque: void_star;

    data_type: int;
    adler: unsigned_long;
    reserved: unsigned_long;
  END;

  z_stream_star = UNTRACED REF z_stream;

<*EXTERNAL*>
VAR zlib_version: const_char_star;

PROCEDURE deflateInit(strm: z_stream_star; level: int): int;

<*EXTERNAL*>
PROCEDURE deflate(strm: z_stream_star; flush: int): int;

<*EXTERNAL*>
PROCEDURE deflateEnd(strm: z_stream_star): int;

PROCEDURE inflateInit(strm: z_stream_star): int;

<*EXTERNAL*>
PROCEDURE inflate(strm: z_stream_star; flush: int): int;

<*EXTERNAL*>
PROCEDURE inflateEnd(strm: z_stream_star): int;

(* The following procedures are trivial wrappers around malloc
   and free. They should be used for the "zalloc" and "zfree" fields
   of "z_stream" structures. *)

PROCEDURE malloc(opaque: void_star;
                 items: unsigned_int;
                 size: unsigned_int): void_star;

PROCEDURE free(opaque: void_star;
               address: void_star);

(* Procedures below this point should not be used directly *)

<*EXTERNAL*>
PROCEDURE deflateInit_(strm: z_stream_star;
                       level: int;
		       version: const_char_star;
		       stream_size: int): int;

<*EXTERNAL*>
PROCEDURE inflateInit_(strm: z_stream_star;
		       version: const_char_star;
		       stream_size: int): int;

END Ugzip.
