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

(* From John D. Polstra's Ugzip.i3, and UgzipP.i3. *)

INTERFACE zlib;

FROM Ctypes IMPORT char_star, const_char_star, unsigned_char_star,
  int, unsigned, unsigned_long, void_star, unsigned_char_star_star;

CONST
  (* ZLIB_VERSION = "1.2.3"; *)
  ZLIB_VERNUM  = 16_1230;

  (* Flush values *)
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;

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

  (* compression strategy; see /usr/include/zlib.h *)
  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_RLE              = 3;
  Z_FIXED            = 4;
  Z_DEFAULT_STRATEGY = 0;

  (* Data type values *)
  Z_BINARY  = 0;
  Z_TEXT    = 1;
  Z_ASCII   = Z_TEXT; (* for compatibility with 1.2.2 and earlier *)
  Z_UNKNOWN = 2;

TYPE
  alloc_func = PROCEDURE(opaque: void_star;
                         items: unsigned;
                         size: unsigned): void_star;

  free_func = PROCEDURE(opaque: void_star;
                        address: void_star);

  z_stream = RECORD
    next_in: unsigned_char_star;
    avail_in: unsigned;
    total_in: unsigned_long;

    next_out: unsigned_char_star;
    avail_out: unsigned;
    total_out: unsigned_long;

    msg: char_star;
    state: void_star;

    zalloc: alloc_func;
    zfree: free_func;
    opaque: void_star;

    data_type: int;
    adler: unsigned_long;
    reserved: unsigned_long;
  END;

  z_streamp = UNTRACED REF z_stream;
  z_stream_star = z_streamp; (* compatibility *)

(*
     gzip header information passed to and from zlib routines.  See RFC 1952
  for more details on the meanings of these fields.
*)
gz_header = RECORD
  text:         int;
  time:         unsigned_long;
  xflags:       int;
  os:           int;
  extra:        ADDRESS;
  extra_len:    unsigned;
  extra_max:    unsigned;
  name:         ADDRESS;
  name_max:     unsigned;
  comment:      ADDRESS;
  comm_max:     unsigned;
  hcrc:         int;
  done:         int;
END;

gz_headerp = UNTRACED REF gz_header;

<*EXTERNAL*>
PROCEDURE zlibVersion(): const_char_star;

PROCEDURE deflateInit(strm: z_streamp; level: int): int;

<*EXTERNAL*>
PROCEDURE deflate(strm: z_streamp; flush: int): int;

<*EXTERNAL*>
PROCEDURE deflateEnd(strm: z_streamp): int;

<*EXTERNAL*>
PROCEDURE inflateBackEnd(strm: z_streamp): int;

PROCEDURE inflateInit(strm: z_streamp): int;

<*EXTERNAL*>
PROCEDURE inflate(strm: z_streamp; flush: int): int;

<*EXTERNAL*>
PROCEDURE inflateEnd(strm: z_streamp): int;

(* The following just call Cstdlib.malloc/free. They should be used for the
   "zalloc" and "zfree" fields of "z_stream" structures. *)

PROCEDURE malloc(opaque: void_star;
                 items: unsigned;
                 size: unsigned): void_star;

PROCEDURE free(opaque: void_star;
               address: void_star);

TYPE in_func = PROCEDURE(a: void_star; b: unsigned_char_star_star): unsigned;
TYPE out_func = PROCEDURE(a: void_star; b: unsigned_char_star; c: unsigned): int;

<*EXTERNAL*>
PROCEDURE inflateBack(strm: z_streamp;
                      in: in_func;
                      in_desc: void_star;
                      out: out_func;
                      out_desc: void_star): int;

PROCEDURE deflateInit2(strm: z_streamp;
                       level: int;
                       method: int;
                       windowBits: int;
                       memLevel: int;
                       strategy: int): int;

<*EXTERNAL*>
PROCEDURE deflateSetDictionary(strm: z_streamp;
                               dictionary: ADDRESS;
                               dictLength: unsigned): int;

<*EXTERNAL*>
PROCEDURE deflateCopy(dest: z_streamp;
                      source: z_streamp): int;

<*EXTERNAL*>
PROCEDURE deflateReset(strm: z_streamp): int;

<*EXTERNAL*>
PROCEDURE deflateParams(strm: z_streamp;
                        level: int;
                        strategy: int): int;

<*EXTERNAL*>
PROCEDURE deflateTune(strm: z_streamp;
                      good_length: int;
                      max_lazy: int;
                      nice_length: int;
                      max_chain: int): int;

<*EXTERNAL*>
PROCEDURE deflateBound(strm: z_streamp;
                       sourceLen: unsigned_long): unsigned_long;

<*EXTERNAL*>
PROCEDURE deflatePrime(strm: z_streamp;
                       bits: int;
                       value: int): int;

<*EXTERNAL*>
PROCEDURE deflateSetHeader(strm: z_streamp;
                           head: gz_headerp): int;

PROCEDURE inflateInit2(strm: z_streamp;
                       windowBits: int): int;

<*EXTERNAL*>
PROCEDURE inflateSetDictionary(strm: z_streamp;
                               dictionary: ADDRESS;
                               dictLength: unsigned): int;

<*EXTERNAL*>
PROCEDURE inflateSync(strm: z_streamp): int;

<*EXTERNAL*>
PROCEDURE inflateCopy(dest: z_streamp;
                      source: z_streamp): int;

<*EXTERNAL*>
PROCEDURE inflateReset(strm: z_streamp): int;

<*EXTERNAL*>
PROCEDURE inflateGetHeader(strm: z_streamp;
                           head: gz_headerp): int;

<*EXTERNAL*>
PROCEDURE inflatePrime(strm: z_streamp;
                       bits: int;
                       value: int): int;

PROCEDURE inflateBackInit(strm: z_streamp;
                          windowBits: int;
                          window: unsigned_char_star): int;



CONST Z_DEFLATED = 8;

(*
ZEXTERN uLong ZEXPORT zlibCompileFlags OF((void));

ZEXTERN int ZEXPORT compress OF((Bytef *dest,   uLongf *destLen,
                                 const Bytef *source, uLong sourceLen));


ZEXTERN int ZEXPORT compress2 OF((Bytef *dest,   uLongf *destLen,
                                  const Bytef *source, uLong sourceLen,
                                  int level));


ZEXTERN uLong ZEXPORT compressBound OF((uLong sourceLen));
ZEXTERN int ZEXPORT uncompress OF((Bytef *dest,   uLongf *destLen,
                                   const Bytef *source, uLong sourceLen));

TYPE gzFile = ADDRESS;

ZEXTERN gzFile ZEXPORT gzopen  OF((const char *path, const char *mode));

ZEXTERN gzFile ZEXPORT gzdopen  OF((int fd, const char *mode));
ZEXTERN int ZEXPORT gzsetparams OF((gzFile file, int level, int strategy));
ZEXTERN int ZEXPORT    gzread  OF((gzFile file, voidp buf, unsigned len));
ZEXTERN int ZEXPORT    gzwrite OF((gzFile file,
                                   voidpc buf, unsigned len));
ZEXTERN int ZEXPORTVA   gzprintf OF((gzFile file, const char *format, ...));

ZEXTERN int ZEXPORT gzputs OF((gzFile file, const char *s));
ZEXTERN char * ZEXPORT gzgets OF((gzFile file, char *buf, int len));
ZEXTERN int ZEXPORT    gzputc OF((gzFile file, int c));
ZEXTERN int ZEXPORT    gzgetc OF((gzFile file));
ZEXTERN int ZEXPORT    gzungetc OF((int c, gzFile file));
ZEXTERN int ZEXPORT    gzflush OF((gzFile file, int flush));
ZEXTERN z_off_t ZEXPORT    gzseek OF((gzFile file,
                                      z_off_t offset, int whence));
ZEXTERN int ZEXPORT    gzrewind OF((gzFile file));
ZEXTERN z_off_t ZEXPORT    gztell OF((gzFile file));
ZEXTERN int ZEXPORT gzeof OF((gzFile file));
ZEXTERN int ZEXPORT gzdirect OF((gzFile file));
ZEXTERN int ZEXPORT    gzclose OF((gzFile file));
ZEXTERN const char * ZEXPORT gzerror OF((gzFile file, int *errnum));
ZEXTERN void ZEXPORT gzclearerr OF((gzFile file));
ZEXTERN uLong ZEXPORT adler32 OF((uLong adler, const Bytef *buf, uInt len));
ZEXTERN uLong ZEXPORT adler32_combine OF((uLong adler1, uLong adler2,
                                          z_off_t len2));
ZEXTERN uLong ZEXPORT crc32   OF((uLong crc, const Bytef *buf, uInt len));
ZEXTERN uLong ZEXPORT crc32_combine OF((uLong crc1, uLong crc2, z_off_t len2));
*)

(* private *)

<*EXTERNAL*>
PROCEDURE deflateInit_(strm: z_streamp;
                       level: int;
                       version: const_char_star;
                       stream_size: int): int;

<*EXTERNAL*>
PROCEDURE inflateInit_(strm: z_streamp;
                       version: const_char_star;
                       stream_size: int): int;

<*EXTERNAL*>
PROCEDURE inflateBackInit_(strm: z_streamp;
                           windowBits: int;
                           window: unsigned_char_star;
                           version: const_char_star;
                           stream_size: int): int;

<*EXTERNAL*>
PROCEDURE inflateInit2_(strm: z_streamp;
                        windowBits: int;
                        version: const_char_star;
                        stream_size: int): int;

<*EXTERNAL*>
PROCEDURE deflateInit2_(strm: z_streamp;
                        level: int;
                        method: int;
                        windowBits: int;
                        memLevel: int;
                        strategy: int;
                        version: const_char_star;
                        stream_size: int): int;

<*EXTERNAL*>
PROCEDURE zError(a: int): const_char_star;

<*EXTERNAL*>
PROCEDURE inflateSyncPoint(z: z_streamp): int;

<*EXTERNAL*>
PROCEDURE get_crc_table(): UNTRACED REF unsigned_long;

END zlib.
