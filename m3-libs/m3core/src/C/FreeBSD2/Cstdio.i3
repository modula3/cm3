(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Sat Jan  7 13:12:39 PST 1995 by kalsow    *)
(*      modified on Thu May  6 09:18:19 PDT 1993 by muller    *)
(*      Olaf Wagner 16.09.1994                                *)

INTERFACE Cstdio;

FROM Ctypes IMPORT int, void_star, char_star,
                   unsigned_char_star, short_int, unsigned_char;

FROM Utypes IMPORT off_t;

CONST 
  NIOBRW = 100;
  NSTDBUF= 3;
  EOF    = VAL(-1, int);

TYPE
  SBUF = RECORD
            base : unsigned_char_star;
            size : int;
         END;

  FILE = RECORD 
        p     : unsigned_char_star;  (* current position in (some) buffer *)
        r     : int;                 (* read space left for getc() *)
        w     : int;                 (* write space left for putc() *)
        flags : short_int;           (* flags, below; this FILE is free if 0 *)
        file  : short_int;           (* fileno, if Unix descriptor, else -1 *)
        bf    : SBUF;                (* the buffer (at least 1 byte, if !NULL) *)
        lbfsize : int;               (* 0 or -_bf._size, for inline putc *)

        (* operations *)
        cookie : void_star;          (* cookie passed to io functions *)
        xxclose: void_star;
        xxread : void_star;
        xxseek : void_star;
        xxwrite: void_star;

        (* separate buffer for long sequences of ungetc() *)
        ub : SBUF;               (* ungetc buffer *)
        up : unsigned_char_star; (* saved _p when _p is doing ungetc data *)
        ur : int;                (* saved _r when _r is counting ungetc data *)
                                 
        (* tricks to meet minimum requirements even when malloc() fails *)
        ubuf : ARRAY[0..2] OF unsigned_char; (* guarantee an ungetc() buffer *)
        nbuf : ARRAY[0..0] OF unsigned_char; (* guarantee a getc() buffer *)

        (* separate buffer for fgetln() when line crosses buffer boundary *)
        lb   : SBUF;         (* buffer for fgetln() *)

        (* Unix stdio files get aligned to block boundaries on fseek() *)
        blksize : int;        (* stat.st_blksize (may be != _bf._size) *)
        offset  : off_t;      (* current lseek offset *)
 
        END;

  FILE_star = UNTRACED REF FILE;

<*EXTERNAL "__sF"*> VAR sF : ARRAY [0..NSTDBUF-1] OF FILE;
                    VAR iF : ARRAY [0..NIOBRW-1]  OF FILE_star;

<*EXTERNAL fbsd_feof*>      PROCEDURE feof (f: FILE_star): int;
<*EXTERNAL fbsd_getc*>      PROCEDURE getc (f: FILE_star): int;
<*EXTERNAL fbsd_ungetc*>    PROCEDURE ungetc (c: int; f: FILE_star): int;
<*EXTERNAL fbsd_putc*>      PROCEDURE putc (c: int; f: FILE_star): int;
<*EXTERNAL fbsd_fflush*>    PROCEDURE fflush (f: FILE_star): int;
<*EXTERNAL fbsd_fdopen*>    PROCEDURE fdopen (fd: int; mode: char_star): FILE_star;
<*EXTERNAL fbsd_fclose*>    PROCEDURE fclose (f: FILE_star): int;

END Cstdio.
