(* Copyright (C) 1989, 1990, Digital Equipment Corporation     *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May  6 12:08:19 PDT 1993 by muller     *)
(*      modified on Wed Feb  7 15:49:32 1990 by jerome         *)

INTERFACE BasicCtypes;

IMPORT Word, Long;

TYPE
  (* the four signed integer types *)
  signed_char        = BITS 8 FOR [-16_7f-1 .. 16_7f];
  short_int          = BITS 16 FOR [-16_7fff-1 .. 16_7fff];
  int                = BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff];
  long_int           = [-16_7fffffffffffffff -1  .. 16_7fffffffffffffff ]; (* implicitly 64 bits *)
  long_long          = [-16_7fffffffffffffffL-1L .. 16_7fffffffffffffffL]; (* implicitly 64 bits *)

  (* the four unsigned integer types *)
  unsigned_char      = BITS 8 FOR [16_0 .. 16_ff];
  unsigned_short_int = BITS 16 FOR [16_0 .. 16_ffff];
  unsigned_int       = BITS 32 FOR [16_0 .. 16_ffffffff];
  unsigned_long_int  = Word.T;	(* implicitly 64 bits *)
  unsigned_long_long = Long.T;	(* implicitly 64 bits *)

  (* the three floating types *)
  float              = REAL;
  double             = LONGREAL;
  long_double        = EXTENDED;

  (* char *)
  char               = signed_char;

END BasicCtypes.


