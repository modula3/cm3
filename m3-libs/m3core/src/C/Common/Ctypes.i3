(* Copyright (C) 1989, 1990, Digital Equipment Corporation     *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri May  7 11:00:07 PDT 1993 by muller     *)
(*      modified on Wed Feb  7 15:49:32 1990 by jerome         *)

INTERFACE Ctypes;

IMPORT BasicCtypes;

TYPE
  char                       = BasicCtypes.char;
  signed_char                = BasicCtypes.signed_char;
  short_int                  = BasicCtypes.short_int;
  int                        = BasicCtypes.int;
  long_int                   = BasicCtypes.long_int;
  unsigned_char              = BasicCtypes.unsigned_char;
  unsigned_short_int         = BasicCtypes.unsigned_short_int;
  unsigned_int               = BasicCtypes.unsigned_int;
  unsigned_long_int          = BasicCtypes.unsigned_long_int;
  float                      = BasicCtypes.float;
  double                     = BasicCtypes.double;
  long_double                = BasicCtypes.long_double;

  (* void *)
  void_star                  = ADDRESS;
  const_void_star            = void_star;

  (* Alternate names for the basic types *)
  short                      = short_int;
  signed_short               = short_int;
  signed_short_int           = short_int;
  unsigned_short             = unsigned_short_int;
  signed                     = int;
  signed_int                 = int;
  long                       = long_int;
  signed_long                = long_int;
  signed_long_int            = long_int;
  unsigned_long              = unsigned_long_int;

  (* Qualified and derived types *)
  char_star                  = UNTRACED REF char;
  char_star_star             = UNTRACED REF char_star;
  char_star_star_star        = UNTRACED REF char_star_star;
  unsigned_char_star         = UNTRACED REF unsigned_char;
  unsigned_char_star_star    = UNTRACED REF unsigned_char_star;
  const_char_star            = char_star;
  const_char_star_star       = char_star_star;
  const_unsigned_char_star   = unsigned_char_star;

  short_star                 = UNTRACED REF short;
  short_star_star            = UNTRACED REF short_star;
  unsigned_short_star        = UNTRACED REF unsigned_short;  

  int_star                   = UNTRACED REF int;
  int_star_star              = UNTRACED REF int_star;
  unsigned_int_star          = UNTRACED REF unsigned_int;  

  long_star                  = UNTRACED REF long;
  long_star_star             = UNTRACED REF long_star;
  unsigned_long_star         = UNTRACED REF unsigned_long;

  float_star                 = UNTRACED REF float;
  double_star                = UNTRACED REF double;

END Ctypes.


