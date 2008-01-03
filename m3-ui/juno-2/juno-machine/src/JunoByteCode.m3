(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun  8 17:05:43 PDT 1994 by heydon                   *)

MODULE JunoByteCode;

CONST Invalid = "INVALID";

BEGIN
  names := ARRAY [PUSHL..SOLVE] OF TEXT{
    "PUSHL", "PUSHG", "POPL", "POPG", "INCSP", "DECSP",            (* 1 -  9 *)
       "PUSHM3NIL", "PUSHNIL", "PUSHNUM",
    "C-OFF", "C-ON",                                              (* 10 - 11 *)
    Invalid, Invalid, Invalid,
    "JUMP", "TJUMP", "FJUMP", "UJUMP",                            (* 15 - 18 *)
    Invalid,
    "CALL", "RET", "ERROR", "FERROR", "CALLEXT",                  (* 20 - 24 *)
    "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "DIV", "MOD",        (* 25 - 43 *)
      "NEGATE", "ABS", "FLOOR", "CEILING", "ROUND", "MAX", "MIN",
      "ATAN", "SIN", "COS", "LN", "EXP", "REL",
    Invalid,
    "CAR", "CDR", "CAR-CDR", "CONS", "LIST", "CONCAT",            (* 45 - 50 *)
    Invalid, Invalid, Invalid, Invalid,
    "IS-REAL", "IS-INT", "IS-TEXT", "IS-PAIR",                    (* 55 - 58 *)
    Invalid,
    "EQUAL", "LESS", "AT-MOST",                                   (* 60 - 62 *)
    Invalid, Invalid,
    "CONG", "PARA", "HOR", "VER",                                 (* 65 - 68 *)
    Invalid,
    "NEWCL", "NEWEXTCL", "CLOSE", "APPLY", "CLDECSP",             (* 70 - 74 *)
    "SOLVE"}                                                           (* 75 *)
END JunoByteCode.
