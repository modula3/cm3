(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE CharRange;
TYPE
  T = SET OF CHAR;
CONST
  NoChars = T{};
  AllChars = T {FIRST(CHAR) .. LAST(CHAR)};
  AllExceptNewline = AllChars - T{'\n'};
  WhiteSpace = T{' ','\t'};
  Letter = T{'A'..'Z','a'..'z','_'};
  Digit = T{'0'..'9'};
  AlphaNum = Letter + Digit;

PROCEDURE FromText(t: TEXT): T;

PROCEDURE FilterText(t: TEXT;
                     replace: T := WhiteSpace;
                     with: CHAR := '\000'): TEXT;
  (* '\000' means delete *)

PROCEDURE Size(a: T): INTEGER;
(* number of chars in the set *)

END CharRange.
