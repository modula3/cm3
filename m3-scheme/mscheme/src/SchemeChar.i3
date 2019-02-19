(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeChar;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

TYPE T <: REFANY;

(* all chars here are SHARED, so don't overwrite them, eh? *)

PROCEDURE Char(x : SchemeObject.T) : CHAR RAISES { E };

PROCEDURE Chr(x : SchemeObject.T) : T RAISES { E };

PROCEDURE IChr(x : INTEGER) : T;
  
PROCEDURE Character(c : CHAR) : T;

CONST Brand = "SchemeChar";

(**********************************************************************)

CONST White = SET OF CHAR { '\t', ' ', '\n', '\r' };

      Delims = SET OF CHAR { '(', ')', '\'', ';', '"', ',', '`' };
      
      NumberChars = SET OF CHAR { '.', '+', '-', '0' .. '9' };

      LowerCase = SET OF CHAR { 'a' .. 'z' };
      
      UpperCase = SET OF CHAR { 'A' .. 'Z' };

      Digits = SET OF CHAR { '0' .. '9' };

PROCEDURE Upcase(c : CHAR) : CHAR;

PROCEDURE Downcase(c : CHAR) : CHAR;

END SchemeChar.
