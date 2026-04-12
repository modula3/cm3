(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Feb 18 13:15:48 PST 1994 by kalsow    *)
(*      modified on Sun Jun  3 14:25:23 1991 by luca          *)

INTERFACE OldLex;
(* Sifting through a reader for booleans, integers, reals, and text strings. *)

IMPORT Rd, Convert;

  EXCEPTION Error;

  CONST
    Blanks = SET OF CHAR {'\t', '\n', '\r', '\f', ' '};
    NonBlanks = SET OF CHAR {'!'..'~'};
    BoolChars = SET OF CHAR {'T','R','U','E','F','A','L','S',
      't','r','u','e','f','a','l','s'};
    DecChars = SET OF CHAR {'-', '+', '0'..'9'};
    HexChars = SET OF CHAR {'0'..'9', 'A'..'F', 'a'..'f'};
    IntChars = SET OF CHAR {'-', '+', '_', '0'..'9', 'A'..'F', 'a'..'f'};
    RealChars = SET OF CHAR {'-', '+', '.', 'D', 'E', 'd', 'e', '0'..'9'};
    (* See Char.i3 for more. *)

<*OBSOLETE*>       
  PROCEDURE Scan(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): TEXT 
    RAISES {Rd.Failure};
    (* Let t be the longest prefix of rd composed of characters in
       cs, and let p be position of rd immediately following t (or EOF).
       Return t, leaving rd at position p. (It may call Rd.UnGetChar.) *)

<*OBSOLETE*>       
  PROCEDURE Skip(rd: Rd.T; READONLY cs: SET OF CHAR := Blanks) 
    RAISES {Rd.Failure};
    (* Let t be the longest initial segment of rd composed of characters in
       cs, and let p be position of rd immediately following t (or EOF).
       Leave rd at position p. (It may call Rd.UnGetChar.) *)

<*OBSOLETE*>       
  PROCEDURE Match(rd: Rd.T; t: TEXT) RAISES {Rd.Failure, Error};
    (* Read the first Text.Length(t) characters of rd and raise Error
       if these are not the same as t or if EOF is encountered. *)

<*OBSOLETE*>       
  PROCEDURE Bool(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): BOOLEAN 
    RAISES {Rd.Failure, Error};
    (* Let t be the result of Scan(rd, cs). 
       Return TRUE if t = "TRUE","True","true","T", or "t".
       Return FALSE if t = "FALSE","False","false","F", or "f".
       Raise Error otherwise. *)

<*OBSOLETE*>       
  PROCEDURE Int(rd: Rd.T; base: Convert.Base := 10; 
    READONLY cs: SET OF CHAR := NonBlanks): INTEGER 
    RAISES {Rd.Failure, Convert.Failed, Error};
    (* Inteprets the longest prefix of rd made of cs's as an integer of 
       given base. It does so by applying Convert.ToInt(buf, (*VAR*)used, base) 
       to a buf obtained by Scan(rd, cs). Raises Error if Convert.ToInt does 
       not use the entire prefix (i.e. if used#NUMBER(buf)). *)

<*OBSOLETE*>       
  PROCEDURE Unsigned(rd: Rd.T; base: Convert.Base := 10; 
    READONLY cs: SET OF CHAR := NonBlanks): INTEGER 
    RAISES {Rd.Failure, Convert.Failed, Error};
    (* Inteprets the longest prefix of rd made of cs's as an unsigned of given
       base. It does so by applying Convert.ToUnsigned(buf, (*VAR*)used, base) 
       to a buf obtained by Scan(rd, cs). Raises Error if Convert.ToUnsigned 
       does not use the entire prefix (i.e. if used#NUMBER(buf)). *)

<*OBSOLETE*>       
  PROCEDURE Real(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): REAL
    RAISES {Rd.Failure, Convert.Failed, Error};
    (* Inteprets the longest prefix of rd made of cs's as a real.
       It does so by applying Convert.ToFloat to the text obtained by 
       Scan(rd, cs). Raises Error if Convert.ToFloat does not use the entire 
       prefix. *)

<*OBSOLETE*>       
  PROCEDURE LongReal(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): LONGREAL 
    RAISES {Rd.Failure, Convert.Failed, Error};
    (* Inteprets the longest prefix of rd made of cs's as a real.
       It does so by applying Convert.ToLongFloat to the text obtained by 
       Scan(rd, cs). Raises Error if Convert.ToLongFloat does not use the 
       entire prefix. *)

  (* Not implemented
  PROCEDURE Extended(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): EXTENDED
    RAISES {Rd.Failure, Convert.Failed, Error};
       Inteprets the longest prefix of rd made of cs's as an extended.
       It does so by applying Convert.ToExtended to the text obtained by 
       Scan(rd, cs). Raises Error if Convert.ToExtended does not use the entire 
       prefix. *)

END OldLex.
