(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id: SIsuffix.m3,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)

MODULE SIsuffix;
IMPORT Text;
IMPORT Scan;
IMPORT SuffixTbl;
IMPORT Lex, FloatMode;

PROCEDURE Int(text : TEXT; mode : Mode) : INTEGER RAISES { OutOfRange, UnknownSuffix, FloatMode.Trap, Lex.Error } =
  CONST
    firstInt = FLOAT(FIRST(INTEGER), LONGREAL);
    lastInt =  FLOAT(LAST(INTEGER),  LONGREAL);
  VAR
    longReal := LongReal(text,mode);
  BEGIN
    IF longReal < firstInt OR longReal > lastInt THEN RAISE OutOfRange END;
    RETURN ROUND(longReal)
  END Int;

PROCEDURE Real(text : TEXT; mode : Mode) : REAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error } = 
  VAR
    longReal := LongReal(text,mode);
  BEGIN
    RETURN FLOAT(longReal, REAL);
  END Real;

PROCEDURE LongReal(text : TEXT; mode : Mode) : LONGREAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error } =
  VAR
    len := Text.Length(text);
    suf := Text.GetChar(text, len - 1);
    pre := Text.Sub(text, 0, len - 1);
    val : LONGREAL;
    mult : T;
  BEGIN

    (* first check for no suffix *)
    IF suf = '.' OR (suf >= '0' AND suf <= '9') THEN
      RETURN Scan.LongReal(text)
    END;

    (* has a suffix.  Scan prefix as a number *)
    val := Scan.LongReal(pre);
    IF NOT tbl.get(suf,mult) THEN
      RAISE UnknownSuffix
    ELSE
      IF mode = Mode.Base10 THEN
        RETURN mult.size * val
      ELSIF mode = Mode.Base2 AND mult.geeky > 0.0d0 THEN
        RETURN mult.geeky * val
      ELSE
        RAISE UnknownSuffix
      END
    END
  END LongReal;

VAR tbl := NEW(SuffixTbl.Default).init();

BEGIN 

  List := ARRAY OF T { 
    T { 'a' , 1.0d-18, "atto" } ,
    T { 'f' , 1.0d-15, "femto" } ,
    T { 'p' , 1.0d-12, "pico" } ,
    T { 'n' , 1.0d-9 , "nano" } ,
    T { 'u' , 1.0d-6 , "micro" } ,
    T { 'm' , 1.0d-3 , "milli" } ,
    T { 'c' , 1.0d-2 , "centi" } ,
    T { 'd' , 1.0d-1 , "deci" } ,
    T { 'D' , 1.0d1  , "deka" } ,
    T { 'h' , 1.0d2  , "hecto" } ,
    T { 'k' , 1.0d3  , "kilo", KiloGeek } ,
    T { 'M' , 1.0d6  , "mega", KiloGeek * KiloGeek } ,
    T { 'G' , 1.0d9  , "giga", KiloGeek * KiloGeek * KiloGeek } ,
    T { 'T' , 1.0d12 , "tera", KiloGeek * KiloGeek * KiloGeek * KiloGeek } ,
    T { 'P' , 1.0d15 , "peta", 
        KiloGeek * KiloGeek * KiloGeek * KiloGeek * KiloGeek } ,
    T { 'E' , 1.0d18 , "exa", 
        KiloGeek * KiloGeek * KiloGeek * KiloGeek * KiloGeek * KiloGeek } 
  };

  FOR i := FIRST(List) TO LAST(List) DO
    VAR
      x : BOOLEAN;
    BEGIN
      x := tbl.put(List[i].char, List[i]);
      <* ASSERT NOT x *>
    END
  END
END SIsuffix.
