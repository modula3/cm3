(* $Id$ *)


(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
MODULE SchemeChar;
FROM SchemeUtils IMPORT StringifyT, Error;
IMPORT SchemeObject;
FROM Scheme IMPORT E;
IMPORT Rd, Wr, Pickle, Thread;

REVEAL T = BRANDED Brand REF CHAR;

PROCEDURE Char(x : SchemeObject.T) : CHAR RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x, T) THEN RETURN NARROW(x,T)^ 
    ELSE RETURN Char(Error("expected a char, got: " & StringifyT(x))) 
    END
  END Char;

PROCEDURE Chr(x : SchemeObject.T) : T RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x, T) THEN RETURN x
    ELSE RETURN Chr(Error("expected a char, got: " & StringifyT(x))) 
    END
  END Chr;

PROCEDURE IChr(x : INTEGER) : T =
  BEGIN RETURN Array[VAL(x MOD NUMBER(CHAR),CHAR)] END IChr;
  
PROCEDURE Character(c : CHAR) : T =
  BEGIN RETURN Array[c] END Character;

PROCEDURE Upcase(c : CHAR) : CHAR =
  BEGIN 
    IF c IN LowerCase THEN 
      RETURN VAL(ORD(c) - ORD('a') + ORD('A'),CHAR)
    ELSE
      RETURN c
    END
  END Upcase;

PROCEDURE Downcase(c : CHAR) : CHAR =
  BEGIN 
    IF c IN UpperCase THEN 
      RETURN VAL(ORD(c) - ORD('A') + ORD('a'),CHAR)
    ELSE
      RETURN c
    END
  END Downcase;

PROCEDURE CharPklWrite (
    <*UNUSED*> sp: Pickle.Special;
    r: REFANY; writer: Pickle.Writer)
    RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    writer.writeInt(ORD(NARROW(r,T)^))
  END CharPklWrite;

PROCEDURE CharPklRead (
    <*UNUSED*> sp: Pickle.Special;
    reader: Pickle.Reader;
    <*UNUSED*> id: Pickle.RefID) : REFANY
    RAISES { Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    WITH res = reader.readInt() DO
      CASE res OF
        ORD(FIRST(CHAR))..ORD(LAST(CHAR)) => RETURN Array[VAL(res,CHAR)]
      ELSE
        RAISE Pickle.Error("bad value")
      END
    END
  END CharPklRead;


VAR (* CONST *) Array := NEW(REF ARRAY CHAR OF T);

BEGIN 
  FOR i := FIRST(CHAR) TO LAST(CHAR) DO
    Array[i] := NEW(T);
    Array[i]^ := i
  END;

  Pickle.RegisterSpecial (NEW (Pickle.Special, sc := TYPECODE (T),
    write := CharPklWrite,
    read  := CharPklRead 
));
END SchemeChar.
