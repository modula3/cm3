(* $Id$ *)

MODULE FramePixel;
IMPORT Word, Fmt;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO IF a[i] # b[i] THEN RETURN FALSE END END;
    RETURN TRUE
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR r : Word.T := 0; BEGIN 
    FOR i := FIRST(a) TO LAST(a) DO r := Word.Plus(r,a[i]) END;
    RETURN r
  END Hash;

PROCEDURE Format(READONLY a : T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO 
      res := res & " " & Fmt.Pad(Fmt.Unsigned(a[i]), Word.Size DIV 4 + 1,
                                 padChar := '0') 
    END;
    RETURN res
  END Format;

PROCEDURE Clone(READONLY a : T) : REF T =
  BEGIN RETURN NEW(REF T, NUMBER(a)) END Clone;
  

BEGIN END FramePixel.
