(* $Id$ *)

MODULE RouteID;
IMPORT Word, Fmt;

PROCEDURE Equal (READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash (READONLY a : T) : Word.T = BEGIN RETURN a END Hash;

PROCEDURE Format(READONLY a : T) : TEXT = 
  BEGIN RETURN Fmt.Int(a,base := 16) END Format;

VAR
  mu := NEW(MUTEX);
  next : T := 1;

PROCEDURE New() : T =
  BEGIN 
    LOCK mu DO
      TRY 
        <* ASSERT next # Nil *> 
        RETURN next 
      FINALLY 
        INC(next) 
      END 
    END
  END New;

BEGIN END RouteID.
