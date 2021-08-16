MODULE R;

IMPORT IO;

PROCEDURE DoIt(READONLY dl := ARRAY OF Rec{}) =
  BEGIN
    IO.PutInt(NUMBER(dl));
    IO.Put("\n");
  END DoIt;

BEGIN
END R.
