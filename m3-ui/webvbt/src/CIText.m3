(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul  4 09:59:11 PDT 1995 by mhb                      *)

MODULE CIText;

IMPORT ASCII, Text, Text8, Word;

PROCEDURE Equal(t, u: T): BOOLEAN =
  VAR
    lt: CARDINAL := Text.Length(t);
    lu: CARDINAL := Text.Length(u);
    i: CARDINAL := 0;
  BEGIN
    IF lt = lu THEN 
      WHILE i<lt DO
        IF ASCII.Upper[Text.GetChar(t, i)] # 
           ASCII.Upper[Text.GetChar(u, i)] THEN 
          RETURN FALSE 
        ELSE INC(i) 
        END;
      END;
      RETURN TRUE;
    ELSE RETURN FALSE
    END;
  END Equal;

PROCEDURE Hash (t: T): Word.T =
  VAR
    len := Text.Length(t);
    u   := Text8.Create(len);
  BEGIN
    u.contents[len] := '\000';
    FOR i := 0 TO Text.Length(t) - 1 DO  
      u.contents[i] := ASCII.Upper[Text.GetChar(t, i)]
    END;
    RETURN Text.Hash(u);
  END Hash;

BEGIN 
END CIText.

