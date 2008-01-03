(* Copyright © 1993, Digital Equipment Corporation            *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sun May 30 10:41:25 PDT 1993 by meehan     *)

MODULE ISOChar;

IMPORT ASCII, Word;

BEGIN
  FOR c := '\000' TO '\377' DO Upper [c] := ASCII.Upper [c] END;
  FOR c := '\340' TO '\376' DO
    Upper [c] := VAL (ORD (c) - ORD ('a') + ORD ('A'), CHAR)
  END;

  FOR c := '\000' TO '\377' DO Lower [c] := ASCII.Lower [c] END;
  FOR c := '\300' TO '\336' DO
    Lower [c] := VAL (ORD (c) - ORD ('A') + ORD ('a'), CHAR);
  END;

  FOR c := '\000' TO '\377' DO
    IF c IN Graphics THEN
      Control [c] := VAL (Word.And (ORD (c), 8_37), CHAR)
    ELSE
      Control [c] := c
    END
  END
END ISOChar.
