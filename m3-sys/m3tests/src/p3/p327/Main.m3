(* FOR variables whose address is taken (passed to a READONLY formal), so
   their slots keep the storage width: an enumeration, and CHAR over its
   whole range (the counter must not wrap); and an enumeration actual that
   is not a designator (B39). *)

MODULE Main;
IMPORT IO, Fmt;
TYPE Color = {Red, Green, Blue};
VAR sum := 0;  chars := 0;
PROCEDURE Put (READONLY k: TEXT;  READONLY v: Color) =
  BEGIN INC (sum, ORD (v) * 10 + 1);  EVAL k END Put;
PROCEDURE Count (READONLY c: CHAR) =
  BEGIN INC (chars, ORD (c)) END Count;
BEGIN
  FOR i := FIRST (Color) TO LAST (Color) DO Put ("x", i) END;
  Put ("y", VAL (1, Color));
  FOR c := FIRST (CHAR) TO LAST (CHAR) DO Count (c) END;
  IO.Put (Fmt.Int (sum) & " " & Fmt.Int (chars) & "\n");
END Main.
