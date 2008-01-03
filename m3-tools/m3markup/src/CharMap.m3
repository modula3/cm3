(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue May 23 07:40:21 PDT 1995 by kalsow     *)

MODULE CharMap;

IMPORT ASCII, Text, TextExtras;

(******
VAR (*READONLY*) SortOrder : ARRAY CHAR OF CHAR;
(* => control characters, punctuation, numbers, "A", "a", "B", "b", ... *)

PROCEDURE Init () =
  VAR next: CHAR;
  BEGIN
    next := FIRST (CHAR);

    FOR c := FIRST (CHAR) TO LAST (CHAR) DO
      IF   (('0' <= c) AND (c <= '9'))
        OR (('a' <= c) AND (c <= 'z'))
        OR (('A' <= c) AND (c <= 'Z')) THEN
        (* skip *)
      ELSE
        SortOrder [c] := next;  INC (next);
      END;
    END;

    FOR c := '0' TO '9' DO
      SortOrder [c] := next;  INC (next);
    END;

    FOR c := 'a' TO 'y' DO
      SortOrder [ASCII.Upper[c]] := next;  INC(next);
      SortOrder [c]              := next;  INC (next);
    END;
    SortOrder ['Z'] := next;  INC(next);
    SortOrder ['z'] := next;
    
    <*ASSERT next = LAST (CHAR)*>
  END Init;
********)

PROCEDURE CmpText (t, u: TEXT): [-1 .. +1] =
  VAR diff := TextExtras.CICompare(t, u);
  BEGIN
    IF diff < 0 THEN
      RETURN -1;
    ELSIF diff > 0 THEN
      RETURN 1;
    ELSE
      RETURN 0;
    END;
  END CmpText;

PROCEDURE Substr (a, b: TEXT): BOOLEAN =
  VAR
    len_a := Text.Length(a);
    len_b := Text.Length(b);
    c_a, c_b : CHAR;
  BEGIN
    FOR i := 0 TO len_a - len_b DO
      FOR j := 0 TO len_b-1 DO
        c_a := ASCII.Upper [Text.GetChar(a, i+j)];
        c_b := ASCII.Upper [Text.GetChar(b, j)];
        IF (c_a # c_b) THEN EXIT END;
        IF (j = len_b-1) THEN RETURN TRUE END;
      END;
    END;
    RETURN FALSE;
  END Substr;

PROCEDURE PrefixMatch (a, b: TEXT;  len: INTEGER): BOOLEAN =
  BEGIN
    IF (len > Text.Length (a)) OR (len > Text.Length (b)) THEN
      RETURN FALSE;
    END;
    FOR i := 0 TO len-1 DO
      IF ASCII.Upper[Text.GetChar(a, i)] # 
         ASCII.Upper[Text.GetChar(b, i)] THEN 
        RETURN FALSE END;
    END;
    RETURN TRUE;
  END PrefixMatch;

BEGIN
  (***Init (); ***)
END CharMap.
