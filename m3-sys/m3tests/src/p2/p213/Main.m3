(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: The Text Interface *)

MODULE Main;

IMPORT Stdio, Text, Wr;
FROM Stdio IMPORT stdout;
<*FATAL ANY*>

VAR t1, t2, t3, t4 : Text.T;

BEGIN

t1 := "Hello";
t2 := " world";
t3 := Text.Cat (t1, t2);
Wr.PutText (stdout, "`");
Wr.PutText (stdout, t3);
Wr.PutText (stdout, "\' and `");

t1 := "wor";
t4 := "Hello " & t1 & "ld";
Wr.PutText (stdout, t4);

IF Text.Equal (t3, t4) THEN
  Wr.PutText (stdout, "\' are equal.\n");
ELSE
  Wr.PutText (stdout, "\' are not equal.\n"); END;

t1 := "Hello" & " world"; 
IF NOT Text.Equal (t1, t3) THEN
  Wr.PutText (stdout, "Cat of constant expressions does not work !\n"); END;

Wr.PutText (stdout, "The length of the first is ");
IF Text.Length (t3) = 11 THEN
  Wr.PutText (stdout, "11\n");
ELSE 
  Wr.PutText (stdout, "not 11\n"); END;

Wr.PutText (stdout, "Extracting four chars from position 3 yields --");
Wr.PutText (stdout, Text.Sub (t3, 3, 4));
Wr.PutText (stdout, "--\n"); 

VAR
  a : ARRAY [5..9] OF CHAR;
BEGIN
  a [5] := 'S';
  a [6] := 'a';
  a [7] := 'l';
  a [8] := 'u';
  a [9] := 't';

  Wr.PutText (stdout, Text.FromChars (a));
  
  Text.SetChars (a, "Hello");
  IF a[5] = 'H' AND a[6] = 'e' AND a[7] = 'l' AND a[8] = 'l' AND a[9]= 'o' THEN
    Wr.PutText (stdout, " = Hello\n");
  ELSE
    Wr.PutText (stdout, " <> Hello\n"); END;
END;

Wr.PutText (stdout, "\ndone.\n");
Wr.Close (Stdio.stdout);
END Main.
