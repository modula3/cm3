(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 18 10:48:39 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 16:46:19 PDT 1992 by muller                   *)

MODULE UnFmt;

IMPORT Text;

PROCEDURE ToInt(t: Text.T): INTEGER =
VAR
  n, i, sign, num: INTEGER;
BEGIN
  n := Text.Length(t);
  i := 0;
  num := 0;
  IF Text.GetChar(t, i) = '-' THEN
     sign := -1;
     i := i + 1;
  ELSIF Text.GetChar(t, i) = '+' THEN
     sign := 1;
     i := i + 1;
  ELSE
     sign := 1;
  END;
  FOR j := i TO n - 1 BY 1 DO
    num := num * 10 + ORD(Text.GetChar(t,j)) - ORD('0');
  END;
  RETURN sign * num;
END ToInt;


PROCEDURE RealPower(base: REAL; power: INTEGER): REAL =
VAR
  res: REAL;
BEGIN
  res := 1.0;
  FOR i := 1 TO power DO
    res := res * base
  END;
  RETURN res
END RealPower;


PROCEDURE ToReal(t: Text.T): REAL =
VAR
  num, sign: REAL;
  i, j: INTEGER;
  n: INTEGER;
BEGIN
  n := Text.Length(t);
  i := 0;
  num := 0.0;
  IF Text.GetChar(t, i) = '-' THEN
     sign := -1.0;
     i := i + 1;
  ELSIF Text.GetChar(t, i) = '+' THEN
     sign := 1.0;
     i := i + 1;
  ELSE
     sign := 1.0;
  END;
  WHILE Text.GetChar(t,i) # '.' DO
    num := num * 10.0 + FLOAT(ORD(Text.GetChar(t,i)) - ORD('0'));
    i := i + 1;  
  END;
  i := i + 1;
  j := 1;
  WHILE i < n DO
    num := num + FLOAT(ORD(Text.GetChar(t,i)) - ORD('0')) / 
                 RealPower (10.0, j);
    i := i + 1;
    j := j + 1;
  END;
  (* To be completed *)
  RETURN (sign * num);
END ToReal;


PROCEDURE ToBool(t: Text.T): BOOLEAN =
VAR
  result: BOOLEAN;
BEGIN
  IF Text.GetChar(t, 0) = '0' THEN
    result := FALSE;
  ELSIF Text.GetChar(t, 0) = '1' THEN
    result := TRUE;
  ELSE
    result := FALSE;
  END;
  RETURN result;
END ToBool;

BEGIN
END UnFmt.
