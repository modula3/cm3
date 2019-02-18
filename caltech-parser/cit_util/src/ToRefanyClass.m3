MODULE ToRefanyClass;
IMPORT RT0;
IMPORT Word;

(* this file is a dummy to keep m3implhack happy.
   It is not included in the m3makefile. *)

PROCEDURE a(:REFANY) : Word.T =
  BEGIN
  END a;

PROCEDURE a(,b: REFANY) : BOOLEAN =
  BEGIN
  END a;

BEGIN
END ToRefanyClass.
