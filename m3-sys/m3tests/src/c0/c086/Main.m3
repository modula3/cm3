(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

CONST
  Vin = 1024;

TYPE
  Cave = ARRAY [0..Vin] OF CHAR;
  Bouteille = [0..Vin];

VAR
  cave : ARRAY [0..Vin] OF CHAR;

CONST
  Napa = LAST (Cave);
  Chianti = LAST (cave);
  Beaune = LAST ([0..Vin]);
  Margaux = LAST (Bouteille);

VAR
  x: INTEGER;

BEGIN
  x := Margaux;
  x := Beaune;
  x := Napa;
  x := Chianti;
END Main.
