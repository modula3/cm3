(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE  Main;

IMPORT Word;
FROM Test IMPORT done;

TYPE 
  Byte = BITS 8 FOR [0..255];

PROCEDURE Main() =
  VAR bytes: ARRAY [0..100] OF Byte;
      adr: UNTRACED REF Byte;
  BEGIN
    adr := ADR(bytes[0]);
    bytes[0] := 255;
    adr^ := Word.Insert(adr^, 1, 1, 1);
  END Main;

BEGIN
  Main();
  done ();
END Main.
