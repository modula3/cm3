(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Test024 EXPORTS Main;

TYPE Byte = BITS 8 FOR [0..255];

PROCEDURE Foo(READONLY table: ARRAY Byte OF REAL) =

  PROCEDURE Bar(ptr: UNTRACED REF REAL) =
    VAR byte: Byte;
    BEGIN
      byte := 17;
      ptr^ := table[byte];
    END Bar;

  VAR real: REAL;
  BEGIN
    Bar(LOOPHOLE(ADR(real), UNTRACED REF REAL))
  END Foo;

VAR 
  table: ARRAY Byte OF REAL;

BEGIN
  Foo(table);
END Test024.
