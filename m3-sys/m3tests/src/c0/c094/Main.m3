(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Test7 EXPORTS Main;

TYPE Bit = BITS 1 FOR [0..1];

PROCEDURE Foo () =
  VAR rt: REF ARRAY [0..LAST(Bit)] OF CARDINAL;
  BEGIN
    rt :=  NEW (REF ARRAY [0..LAST(Bit)] OF CARDINAL);
  END Foo;

BEGIN
  Foo ();
END Test7.
