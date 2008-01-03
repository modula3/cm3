(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* [Jerome Chailloux, 29 dec 89] *)
(* The compiler dies rather than reporting that the initialization of 
   v is a non-constant expression -- Report, p33 *)

INTERFACE Foo;

IMPORT Baz;

VAR v : ADDRESS := Baz.f (-1);

END Foo.
