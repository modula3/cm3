(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE 
  A = RECORD 
       x, y:   BITS 16 FOR [0..65535];
       sel:    BITS 1 FOR BOOLEAN;
       stroke: BITS 7 FOR [0..127]; END;

  B = RECORD 
       x, y:   [0..65535];
       sel:    BOOLEAN;
       stroke: [0..127]; END;


VAR
  a: A;
  b: B;


BEGIN
  a.sel := TRUE;
  b.sel := TRUE;
END Main.
