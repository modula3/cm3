(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

EXCEPTION
    problem;
CONST
    c = ARRAY OF INTEGER { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    
PROCEDURE Proc1( x : ARRAY OF INTEGER) =
BEGIN
    (* Somehow these parameters don't get sent right *)
    Proc2( x, 10);
END Proc1;

PROCEDURE Proc2( <*UNUSED*>x : ARRAY[1..10] OF INTEGER; y : INTEGER) =
<*FATAL problem*>
BEGIN    
    IF y # 10 THEN RAISE problem; END;    
END Proc2;

BEGIN
    Proc1( c);
END Main.

