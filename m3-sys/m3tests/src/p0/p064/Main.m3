(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main ;

IMPORT Wr, Stdio, Fmt ;
<*FATAL ANY*>

PROCEDURE g( f:PROCEDURE(i:INTEGER); j:INTEGER) =
  BEGIN 
    Wr.PutText(Stdio.stdout,Fmt.Int(j)&"\n");
    f(j) 
  END g;

PROCEDURE doit() =
  PROCEDURE pr( i:INTEGER ) =
    BEGIN Wr.PutText(Stdio.stdout,Fmt.Int(i)&"\n") END pr;
  BEGIN
    g(pr,1);
  END doit;

BEGIN
  doit() ;
END Main.
