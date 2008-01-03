(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

VAR buffer := NEW (REF ARRAY OF INTEGER, 10);

BEGIN

buffer[1] := 6;
buffer[3] := 12;

WITH b = buffer^ DO b[7] := b[1] + b[3]; END;

checkI (buffer[7], 18);
done();

END Main.
