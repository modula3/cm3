(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: ASSERT *)

MODULE Main;

VAR i : INTEGER := 7;

BEGIN

<* ASSERT i = 7 *>
<* ASSERT 2 < 3 *>
<* ASSERT 2 = 3 *>

END Main.

