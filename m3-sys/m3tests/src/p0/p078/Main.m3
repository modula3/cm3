(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT done, checkI, checkB;

CONST
  Numbers = ARRAY OF INTEGER {2, 3, 5, 7, 11};

  First = Numbers [FIRST (Numbers)];
  Last = Numbers [LAST (Numbers)];
  Number = NUMBER (Numbers);

  Empty = ARRAY OF INTEGER {};
  EFirst = FIRST (Empty);
  ELast  = LAST (Empty);
  ENumber = NUMBER (Empty);

BEGIN

checkI (First, 2);
checkI (Last, 11);
checkI (Number, 5);

checkB (EFirst > ELast, TRUE);
checkI (ENumber, 0);

done ();

END Main.


