(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT A, X, X AS Y;
FROM Test IMPORT checkI, done;

BEGIN
  checkI (A.V, X.KeyV);
  checkI (A.V, X.EltV);
  checkI (A.V, Y.KeyV);
  checkI (A.V, Y.EltV);

  checkI (A.v, X.Key_v);
  checkI (A.v, X.Elt_v);
  checkI (A.v, Y.Key_v);
  checkI (A.v, Y.Elt_v);

  done ();
END Main.
