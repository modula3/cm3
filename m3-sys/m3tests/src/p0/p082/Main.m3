(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkB, done;

TYPE
   T = {E0,  E1,  E2,  E3,
        E4,  E5,  E6,  E7,
        E8,  E9,  E10, E11,
        E12, E13, E14, E15,
        E16, E17, E18, E19,
        E20, E21, E22, E23,
        E24, E25, E26};
  (* Proc = [T.E0..T.E2]; *)
  Func = [T.E3..T.E26];

  ProcFuncSet = SET OF T;

CONST
  AllFunc = ProcFuncSet { FIRST(Func) .. LAST(Func)};
  A = AllFunc - ConstantExpressions;
  ConstantExpressions = 
      ProcFuncSet{T.E3, T.E18, T.E19, T.E20, T.E24, T.E25, T.E26};

(* VAR pf: T := T.E24; *)

BEGIN
  checkB (T.E0 IN A, FALSE);
  checkB (T.E1 IN A, FALSE);
  checkB (T.E2 IN A, FALSE);
  checkB (T.E3 IN A, FALSE);
  checkB (T.E4 IN A, TRUE);
  checkB (T.E5 IN A, TRUE);
  checkB (T.E6 IN A, TRUE);
  checkB (T.E7 IN A, TRUE);
  checkB (T.E8 IN A, TRUE);
  checkB (T.E9 IN A, TRUE);

  checkB (T.E10 IN A, TRUE);
  checkB (T.E11 IN A, TRUE);
  checkB (T.E12 IN A, TRUE);
  checkB (T.E13 IN A, TRUE);
  checkB (T.E14 IN A, TRUE);
  checkB (T.E15 IN A, TRUE);
  checkB (T.E16 IN A, TRUE);
  checkB (T.E17 IN A, TRUE);
  checkB (T.E18 IN A, FALSE);
  checkB (T.E19 IN A, FALSE);

  checkB (T.E20 IN A, FALSE);
  checkB (T.E21 IN A, TRUE);
  checkB (T.E22 IN A, TRUE);
  checkB (T.E23 IN A, TRUE);
  checkB (T.E24 IN A, FALSE);
  checkB (T.E25 IN A, FALSE);
  checkB (T.E26 IN A, FALSE);

  done ();
END Main.

