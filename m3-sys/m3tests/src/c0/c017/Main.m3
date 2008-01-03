(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: TYPECASE statement *)

MODULE Main;

VAR
  i: REFANY;
  c: CHAR;

BEGIN

  TYPECASE i OF
  | (REF INTEGER) =>  c := '1';
  | (REF CHAR)    =>  c := '2';
  | NULL          =>  c := '3';
  | REFANY        =>  c := '4';
  END;

  TYPECASE i OF
  | (REF INTEGER) =>  c := '1';
  | (REF CHAR)    =>  c := '2';
  | NULL          =>  c := '3';
  | REFANY        =>  c := '4';
  ELSE                c := '5';
  END;

  TYPECASE i OF
  | (REF INTEGER) =>  c := '1';
  | (REF CHAR)(x) =>  c := x^;
  | NULL          =>  c := '3';
  | REFANY        =>  c := '4';
  END;

  TYPECASE i OF
  ELSE
  END;

  TYPECASE i OF
  END;

END Main.
