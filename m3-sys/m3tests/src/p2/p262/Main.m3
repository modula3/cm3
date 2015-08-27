(* Test the correctness of Structure_size_boundary = 16 for PA32_HPUX, PA64_HPUX.
   I think it is wrong. *)

MODULE Main;

TYPE T1 = RECORD a: CHAR END;

PROCEDURE F1(<*UNUSED*>a:INTEGER) =
BEGIN
END F1;

PROCEDURE F2() =
BEGIN
  F1(16_123);
  F1(123);
  F1(BYTESIZE(T1));
END F2;

BEGIN
 F2();
END Main.
