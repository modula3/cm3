(* This program crashed gcc 4.5 backend if "fre" optimization enabled. *)

MODULE Main;
IMPORT Cstdint, Word, RTIO;

TYPE UINT16 = Cstdint.int16_t;

VAR a:UINT16 := 1;

PROCEDURE uAnd_var_u16_u16():Word.T=
BEGIN
  RETURN Word.And(a, a);
END uAnd_var_u16_u16;

BEGIN
  RTIO.PutInt(uAnd_var_u16_u16());
  RTIO.PutText("\n");
  RTIO.Flush();
END Main.
