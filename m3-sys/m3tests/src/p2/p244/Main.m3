(* This program demonstrates a problem
 * porting to gcc 4.5.
 * Variables are being referenced as if they
 * are INTEGER instead of smaller types.
 * If optimizations not enable.
 * Or not?
 *
 * This also crashes if "fre" optimization is enabled.
 *)

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
