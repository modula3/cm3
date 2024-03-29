(* Grisu inspired tests -- int/float loophole and LONGINT multiply in parameter list,
*) 

UNSAFE MODULE Main;
IMPORT Cstdint;
(*TYPE i8t = Cstdint.int8_t;*)
(*TYPE i16t = Cstdint.int16_t;*)
TYPE i32t = Cstdint.int32_t;
TYPE i64t = Cstdint.int64_t;
 
VAR
 (*i8: i8t;*)
 (*i16: i16t;*)
 i32: i32t;
 i64: i64t;
 L: LONGINT;
 r: REAL;
 LR: LONGREAL;
 e: EXTENDED;

PROCEDURE grisu2(<*UNUSED*>a: LONGINT) =
BEGIN
END grisu2;

PROCEDURE grisu1() =
BEGIN

  grisu2(i64 * i64);

  (* EVAL LOOPHOLE(i8, REAL); *)
  (* EVAL LOOPHOLE(i16, REAL); *)
  EVAL LOOPHOLE(i32, REAL);
  (* EVAL LOOPHOLE(i64, REAL); *)
  EVAL LOOPHOLE(i32, REAL);
  (* EVAL LOOPHOLE(L, REAL); *)
  EVAL LOOPHOLE(r, REAL);
  (* EVAL LOOPHOLE(LR, REAL); *)
  (* EVAL LOOPHOLE(e, REAL); *)

  (* EVAL LOOPHOLE(i8, LONGREAL); *)
  (* EVAL LOOPHOLE(i16, LONGREAL); *)
  (* EVAL LOOPHOLE(i32, LONGREAL); *)
  EVAL LOOPHOLE(i64, LONGREAL);
  (* EVAL LOOPHOLE(i, LONGREAL); *)
  EVAL LOOPHOLE(L, LONGREAL);
  (* EVAL LOOPHOLE(r, LONGREAL); *)
  EVAL LOOPHOLE(LR, LONGREAL);
  EVAL LOOPHOLE(e, LONGREAL);

  (* EVAL LOOPHOLE(i8, EXTENDED); *)
  (* EVAL LOOPHOLE(i16, EXTENDED); *)
  (* EVAL LOOPHOLE(i32, EXTENDED); *)
  EVAL LOOPHOLE(i64, EXTENDED);
  (* EVAL LOOPHOLE(i, EXTENDED); *)
  EVAL LOOPHOLE(L, EXTENDED);
  (* EVAL LOOPHOLE(r, EXTENDED); *)
  EVAL LOOPHOLE(LR, EXTENDED);
  EVAL LOOPHOLE(e, EXTENDED);


  (* EVAL LOOPHOLE(r, i8t); *)
  (* EVAL LOOPHOLE(r, i16t); *)
  EVAL LOOPHOLE(r, i32t);
  (* EVAL LOOPHOLE(r, i64t); *)
  EVAL LOOPHOLE(r, i32t);
  (* EVAL LOOPHOLE(r, LONGINT); *)
  EVAL LOOPHOLE(r, REAL);
  (* EVAL LOOPHOLE(r, LONGREAL); *)
  (* EVAL LOOPHOLE(r, EXTENDED); *)

  (* EVAL LOOPHOLE(LR, i8t); *)
  (* EVAL LOOPHOLE(LR, i16t); *)
  (* EVAL LOOPHOLE(LR, i32t); *)
  EVAL LOOPHOLE(LR, i64t);
  (* EVAL LOOPHOLE(LR, INTEGER); *)
  EVAL LOOPHOLE(LR, LONGINT);
  (* EVAL LOOPHOLE(LR, REAL); *)
  EVAL LOOPHOLE(LR, LONGREAL);
  EVAL LOOPHOLE(LR, EXTENDED);

  (* EVAL LOOPHOLE(e, i8t); *)
  (* EVAL LOOPHOLE(e, i16t); *)
  (* EVAL LOOPHOLE(e, i32t); *)
  EVAL LOOPHOLE(e, i64t);
  (* EVAL LOOPHOLE(e, INTEGER); *)
  EVAL LOOPHOLE(e, LONGINT);
  (* EVAL LOOPHOLE(e, REAL); *)
  EVAL LOOPHOLE(e, LONGREAL);
  EVAL LOOPHOLE(e, EXTENDED);

END grisu1;

BEGIN
 grisu1 ();
END Main.
