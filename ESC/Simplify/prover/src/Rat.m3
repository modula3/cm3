(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Sep 28 16:21:17 PDT 2001 by saxe                     *)
(*      modified on Wed Aug 10 13:17:37 PDT 1994 by detlefs                  *)

MODULE Rat;

IMPORT Text, Fmt;
IMPORT Env, Scan, Wr, Stdio; (* to set overflowCheck and warn if bogus *)
IMPORT Thread;

<*FATAL Wr.Failure, Thread.Alerted, Error*>

EXCEPTION Error(TEXT);

VAR overflowCheck: INTEGER := 0;
(* If 0, do no overflow checking.  If 1, warn of overflows on
   current processor.  If 2, warn of computations that would
   overflow on a 32-bit processor, even if currently running
   on a 64-bit processor.  Set by environment variable
   PROVER_OVERFLOW_CHECK. *)

PROCEDURE GCD(a, b: INTEGER): INTEGER =
  (* Requires a > 0 AND b > 0.  Return the GCD of a and b. *)
  BEGIN
    <*ASSERT a > 0 AND b > 0*>
    (* Let g0 = gcd(a, b) *)
    IF b > a THEN
      VAR t := b; BEGIN b := a; a := t END (* BEGIN *)
    END (* IF *);
    WHILE a # b DO
      (* Invariant: gcd(a, b) = g0 AND a >= b*)
      VAR m := a MOD b; BEGIN
        IF m = 0 THEN
          a := b
        ELSE
          a := b; b := m
        END (* IF *)
      END (* BEGIN *)
    END (* WHILE *);
    <*ASSERT a > 0*>
    RETURN a
  END GCD;

PROCEDURE Normalize(num, den: INTEGER): T =
  (* Requires "den > 0".  Returns the "T" representing "num/den". *)
  BEGIN
    <*ASSERT den > 0*> 
    IF num = 0 THEN
      RETURN Zero
    ELSE
      VAR div := GCD(ABS(num), den); BEGIN
        RETURN T{num DIV div, den DIV div}
      END (* BEGIN *)
    END (* IF *)
  END Normalize;

PROCEDURE Plus(READONLY q0, q1: T): T =
  BEGIN
    IF q0.den = q1.den THEN
      IF overflowCheck > 0 THEN
        AddCheck(q0.num, q1.num)
      END;
      RETURN Normalize(q0.num + q1.num, q0.den)
    ELSE
      IF overflowCheck > 0 THEN
        MulCheck(q0.num, q1.den);
        MulCheck(q1.num, q0.den);
        AddCheck(q0.num*q1.den, q1.num*q0.den);
        MulCheck(q0.den, q1.den)
      END;
      RETURN Normalize(q0.num*q1.den + q1.num*q0.den, q0.den*q1.den)
    END (* IF *)
  END Plus;

PROCEDURE Minus(READONLY q0, q1: T): T =
  BEGIN
    IF q0.den = q1.den THEN
      IF overflowCheck > 0 THEN
        SubCheck(q0.num, q1.num)
      END;
      RETURN Normalize(q0.num - q1.num, q0.den)
    ELSE
      IF overflowCheck > 0 THEN
        MulCheck(q0.num, q1.den);
        MulCheck(q1.num, q0.den);
        SubCheck(q0.num*q1.den, q1.num*q0.den);
        MulCheck(q0.den, q1.den)
      END;
      RETURN Normalize(q0.num*q1.den - q1.num*q0.den, q0.den*q1.den)
    END (* IF *)
  END Minus;

PROCEDURE Times(READONLY q0, q1: T): T =
  BEGIN
    IF overflowCheck > 0 THEN
      MulCheck(q0.num, q1.num);
      MulCheck(q0.den, q1.den)
    END;
    RETURN Normalize(q0.num*q1.num, q0.den*q1.den)
  END Times;

PROCEDURE Div(READONLY q0, q1: T): T =
  BEGIN
    <*ASSERT q1.num # 0*>
    IF q1.num < 0 THEN
      IF overflowCheck > 0 THEN
        NegCheck(q0.num);
        NegCheck(q0.den);
        MulCheck(-q0.num, q1.den);
        MulCheck(-q0.den, q1.num)
      END;
      RETURN Normalize(-q0.num*q1.den, -q0.den*q1.num)
    ELSE
      IF overflowCheck > 0 THEN
        MulCheck(q0.num, q1.den);
        MulCheck(q0.den, q1.num)
      END;
      RETURN Normalize(q0.num*q1.den, q0.den*q1.num)
    END (* IF *)
  END Div;

PROCEDURE Recip(READONLY q: T): T =
  BEGIN
    <*ASSERT q.num # 0*>
    IF q.num < 0 THEN
      IF overflowCheck > 0 THEN
        NegCheck(q.num);
        NegCheck(q.den)
      END;
      RETURN T{-q.den, -q.num}
    ELSE
      RETURN T{q.den, q.num}
    END (* IF *)
  END Recip;

PROCEDURE Abs(READONLY q: T): T =
  BEGIN
    IF q.num >= 0 THEN
      RETURN q
    ELSE
      VAR res := q; BEGIN
        IF overflowCheck > 0 THEN
          NegCheck(res.num)
        END;
        res.num := -res.num; RETURN res
      END (* BEGIN *)
    END (* IF *)
  END Abs;

PROCEDURE GT(READONLY q0, q1: T): BOOLEAN =
  BEGIN
    IF overflowCheck > 0 THEN
      MulCheck(q0.num, q1.den);
      MulCheck(q0.den, q1.num)
    END;
    RETURN q0.num*q1.den > q0.den*q1.num
  END GT;

PROCEDURE GE(READONLY q0, q1: T): BOOLEAN =
  BEGIN
    IF overflowCheck > 0 THEN
      MulCheck(q0.num, q1.den);
      MulCheck(q0.den, q1.num)
    END;
    RETURN q0.num*q1.den >= q0.den*q1.num
  END GE;

PROCEDURE LT(READONLY q0, q1: T): BOOLEAN =
  BEGIN
    IF overflowCheck > 0 THEN
      MulCheck(q0.num, q1.den);
      MulCheck(q0.den, q1.num)
    END;
    RETURN q0.num*q1.den < q0.den*q1.num
  END LT;

PROCEDURE LE(READONLY q0, q1: T): BOOLEAN =
  BEGIN
    IF overflowCheck > 0 THEN
      MulCheck(q0.num, q1.den);
      MulCheck(q0.den, q1.num)
    END;
    RETURN q0.num*q1.den <= q0.den*q1.num
  END LE;

PROCEDURE Floor(READONLY q: T): T =
  BEGIN
    IF q.den = 1 THEN
      RETURN q
    ELSE
      IF overflowCheck > 0 THEN
        MulCheck((q.num DIV q.den), q.den)
      END;
      RETURN Normalize((q.num DIV q.den) * q.den, q.den)
    END (* IF *)
  END Floor;

PROCEDURE ToText(READONLY q: T): Text.T =
  BEGIN
    IF q.num = 0 THEN
      RETURN "0"
    ELSIF q.den = 1 THEN
      RETURN Fmt.Int(q.num)
    ELSE
      RETURN Fmt.Int(q.num) & "/" & Fmt.Int(q.den)
    END (* IF *)
  END ToText;


PROCEDURE AddCheck(x, y: INTEGER) =
  VAR sum := x + y; BEGIN
    IF x > 0 AND y > 0 THEN
      <*ASSERT sum > 0 *>
    ELSIF x < 0 AND y < 0 THEN
      <*ASSERT sum < 0 *>
    END;
    IF overflowCheck > 1 THEN
      <*ASSERT sum <= 2147483647 *>
      <*ASSERT sum >= -2147483647-1 *>
    END;
  END AddCheck;

PROCEDURE SubCheck(x, y: INTEGER) =
  VAR diff := x - y; BEGIN
    IF x >= 0 AND y < 0 THEN
      <*ASSERT diff > 0 *>
    ELSIF x <= 0 AND y > 0 THEN
      <*ASSERT diff < 0 *>
    END;
    IF overflowCheck > 1 THEN
      <*ASSERT diff <= 2147483647 *>
      <*ASSERT diff >= -2147483647-1 *>
    END;
  END SubCheck;

PROCEDURE MulCheck(x, y: INTEGER) =
  VAR prod := x * y; BEGIN
    IF x # 0 THEN
      <*ASSERT prod DIV x = y *>
      <*ASSERT prod MOD x = 0 *>
    END;
    IF y # 0 THEN
      <*ASSERT prod DIV y = x *>
      <*ASSERT prod MOD y = 0 *>
    END;
    IF overflowCheck > 1 THEN
      <*ASSERT prod <= 2147483647 *>
      <*ASSERT prod >= -2147483647-1 *>
    END;
  END MulCheck;

PROCEDURE NegCheck(x: INTEGER) =
  VAR neg := -x; BEGIN
    IF x > 0 THEN
      <*ASSERT neg < 0 *>
    ELSIF x < 0 THEN
      <*ASSERT neg > 0 *>
    END;
    IF overflowCheck > 1 THEN
      <*ASSERT neg <= 2147483647 *>
      <*ASSERT neg >= -2147483647-1 *>
    END;
  END NegCheck;

PROCEDURE UpdateInt(VAR v: INTEGER; name: Text.T;
          min := FIRST(INTEGER); max := LAST(INTEGER)) RAISES { Error } =
  VAR str := Env.Get(name); BEGIN
    IF str # NIL THEN
(*      envVars := envVars & name & "=" & str & "\n"; *)
      TRY
        v := Scan.Int(str);
      EXCEPT ELSE
        BadInt(name, min, max)
      END;
      IF v < min OR v > max THEN BadInt(name, min, max) END
    END
  END UpdateInt;

PROCEDURE BadInt(name: Text.T; min, max: INTEGER) RAISES { Error } =
  BEGIN
    Wr.PutText(Stdio.stdout,
      "Bad value for environment value " & name & "\n" &
      "Expecting an integer");
    IF min > FIRST(INTEGER) THEN
      Wr.PutText(Stdio.stdout, " at least " & Fmt.Int(min));
      IF max < LAST(INTEGER) THEN
        Wr.PutText(Stdio.stdout, " and")
      END
    END;
    IF max < LAST(INTEGER) THEN
      Wr.PutText(Stdio.stdout, " at most " & Fmt.Int(max))
    END;
    Wr.PutText(Stdio.stdout, ".\n");
    Wr.Flush(Stdio.stdout);
    RAISE Error("Bad environment variable.")
  END BadInt;

BEGIN
  UpdateInt(overflowCheck, "PROVER_OVERFLOW_CHECK", 0, 2); 
END Rat.
