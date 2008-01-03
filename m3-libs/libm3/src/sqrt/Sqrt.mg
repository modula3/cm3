(*
     Sqrt.mg
        A portable square root routine
        David Goldberg, Xerox PARC
        goldberg@parc.xerox.com
        November, 1993
*)

(* Copyright (c) 1993 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

(* Last modified on Tue Mar  1 16:30:13 PST 1994 by kalsow   *)
(*      modified on Sun Nov 14 20:11:53 PST 1993 by goldberg *)

GENERIC MODULE Sqrt(Type, Float, FloatExtras);

IMPORT FloatMode;
FROM FloatMode IMPORT RoundingMode, Flag;

CONST
  Zero = FLOAT(0.0, Type.T);
  One  = FLOAT(1.0, Type.T);
  Two  = FLOAT(2.0, Type.T);

CONST
  (*
   * Table of starting approximations for Newton iterations
   *
   *    Table[n, 0] = Average(sqrt(n/2), sqrt((n+1)/2))
   *    Table[n, 1] = Average(sqrt(n), sqrt(n+1))
   *)
  Table = ARRAY [32 .. 63], [0 .. 1] OF
            LONGREAL{ARRAY [0 .. 1] OF LONGREAL{4.031010d0, 5.700708d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.092562d0, 5.787757d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.153203d0, 5.873516d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.212970d0, 5.958040d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.271902d0, 6.041381d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.330031d0, 6.123588d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.387390d0, 6.204706d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.444008d0, 6.284777d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.499914d0, 6.363840d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.555134d0, 6.441932d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.609692d0, 6.519090d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.663613d0, 6.595344d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.716916d0, 6.670727d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.769624d0, 6.745267d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.821756d0, 6.818992d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.873330d0, 6.891929d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.924363d0, 6.964102d0},
                     ARRAY [0 .. 1] OF LONGREAL{4.974874d0, 7.035534d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.024876d0, 7.106248d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.074386d0, 7.176265d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.123417d0, 7.245606d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.171984d0, 7.314290d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.220098d0, 7.382334d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.267773d0, 7.449757d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.315021d0, 7.516575d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.361852d0, 7.582804d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.408278d0, 7.648459d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.454308d0, 7.713556d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.499953d0, 7.778108d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.545222d0, 7.842129d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.590125d0, 7.905631d0},
                     ARRAY [0 .. 1] OF LONGREAL{5.634670d0, 7.968627d0}};

  (*
   * Using table above as seed, need 2 iterations for single precision,
   * 3 iterations for double precision.  Reason:
   *
   * If x(n) is n-th Newton iteration, then
   *
   *   x(n+1) - sqrt(x) = [(x(n) - sqrt(x))/sqrt(x)]^2 * (x/(2*x(n)))
   *
   * so if r(n) = (x(n) - sqrt(x))/sqrt(x) is relative error, then
   *
   *   r(n+1) = r(n)^2 * 0.5 * (sqrt(x)/x(n))
   *
   * The largest error is when x=16, then table gives 4.031010, but
   * true answer is 4, so relative error is .031010/4 = .00775
   * So relative error (in exact arithmetic) is
   *
   * r(0):  7.75e-3
   * r(1):  3.0e-5
   * r(2):  4.5e-10
   * r(3):  1.0e-19
   *
   * In double precision, 1 ulp is 1.1e-16, so three iterations
   * is approximately within .001 ulp.  In single precision,
   * 1 ulp is 6.0e-8, so two iterations within .0075 ulp.
   *
   *)

PROCEDURE Sqrt (x: Type.T): Type.T RAISES {FloatMode.Trap} =
  <* FATAL FloatMode.Failure *>
  CONST Big = Type.MaxFinite;
  VAR
    exp, n  : INTEGER;
    y, z    : Type.T;
    inexact : BOOLEAN;
    saveMode: RoundingMode;
  BEGIN
    CASE Float.Class(x) OF
    | Float.IEEEClass.SignalingNaN =>
        RETURN (x + One); (* force exception *)
    | Float.IEEEClass.QuietNaN => RETURN (x);
    | Float.IEEEClass.Infinity =>
        IF x > Zero THEN
          RETURN (x)
        ELSE
          RETURN (FloatExtras.RaiseInvalid());
        END;
    | Float.IEEEClass.Zero => RETURN (x); (* sqrt(-0) = -0 *)
    | Float.IEEEClass.Normal =>
    | Float.IEEEClass.Denormal =>
    END;
    IF x < Zero THEN RETURN (FloatExtras.RaiseInvalid()) END;

    (* compute starting approximation *)
    exp := Float.ILogb(x);
    n := TRUNC(Float.Scalb(x, -exp + 5)); (* between 32 and 64 *)
    y := FLOAT(Table[n, exp MOD 2], Type.T);
    y := Float.Scalb(y, exp DIV 2 - 2);

    (* compute Newton iterations in round-to-nearest mode *)
    IF FloatMode.IEEE THEN
      saveMode := FloatMode.GetRounding();
      IF saveMode # RoundingMode.NearestElseEven THEN
        FloatMode.SetRounding(RoundingMode.NearestElseEven);
      END;
    END;

    (* iterate to get within 1 ulp *)
    y := (y + x / y) / Two;
    y := (y + x / y) / Two;
    IF BITSIZE(Type.T) = 64 THEN y := (y + x / y) / Two; END;

    (*
     * In exact arithmetic, final (y + x/y)/2 is within .001 ulp.
     * Perhaps it could have 2 rounding errors, giving > 1 ulp error?
     * However, empirically largest error is about .75 ulps.
     *)

    IF FloatMode.IEEE THEN
      (* Use Kahan-Ng method to get exactly rounded result. *)
      FloatMode.SetRounding(RoundingMode.TowardZero);
      FloatMode.ClearFlag(Flag.Inexact);
      z := x / y;
      inexact := Flag.Inexact IN FloatMode.GetFlags();
      (*
       * y' is correctly rnded square root iff (y')*(y'-) < x <= (y')*(y'+)
       * (see "Computation of elementary functions on the IBM RISC
       * System/6000 processor", by P. W. Markstein,
       * IBM J Res Develop, 34(1),  January 1990, p. 115).
       *
       *  Analysis when inexact is as follows.
       *  Let r = correctly rounded square root.
       *
       *  If z = y+, then y+ < x/y < y++ or
       *       y*(y+) < x < y*(y++) < (y+)*(y++)
       *  r = y+
       *
       *  If z = y, then y < x/y < y+ or y*(y-) < y*y < x < y*(y+)
       *  r = y
       *
       *  If z = y-, then y- < x/y < y or y*(y-) < x < y*y
       *  r = y
       *
       *  If z = y--, then y-- < x/y < y- or
       *     (y-)*(y--) < y*y-- < x < y*(y-)
       *  r = y-
       *
       *  Thus in every case, r = floor[((z+) + y)/2]
       *      Fine point: In first case, if y, y++ cross binade, then
       *      (y + y++)/2 # y+.  But this can only happen when
       *      z = y+ = 2^t, so 2^t = floor(x/(2^t - 2e)).  Then either
       *      x=2^t*(2^t - 2e) and operation is exact, or
       *      x=2^(2*t) and since y is within 1 ulp of square root,
       *      y=2^t.
       *
       *
       *  Next let r = truncated square root.  Know that either
       *  r = y or r = y-.  In cases above,
       *   z = y+ => y*y < y*y+ < x =>  r=y
       *   z = y => y*y < x         =>  r=y
       *   z = y- => y*y > x        =>  r=y-
       *   z = y-- => y*y > y*y- > x => r=y-
       *
       *  Thus in every case, r = floor[(z + y)/2]
       *
       *
       *  Finally  let r = ceil of square root.  Know that either
       *  r = y or r = y+.  In cases above,
       *   z = y+ => x > y*y+ > y*y  =>  r=y+
       *   z = y => x > y*y          =>  r=y+
       *   z = y- => x < y*y         =>  r=y
       *   z = y-- => x < y*y- < y*y =>  r=y
       *
       *  Thus in every case, r = floor[(z + y)/2]+
       *)
      IF inexact THEN
        CASE saveMode OF <* NOWARN *>(* other cases not possible *)
        | RoundingMode.TowardMinusInfinity,
          RoundingMode.TowardZero =>
            y := (z + y) / Two;
        | RoundingMode.TowardPlusInfinity =>
            y := (z + y) / Two;
            y := Float.NextAfter(y, Big);
        | RoundingMode.NearestElseEven =>
            z := Float.NextAfter(z, Big);
            y := (z + y) / Two;
        END;
      ELSE (* exact *)
        IF z # y THEN
          (*
           * e.g. x = 2^53*(2^53-1) or x = 2^52*(2^52+1)
           *
           * If x = y*(y+u), u an ulp, then sqrt(x) =
           *    y*(1 + u/(2*y) - stuff), so sqrt(x) < y + u/2.
           *
           *  Thus correctly rnded sqrt = min(y, z).
           *)
          inexact := TRUE; (* square root not exact *)
          IF saveMode = RoundingMode.TowardPlusInfinity THEN
            y := MAX(z, y);
          ELSE
            y := MIN(z, y); (* or y := (y + z)/2.0 *)
          END;
        END;
        (* ELSE y = z = exact square root *)
      END;
      FloatMode.SetRounding(saveMode);
      IF inexact THEN FloatExtras.SetFlag(Flag.Inexact) END;
    END; (* IF FloatMode.IEEE *)
    RETURN (y);
  END Sqrt;

BEGIN
END Sqrt.

