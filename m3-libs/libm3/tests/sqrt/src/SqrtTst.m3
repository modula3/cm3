(*
     SqrtTst.m3
        A simple test suite for square root
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

(* Last modified onSat Nov 13 21:28:39 PST 1993by goldberg *)

MODULE SqrtTst EXPORTS Main;

IMPORT Rd, Stdio, Wr, Fmt, Math, RealFloat, LongFloat, Random;
IMPORT Real, LongReal, LongFloatExtras;
IMPORT Scan, Params, Text, Lex;
IMPORT LongSqrt, RealSqrt;
IMPORT FloatMode, Thread; (* for FATAL pragma *)
FROM FloatMode IMPORT Flag, RoundingMode;

CONST
  Up    = Real.MaxFinite;
  Down  = -Real.MaxFinite;
  UpL   = LongReal.MaxFinite;
  DownL = -LongReal.MaxFinite;

PROCEDURE FmtReal (x: REAL): TEXT =
  BEGIN
    RETURN (Fmt.Real(x, style := Fmt.Style.Sci, prec := 8));
  END FmtReal;

PROCEDURE ToBinary (r: LONGREAL): TEXT =
  <* FATAL FloatMode.Trap *>
  CONST X = ARRAY [0 .. 1] OF CHAR{'0', '1'};
  VAR
    x  := LongFloatExtras.ToBinary(r);
    n  := LAST(x.bits^);
    buf:  ARRAY [0 .. 100] OF CHAR;
  BEGIN
    buf[0] := X[x.bits[0]];
    buf[1] := '.';
    FOR i := 1 TO n DO buf[i + 1] := X[x.bits[i]]; END;
    RETURN (Text.FromChars(SUBARRAY(buf, 0, n + 2))
              & Fmt.F("*10^(%s)", Fmt.Int(x.exp, base := 2)));
  END ToBinary;

PROCEDURE FmtLong (x: LONGREAL): TEXT =
  BEGIN
    RETURN (Fmt.LongReal(x, style := Fmt.Style.Sci, prec := 16));
  END FmtLong;

(*
 * Test if y is nearest float to square root of x.
 *
 * Uses  y is correctly rnded square root iff y*y- < x <= y*y+
 *
 * See "Computation of elementary functions on the IBM RISC System/6000
 *      processor", by P. W. Markstein, IBM J Res Develop, 34(1),
 *      January 1990, p. 115.
 *)
PROCEDURE RealCheckNearest (x, y: REAL): BOOLEAN =
  <* FATAL FloatMode.Trap *>
  VAR
    yPlus, yMinus: REAL;
    right, left  : REAL;
  BEGIN
    yMinus := RealFloat.NextAfter(y, Down);
    yPlus := RealFloat.NextAfter(y, Up);
    left := ABC(y, yMinus, x);
    right := ABC(y, yPlus, x);
    RETURN (left < 0.0 AND right >= 0.0);
  END RealCheckNearest;

PROCEDURE LongCheckNearest (x, y: LONGREAL): BOOLEAN =
  <* FATAL FloatMode.Trap *>
  VAR
    yPlus, yMinus: LONGREAL;
    right, left  : LONGREAL;
  BEGIN
    yMinus := LongFloat.NextAfter(y, DownL);
    yPlus := LongFloat.NextAfter(y, UpL);
    left := LongABC(y, yMinus, x);
    right := LongABC(y, yPlus, x);
    RETURN (left < 0.0d0 AND right >= 0.0d0);
  END LongCheckNearest;

(*
 * Test if y is ceil[sqrt(x)]
 *     true if y- * y- < x <= y*y
 *)
PROCEDURE RealCheckUp (x, y: REAL): BOOLEAN =
  <* FATAL FloatMode.Trap *>
  VAR
    yMinus     : REAL;
    right, left: REAL;
  BEGIN
    yMinus := RealFloat.NextAfter(y, Down);
    left := ABC(yMinus, yMinus, x);
    right := ABC(y, y, x);
    RETURN (left < 0.0 AND right >= 0.0);
  END RealCheckUp;

PROCEDURE LongCheckUp (x, y: LONGREAL): BOOLEAN =
  <* FATAL FloatMode.Trap *>
  VAR
    yMinus     : LONGREAL;
    right, left: LONGREAL;
  BEGIN
    yMinus := LongFloat.NextAfter(y, DownL);
    left := LongABC(yMinus, yMinus, x);
    right := LongABC(y, y, x);
    RETURN (left < 0.0d0 AND right >= 0.0d0);
  END LongCheckUp;

(*
 * Test if y is floor[sqrt(x)]
 *    true if y*y <= x < y+ * y+
 *)
PROCEDURE RealCheckDown (x, y: REAL): BOOLEAN =
  <* FATAL FloatMode.Trap *>
  VAR
    yPlus      : REAL;
    right, left: REAL;
  BEGIN
    yPlus := RealFloat.NextAfter(y, Up);
    left := ABC(y, y, x);
    right := ABC(yPlus, yPlus, x);
    RETURN (left <= 0.0 AND right > 0.0);
  END RealCheckDown;

PROCEDURE LongCheckDown (x, y: LONGREAL): BOOLEAN =
  <* FATAL FloatMode.Trap *>
  VAR
    yPlus      : LONGREAL;
    right, left: LONGREAL;
  BEGIN
    yPlus := LongFloat.NextAfter(y, UpL);
    left := LongABC(y, y, x);
    right := LongABC(yPlus, yPlus, x);
    RETURN (left <= 0.0d0 AND right > 0.0d0);
  END LongCheckDown;

PROCEDURE TestSingle (x: REAL; mode: RoundingMode) =
  <* FATAL Thread.Alerted, Wr.Failure, FloatMode.Trap *>
  VAR
    y        :  REAL;    (* computed sqrt(x) *)
    ulp      :  REAL;
    ok       :  BOOLEAN;
    reference:  REAL;
    exact    :  BOOLEAN;
    print    := FALSE;
  BEGIN
    FloatMode.ClearFlag(Flag.Inexact);
    y := RealSqrt.Sqrt(x);
    exact := NOT Flag.Inexact IN FloatMode.GetFlags();
    CASE mode OF <* NOWARN *>
    | RoundingMode.NearestElseEven => ok := RealCheckNearest(x, y);
    | RoundingMode.TowardPlusInfinity => ok := RealCheckUp(x, y);
    | RoundingMode.TowardMinusInfinity => ok := RealCheckDown(x, y);
    END;

    reference := FLOAT(Math.sqrt(FLOAT(x, LONGREAL)), REAL);
    ulp := RealUlp(x, y);

    <* ASSERT NOT (mode = RoundingMode.NearestElseEven AND ABS(ulp) > 0.51 AND ok) *>

    IF NOT ok THEN
      print := TRUE;
      Wr.PutText(
        Stdio.stderr,
        Fmt.F("** ERROR ** sqrt(%s)=%s, error=%s ulps\n\n", FmtReal(x),
              FmtReal(y), Fmt.Real(ulp, prec := 2)));
      Wr.PutText(Stdio.stderr,
                 Fmt.F("   sqrt(%s)=%s\n", ToBinary(FLOAT(x, LONGREAL)),
                       ToBinary(FLOAT(y, LONGREAL))));
    END;
    IF y # reference THEN
      print := TRUE;
      Wr.PutText(
        Stdio.stderr, Fmt.F("x=%s, Math.sqrt(x)=%s, RealSqrt.Sqrt(x)=%s\n",
                            FmtReal(x), FmtReal(reference), FmtReal(y)));
    END;

    FloatMode.ClearFlag(Flag.Inexact);
    WITH z       = y * y,
         inexact = Flag.Inexact IN FloatMode.GetFlags() DO
      IF (exact # (z = x AND NOT inexact)) THEN
        print := TRUE;
        Wr.PutText(
          Stdio.stderr,
          Fmt.F(
            "** flag ERROR ** x=%s, RealSqrt.Sqrt(x)=%s, Flag.Inexact=%s\n",
            FmtReal(x), FmtReal(y), Fmt.Bool(inexact)));
        Wr.PutText(Stdio.stderr,
                   Fmt.F("   sqrt(%s)=%s\n", ToBinary(FLOAT(x, LONGREAL)),
                         ToBinary(FLOAT(y, LONGREAL))));
      END;
    END;
    IF print THEN Wr.PutText(Stdio.stderr, "\n"); END;
  END TestSingle;

PROCEDURE TestDouble (x: LONGREAL; mode: RoundingMode) =
  <* FATAL Thread.Alerted, Wr.Failure, FloatMode.Trap *>
  VAR
    y        :  LONGREAL; (* computed sqrt(x) *)
    reference:  LONGREAL;
    ulp      :  REAL;
    ok       :  BOOLEAN;
    exact    :  BOOLEAN;
    print    := FALSE;
  BEGIN

    FloatMode.ClearFlag(Flag.Inexact);
    y := LongSqrt.Sqrt(x);
    exact := NOT Flag.Inexact IN FloatMode.GetFlags();
    CASE mode OF <* NOWARN *>
    | RoundingMode.NearestElseEven => ok := LongCheckNearest(x, y);
    | RoundingMode.TowardPlusInfinity => ok := LongCheckUp(x, y);
    | RoundingMode.TowardMinusInfinity => ok := LongCheckDown(x, y);
    END;

    ulp := LongUlp(x, y);
    reference := Math.sqrt(x);
    <* ASSERT NOT (mode = RoundingMode.NearestElseEven AND ABS(ulp) > 0.51 AND ok) *>

    IF NOT ok THEN
      print := TRUE;
      Wr.PutText(Stdio.stderr,
                 Fmt.F("** ERROR ** sqrt(%s)=%s, error=%s\n", FmtLong(x),
                       FmtLong(y), Fmt.Real(ulp, prec := 2)));
      Wr.PutText(
        Stdio.stderr, Fmt.F("   sqrt(%s)=%s\n", ToBinary(x), ToBinary(y)));
    END;
    IF y # reference THEN
      print := TRUE;
      Wr.PutText(
        Stdio.stderr, Fmt.F("x=%s, Math.sqrt(x)=%s, LongSqrt.Sqrt(x)=%s\n",
                            FmtLong(x), FmtLong(reference), FmtLong(y)));
    END;
    FloatMode.ClearFlag(Flag.Inexact);
    WITH z       = y * y,
         inexact = Flag.Inexact IN FloatMode.GetFlags() DO
      IF (exact # (z = x AND NOT inexact)) THEN
        print := TRUE;
        Wr.PutText(
          Stdio.stderr,
          Fmt.F(
            "** flag ERROR ** x=%s, LongSqrt.Sqrt(x)=%s, Flag.Inexact=%s\n",
            FmtLong(x), FmtLong(y), Fmt.Bool(inexact)));
        Wr.PutText(Stdio.stderr,
                   Fmt.F("   sqrt(%s)=%s\n", ToBinary(x), ToBinary(y)));
      END;
    END;
    IF print THEN Wr.PutText(Stdio.stderr, "\n"); END;
  END TestDouble;

(*
 * Estimate ulp error between y and sqrt(x)
 *
 *  sqrt(x) = sqrt(y^2 + (x - y*y) = sqrt(y^2*(1 + (x-y^1)/y^2))) =
 *          = y*(1 + 0.5*(x-y*y)/y*y)
 *
 *  So y - sqrt(x) = (x - y*y)/2*y
 *)
PROCEDURE RealUlp (x, y: REAL): REAL =
  <*FATAL FloatMode.Trap*>
  VAR err, ulp: REAL;
  BEGIN
    (* estimate error *)
    WITH x1 = FLOAT(x, LONGREAL),
         y1 = FLOAT(y, LONGREAL) DO
      err := FLOAT((x1 - y1 * y1)) / (2.0 * y);
    END;

    IF err > 0.0 THEN
      ulp := RealFloat.NextAfter(y, Up) - y;
    ELSE
      ulp := y - RealFloat.NextAfter(y, Down);
    END;
    RETURN (err / ulp);
  END RealUlp;

(* estimate ulp error between y and sqrt(x) *)
PROCEDURE LongUlp (x, y: LONGREAL): REAL =
  <*FATAL FloatMode.Trap*>
  VAR err, ulp: LONGREAL;
  BEGIN
    (* estimate error *)
    (* err := (x - y * y) / (2.0d0 * y);*)
    err := -LongABC(y, y, x) / (2.0d0 * y);

    IF err > 0.0d0 THEN
      ulp := LongFloat.NextAfter(y, UpL) - y;
    ELSE
      ulp := y - LongFloat.NextAfter(y, DownL);
    END;
    RETURN (FLOAT(err / ulp, REAL));
  END LongUlp;

CONST
  RealHalf = ARRAY OF (* have square root that is almost halfway between 2
                         REAL's *)
               REAL{
               4194304.5, 4194305.5, 4194306.5, 4194307.5, 4194308.5,
               4194309.5, 4194310.5, 4194311.5, 4194312.5, 4241585.0,
               4300071.0, 4334264.0, 4424608.0, 4484453.0, 4561317.0,
               4685408.5, 4730693.5, 4739396.5, 4795436.5, 4901269.0,
               4965190.0, 5153825.0, 5161086.0, 5186775.5, 5192893.5,
               5252282.0, 5358336.0, 5372677.5, 5532561.5, 5553655.5,
               5672622.0, 5675446.0, 5675902.0, 5769366.5, 5778956.0,
               5792609.5, 5808845.5, 5809996.5, 5835269.5, 5865669.5,
               5953226.5, 5983573.5, 6010646.5, 6055475.0, 6076358.0,
               6118427.5, 6140155.5, 6238552.0, 6280227.5, 6282888.0,
               6346957.0, 6416995.0, 6417397.0, 6432958.5, 6533868.0,
               6571068.5, 6577431.0, 6660340.5, 6734848.0, 6739801.0,
               6869012.0, 6873161.5, 6893856.0, 6946292.0, 6950878.0,
               6959352.5, 6959808.5, 6975854.0, 7117671.0, 7269508.0,
               7392150.5, 7486288.5, 7504679.0, 7602386.5, 7637093.5,
               7838557.0, 7869005.0, 7869525.5, 7892199.0, 7935994.5,
               7960637.0, 8001493.0, 8018853.0, 8125035.0, 8166092.5,
               8185785.5, 8259749.0, 8515487.0, 9118273.0, 9418507.0,
               9429058.0, 9509432.0, 9575999.0, 9801419.0, 9948955.0,
               9982711.0, 10049797.0, 10197969.0, 10236248.0, 10339317.0,
               10361627.0, 10535046.0, 10777556.0, 10800178.0, 11068332.0,
               11147135.0, 11427583.0, 11470126.0, 11613726.0, 11731646.0,
               12291020.0, 12291626.0, 12351047.0, 12406798.0, 12515460.0,
               12696654.0, 12805581.0, 12820628.0, 13015299.0, 13234766.0,
               13440042.0, 13472236.0, 13633360.0, 13674210.0, 13720183.0,
               13761391.0, 13853921.0, 13900741.0, 13939818.0, 13941601.0,
               13966783.0, 14002934.0, 14040213.0, 14282785.0, 14344251.0,
               14378204.0, 14639013.0, 14671061.0, 14900238.0, 14939832.0,
               14950214.0, 15635314.0, 15654183.0, 15703553.0, 15736966.0,
               15752836.0, 15835780.0, 15863197.0, 16084779.0, 16151318.0,
               16215796.0, 16521227.0, 16777191.0, 16777193.0, 16777195.0,
               16777197.0, 16777199.0, 16777201.0, 16777203.0, 16777205.0,
               16777207.0, 16777209.0, 16777211.0, 16777213.0, 16777215.0};

  LongHalf = ARRAY OF (* have square root that is almost halfway between 2
                         LONGREAL's *)
               LONGREAL{12630985433917202.0d0, 14140115493451522.0d0,
                        10622359326829314.0d0, 14529426478656038.0d0,
                        18014398509481946.0d0, 10412505862361804.0d0,
                        14857979544772656.0d0, 12272110458011752.0d0,
                        15012207918062568.0d0, 10620364195334306.0d0,
                        18014398509481950.0d0, 9416835178091566.0d0,
                        10817519644772962.0d0, 11327314254525054.0d0,
                        15664840923438180.0d0, 18014398509481954.0d0,
                        13724276018133678.0d0, 14087257551374538.0d0,
                        18014398509481958.0d0, 14953788419250536.0d0,
                        10138921194009788.0d0, 11074576742473558.0d0,
                        18014398509481962.0d0, 17784146910199718.0d0,
                        9475170008135150.0d0, 18014398509481966.0d0,
                        17029961451695424.0d0, 12729398101772876.0d0,
                        9949698353471908.0d0, 18014398509481970.0d0,
                        13204068004119562.0d0, 18014398509481974.0d0,
                        18014398509481978.0d0, 18014398509481982.0d0,
                        12229483590213572.0d0, 11776590070351698.0d0,
                        13660886811523434.0d0, 14175667448115794.0d0,
                        17107648573165680.0d0, 9017704909124822.0d0,
                        16400601912255636.0d0, 9543808112968898.0d0,
                        11987057271432288.0d0, 13960287874037450.0d0,
                        10630475661173506.0d0, 15932592072887298.0d0,
                        17208138202032452.0d0, 11278530422044232.0d0,
                        16924379665868532.0d0, 9646773584056588.0d0,
                        17004998054963212.0d0, 11103514787263370.0d0,
                        10378240392362754.0d0, 9955285241716116.0d0,
                        10613188191822598.0d0, 7910134168456205.0d0,
                        6792129108504523.0d0, 6526785180155591.0d0,
                        6780829509418618.0d0, 4503599627370515.0d0,
                        6309645949112991.0d0, 7668330337554096.0d0,
                        4557229933882990.0d0, 8198765376547915.0d0,
                        6209798762725737.0d0, 5820379216527142.0d0,
                        4503599627370513.0d0, 9005390497796871.0d0,
                        7796844571085252.0d0, 5013792105612958.0d0,
                        5801462123391087.0d0, 6852508129293224.0d0,
                        4503599627370511.0d0, 7085588333036031.0d0,
                        6256852942279850.0d0, 5661667300616590.0d0,
                        6589730288677452.0d0, 5332856158998094.0d0,
                        6828847101306741.0d0, 4503599627370509.0d0,
                        5246631634595212.0d0, 6611732972257822.0d0,
                        6247465196992739.0d0, 4503599627370507.0d0,
                        4619836149807707.0d0, 4738990312665130.0d0,
                        6319920530813328.0d0, 4503599627370505.0d0,
                        5016562740764439.0d0, 7832596519208111.0d0,
                        4503599627370503.0d0, 5211084258275080.0d0,
                        7468007935834813.0d0, 4503599627370501.0d0,
                        5161478502861812.0d0, 4503599627370499.0d0,
                        4503599627370497.0d0, 8233674359446949.0d0,
                        5260294500220744.0d0, 8613063800570571.0d0,
                        7780471400773538.0d0, 5190435713359146.0d0,
                        7131261283482639.0d0, 6529697357028814.0d0,
                        5025703634653482.0d0, 6767526937110803.0d0,
                        4867181094540022.0d0, 7758367381958044.0d0,
                        4974534772770429.0d0, 8682299446046386.0d0,
                        6104896114926238.0d0, 6729242612525228.0d0,
                        6949879155776626.0d0, 5367285068086473.0d0,
                        5305742241572926.0d0, 8434800808185807.0d0,
                        6917736263647589.0d0, 5640343457202975.0d0,
                        7587287300470286.0d0, 5943359721809592.0d0,
                        4588342722451536.0d0, 5325674572325366.0d0,
                        4603096699845568.0d0, 4920573350852735.0d0,
                        5040686290262644.0d0, 5074120037052501.0d0,
                        5030125637522707.0d0, 7533911277612234.0d0,
                        8537178506910776.0d0, 8850979536606826.0d0,
                        5792564482505966.0d0, 6710281479429148.0d0,
                        6392758346181780.0d0, 8784866593395592.0d0,
                        8874390357806356.0d0, 5444568097723958.0d0,
                        7384673475062338.0d0};

  RealWhole = ARRAY OF (* have square root that is almost exactly a REAL *)
                REAL{
                10260687.0, 10420739.0, 10421855.0, 10472554.0, 10695680.0,
                10809581.0, 10970358.0, 11413802.0, 12101778.0, 12646506.0,
                12691655.0, 12749267.0, 13689673.0, 14508773.0, 14533654.0,
                16410421.0, 16755549.0, 16777198.0, 16777202.0, 16777206.0,
                16777210.0, 16777214.0, 16777220.0, 16777228.0, 16777236.0,
                16777244.0, 16777252.0, 16820592.0, 17464494.0, 17522968.0,
                17667602.0, 17981422.0, 18996186.0, 19432526.0, 19435574.0,
                19574122.0, 20185702.0, 20499030.0, 20942578.0, 21747244.0,
                21808184.0, 22173398.0, 23893408.0, 24103406.0, 25647998.0,
                25776638.0, 25940922.0, 26489312.0, 26655848.0, 26786988.0,
                28413996.0, 28642958.0, 30593468.0, 31025754.0, 31098534.0,
                32077428.0, 32631680.0, 33029732.0, 4194305.0, 4194307.0,
                4194309.0, 4194311.0, 4194313.0, 4205148.0, 4380742.0,
                5436811.0, 5452046.0, 5973352.0, 6622328.0, 6663962.0,
                6696747.0, 7103499.0, 7648367.0, 8019357.0, 8157920.0,
                8257433.0, 8455693.0, 8501366.0, 8502374.0, 8648124.0,
                8998648.0, 9110483.0, 9183766.0, 9694051.0, 9769640.0,
                9886153.0};

  LongWhole = ARRAY OF (* have square root that is almost exactly a
                          LONGREAL *)
                LONGREAL{10360130682438606.0d0, 10796352092159220.0d0,
                         11178073652009938.0d0, 11411018823848480.0d0,
                         11892846029749078.0d0, 11947604209971110.0d0,
                         11977026118312904.0d0, 12076922328356474.0d0,
                         13080572121446398.0d0, 14043274578617222.0d0,
                         15222545564554840.0d0, 15390827971642808.0d0,
                         15461952290984320.0d0, 15562211965401224.0d0,
                         16040491365782944.0d0, 16073183839909748.0d0,
                         16224312206505084.0d0, 16274642198083330.0d0,
                         16616520988040720.0d0, 17526446283873082.0d0,
                         18014398509481948.0d0, 18014398509481956.0d0,
                         18014398509481964.0d0, 18014398509481972.0d0,
                         18014398509481980.0d0, 4503599627370498.0d0,
                         4503599627370502.0d0, 4503599627370506.0d0,
                         4503599627370510.0d0, 4503599627370514.0d0,
                         4655373177220503.0d0, 4752600422275384.0d0,
                         4949272201296351.0d0, 5208794326873153.0d0,
                         5244874648518758.0d0, 5368190801483459.0d0,
                         5439724223157472.0d0, 5468883487826070.0d0,
                         5557185300159266.0d0, 5576434863294694.0d0,
                         5700234001235401.0d0, 5738163233937461.0d0,
                         5758558771032087.0d0, 5827890174646361.0d0,
                         5864189617824254.0d0, 5925996330712030.0d0,
                         5970160885624454.0d0, 6075721009689454.0d0,
                         6531209183803571.0d0, 6859528236067988.0d0,
                         7216483987312563.0d0, 7561391700868156.0d0,
                         8068575271197489.0d0, 8191217256426629.0d0,
                         8243126763392485.0d0, 8316375373258609.0d0,
                         8359726611374516.0d0, 8443219239906722.0d0,
                         8467955559047936.0d0, 8514171063945736.0d0,
                         8666982414488449.0d0, 8691017882404881.0d0,
                         8802241219178795.0d0, 8839323001382567.0d0,
                         8931047894937074.0d0, 9139394906467948.0d0,
                         9490869952260726.0d0, 9908582856408708.0d0};

(* compute a*b - c with correct sign *)
PROCEDURE ABC (a, b, c: REAL): REAL =
  <* FATAL Thread.Alerted, Wr.Failure, FloatMode.Failure *>

  (* round to 24/2 = 12 bits *)
  PROCEDURE Round (x: REAL): REAL =
    CONST m = 4097.0; (* 2^12 + 1 *)
    VAR y: REAL;
    BEGIN
      WITH mx = m * x DO y := mx - (mx - x); RETURN (y); END;
    END Round;

  VAR
    a1, a2, b1, b2:  REAL;
    ansLo, ansHi  :  REAL;
    saveMode      := FloatMode.GetRounding();
  BEGIN
    (* Split a = a1 + a2, similarly for b.  Round() wants round-to-nearest
       mode *)
    FloatMode.SetRounding(RoundingMode.NearestElseEven);
    a1 := Round(a);
    a2 := a - a1;
    b1 := Round(b);
    b2 := b - b1;

    FloatMode.SetRounding(RoundingMode.TowardPlusInfinity);
    ansHi := (((a1 * b1 - c) + a1 * b2 + a2 * b1) + a2 * b2);
    FloatMode.SetRounding(RoundingMode.TowardMinusInfinity);
    ansLo := (((a1 * b1 - c) + a1 * b2 + a2 * b1) + a2 * b2);
    IF ansHi # ansLo AND ansHi * ansLo <= 0.0 THEN
      (* I don't think this can happen (proof?) *)
      Wr.PutText(Stdio.stderr, "\n  Help! ABC test failed!!\n");
    END;

    FloatMode.SetRounding(saveMode);
    RETURN (ansLo);
  END ABC;

 (* Compute a*b - c with the correct sign. *)
PROCEDURE LongABC (a, b, c: LONGREAL): LONGREAL =
  <* FATAL Thread.Alerted, Wr.Failure, FloatMode.Failure *>

  (* round to 53/2 = 27 bits *)
  PROCEDURE Round (x: LONGREAL): LONGREAL =
    CONST m = 134217729.0d0; (* 2^27 + 1 *)
    VAR y: LONGREAL;
    BEGIN
      WITH mx = m * x DO y := mx - (mx - x); RETURN (y); END;
    END Round;

  VAR
    a1, a2, b1, b2:  LONGREAL;
    ansLo, ansHi  :  LONGREAL;
    saveMode      := FloatMode.GetRounding();
  BEGIN
    (* Round() wants round-to-nearest mode *)
    FloatMode.SetRounding(RoundingMode.NearestElseEven);
    a1 := Round(a);
    a2 := a - a1;
    b1 := Round(b);
    b2 := b - b1;

    FloatMode.SetRounding(RoundingMode.TowardPlusInfinity);
    ansHi := (((a1 * b1 - c) + a1 * b2 + a2 * b1) + a2 * b2);
    FloatMode.SetRounding(RoundingMode.TowardMinusInfinity);
    ansLo := (((a1 * b1 - c) + a1 * b2 + a2 * b1) + a2 * b2);
    IF ansHi # ansLo AND ansHi * ansLo <= 0.0d0 THEN
      (* I don't think this can happen (proof?) *)
      Wr.PutText(Stdio.stderr, "\n  Help! LongABC test failed!!\n");
    END;

    FloatMode.SetRounding(saveMode);
    RETURN (ansLo);
  END LongABC;

PROCEDURE DefaultTest (READONLY realArr: ARRAY OF REAL;
                       READONLY longArr: ARRAY OF LONGREAL;
                                mode   : RoundingMode       ) =
  <* FATAL Thread.Alerted, Wr.Failure, FloatMode.Trap *>
  VAR
    in, in1      : REAL;
    inDbl, inDbl1: LONGREAL;
    rand         := NEW (Random.Default).init();
  BEGIN
    Wr.PutText(Stdio.stderr, "testing hard to round cases\n");
    Wr.PutText(Stdio.stderr, "  single\n");
    FOR i := FIRST(realArr) TO LAST(realArr) DO
      in := realArr[i];
      TestSingle(in, mode);
    END;
    Wr.PutText(Stdio.stderr, "  double\n");
    FOR i := FIRST(longArr) TO LAST(longArr) DO
      inDbl := longArr[i];
      TestDouble(inDbl, mode);
    END;

    Wr.PutText(Stdio.stderr, "testing random cases\n");
    Wr.PutText(Stdio.stderr, "  single\n");
    FOR i := 0 TO 2000 DO
      in := rand.real (1.0, 4.0);
      TestSingle(in, mode);
    END;
    Wr.PutText(Stdio.stderr, "  double\n");
    FOR i := 0 TO 2000 DO
      inDbl := rand.longreal (1.0d0, 4.0d0);
      TestDouble(inDbl, mode);
    END;

    Wr.PutText(
      Stdio.stderr, "testing preturbations of regularly spaced args\n");

    Wr.PutText(Stdio.stderr, "  single\n");
    FOR i := 0 TO 96 DO
      in := 32.0 + FLOAT(i);

      in1 := in;
      FOR j := 0 TO 5 DO
        TestSingle(in1, mode);
        in1 := RealFloat.NextAfter(in1, Up);
      END;

      in1 := in;
      FOR j := 0 TO 5 DO
        in1 := RealFloat.NextAfter(in1, Down);
        TestSingle(in1, mode);
      END;
    END;

    Wr.PutText(Stdio.stderr, "  double\n");
    FOR i := 0 TO 96 DO
      inDbl := 32.0d0 + FLOAT(i, LONGREAL);

      inDbl1 := inDbl;
      FOR j := 0 TO 5 DO
        TestDouble(inDbl1, mode);
        inDbl1 := LongFloat.NextAfter(inDbl1, UpL);
      END;

      inDbl1 := inDbl;
      FOR j := 0 TO 5 DO
        inDbl1 := LongFloat.NextAfter(inDbl1, DownL);
        TestDouble(inDbl1, mode);
      END;
    END;
    Wr.PutText(Stdio.stderr, "Done testing.\n");
  END DefaultTest;

(* XXX: can't set rounding mode *)
PROCEDURE FromFile (rd: Rd.T) =
  <* FATAL Rd.Failure, Thread.Alerted *>
  VAR x: LONGREAL;
  BEGIN
    TRY
      LOOP
        WITH txt = Rd.GetLine(rd) DO
          TRY
            x := Scan.LongReal(txt);
            TestDouble(x, RoundingMode.NearestElseEven);
          EXCEPT
            Lex.Error, FloatMode.Trap =>
          END;
        END;
      END;
    EXCEPT
      Rd.EndOfFile =>
    END;
  END FromFile;

PROCEDURE Main () =
  <* FATAL Thread.Alerted, Wr.Failure, FloatMode.Failure *>
  BEGIN
    IF Params.Count > 1 THEN
      FromFile(Stdio.stdin)
    ELSE
      Wr.PutText(Stdio.stderr, "ROUND-TO-NEAREST MODE\n");
      DefaultTest(RealHalf, LongHalf, RoundingMode.NearestElseEven);

      FloatMode.SetRounding(RoundingMode.TowardPlusInfinity);
      Wr.PutText(Stdio.stderr, "\nROUND-TO-PLUS-INF MODE\n");
      DefaultTest(RealWhole, LongWhole, RoundingMode.TowardPlusInfinity);

      FloatMode.SetRounding(RoundingMode.TowardMinusInfinity);
      Wr.PutText(Stdio.stderr, "\nROUND-TO-MINUS-INF MODE\n");
      DefaultTest(RealWhole, LongWhole, RoundingMode.TowardMinusInfinity);
    END;
  END Main;

BEGIN
  Main();
END SqrtTst.

