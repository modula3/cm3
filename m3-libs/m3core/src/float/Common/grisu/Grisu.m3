(* Copyright (C) 2016 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

MODULE Grisu;

IMPORT Word,Long;
IMPORT IEEE;
IMPORT CachedPowers AS CP;
FROM SimFP IMPORT SignificandSize,Uint64,Uint32,GFP;

CONST

(* The minimal and maximal target exponent define the range of w's binary
   exponent, where 'w' is the result of multiplying the input by a cached power
   of ten.

   A different range might be chosen on a different platform, to optimize digit
   generation, but a smaller range requires more powers of ten to be cached.*)
  
  MinTargetExponent = -60;
  MaxTargetExponent = -32;

  SmallPowersOfTen = ARRAY[0..10] OF INTEGER
    {0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000,
     1000000000};
     
TYPE

(* Adjusts the last digit of the generated number, and screens out generated
   solutions that may be inaccurate. A solution may be inaccurate if it is
   outside the safe interval, or if we cannot prove that it is closer to the
   input than a neighboring representation of the same length.
  
   Input: * buffer containing the digits of tooHigh / 10^kappa
          * the buffer's length
          * distanceTooHigh = (tooHigh - w).f() * unit
          * unsafeInterval = (tooHigh - tooLow).f() * unit
          * rest = (tooHigh - buffer * 10^kappa).f() * unit
          * tenKappa = 10^kappa * unit
          * unit = the common multiplier
   Output: returns true if the buffer is guaranteed to contain the closest
      representable number to the input.
    Modifies the generated digits in the buffer to approach (round towards) w. 

  
     Let wLow  = tooHigh - bigDistance, and
         wHigh = tooHigh - smallDistance.
     Note: wLow < w < wHigh
    
     The real w (unit) must lie somewhere inside the interval
     [wLow; wHigh] (often written as "(wLow; wHigh)")

     Basically the buffer currently contains a number in the unsafe interval
     [tooLow; tooHigh] with tooLow < w < tooHigh
    
      tooHigh  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                         ^v 1 unit            ^      ^                 ^      ^
      boundaryHigh  ---------------------     .      .                 .      .
                         ^v 1 unit            .      .                 .      .
       - - - - - - - - - - - - - - - - - - -  +  - - + - - - - - -     .      .
                                              .      .         ^       .      .
                                              .  bigDistance   .       .      .
                                              .      .         .       .    rest
                                  smallDistance      .         .       .      .
                                              v      .         .       .      .
      wHigh  - - - - - - - - - - - - - - - - - -     .         .       .      .
                         ^v 1 unit                   .         .       .      .
      w ----------------------------------------     .         .       .      .
                         ^v 1 unit                   v         .       .      .
      wLow   - - - - - - - - - - - - - - - - - - - - -         .       .      .
                                                               .       .      v
      buffer --------------------------------------------------+-------+--------
                                                               .       .
                                                      safeInterval     .
                                                               v       .
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     .
                         ^v 1 unit                                     .
      boundaryLow  -------------------------                     unsafeInterval 
                         ^v 1 unit                                     v
      tooLow   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    
     Note that the value of buffer could lie anywhere inside the range tooLow
     to tooHigh.
    
     boundaryLow, boundaryHigh and w are approximations of the real boundaries
     and v (the input number). They are guaranteed to be precise up to one unit.
     In fact the error is guaranteed to be strictly less than one unit.
    
     Anything that lies outside the unsafe interval is guaranteed not to round
     to v when read again.
     Anything that lies inside the safe interval is guaranteed to round to v
     when read again.
     If the number inside the buffer lies inside the unsafe interval but not
     inside the safe interval then we simply do not know and bail out (returning
     false).
    
     Similarly we have to take into account the imprecision of 'w' when finding
     the closest representation of 'w'. If we have two potential
     representations, and one is closer to both wLow and wHigh, then we know
     it is closer to the actual value v.
    
     By generating the digits of tooHigh we got the largest (closest to
     tooHigh) buffer that is still in the unsafe interval. In the case where
     wHigh < buffer < tooHigh we try to decrement the buffer.
     This way the buffer approaches (rounds towards) w.
     There are 3 conditions that stop the decrementation process:
       1) the buffer is already below wHigh
       2) decrementing the buffer would make it leave the unsafe interval
       3) decrementing the buffer would yield a number below wHigh and farther
          away than the current number. In other words:
                  (buffer{-1} < wHigh) AND wHigh - buffer{-1} > buffer - wHigh
     Instead of using the buffer directly we use its distance to tooHigh.
     Conceptually rest ~= tooHigh - buffer
     We need to do the following tests in this order to avoid over- and
     underflows.*)
  
PROCEDURE RoundWeed(VAR buffer : DigitArr;
                    length : INTEGER;
                    distanceTooHigh,unsafeInterval,rest,tenKappa,unit : Uint64) : BOOLEAN =
  VAR
    smallDistance : Uint64 := distanceTooHigh - unit;
    bigDistance : Uint64 := distanceTooHigh + unit;
  BEGIN
    <*ASSERT Long.LE(rest, unsafeInterval) *>
    WHILE (rest < smallDistance AND
           unsafeInterval - rest >= tenKappa AND
          (rest + tenKappa < smallDistance OR
           smallDistance - rest >= rest + tenKappa - smallDistance)) DO
      DEC(buffer[length - 1]);
      rest := rest + tenKappa;
    END;
    (* We have approached w+ as much as possible. We now test if approaching w-
       would require changing the buffer. If yes, then we have two possible
       representations close to w, but we cannot decide which one is closer. *)
    IF (rest < bigDistance) AND (unsafeInterval - rest >= tenKappa) AND
       (rest + tenKappa < bigDistance OR
        bigDistance - rest > rest + tenKappa - bigDistance) THEN
      RETURN FALSE;
    END;

    (* Weeding test.
       The safe interval is [tooLow + 2 ulp; tooHigh - 2 ulp]
       Since tooLow = tooHigh - unsafeInterval this is equivalent to
          [tooHigh - unsafeInterval + 4 ulp; tooHigh - 2 ulp]
       Conceptually we have: rest ~= tooHigh - buffer *)    
    RETURN (2L * unit <= rest) AND (rest <= unsafeInterval - 4L * unit);
  END RoundWeed;

(* Rounds the buffer upwards if the result is closer to v by possibly adding
   1 to the buffer. If the precision of the calculation is not sufficient to
   round correctly, return false.
   The rounding might shift the whole buffer in which case the kappa is
   adjusted. For example "99", kappa = 3 might become "10", kappa = 4.

   If 2*rest > tenKappa then the buffer needs to be rounded up.
   rest can have an error of +/- 1 unit. This function accounts for the
   imprecision and returns false, if the rounding direction cannot be
   unambiguously determined.

   Precondition: rest < tenKappa. *)
   
TYPE
  (* The main interface is designed to agree with the Dragon interface buffer
     base type which is 0..9. But this procedure temporarily raises the
     base type to 0..10 so we copy the buffer before the calcs then copy
     it back at the end*)
  BufType = REF ARRAY OF [0..10];
  
PROCEDURE RoundWeedCounted(VAR buffer : DigitArr; 
                           length : INTEGER;
                           rest, tenKappa, unit : Uint64;
                           VAR kappa : INTEGER) : BOOLEAN =
  VAR buf : BufType;
  BEGIN
    <*ASSERT rest < tenKappa *>
    (* The following tests are done in a specific order to avoid overflows. They
       will work correctly with any uint64 values of rest < tenKappa and unit.
    
       If the unit is too big, then we don't know which way to round. For example
       a unit of 50 means that the real number lies within rest +/- 50. If
       10^kappa = 40 then there is no way to tell which way to round. *)    
    IF (unit >= tenKappa) THEN RETURN FALSE; END;
    (* Even if unit is just half the size of 10^kappa we are already completely
       lost. (And after the previous test we know that the expression will not
       over/underflow.) *)    
    IF (tenKappa - unit <= unit) THEN RETURN FALSE; END;
    (* If 2 * (rest + unit) <= 10^kappa we can safely round down. *)
    IF ((tenKappa - rest > rest) AND (tenKappa - 2L * rest >= 2L * unit)) THEN
      RETURN TRUE;
    END;
    
    (* If 2 * (rest - unit) >= 10^kappa, then we can safely round up.*)
    IF (rest > unit) AND (tenKappa - (rest - unit) <= (rest - unit)) THEN
      buf := NEW(BufType,length);
      FOR i := 0 TO length-1 DO buf^[i] := buffer[i]; END;
      INC(buf[length - 1]);
      FOR i := length - 1 TO 1 BY -1 DO
        IF buf[i] # 10 THEN EXIT; END;
        buf[i] := 0;
        INC(buf[i - 1]);
      END;
      (* If the first digit is now '0'+ 10 we had a buffer with all '9's. With the
         exception of the first digit all digits are now '0'. Simply switch the
         first digit to '1' and adjust the kappa. Example: "99" becomes "10" and
         the power (the kappa) is increased. *)
      IF buf[0] = 10 THEN
        buf[0] := 1;
        INC(kappa);
      END;
      FOR i := 0 TO length-1 DO buffer[i] := buf^[i]; END;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END RoundWeedCounted;

(* Returns the biggest power of ten that is less than or equal to the given
   number. We furthermore receive the maximum number of bits 'number' has.

   Returns power = 10^(expPlusOne-1) such that
   power <= number < power * 10.
   If numBits = 0 then 0^(0-1) is returned.
   The number of bits must be <= 32.
   Precondition: number < (1 << (numBits + 1)).

   Inspired by the method for finding an integer log base 10 from here:
   http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog10 *)

PROCEDURE BiggestPowerTen(number : Uint32;
                          numBits : INTEGER;
                          VAR power : INTEGER;
                          VAR expPlusOne : INTEGER) =
  VAR
    expPlusOneGuess : INTEGER;
  BEGIN
    <*ASSERT number < Word.LeftShift(1, (numBits + 1)) *>
    (* 1233/4096 is approximately 1/lg(10). *)
    expPlusOneGuess := Word.RightShift((numBits + 1) * 1233, 12);
    (* We increment to skip over the first entry in the SmallPowersOf10 table.
      Note: SmallPowersOf10[i] = 10^(i-1). *)
    INC(expPlusOneGuess);
    (* We don't have any guarantees that 2^numBits <= number. *)
    IF number < SmallPowersOfTen[expPlusOneGuess] THEN
      DEC(expPlusOneGuess);
    END;
    power := SmallPowersOfTen[expPlusOneGuess];
    expPlusOne := expPlusOneGuess;
  END BiggestPowerTen;

(* Generates the digits of input number w.
   w is a floating-point number (GFP), consisting of a significand and an
   exponent. Its exponent is bounded by MinTargetExponent and MaxTargetExponent.
         Hence -60 <= w.e() <= -32.
  
   Returns false if it fails, in which case the generated digits in the buffer
   should not be used.
   Preconditions:
    * low, w and high are correct up to 1 ulp (unit in the last place). That
      is, their error must be less than a unit of their last digits.
    * low.e() = w.e() = high.e()
    * low < w < high, and taking into account their error: low~ <= high~
    * MinTargetExponent <= w.e() <= MaxTargetExponent
   Postconditions: returns false if procedure fails.
     otherwise:
       * buffer is not null-terminated, but len contains the number of digits.
       * buffer contains the shortest possible decimal digit-sequence
         such that LOW < buffer * 10^kappa < HIGH, where LOW and HIGH are the
         correct values of low and high (without their error).
       * if more than one decimal representation gives the minimal number of
         decimal digits then the one closest to W (where W is the correct value
         of w) is chosen.
   Remark: this procedure takes into account the imprecision of its input
     numbers. If the precision is not enough to guarantee all the postconditions
     then false is returned. This usually happens rarely (~0.5%).
  
   Say, for the sake of example, that
     w.e() = -48, and w.f() = 0x1234567890abcdef
   w's value can be computed by w.f() * 2^w.e()
   We can obtain w's integral digits by simply shifting w.f() by -w.e().
    -> w's integral part is 0x1234
    w's fractional part is therefore 0x567890abcdef.
   Printing w's integral part is easy (simply print 0x1234 in decimal).
   In order to print its fraction we repeatedly multiply the fraction by 10 and
   get each digit. Example the first digit after the point would be computed by
     (0x567890abcdef * 10) >> 48. -> 3
   The whole thing becomes slightly more complicated because we want to stop
   once we have enough digits. That is, once the digits inside the buffer
   represent 'w' we can stop. Everything inside the interval low - high
   represents w. However we have to pay attention to low, high and w's
   imprecision. *)

PROCEDURE DigitGen(low,w,high : GFP; 
                   VAR buffer : DigitArr;
                   VAR length : CARDINAL;
                   VAR kappa : INTEGER) : BOOLEAN =
  VAR
    tooLow,tooHigh,unsafeInterval,one : GFP;
    integrals,divisor,divisorExpPlusOne,digit : INTEGER;    
    fractionals,rest,unit : Uint64;
  BEGIN
    <*ASSERT low.e() = w.e() AND w.e() = high.e() *>
    <*ASSERT Long.LE(low.f() + 1L, high.f() - 1L) *>
    <*ASSERT MinTargetExponent <= w.e() AND w.e() <= MaxTargetExponent *>
    
    unit := 1L;
    
    (* low, w and high are imprecise, but by less than one ulp (unit in the last
     place).
     If we remove (resp. add) 1 ulp from low (resp. high) we are certain that
     the new numbers are outside of the interval we want the final
     representation to lie in.
     Inversely adding (resp. removing) 1 ulp from low (resp. high) would yield
     numbers that are certain to lie in the interval. We will use this fact
     later on.
     We will now start by generating the digits within the uncertain
     interval. Later we will weed out representations that lie outside the safe
     interval and thus _might_ lie outside the correct interval. *)

    tooLow := NEW(GFP).init(low.f() - unit, low.e());
    tooHigh := NEW(GFP).init(high.f() + unit, high.e());

    (* tooLow and tooHigh are guaranteed to lie outside the interval we want the
       generated number in. *)
    unsafeInterval := tooHigh.minus(tooLow);

    (* We now cut the input number into two parts: the integral digits and the
       fractionals. We will not write any decimal separator though, but adapt
       kappa instead.
       Reminder: we are currently computing the digits (stored inside the buffer)
       such that:   tooLow < buffer * 10^kappa < tooHigh
       We use tooHigh for the digit_generation and stop as soon as possible.
       If we stop early we effectively round down. *)
    
    one := NEW(GFP).init(Long.LeftShift(1L, -w.e()), w.e());
    (* Division by one is a shift. *)
    integrals := VAL(Long.RightShift(tooHigh.f(), -one.e()),INTEGER);
    (* Modulo by one is an and. *)
    fractionals := Long.And(tooHigh.f(), (one.f() - 1L));

    BiggestPowerTen(integrals, SignificandSize - (-one.e()),
                    divisor, divisorExpPlusOne);

    kappa := divisorExpPlusOne;
    length := 0;
    (* Loop invariant: buffer = tooHigh / 10^kappa  (integer division)
       The invariant holds for the first iteration: kappa has been initialized
       with the divisor exponent + 1. And the divisor is the biggest power of ten
       that is smaller than integrals. *)
    WHILE kappa > 0 DO
      digit := integrals DIV divisor;
      <* ASSERT digit <= 9 *>
      buffer[length] := digit;
      INC(length);
      integrals := integrals MOD divisor;
      DEC(kappa);
      (* Note that kappa now equals the exponent of the divisor and that the
         invariant thus holds again. *)
      rest := Long.LeftShift(VAL(integrals,Uint64), -one.e()) + fractionals;

      (* Invariant: tooHigh = buffer * 10^kappa + GFP(rest, one.e())
         Reminder: unsafeInterval.e() = one.e() *)
      IF rest < unsafeInterval.f() THEN
        (* Rounding down (by not emitting the remaining digits) yields a number
           that lies within the unsafe interval. *)
        RETURN RoundWeed(buffer, length, tooHigh.minus(w).f(),
                         unsafeInterval.f(), rest,
                         Long.LeftShift(VAL(divisor,Uint64), -one.e()), unit);
      END;
      divisor := divisor DIV 10;
    END;

    (* The integrals have been generated. We are at the point of the decimal
       separator. In the following loop we simply multiply the remaining digits by
       10 and divide by one. We just need to pay attention to multiply associated
       data (like the interval or 'unit'), too.
       Note that the multiplication by 10 does not overflow, because w.e >= -60
       and thus one.e >= -60. *)
    <*ASSERT one.e() >= -60 *>
    <*ASSERT fractionals < one.f() *>
    <*ASSERT Long.Divide(16_FFFFFFFFFFFFFFFFL, 10L) >= one.f() *>
    
    LOOP
      fractionals := fractionals * 10L;
      unit := unit * 10L;
      unsafeInterval.setF(unsafeInterval.f() * 10L);
      (* Integer division by one. *)
      digit := VAL(Long.RightShift(fractionals, -one.e()),INTEGER);
      <*ASSERT digit <= 9 *>
      buffer[length] := digit;
      INC(length);
      fractionals := Long.And(fractionals, one.f() - 1L); (* Modulo by one. *)
      DEC(kappa);
      IF fractionals < unsafeInterval.f() THEN
        RETURN RoundWeed(buffer, length, tooHigh.minus(w).f() * unit, 
                         unsafeInterval.f(), fractionals, one.f(), unit);
      END;
    END;
  END DigitGen;

(* Generates (at most) requested_digits digits of input number w.
   w is a floating-point number (GFP), consisting of a significand and an
   exponent. Its exponent is bounded by MinTargetExponent and MaxTargetExponent.
         Hence -60 <= w.e() <= -32.
  
   Returns false if it fails, in which case the generated digits in the buffer
   should not be used.
   Preconditions:
    * w is correct up to 1 ulp (unit in the last place). That
      is, its error must be strictly less than a unit of its last digit.
    * MinTargetExponent <= w.e() <= MaxTargetExponent
  
   Postconditions: returns false if procedure fails.
     otherwise:
       * buffer is not null-terminated, but length contains the number of
         digits.
       * the representation in buffer is the most precise representation of
         requested_digits digits.
       * buffer contains at most requested_digits digits of w. If there are less
         than requested_digits digits then some trailing '0's have been removed.
       * kappa is such that
              w = buffer * 10^kappa + eps with |eps| < 10^kappa / 2.
  
   Remark: This procedure takes into account the imprecision of its input
     numbers. If the precision is not enough to guarantee all the postconditions
     then false is returned. This usually happens rarely, but the failure-rate
     increases with higher requested_digits. *)

PROCEDURE DigitGenCounted(w : GFP;
                          requestedDigits : INTEGER;
                          VAR buffer : DigitArr;
                          VAR length : CARDINAL;
                          VAR kappa : INTEGER) : BOOLEAN =
  VAR
    one : GFP;
    error,fractionals,rest : Uint64;
    integrals,divisor,divisorExpPlusOne,digit : INTEGER;
  BEGIN
    <*ASSERT MinTargetExponent <= w.e() AND w.e() <= MaxTargetExponent *>
    <*ASSERT MinTargetExponent >= -60 *>
    <*ASSERT MaxTargetExponent <= -32 *>
    
    error := 1L;
    (*w is assumed to have an error less than 1 unit. Whenever w is scaled we
      also scale its error. *)

    (* We cut the input number into two parts: the integral digits and the
       fractional digits. We don't emit any decimal separator, but adapt kappa
       instead. Example: instead of writing "1.2" we put "12" into the buffer and
       increase kappa by 1. *)
       
    one := NEW(GFP).init(Long.LeftShift(1L, -w.e()), w.e());
    (* Division by one is a shift. *)
    integrals := VAL(Long.RightShift(w.f(), -one.e()),INTEGER);
    (* Modulo by one is an and. *)
    fractionals := Long.And(w.f(), (one.f() - 1L));
    
    BiggestPowerTen(integrals, SignificandSize - (-one.e()),
                    divisor, divisorExpPlusOne);
                    
    kappa := divisorExpPlusOne;
    length := 0;

    (* Loop invariant: buffer = w / 10^kappa  (integer division)
       The invariant holds for the first iteration: kappa has been initialized
        with the divisor exponent + 1. And the divisor is the biggest power of ten
        that is smaller than 'integrals'. *)
    WHILE kappa > 0  DO
      digit := integrals DIV divisor;
      <*ASSERT digit <= 9 *>
      buffer[length] := digit;
      INC(length);
      DEC(requestedDigits);
      integrals := integrals MOD divisor;
      DEC(kappa);
      (* Note that kappa now equals the exponent of the divisor and that the
         invariant thus holds again. *)
      IF requestedDigits = 0 THEN EXIT; END;
      divisor := divisor DIV 10;
    END;

    IF requestedDigits = 0 THEN
      rest := Long.LeftShift(VAL(integrals,Uint64), -one.e()) + fractionals;
      RETURN RoundWeedCounted(buffer, length, rest, 
                              Long.LeftShift(VAL(divisor,Uint64), -one.e()), error, kappa);
    END;

    (* The integrals have been generated. We are at the point of the decimal
       separator. In the following loop we simply multiply the remaining digits by
       10 and divide by one. We just need to pay attention to multiply associated
       data (the 'unit'), too.
       Note that the multiplication by 10 does not overflow, because w.e >= -60
       and thus one.e >= -60. *)
    <*ASSERT one.e() >= -60*>
    <*ASSERT fractionals < one.f()*>
    <*ASSERT Long.Divide(16_FFFFFFFFFFFFFFFFL, 10L) >= one.f()*>
    
    WHILE requestedDigits > 0 AND fractionals > error DO
      fractionals := fractionals * 10L;
      error := error * 10L;
      (* Integer division by one. *)
      digit := VAL(Long.RightShift(fractionals, -one.e()), INTEGER);
      <*ASSERT digit <= 9 *>
      buffer[length] := digit;
      INC(length);
      DEC(requestedDigits);
      fractionals := Long.And(fractionals, one.f() - 1L); (* Modulo by one.*)
      DEC(kappa);
    END;
    IF requestedDigits # 0 THEN RETURN FALSE; END;
    RETURN RoundWeedCounted(buffer, length, fractionals, one.f(), error, kappa);
  END DigitGenCounted;

(* Provides a decimal representation of v.
   Returns true if it succeeds, otherwise the result cannot be trusted.
   There will be length digits inside the buffer (not null-terminated).
   If the function returns true then
          v = (LONGREAL) (buffer * 10^decimalExp).
   The digits in the buffer are the shortest representation possible: no
   0.09999999999999999 instead of 0.1. The shorter representation will even be
   chosen even if the longer one would be closer to v.
   The last digit will be closest to the actual v. That is, even if several
   digits might correctly yield 'v' when read again, the closest will be
   computed. *)

PROCEDURE Grisu3(v : LONGREAL;
                 mode : FastDtoaMode;
                 VAR buffer : DigitArr;
                 VAR length : CARDINAL;
                 VAR decimalExp : INTEGER) : BOOLEAN =
  VAR
    w,boundaryMinus,boundaryPlus,tenMk,wScaled,scaledBoundaryMinus,scaledBoundaryPlus : GFP;
    tenMkMaxBinExp,tenMkMinBinExp,mk,kappa : INTEGER;
    real : REAL;
    result : BOOLEAN;
    d : IEEE.Double;
    s : IEEE.Single;
  BEGIN
    d := NEW(IEEE.Double).init(v);
    w := d.asNormalizedGFP();
    
    (* boundaryMinus and boundaryPlus are the boundaries between v and its
       closest floating-point neighbors. Any number strictly between
       boundaryMinus and boundaryPlus will round to v when convert to a LONGREAL.
       Grisu3 will never output representations that lie exactly on a boundary.*)  
    IF mode = FastDtoaMode.FAST_DTOA_SHORTEST THEN
      d.normalizedBoundaries(boundaryMinus, boundaryPlus);
    ELSE
      <*ASSERT mode = FastDtoaMode.FAST_DTOA_SHORTEST_SINGLE *>
      real := FLOAT(v,REAL);
      s := NEW(IEEE.Single).init(real);
      s.normalizedBoundaries(boundaryMinus, boundaryPlus);
    END;

    (* we dont assert on this next line *)
    IF boundaryPlus.e() # w.e() THEN RETURN FALSE; END;
    
    tenMkMinBinExp := MinTargetExponent - (w.e() + SignificandSize);
    tenMkMaxBinExp := MaxTargetExponent - (w.e() + SignificandSize);

    CP.GetCachedPowerForBinaryExponentRange(tenMkMinBinExp,tenMkMaxBinExp,
                                            tenMk, mk);

    <*ASSERT (MinTargetExponent <= w.e() + tenMk.e() + SignificandSize) AND
             (MaxTargetExponent >= w.e() + tenMk.e() + SignificandSize) *>
              
    (* Note that tenMk is only an approximation of 10^-k. A GFP only contains a
       64 bit significand and tenMk is thus only precise up to 64 bits.

       The GFP__Times procedure rounds its result, and tenMk is approximated
       too. The variable wScaled (as well as scaledBoundaryMinus/plus) are now
       off by a small amount.
       In fact: wScaled - w*10^k < 1ulp (unit in the last place) of wScaled.
       In other words: let f = wScaled.f() and e = wScaled.e(), then
         (f-1) * 2^e < w*10^k < (f+1) * 2^e *)
         
    wScaled := w.times(tenMk);

    <* ASSERT wScaled.e() = boundaryPlus.e() + tenMk.e() + SignificandSize *>
    
    (* In theory it would be possible to avoid some recomputations by computing
       the difference between w and boundaryMinus/plus (a power of 2) and to
       compute scaledBoundaryMinus/plus by subtracting/adding from
       wScaled. However the code becomes much less readable and the speed
       enhancements are not terrific. *)

    scaledBoundaryMinus := boundaryMinus.times(tenMk);
    scaledBoundaryPlus  := boundaryPlus.times(tenMk);

    (* DigitGen will generate the digits of wScaled. Therefore we have
       v = (LONGREAL) (wScaled * 10^-mk).
       Set decimalExp = -mk and pass it to DigitGen. If wScaled is not an
       integer than it will be updated. For instance if wScaled = 1.23 then
       the buffer will be filled with "123" und the decimalExp will be
       decreased by 2. *)
    result := DigitGen(scaledBoundaryMinus, wScaled, scaledBoundaryPlus,
                       buffer, length, kappa);
    decimalExp := -mk + kappa;
    RETURN result;
  END Grisu3;

(* The "counted" version of grisu3 (see above) only generates requestedDigits
   number of digits. This version does not generate the shortest representation,
   and with enough requested digits 0.1 will at some point print as 0.9999999...
   Grisu3 is too imprecise for real halfway cases (1.5 will not work) and
   therefore the rounding strategy for halfway cases is irrelevant. *)
 
PROCEDURE Grisu3Counted(v : LONGREAL;
                        requestedDigits : INTEGER;
                        VAR buffer : DigitArr;
                        VAR length : CARDINAL;
                        VAR decimalExp : INTEGER) : BOOLEAN =
  VAR
    w,tenMk,wScaled : GFP;
    tenMkMaxBinExp,tenMkMinBinExp,mk,kappa : INTEGER;
    result : BOOLEAN;
    d : IEEE.Double;
  BEGIN
    d := NEW(IEEE.Double).init(v);
    w := d.asNormalizedGFP();

    tenMkMinBinExp := MinTargetExponent - (w.e() + SignificandSize);
    tenMkMaxBinExp := MaxTargetExponent - (w.e() + SignificandSize);
    
    CP.GetCachedPowerForBinaryExponentRange(tenMkMinBinExp,tenMkMaxBinExp,
                                            tenMk, mk);
      
    <*ASSERT (MinTargetExponent <= w.e() + tenMk.e() + SignificandSize) AND
             (MaxTargetExponent >= w.e() + tenMk.e() + SignificandSize) *>

    (* Note that tenMk is only an approximation of 10^-k. A GFP only contains a
       64 bit significand and tenMk is thus only precise up to 64 bits.

       The GFP::Times procedure rounds its result, and tenMk is approximated
       too. The variable wScaled (as well as scaledBoundaryMinus/plus) are now
       off by a small amount.
       In fact: wScaled - w*10^k < 1ulp (unit in the last place) of wScaled.
       In other words: let f = wScaled.f() and e = wScaled.e(), then
             (f-1) * 2^e < w*10^k < (f+1) * 2^e *)
    wScaled := w.times(tenMk);
    
    (* We now have (LONGREAL) (wScaled * 10^-mk).
       DigitGen will generate the first requestedDigits digits of wScaled and
       return together with a kappa such that wScaled ~= buffer * 10^kappa. (It
       will not always be exactly the same since DigitGenCounted only produces a
       limited number of digits.) *)
    result := DigitGenCounted(wScaled, requestedDigits, buffer, length, kappa);
    decimalExp := -mk + kappa;
    RETURN result;
  END Grisu3Counted;

PROCEDURE FastDtoa(v : LONGREAL;
                   mode : FastDtoaMode;
                   requestedDigits : INTEGER;
                   VAR buffer : DigitArr;
                   VAR length : CARDINAL;
                   VAR decimalPoint : INTEGER) : BOOLEAN =
  VAR
    result : BOOLEAN := FALSE;
    decimalExp : INTEGER := 0;
    d : IEEE.Double;
  BEGIN

  (* temporarily disable for NT386 until problems investigated *)
  IF Compiler.ThisPlatform = Compiler.Platform.NT386 THEN RETURN FALSE END;

    <*ASSERT v > 0.0D0, "Grisu input must be greater than 0.0D0"  *>
    
    d := NEW(IEEE.Double).init(v);
    <*ASSERT NOT d.isSpecial() *>
    
    CASE mode OF
    | FastDtoaMode.FAST_DTOA_SHORTEST, FastDtoaMode.FAST_DTOA_SHORTEST_SINGLE =>
      result := Grisu3(v, mode, buffer, length, decimalExp);
    | FastDtoaMode.FAST_DTOA_PRECISION =>
      result := Grisu3Counted(v, requestedDigits, buffer, length, decimalExp);
    END;
    IF result THEN
      decimalPoint := length + decimalExp - 1;
    END;
    RETURN result;
  END FastDtoa;

BEGIN
END Grisu.
