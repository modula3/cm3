INTERFACE Statistic;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Statistics routines *)

FROM Arithmetic IMPORT Error;
IMPORT LongRealBasic AS R;

TYPE
  AvrgVar = RECORD
              avrg,              (* average *)
                var: R.T;        (* variance *)
            END;

  T = RECORD
        avrgVar: AvrgVar;
        min,                     (* minimum *)
          max,                   (* maximum *)
          adev,                  (* average deviation *)
          sdev,                  (* standard deviation *)
          skew,                  (* skew *)
          kurt: R.T;             (* kurtosis *)
      END;

PROCEDURE FromData (READONLY data: ARRAY OF R.T; ): T;
(* Note: IF var < TINY, then skew and kurt are meaningless *)

PROCEDURE ComputeAvrgVar (READONLY data: ARRAY OF R.T; ): AvrgVar;

TYPE
  TTestResult = RECORD
                  t,             (* Student's t-test *)
                    prob: R.T;   (* probability of insignificance *)
                END;

PROCEDURE TTest (READONLY data1, data2: ARRAY OF R.T; ): TTestResult
  RAISES {Error};
(* Do Student's t test.  Find t, which shows how close the means are, and
   find prob, which is small if this similarity is unlikely to be due to
   chance.  Note that their variances need to be similar. *)

TYPE
  FTestResult = RECORD
                  f,             (* F value *)
                    prob: R.T;   (* probability of significance *)
                END;

PROCEDURE FTest (READONLY data1, data2: ARRAY OF R.T; ): FTestResult
  RAISES {Error};
(* do F-test, returning F and the probability that a difference between
   vars is due to chance *)

TYPE
  ChiSqrResult = RECORD
                   df  : R.T;    (* degrees of freedom *)
                   chsq: R.T;    (* chi squared *)
                   prob: R.T;    (* probability of significance *)
                 END;

PROCEDURE ChiSqr1 (READONLY bins : ARRAY OF R.T;  (* actual bin counts *)
                   READONLY ebins: ARRAY OF R.T;  (* expected bin counts *)
                   constraints: CARDINAL := 1; ): ChiSqrResult
  RAISES {Error};
(* bins has an integer number of events in each bin, ebins has the expected
   number in each bin (possibly non integer), contraints gives the
   constraint count which reduces the df from the number of bins.  chsq
   then is a measure of the difference in the bin-by-bin numbers, while
   prob gives the significance of that measure.  Big chsq means big
   difference, big prob means big chance this large chsq came from pure
   random events.

   requires: count in each bin >=5 *)

PROCEDURE ChiSqr2 (READONLY bins1: ARRAY OF R.T;  (* actual bin1 counts *)
                   READONLY bins2: ARRAY OF R.T;  (* actual bin2 counts *)
                   constraints: CARDINAL := 1; ): ChiSqrResult
  RAISES {Error};
(* bins1 and bins2 have an integer number of events in each bin, contraints
   gives the constraint count which reduces the df from the number of bins.
   chsq then is a measure of the difference in the bin-by-bin numbers,
   while prob gives the significance of that measure.  Big chsq means big
   difference, big prob means big chance this large chsq came from pure
   random events.

   requires: count in each bin >=5 *)

END Statistic.
