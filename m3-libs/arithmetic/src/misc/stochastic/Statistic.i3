INTERFACE Statistic;
(*Copyright (c) 1996, m3na project

Abstract: Statistics routines

3/16/96  Harry George    Initial version
*)
FROM xUtils IMPORT Error;
IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;

(*==========================*)
TYPE
  StatRec = RECORD   min,  (*minimum*)
                     max,  (*maximum*)
                     avg,  (*average*)
                     adev, (*average deviation*)
                     sdev, (*standard deviation*)
                     var,  (*variance*)
                     skew, (*skew*)
                     kurt  (*kurtosis*)
                     :REAL64;
              END;

PROCEDURE describe(data:R.Array;
                  VAR r:StatRec) RAISES {Error};
(*Note: IF var < TINY, then skew and kurt are meaningless*)

PROCEDURE ttest(data1,data2:R.Array;
                VAR t,    (*Student's t-test*)
                    prob  (*probability of insignificance*)
                    :REAL64) RAISES {Error};
(*Do Student's t test.
Find t, which shows how close the means are, and
find prob, which is small if this similarity is unlikely to
be due to chance.  Note that their variances need to be
similar.*)

PROCEDURE ftest(data1,data2:R.Array;
            VAR f,    (*F value*)
                prob  (*probability of significance*)
                :REAL64) RAISES {Error};
(*do F-test, returning F and the probability that
a difference between vars is due to chance*)

PROCEDURE chi_sqr1
               (bins:R.Array;     (*actual bin counts*)
                ebins:R.Array;     (*expected bin counts*)
                constraints:CARDINAL:=1;
                VAR df:REAL64;    (*degrees of freedom*)
                VAR chsq:REAL64;  (*chi squared*)
                VAR prob:REAL64   (*probability of significance*)
                ) RAISES {Error};
(*bins has an integer number of events in each bin, ebins
has the expected number in each bin (possibly non integer),
contraints gives the constraint count which reduces the
df from the number of bins.  chsq then is a measure of the
difference in the bin-by-bin numbers, while prob gives the
significance of that measure.  Big chsq means big difference,
big prob means big chance this large chsq came from pure random
events.

requires: count in each bin >=5
*)

PROCEDURE chi_sqr2
               (bins1:R.Array;    (*actual bin1 counts*)
                bins2:R.Array;    (*actual bin2 counts*)
                constraints:CARDINAL:=1;
                VAR df:REAL64;    (*degrees of freedom*)
                VAR chsq:REAL64;  (*chi squared*)
                VAR prob:REAL64   (*probability of significance*)
                ) RAISES {Error};
(*bins1 and bins2 have an integer number of events in each bin,
contraints gives the constraint count which reduces the
df from the number of bins.  chsq then is a measure of the
difference in the bin-by-bin numbers, while prob gives the
significance of that measure.  Big chsq means big difference,
big prob means big chance this large chsq came from pure random
events.

requires: count in each bin >=5
*)

(*==========================*)
END Statistic.
