GENERIC INTERFACE EulerSum(R);
(*Copyright (c) 1995, Harry George

   Abstract: Approximately sum up the function values
   f(0)+f(1)+f(2)+...+f(x) for given f and x, x might be a fraction

   This is an asymptotic expansion, it may not converge but there are
   possibly good approximations before it starts to diverge

   *** seems to be untested, maybe unfunctional *** *)

PROCEDURE EulerSum (VAR sum  : R.T;       (*partial sum to date*)
                        term : R.T;       (*jth value*)
                        jterm: CARDINAL;  (*which j*)
                    VAR nterm: CARDINAL (*how high is n so far*)
  );

(*========================*)
END EulerSum.
