GENERIC INTERFACE EulerSum(R);
(*
Abstract:  Approximately sum up the function values
           f(0)+f(1)+f(2)+...+f(x)
           for given f and x, x might be a fraction
           This is an asymptotic expansion,
           it may not converge but there are possibly
           good approximations before it starts to diverge

12/13/95   Harry George      Initial version
1/22/96    Harry George      Change to m3na project
*)

PROCEDURE EulerSum
                (VAR sum:R.T;       (*partial sum to date*)
                    term:R.T;       (*jth value*)
                   jterm:CARDINAL;  (*which j*)
               VAR nterm:CARDINAL   (*how high is n so far*)
                        );

(*========================*)
END EulerSum.
