GENERIC MODULE EulerSum(RT,R);
(*Copyright (c) 1995, Harry George

Abstract: Implementation of Modula-3 version of
          NR92, ch 5.

12/27/95  Harry George    Initial version
*)

CONST Module = "EulerSum.";
(*----------------------*)
PROCEDURE EulerSum
                (VAR sum:R.T;       (*partial sum to date*)
                    term:R.T;       (*jth value*)
                   jterm:CARDINAL;  (*which j*)
               VAR nterm:CARDINAL   (*how high is n so far*)
                        ) =
(*Used to evaluate summation series.  The caller has to provide
vars for wksp and nterm, but never modify them directly.
Start with jterm:=1 and term:=<first value>.  Then for
each call thereafter do the next jterm and term, in order.
Sum remains up to date, so you can quit whenever your exit criteria
are met.
*)
<*UNUSED*> CONST ftn = Module & "EulerSum";
VAR
  tmp1,tmp2:R.T;
  wksp:=NEW(REF ARRAY OF R.T,nterm+1);    (*workspace for calcs*)
BEGIN
  IF jterm=1 THEN
    nterm:=1;
    wksp[1]:=term;
    sum:=RT.Half*term;
  ELSE
    tmp1:=wksp[1];
    FOR j:=1 TO nterm-1 DO
      tmp2:=wksp[j+1];
      wksp[j+1]:=RT.Half*(wksp[j]+tmp1);
      tmp1:=tmp2;
    END;
    wksp[nterm+1]:=RT.Half*(wksp[nterm]+tmp1);
    IF ABS(wksp[nterm+1]) < ABS(wksp[nterm]) THEN
      (*increase p, and use longer table*)
      INC(nterm);
      sum:=sum+RT.Half*wksp[nterm];
    ELSE
      (*increase n, table doesn't change*)
      sum:=sum+wksp[nterm+1];
    END;
  END;
END EulerSum;
(*==========================*)
BEGIN
END EulerSum.
