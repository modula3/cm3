(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:49:26 PST 1992 by muller   *)
(*      modified on Sun Nov 10 22:38:49 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:56:02 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE TypeInTest EXPORTS Main;

IMPORT TypeInVBT, Trestle, HVSplit, Axis, Wr, Stdio, TrestleComm;

<*FATAL ANY*>

VAR v, w := HVSplit.Cons(Axis.T.Ver, TypeInVBT.New(), TypeInVBT.New());

BEGIN
  TRY
  Trestle.Install(v);
  Trestle.Install(w);
  Trestle.AwaitDelete(v);
  Trestle.AwaitDelete(w)
  EXCEPT
    TrestleComm.Failure => 
      Wr.PutText(Stdio.stderr, "Can't contact trestle server")
  END
END TypeInTest.
