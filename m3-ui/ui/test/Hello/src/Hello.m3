(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 14:01:10 PST 1992 by muller   *)
(*      modified on Sun Nov 10 22:39:15 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:55:11 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE Hello EXPORTS Main;
IMPORT TextVBT, Trestle;

<*FATAL ANY*>

VAR v := TextVBT.New("Hello, world");

BEGIN
Trestle.Install(v);
Trestle.AwaitDelete(v)
END Hello.
