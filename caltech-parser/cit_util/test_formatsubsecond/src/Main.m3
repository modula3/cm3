(*
   Copyright (c) 2010 Generation Capital Ltd.
   All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>
*)

(* $Id: Main.m3,v 1.1 2010/07/04 22:45:28 mika Exp $ *)

MODULE Main;
IMPORT TZ, IO, Random;

PROCEDURE Put(t : TEXT) =
  BEGIN IO.Put(t & "\n") END Put;

VAR
  tz := TZ.New("America/Los_Angeles");
  rand := NEW(Random.Default).init();
BEGIN
  Put(TZ.FormatSubsecond(tz, 1.0d9 + 0.9991d0, 3, TRUE, TRUE, TRUE));
  Put(TZ.FormatSubsecond(tz, 1.0d9 + 0.9999d0, 3, TRUE, TRUE, TRUE));

  FOR i := 1 TO 100000 DO
    Put(TZ.FormatSubsecond(tz, rand.longreal(1.0d9, 1.1d9), 2,TRUE,TRUE,TRUE))
  END
END Main.
