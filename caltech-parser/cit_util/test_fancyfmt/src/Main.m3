(*
   Copyright (c) 2008 Generation Capital Ltd.
   All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>
*)

(* $Id: Main.m3,v 1.2 2008/06/07 06:45:04 mika Exp $ *)

MODULE Main;
IMPORT FancyFmt, IO;

PROCEDURE Put(t : TEXT) =
  BEGIN IO.Put(t & "\n") END Put;

BEGIN
  WITH ints = ARRAY OF INTEGER { -1000*1000*1000, -1000*1000,-100*1000,-10*1000,-1000,-100,-10,-1,0,1,10,100,1000,10*1000,100*1000,1000*1000,100*1000*1000,1000*1000*1000 } DO
    FOR i := FIRST(ints) TO LAST(ints) DO
      Put(FancyFmt.Int(ints[i]))
    END
  END;

  Put(FancyFmt.LongReal(123456.7890123456789d5))

END Main.
