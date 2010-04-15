(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Nov 24 09:44:51 PST 1993 by kalsow  *)
(*      modified on Fri Apr 30 17:08:11 PDT 1993 by mjordan *)
(*      modified on Tue Jan 12 13:09:41 PST 1993 by mcjones *)

MODULE TimePosix EXPORTS Time;
IMPORT Time, TimePosix;

PROCEDURE Now(): Time.T =
  BEGIN
    RETURN TimePosix.Now();
  END Now;

BEGIN
  Grain := TimePosix.ComputeGrain();
END TimePosix.
