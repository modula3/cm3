(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DFATrans.m3,v 1.2 2001-09-19 15:05:08 wagner Exp $ *)

MODULE DFATrans;
IMPORT Fmt;

PROCEDURE Equal(a,b:T):BOOLEAN=
  BEGIN
    RETURN a.keyBegin = b.keyBegin AND a.keyEnd = b.keyEnd
       AND a.target = b.target;
  END Equal;

PROCEDURE Hash(a: T): INTEGER = 
  BEGIN
    RETURN ORD(a.keyBegin)*3 + ORD(a.keyEnd)*5 + a.target;
  END Hash;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    RETURN Fmt.Int(ORD(a.keyBegin)) & ".." &
           Fmt.Int(ORD(a.keyEnd)) & "->" &
           Fmt.Int(a.target) & ";" &
           Fmt.Int(a.prio);
  END Format;

BEGIN END DFATrans.
