MODULE M3Time;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT Fmt, Time;

REVEAL
  T = BRANDED REF RECORD t: LONGREAL END;

PROCEDURE Now(): T RAISES {} =
  BEGIN
    RETURN NEW(T, t := Time.Now());
  END Now;


PROCEDURE Interval(t: T): T RAISES {} =
  VAR nt: T := Now();
  BEGIN
    nt.t := nt.t -  t.t;
    RETURN nt;
  END Interval;


PROCEDURE Add(t1, t2: T): T RAISES {} =
  BEGIN
    RETURN NEW(T, t := t1.t + t2.t);
  END Add;


PROCEDURE AsString(t: T): TEXT RAISES {} =
  BEGIN
    RETURN Fmt.LongReal(t.t, Fmt.Style.Fix, prec := 2);
  END AsString;


PROCEDURE Zero(): T RAISES {} =
  BEGIN
    RETURN NEW(T, t := 0.0d0);
  END Zero;

BEGIN
END M3Time.
