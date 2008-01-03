INTERFACE M3Time;

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

TYPE T <: REFANY;

PROCEDURE Now(): T RAISES {};

PROCEDURE Interval(t: T): T RAISES {};
(* Now() - 't'. *)

PROCEDURE Add(t1, t2: T): T RAISES {};
(* 't1' + 't2'. *)

PROCEDURE AsString(t: T): TEXT RAISES {};
(* formatted as "nn.mm". *)

PROCEDURE Zero(): T RAISES {};
(* time or interval 00.00 *)

END M3Time.
