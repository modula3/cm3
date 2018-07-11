MODULE M3CBE_C_amd64;

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

IMPORT M3CBackEnd_C_cc;
FROM M3CBackEnd_C_cc IMPORT a32, a64, a16, a8, minAlignment, recAlignment,
  arrayAlignment, ptrA, ptrS, realA, realS, longRealA, longRealS, intA, intS,
  longintA, longintS, wideCharA, wideCharS, target;

CONST
  amd64 = "amd64";

PROCEDURE Init() RAISES {}=
  BEGIN
    a64 := 64; a32 := 32; a16 := 16; a8 := 8;
    minAlignment := 8; recAlignment := 8; arrayAlignment := 8;
    ptrA := a64; realA := a32; longRealA := a64;
    wideCharA := 16; intA := a64; longintA := a64;
    ptrS := 64; realS := 32; longRealS := 64;
    wideCharS := 16; intS := 64; longintS := 64;
    target := amd64;
  END Init;

BEGIN
  M3CBackEnd_C_cc.RegisterTarget(amd64, Init);
END M3CBE_C_amd64.
