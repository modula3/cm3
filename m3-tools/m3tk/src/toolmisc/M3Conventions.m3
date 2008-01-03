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

MODULE M3Conventions;

IMPORT Text, TextExtras, Pathname, M3Time;

PROCEDURE IsStandard(name: TEXT): BOOLEAN RAISES {} =
  BEGIN
    RETURN (name = NIL) OR Text.Equal(ModuleName(name), Standard);
  END IsStandard;

PROCEDURE ModuleName(name: TEXT): TEXT RAISES {} =
  VAR 
    index: CARDINAL;
  BEGIN
    index := 0;
    (* strip leading pathnames and trailing extensions *)
    name := Pathname.LastBase(name);
    (* strip trailing version tags -xxx *)
    IF TextExtras.FindChar(name, VersionSep, index) THEN
      name := TextExtras.Extract(name, 0, index);
    END;
    RETURN name;
  END ModuleName;

REVEAL
  CompTime = CompTime_public BRANDED OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(<*UNUSED*> c: CompTime): CompTime =
  BEGIN
    RETURN NEW(CompTime, open := M3Time.Zero(), parse := M3Time.Zero(),
               semantic := M3Time.Zero());
  END Init;

BEGIN

END M3Conventions.
