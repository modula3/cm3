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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


(* Version which support DEC SRC Modula-3 extensions *)

MODULE M3Extension;

IMPORT Pathname, TextRefTbl;


VAR
  exts_g := NEW(TextRefTbl.Default).init();

PROCEDURE ToText(t: T): TEXT  =
  BEGIN
    CASE t OF
      T.Int =>   RETURN "i3";
    | T.IntG => RETURN "ig";
    | T.PInt =>  RETURN "pi";
    | T.PIntR => RETURN "pir";
    | T.Mod =>   RETURN "m3";
    | T.ModG => RETURN "mg";
    | T.PMod =>  RETURN "pm";
    | T.PModR => RETURN "pmr";
    | T.IObj => RETURN "io";
    | T.MObj => RETURN "mo";
    | T.Exe => RETURN "out";
    | T.MC => RETURN "mc";
    | T.IC => RETURN "ic";
    | T.MX => RETURN "mx";
    | T.IX => RETURN "ix";
    | T.MAsm => RETURN "ms";
    | T.IAsm => RETURN "is";
    | T.Tmp =>  RETURN "tmp";
    | T.ObjLib => RETURN "a";
    | T.ObjLibX => RETURN "ax";
    | T.Null => RETURN "";
    END;
  END ToText;



PROCEDURE FromText(text: TEXT; VAR t: T): BOOLEAN=
  VAR
    ref: REFANY;
  BEGIN
    IF exts_g.get(text, ref) THEN
      t := NARROW(ref, REF T)^;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END FromText;


PROCEDURE Extend(name: TEXT; t: T): TEXT=
  VAR b := Pathname.Base(name);
  BEGIN
    IF t = T.Null THEN RETURN b;
    ELSE RETURN Pathname.Join(NIL, b, ToText(t));
    END;
  END Extend;


PROCEDURE Has(name: TEXT; VAR t: T): BOOLEAN=
  BEGIN
    RETURN FromText(Pathname.LastExt(name), t);
  END Has;

EXCEPTION Fatal;

PROCEDURE Init() =
  BEGIN
    FOR i := FIRST(T) TO LAST(T) DO
      IF exts_g.put(ToText(i), NewRefT(i)) THEN
        <*FATAL Fatal*> BEGIN RAISE Fatal; END;
      END;
    END; (* for *)
  END Init;

PROCEDURE NewRefT(t: T): REF T=
  VAR
    r: REF T;
  BEGIN
    r := NEW(REF T);
    r^ := t;
    RETURN r;    
  END NewRefT;


BEGIN
  Init();
END M3Extension.




