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

(* "M3Extension" provides operations on filename extensions. *)

INTERFACE M3Extension;

TYPE
  T =
    {Int,       (* interface source *)
     IntG,      (* generic interface source *)
     PInt,      (* pickled interface (AST) *)
     PIntR,     (* pickled interface, reduced AST *)
     Mod,       (* module source *)
     ModG,      (* generic module source *)
     PMod,      (* pickled module (AST)  *)
     PModR,     (* pickled module, reduced AST *)
     IObj,      (* (interface) object *)
     MObj,      (* (module) object *)
     Exe,       (* executable *)
     IC,        (* (interface) C intermediate *)
     MC,        (* (module) C intermediate *)
     IAsm,      (* (interface) assembly code *)
     MAsm,      (* (module) assembly code *)
     IX,        (* (interface) pre-linker input *)
     MX,        (* (module) pre-linker input *)
     ObjLib,    (* object library *)
     ObjLibX,   (* pre-linker input library *)
     Tmp,       (* temporary file *)
     Null};     (* no extension *)

  TSet = SET OF T;

(* There are a number of extensions commonly used by an M3 system, captured
by the enumeration type "T". Not all the extension types may be utilised
by a given implementation. The actual characters used to denote a "T"
may vary between implementations. *)

CONST
  Ints = TSet{T.Int, T.IntG};
  Mods = TSet{T.Mod, T.ModG};
  All = TSet{FIRST(T)..LAST(T)};

PROCEDURE ToText(t: T): TEXT;
(* Returns the "TEXT" corresponding to "t" *)

PROCEDURE FromText(text: TEXT; VAR t: T): BOOLEAN;
(* If "text" is a valid Modula-3 extension, set "t" and return "TRUE".
Otherwise, leave "t" unchanged and return "FALSE". *)

PROCEDURE Extend(name: TEXT; t: T): TEXT;
(* If "t = T.Null", then return "Pathname.Base(name)" else
return "Pathname.Join(NIL, Pathname.Base(name), ToText(t))". *)

PROCEDURE Has(name: TEXT; VAR t: T): BOOLEAN;
(* Returns "FromText(Pathname.LastExt(name), t)". *)

END M3Extension.

