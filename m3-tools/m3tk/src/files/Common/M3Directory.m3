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

MODULE M3Directory;

IMPORT
  M3Extension, FS, Pathname, OSError;

REVEAL
  Iter = Public BRANDED OBJECT
    d: Pathname.T; (* name of directory we're iterating *)
    e: M3Extension.TSet; (* extensions this iterator is interested in *)
    dirIter: FS.Iterator;
  OVERRIDES
    next := Next;
    close := Close;
  END;

(*PUBLIC*)
PROCEDURE NewIter(
    d: Pathname.T;
    exts: M3Extension.TSet
      := M3Extension.TSet{FIRST(M3Extension.T)..LAST(M3Extension.T)})
    : Iter RAISES {OSError.E} =
  BEGIN
    RETURN NEW(Iter, d := d, e := exts, dirIter := FS.Iterate(d));
  END NewIter;

PROCEDURE Close(t: Iter)=
  BEGIN
    t.dirIter.close();
  END Close;

(*PUBLIC*)
PROCEDURE Next(
    t: Iter;
    VAR name: TEXT;
    VAR ext: M3Extension.T)
    : BOOLEAN  =
  VAR
    p: Pathname.T;
  BEGIN
    WHILE t.dirIter.next(p) DO
      IF M3Extension.Has(p, ext) THEN
        IF ext IN t.e THEN
          name := Pathname.LastBase(p);
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END Next;

BEGIN
END M3Directory.
