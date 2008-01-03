(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

(* Last modified on Fri Jul 30 10:19:31 PDT 1993 by wobber  *)

INTERFACE Export;

IMPORT PackageObj;

TYPE T = PackageObj.ExportLink;

PROCEDURE Compare(a, b: T) : [-1 .. 1];

END Export.

