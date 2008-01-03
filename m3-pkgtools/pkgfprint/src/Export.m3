(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

(* Last modified on Fri Jul 30 10:23:07 PDT 1993 by wobber  *)

MODULE Export;

IMPORT NetPath;

PROCEDURE Compare(a, b: T) : [-1 .. 1] =
  BEGIN
    RETURN NetPath.Compare(a.link, b.link);
  END Compare;

BEGIN
END Export.

